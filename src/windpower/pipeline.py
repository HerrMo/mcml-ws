"""End-to-end analysis: load data, fit models, write figures and tables.

The results are presented on the report page of the documentation.
"""

from __future__ import annotations

import logging

import matplotlib as mpl
import matplotlib.pyplot as plt
import pandas as pd
from joblib import Memory

from windpower import data_power, data_wind, model, plot
from windpower.config import Config
from windpower.evaluate import bootstrap_rmse, derive_seeds, rmse

log = logging.getLogger(__name__)


def load_tables(cfg: Config) -> tuple[pd.DataFrame, pd.DataFrame]:
    """Read the energy data and the gridded wind data."""
    stations_grid = data_wind.get_stations_grid(
        data_wind.read_data_stations_raw(cfg.paths.stations, cfg.dates, cfg.grid), cfg.grid
    )
    energy = data_power.read_data_energy(cfg.paths.power)
    wind = data_wind.get_wind_in_grid(
        data_wind.read_data_wind_raw(cfg.paths.measure), stations_grid
    )
    return energy, wind


def run(cfg: Config) -> pd.DataFrame:
    """Run the full analysis.

    Intermediate results are cached with :class:`joblib.Memory` in
    ``cfg.paths.intermediate``. The cache key contains all function arguments,
    so every random seed has to be passed explicitly. The seeds of all random
    components are derived from ``cfg.model.seed`` with :func:`derive_seeds`.

    Returns:
        Root mean squared error of each model on the prediction period, with
        bootstrap interval.
    """
    mpl.use("Agg")
    memory = Memory(cfg.paths.intermediate, verbose=0)
    out = cfg.paths.plots
    out.mkdir(parents=True, exist_ok=True)

    def save(fig, name: str) -> None:
        fig.savefig(out / f"{name}.png", dpi=150)
        plt.close(fig)

    log.info("loading data")
    stations_raw = data_wind.read_data_stations_raw(cfg.paths.stations, cfg.dates, cfg.grid)
    energy, wind = memory.cache(load_tables)(cfg)
    energy_daily = data_power.get_energy_daily(energy)

    save(plot.plot_wind_daily(wind), "wind_speed")
    save(plot.plot_power_daily(energy_daily), "line_total_comp")
    save(plot.plot_wind_stations(stations_raw, cfg.grid, cfg.paths.germany), "grid_on_map")
    save(
        plot.plot_hourly(data_power.convert_energy_to_power(energy), wind), "line_in_day_with_wind"
    )

    log.info("fitting models")
    d = cfg.dates
    wind_wide = model.wind_to_wide(wind)
    modelinput = model.build_model_data(wind_wide, energy, d.start_train, d.end_train)
    fit_penalized = memory.cache(model.fit_model_penalized)
    folds = cfg.model.cv_folds
    operators = sorted(energy["operator"].unique())
    seeds = derive_seeds(cfg.model.seed, operators)

    model_lm = model.fit_model_lm(modelinput)
    model_penalized = fit_penalized(modelinput, cv_folds=folds, seed=seeds.cv)
    save(
        plot.col_square(
            model_lm.coef, stations_raw, cfg.grid, cfg.paths.germany, "Linear Model Coefficients"
        ),
        "squares_lm",
    )
    save(
        plot.col_square(
            model_penalized.coef,
            stations_raw,
            cfg.grid,
            cfg.paths.germany,
            "Penalized Linear Model Coefficients",
        ),
        "squares_penalized",
    )

    for operator, seed_op in seeds.operators.items():
        energy_op = energy[energy["operator"] == operator]
        md = model.build_model_data(wind_wide, energy_op, d.start_train, d.end_train)
        fit = fit_penalized(md, cv_folds=folds, seed=seed_op)
        save(
            plot.col_square(fit.coef, stations_raw, cfg.grid, cfg.paths.germany, operator),
            f"squares_{operator}",
        )

    log.info("predicting")
    data_predict = model.build_model_data(
        wind_wide, energy, d.start_predict, d.end_predict, keep_datetime=True
    )
    predictions = {
        "ground truth": data_predict["wind_energy"].to_numpy(),
        "lm prediction": model.predict_model(model_lm, data_predict),
        "penalized prediction": model.predict_model(model_penalized, data_predict),
        "random forest prediction": memory.cache(model.fit_predict_model_forest)(
            modelinput, data_predict, n_estimators=cfg.model.n_estimators, seed=seeds.forest
        ),
    }
    save(
        plot.plot_predicted_wind_energy(data_predict["datetime"], predictions),
        "wind_power_prediction",
    )

    truth = predictions.pop("ground truth")
    metrics = pd.DataFrame(
        [
            {
                "model": name,
                "rmse": rmse(truth, p),
                **dict(
                    zip(
                        ["rmse_lower", "rmse_upper"],
                        bootstrap_rmse(truth, p, n_boot=cfg.model.n_boot, seed=seeds.bootstrap),
                        strict=True,
                    )
                ),
            }
            for name, p in predictions.items()
        ]
    )
    metrics.to_csv(out / "metrics.csv", index=False)
    return metrics
