"""Models of wind power production as a function of gridded wind speed.

All linear models share the mean function

.. math::

    \\text{Power}_i = \\beta_{0,\\text{TimeOfDay}(i)}
        + \\beta_1 \\text{Wind@Grid.01}_i + \\dots + \\beta_{16} \\text{Wind@Grid.16}_i
"""

from __future__ import annotations

import datetime as dt
from dataclasses import dataclass

import numpy as np
import pandas as pd
from sklearn.ensemble import RandomForestRegressor
from sklearn.linear_model import LassoCV
from sklearn.model_selection import KFold

from windpower.utils import assert_columns

HOURS = range(24)


def grid_columns(data: pd.DataFrame) -> list[str]:
    """Sorted names of all ``grid_XX`` columns of ``data``."""
    return sorted(c for c in data.columns if c.startswith("grid_"))


def build_model_data(
    data_wind_wide: pd.DataFrame,
    data_energy: pd.DataFrame,
    date_from: dt.date,
    date_to: dt.date,
    keep_datetime: bool = False,
) -> pd.DataFrame:
    """Join wide wind data with hourly total wind energy of all operators.

    The 15-minute energy values are summed over operators and averaged within
    each hour. Rows with missing values are dropped.

    Args:
        data_wind_wide: Output of :func:`wind_to_wide`.
        data_energy: Output of :func:`windpower.data_power.read_data_energy`.
        date_from: First date (inclusive, UTC).
        date_to: Last date (inclusive, UTC).
        keep_datetime: Whether to keep the ``datetime`` column.

    Returns:
        Data frame with columns ``grid_XX``, ``wind_energy``, ``time_of_day``
        (hour, categorical) and optionally ``datetime``.
    """
    assert_columns(data_energy, ["datetime", "wind_energy"])
    day = data_energy["datetime"].dt.date
    energy = data_energy.loc[(day >= date_from) & (day <= date_to)]
    energy = energy.groupby("datetime")["wind_energy"].sum()
    energy = energy.groupby(energy.index.floor("h")).mean().rename_axis("datetime").reset_index()

    model_tbl = data_wind_wide.merge(energy, on="datetime", how="inner").dropna()
    model_tbl["time_of_day"] = pd.Categorical(model_tbl["datetime"].dt.hour, categories=HOURS)
    if not keep_datetime:
        model_tbl = model_tbl.drop(columns="datetime")
    return model_tbl.reset_index(drop=True)


# Exercise 03 (a)
#
# We want to model power production from wind in Germany. For this, we scraped
# together wind speed data of numerous measuring stations and binned the
# location of these stations into a 4x4 grid across Germany, averaging the wind
# speed for every grid cell for each point in time.
#
# To fit a model that has the average wind speed in each grid cell as a
# covariate, the data needs to be reshaped into "wide" format.
#
# Write a function that takes a data frame `data_wind` with columns "grid_id",
# "datetime" and "wind_speed" and returns a wide-format data frame with columns
# "datetime", "grid_01", "grid_02", ..., "grid_16". See
# `tests/test_model.py::test_wind_to_wide` for an example input and output.
#
# The ordering of rows or columns does not matter.
def wind_to_wide(data_wind: pd.DataFrame) -> pd.DataFrame:
    """Reshape long-format grid wind speeds to one column per grid cell.

    Args:
        data_wind: Data frame with columns ``grid_id``, ``datetime``,
            ``wind_speed``.

    Returns:
        Data frame with columns ``datetime``, ``grid_01``, ``grid_02``, ...
    """
    # SOLUTION
    wide = data_wind.assign(cell=data_wind["grid_id"].map("grid_{:02d}".format)).pivot(
        index="datetime", columns="cell", values="wind_speed"
    )
    return wide.rename_axis(columns=None).reset_index()
    # SOLUTION END


def design_matrix(data: pd.DataFrame) -> pd.DataFrame:
    """One indicator column per hour of the day, followed by the grid columns."""
    hours = pd.get_dummies(data["time_of_day"], prefix="time_of_day", dtype=float)
    return pd.concat([hours, data[grid_columns(data)]], axis=1)


@dataclass(frozen=True)
class LinearPredictor:
    """Linear predictor without global intercept.

    Attributes:
        coef: Coefficients indexed by the columns of :func:`design_matrix`.
        alpha: L1 penalty chosen by cross-validation (``None`` if unpenalized).
    """

    coef: pd.Series
    alpha: float | None = None

    def predict(self, data: pd.DataFrame) -> np.ndarray:
        return design_matrix(data)[self.coef.index].to_numpy() @ self.coef.to_numpy()


def fit_model_lm(data_modelinput: pd.DataFrame) -> LinearPredictor:
    """Least-squares fit of the linear model, with one intercept per hour of the day."""
    x = design_matrix(data_modelinput)
    beta, *_ = np.linalg.lstsq(x.to_numpy(), data_modelinput["wind_energy"].to_numpy(), rcond=None)
    return LinearPredictor(coef=pd.Series(beta, index=x.columns))


def fit_model_penalized(data_modelinput: pd.DataFrame, cv_folds: int, seed: int) -> LinearPredictor:
    """Lasso fit with non-negative grid coefficients and unpenalized hourly intercepts.

    By the Frisch-Waugh-Lovell theorem, profiling out the unpenalized hourly
    intercepts is equivalent to fitting the lasso on data that is centered
    within each hour. The penalty is chosen by K-fold cross-validation.

    Args:
        data_modelinput: Output of :func:`build_model_data`.
        cv_folds: Number of cross-validation folds.
        seed: Seed for the fold assignment.
    """
    x = data_modelinput[grid_columns(data_modelinput)]
    y = data_modelinput["wind_energy"]
    g = data_modelinput["time_of_day"]
    xc = x - x.groupby(g, observed=True).transform("mean")
    yc = y - y.groupby(g, observed=True).transform("mean")

    cv = KFold(n_splits=cv_folds, shuffle=True, random_state=seed)
    lasso = LassoCV(positive=True, fit_intercept=False, cv=cv).fit(xc, yc)
    beta = pd.Series(lasso.coef_, index=x.columns)

    intercepts = (y - x @ beta).groupby(g, observed=False).mean().fillna(0)
    intercepts.index = [f"time_of_day_{h}" for h in intercepts.index]
    return LinearPredictor(coef=pd.concat([intercepts, beta]), alpha=float(lasso.alpha_))


def predict_model(model: LinearPredictor, data_predict: pd.DataFrame) -> np.ndarray:
    """Predictions of a linear model for ``data_predict``."""
    return model.predict(data_predict)


def forest_features(data: pd.DataFrame) -> pd.DataFrame:
    """Grid columns and the hour of the day as integer feature."""
    return data[grid_columns(data)].assign(time_of_day=data["time_of_day"].astype(int))


# Exercise 03 (b)
#
# Next to the linear model and the penalized linear model, we also want to try
# a random forest.
#
# Fill out `fit_model_forest()` so that it fits a
# `sklearn.ensemble.RandomForestRegressor` with `n_estimators` trees,
# `min_samples_leaf=5` and `random_state=seed` on `forest_features(data_modelinput)`
# with response "wind_energy". Return the fitted model.
#
# Then fill out `predict_model_forest()`, which returns the vector of
# predictions for `data_predict`.
def fit_model_forest(
    data_modelinput: pd.DataFrame, n_estimators: int, seed: int
) -> RandomForestRegressor:
    """Fit a random forest regressor for ``wind_energy``."""
    # SOLUTION
    forest = RandomForestRegressor(
        n_estimators=n_estimators, min_samples_leaf=5, random_state=seed, n_jobs=-1
    )
    return forest.fit(forest_features(data_modelinput), data_modelinput["wind_energy"])
    # SOLUTION END


def predict_model_forest(
    model_forest: RandomForestRegressor, data_predict: pd.DataFrame
) -> np.ndarray:
    """Random forest predictions for ``data_predict``."""
    # SOLUTION
    return model_forest.predict(forest_features(data_predict))
    # SOLUTION END


def fit_predict_model_forest(
    data_modelinput: pd.DataFrame, data_predict: pd.DataFrame, n_estimators: int, seed: int
) -> np.ndarray:
    """Fit a random forest and return its predictions.

    Only the predictions are returned so that caching them is cheap; the fitted
    forest itself is several hundred MB.
    """
    model = fit_model_forest(data_modelinput, n_estimators=n_estimators, seed=seed)
    return predict_model_forest(model, data_predict)
