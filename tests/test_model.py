import numpy as np
import pandas as pd
import pytest

from windpower import model


def test_wind_to_wide():
    times = pd.to_datetime(
        ["2023-04-21 06:00", "2022-10-23 06:00", "2022-10-23 06:00", "2023-02-14 12:00",
         "2022-12-19 12:00", "2022-11-11 12:00", "2022-11-11 12:00", "2023-04-21 06:00",
         "2022-10-23 06:00", "2023-02-14 12:00", "2023-02-14 12:00", "2022-12-19 12:00",
         "2022-12-19 12:00", "2022-11-11 12:00", "2023-04-21 06:00"],
        utc=True,
    )  # fmt: skip
    data_wind = pd.DataFrame(
        {
            "grid_id": [12, 1, 7, 12, 12, 1, 7, 7, 12, 7, 1, 7, 1, 12, 1],
            "datetime": times,
            "wind_speed": [2.388889, 1.5, 1.25, 1.888889, 2.611111, 4.2, 2.333333, 2.866667,
                           2.0, 1.4375, 1.0, 1.5, 1.764706, 1.636364, 2.2],
        }
    )  # fmt: skip
    expected = pd.DataFrame(
        {
            "datetime": pd.to_datetime(
                [
                    "2023-04-21 06:00",
                    "2022-10-23 06:00",
                    "2023-02-14 12:00",
                    "2022-12-19 12:00",
                    "2022-11-11 12:00",
                ],
                utc=True,
            ),  # fmt: skip
            "grid_01": [2.2, 1.5, 1.0, 1.764706, 4.2],
            "grid_07": [2.866667, 1.25, 1.4375, 1.5, 2.333333],
            "grid_12": [2.388889, 2.0, 1.888889, 2.611111, 1.636364],
        }
    )
    result = model.wind_to_wide(data_wind)
    # ordering of rows and columns does not matter
    result = result.sort_values("datetime", ignore_index=True)[expected.columns]
    expected = expected.sort_values("datetime", ignore_index=True)
    pd.testing.assert_frame_equal(result, expected, check_names=False)


@pytest.fixture
def modelinput():
    """Simulated data from the model with known coefficients."""
    rng = np.random.default_rng(0)
    n = 2000
    x = pd.DataFrame(rng.gamma(2, 2, size=(n, 3)), columns=["grid_01", "grid_02", "grid_03"])
    hour = rng.integers(0, 24, n)
    beta = np.array([3.0, 0.0, 1.0])
    y = 10 + np.sin(hour / 24 * 2 * np.pi) + x.to_numpy() @ beta + rng.normal(0, 0.1, n)
    return x.assign(wind_energy=y, time_of_day=pd.Categorical(hour, categories=model.HOURS))


def test_fit_model_lm_recovers_coefficients(modelinput):
    fit = model.fit_model_lm(modelinput)
    np.testing.assert_allclose(fit.coef[["grid_01", "grid_02", "grid_03"]], [3, 0, 1], atol=0.01)
    assert fit.predict(modelinput).shape == (len(modelinput),)


def test_fit_model_penalized(modelinput):
    fit = model.fit_model_penalized(modelinput, cv_folds=5, seed=1)
    grid = fit.coef.filter(like="grid_")
    assert (grid >= 0).all()
    np.testing.assert_allclose(grid, [3, 0, 1], atol=0.05)
    residual = modelinput["wind_energy"] - fit.predict(modelinput)
    assert residual.std() < 0.2


def test_forest_is_reproducible(modelinput):
    kwargs = dict(data_modelinput=modelinput, data_predict=modelinput.head(50), n_estimators=20)
    p1 = model.fit_predict_model_forest(**kwargs, seed=1)
    p2 = model.fit_predict_model_forest(**kwargs, seed=1)
    p3 = model.fit_predict_model_forest(**kwargs, seed=2)
    # with n_jobs=-1 the tree predictions are summed in thread completion order, so
    # results agree only up to floating point rounding, not bitwise.
    np.testing.assert_allclose(p1, p2, rtol=1e-12)
    assert not np.allclose(p1, p3)
