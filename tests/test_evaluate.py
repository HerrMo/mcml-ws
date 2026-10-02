import numpy as np
import pytest

from windpower.evaluate import bootstrap_rmse, rmse


@pytest.fixture
def predictions():
    rng = np.random.default_rng(0)
    y = rng.normal(size=500)
    return y, y + rng.normal(scale=0.5, size=500)


def test_rmse():
    assert rmse(np.array([0.0, 0.0]), np.array([3.0, 4.0])) == pytest.approx(np.sqrt(12.5))


def test_bootstrap_contains_estimate(predictions):
    lower, upper = bootstrap_rmse(*predictions, n_boot=500, seed=1)
    assert lower < rmse(*predictions) < upper
    assert upper - lower < 0.2


def test_bootstrap_same_seed_same_result(predictions):
    assert bootstrap_rmse(*predictions, n_boot=200, seed=1) == bootstrap_rmse(
        *predictions, n_boot=200, seed=1
    )


def test_bootstrap_different_seed_different_result(predictions):
    assert bootstrap_rmse(*predictions, n_boot=200, seed=1) != bootstrap_rmse(
        *predictions, n_boot=200, seed=2
    )


# The following tests use the legacy global state on purpose, hence the noqa.
def test_bootstrap_ignores_global_state(predictions):
    np.random.seed(1)  # noqa: NPY002
    first = bootstrap_rmse(*predictions, n_boot=200, seed=1)
    np.random.seed(2)  # noqa: NPY002
    assert bootstrap_rmse(*predictions, n_boot=200, seed=1) == first


def test_bootstrap_does_not_touch_global_state(predictions):
    np.random.seed(1)  # noqa: NPY002
    expected = np.random.random()  # noqa: NPY002
    np.random.seed(1)  # noqa: NPY002
    bootstrap_rmse(*predictions, n_boot=200, seed=1)
    assert np.random.random() == expected  # noqa: NPY002
