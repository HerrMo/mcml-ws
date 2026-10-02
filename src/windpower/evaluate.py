"""Evaluation of predictions."""

from __future__ import annotations

import numpy as np


def rmse(y: np.ndarray, y_hat: np.ndarray) -> float:
    return float(np.sqrt(np.mean((np.asarray(y) - np.asarray(y_hat)) ** 2)))


def spawn_seeds(seed: int, n: int) -> list[int]:
    """Derive ``n`` independent integer seeds from a single seed.

    Uses :meth:`numpy.random.SeedSequence.spawn`, so the child streams are
    statistically independent (unlike ``seed, seed + 1, ...``). Integers are
    returned because scikit-learn's ``random_state`` does not accept
    :class:`numpy.random.Generator` objects.

    Examples:
        >>> spawn_seeds(1, 2) == spawn_seeds(1, 2)
        True
        >>> len(set(spawn_seeds(1, 100)))
        100
    """
    return [int(child.generate_state(1)[0]) for child in np.random.SeedSequence(seed).spawn(n)]


# Exercise 05
#
# Implement a nonparametric bootstrap for the RMSE: draw `n_boot` samples of
# size n with replacement from the pairs (y_i, y_hat_i), compute the RMSE of
# each, and return the `level` percentile interval of the bootstrap
# replicates.
#
# Use a NumPy Generator created from `seed` with `np.random.default_rng()`.
# Do not use `np.random.seed()` or the functions `np.random.choice()`,
# `np.random.randint()`, ... of the legacy global interface.
# `tests/test_evaluate.py` checks your implementation.
def bootstrap_rmse(
    y: np.ndarray, y_hat: np.ndarray, n_boot: int, seed: int, level: float = 0.95
) -> tuple[float, float]:
    """Percentile bootstrap interval for the RMSE.

    Resamples observations independently; this ignores the autocorrelation of
    the time series and understates the uncertainty.

    Args:
        y: Observed values.
        y_hat: Predicted values.
        n_boot: Number of bootstrap replicates.
        seed: Seed of the random number generator.
        level: Nominal coverage of the interval.

    Returns:
        Lower and upper interval bound.
    """
    # SOLUTION
    rng = np.random.default_rng(seed)
    y, y_hat = np.asarray(y), np.asarray(y_hat)
    idx = rng.integers(0, len(y), size=(n_boot, len(y)))
    replicates = np.sqrt(np.mean((y[idx] - y_hat[idx]) ** 2, axis=1))
    alpha = 1 - level
    lower, upper = np.quantile(replicates, [alpha / 2, 1 - alpha / 2])
    return float(lower), float(upper)
    # SOLUTION END
