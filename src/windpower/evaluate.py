"""Evaluation of predictions."""

from __future__ import annotations

from collections.abc import Sequence
from dataclasses import dataclass

import numpy as np


def rmse(y: np.ndarray, y_hat: np.ndarray) -> float:
    return float(np.sqrt(np.mean((np.asarray(y) - np.asarray(y_hat)) ** 2)))


@dataclass(frozen=True)
class Seeds:
    """Integer seeds of all random components of the analysis."""

    cv: int
    """Fold assignment of the cross-validation of the penalized model."""
    forest: int
    """Random forest."""
    bootstrap: int
    """Bootstrap interval of the RMSE."""
    operators: dict[str, int]
    """Fold assignment of the penalized model of each operator."""


def derive_seeds(seed: int, operators: Sequence[str]) -> Seeds:
    """Derive the seeds of all random components from the single seed in the configuration.

    :meth:`numpy.random.SeedSequence.spawn` creates statistically independent
    child streams (unlike ``seed, seed + 1, ...``); the per-operator seeds are
    spawned from a child of their own. Integers are returned because
    scikit-learn's ``random_state`` does not accept
    :class:`numpy.random.Generator` objects.

    Args:
        seed: Seed recorded in the configuration (``model.seed``).
        operators: Names of the operators that get a seed of their own.

    Returns:
        One seed per random component.

    Examples:
        >>> derive_seeds(1, ["a", "b"]) == derive_seeds(1, ["a", "b"])
        True
        >>> s = derive_seeds(1, ["a", "b"])
        >>> len({s.cv, s.forest, s.bootstrap, *s.operators.values()})
        5
    """
    cv, forest, bootstrap, per_operator = np.random.SeedSequence(seed).spawn(4)
    return Seeds(
        cv=_to_int(cv),
        forest=_to_int(forest),
        bootstrap=_to_int(bootstrap),
        operators={
            op: _to_int(child)
            for op, child in zip(operators, per_operator.spawn(len(operators)), strict=True)
        },
    )


def _to_int(seed_sequence: np.random.SeedSequence) -> int:
    return int(seed_sequence.generate_state(1)[0])


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
    rng = np.random.default_rng(seed)
    y, y_hat = np.asarray(y), np.asarray(y_hat)
    idx = rng.integers(0, len(y), size=(n_boot, len(y)))
    replicates = np.sqrt(np.mean((y[idx] - y_hat[idx]) ** 2, axis=1))
    alpha = 1 - level
    lower, upper = np.quantile(replicates, [alpha / 2, 1 - alpha / 2])
    return float(lower), float(upper)
