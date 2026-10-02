"""Small helpers shared across modules."""

from __future__ import annotations

import pandas as pd


def get_time_of_day(t: pd.Series) -> pd.Series:
    """Time of day of a datetime series, in hours since midnight.

    Args:
        t: Series of (timezone-aware or naive) timestamps.

    Returns:
        Float series with values in ``[0, 24)``.

    Examples:
        >>> s = pd.Series(pd.to_datetime(["2023-01-01 06:30", "2023-01-02 00:00"]))
        >>> get_time_of_day(s).tolist()
        [6.5, 0.0]
    """
    return (t - t.dt.normalize()) / pd.Timedelta(hours=1)


def assert_columns(df: pd.DataFrame, required: list[str]) -> None:
    """Raise ``ValueError`` if ``df`` lacks any of the ``required`` columns."""
    missing = set(required) - set(df.columns)
    if missing:
        raise ValueError(f"missing columns: {sorted(missing)}")
