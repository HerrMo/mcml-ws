"""Power generation data of the four German transmission system operators."""

from __future__ import annotations

import zipfile
from pathlib import Path

import pandas as pd

from windpower.utils import assert_columns

_WIND_COLS = ["Wind Offshore [MWh] Originalauflösungen", "Wind Onshore [MWh] Originalauflösungen"]


def _read_operator_csv(fh, operator: str) -> pd.DataFrame:
    df = pd.read_csv(
        fh,
        sep=";",
        decimal=",",
        thousands=".",
        na_values="-",
        encoding="utf-8-sig",
        dtype={"Datum": str, "Anfang": str},
    )
    local = pd.to_datetime(df["Datum"] + " " + df["Anfang"], format="%d.%m.%Y %H:%M")
    # the hour 02:00-03:00 appears twice when daylight saving time ends; rows are
    # ordered, so pandas can infer which occurrence is which.
    datetime = local.dt.tz_localize(
        "Europe/Berlin", ambiguous="infer", nonexistent="shift_forward"
    ).dt.tz_convert("UTC")
    wind = df.reindex(columns=_WIND_COLS).fillna(0).sum(axis=1)
    return pd.DataFrame({"datetime": datetime, "operator": operator, "wind_energy": wind})


def read_data_energy(file_power: str | Path) -> pd.DataFrame:
    """Load historical wind power generation of major German TSOs.

    Args:
        file_power: Zip archive with one SMARD.de CSV export per operator.

    Returns:
        Data frame with columns ``datetime`` (UTC), ``operator`` and
        ``wind_energy`` (energy produced in a 15-minute interval, MWh).
    """
    with zipfile.ZipFile(file_power) as zf:
        tables = [
            _read_operator_csv(zf.open(name), Path(name).stem)
            for name in sorted(zf.namelist())
            if name.endswith(".csv")
        ]
    return pd.concat(tables, ignore_index=True)


def convert_energy_to_power(table_power: pd.DataFrame) -> pd.DataFrame:
    """Replace ``wind_energy`` (MWh per 15 min) by ``power_wind`` (average MW)."""
    assert_columns(table_power, ["datetime", "operator", "wind_energy"])
    out = table_power.drop(columns="wind_energy")
    out["power_wind"] = table_power["wind_energy"] * 4
    return out


def get_energy_daily(wind_power: pd.DataFrame) -> pd.DataFrame:
    """Aggregate to daily total energy per operator.

    Returns:
        Data frame with columns ``date``, ``operator``, ``wind_energy`` (MWh).
    """
    assert_columns(wind_power, ["datetime", "operator", "wind_energy"])
    return (
        wind_power.assign(date=wind_power["datetime"].dt.date)
        .groupby(["date", "operator"], as_index=False)["wind_energy"]
        .sum()
    )
