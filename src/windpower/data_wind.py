"""Wind measurements of the Deutscher Wetterdienst (DWD)."""

from __future__ import annotations

import datetime as dt
from pathlib import Path

import numpy as np
import pandas as pd

from windpower.config import Dates, Grid
from windpower.utils import assert_columns

STATION_COLUMNS = [
    "Stations_id",
    "von_datum",
    "bis_datum",
    "Stationshoehe",
    "geoBreite",
    "geoLaenge",
    "Stationsname",
    "Bundesland",
]


def read_data_stations_raw(file_stations: str | Path, dates: Dates, grid: Grid) -> pd.DataFrame:
    """Read the station list and keep stations active for the whole data range.

    Station coordinates outside of the grid are clipped to the grid boundary.

    Args:
        file_stations: Whitespace separated station description file.
        dates: Date configuration; ``start_min`` and ``end_max`` are used.
        grid: Grid configuration.

    Returns:
        Data frame with the columns in :data:`STATION_COLUMNS`.
    """
    stations = pd.read_csv(file_stations, sep=r"\s+")
    for col in ("von_datum", "bis_datum"):
        stations[col] = pd.to_datetime(stations[col].astype(str), format="%Y%m%d").dt.date
    active = (stations["von_datum"] <= dates.start_min) & (stations["bis_datum"] >= dates.end_max)
    stations = stations.loc[active].reset_index(drop=True)
    stations["geoLaenge"] = stations["geoLaenge"].clip(min(grid.bins_long), max(grid.bins_long))
    stations["geoBreite"] = stations["geoBreite"].clip(min(grid.bins_lat), max(grid.bins_lat))
    return stations


def get_stations_grid(data_stations_raw: pd.DataFrame, grid: Grid) -> pd.DataFrame:
    """Assign every station to a grid cell.

    Cells are numbered ``1, ..., n_cells`` in longitude-major order, i.e. cell 1
    is the south-western cell and cell 2 lies north of it.

    Returns:
        Data frame with columns ``stations_id``, ``geoLaenge``, ``geoBreite``,
        ``grid_id``.
    """
    assert_columns(data_stations_raw, STATION_COLUMNS)
    xcut = pd.cut(data_stations_raw["geoLaenge"], grid.bins_long, labels=False, include_lowest=True)
    ycut = pd.cut(data_stations_raw["geoBreite"], grid.bins_lat, labels=False, include_lowest=True)
    n_lat = len(grid.bins_lat) - 1
    out = pd.DataFrame(
        {
            "stations_id": data_stations_raw["Stations_id"].astype(int),
            "geoLaenge": data_stations_raw["geoLaenge"],
            "geoBreite": data_stations_raw["geoBreite"],
            "grid_id": (xcut * n_lat + ycut + 1).astype(int),
        }
    )
    return out.sort_values(["geoLaenge", "geoBreite"], ignore_index=True)


def read_data_wind_raw(file_measure: str | Path) -> pd.DataFrame:
    """Read hourly wind measurements (xz-compressed CSV)."""
    return pd.read_csv(file_measure, na_values=["NA", "-999"])


def get_wind_in_grid(data_wind_raw: pd.DataFrame, data_stations_grid: pd.DataFrame) -> pd.DataFrame:
    """Average hourly wind speed over all stations within each grid cell.

    Returns:
        Long-format data frame with columns ``grid_id``, ``datetime`` (UTC),
        ``wind_speed`` (m/s).
    """
    assert_columns(data_wind_raw, ["MESS_DATUM", "STATIONS_ID", "F", "D"])
    assert_columns(data_stations_grid, ["stations_id", "grid_id"])
    wind = pd.DataFrame(
        {
            "stations_id": data_wind_raw["STATIONS_ID"],
            "datetime": pd.to_datetime(
                data_wind_raw["MESS_DATUM"].astype(str), format="%Y%m%d%H", utc=True
            ),
            "wind_speed": data_wind_raw["F"],
        }
    )
    wind = wind.merge(data_stations_grid[["stations_id", "grid_id"]], on="stations_id").dropna()
    return wind.groupby(["grid_id", "datetime"], as_index=False)["wind_speed"].mean()


def restrict_wind_data(
    data_wind: pd.DataFrame, beginning: dt.date, end: dt.date, dates: Dates
) -> pd.DataFrame:
    """Subset ``data_wind`` to the dates ``beginning`` to ``end`` (inclusive)."""
    assert_columns(data_wind, ["datetime"])
    if not dates.start_min <= beginning <= end <= dates.end_max:
        raise ValueError(f"invalid date range {beginning} - {end}")
    day = data_wind["datetime"].dt.date
    return data_wind.loc[np.asarray((day >= beginning) & (day <= end))].reset_index(drop=True)
