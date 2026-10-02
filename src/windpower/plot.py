"""Figures of the report. All functions return a :class:`matplotlib.figure.Figure`."""

from __future__ import annotations

import json
from collections.abc import Mapping
from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
from matplotlib.colors import CenteredNorm
from matplotlib.figure import Figure
from matplotlib.patches import Rectangle

from windpower.config import Grid
from windpower.utils import assert_columns, get_time_of_day


def read_outline(file_germany: str | Path) -> list[np.ndarray]:
    """Exterior rings of a GeoJSON (Multi)Polygon feature collection."""
    with Path(file_germany).open(encoding="utf-8") as f:
        features = json.load(f)["features"]
    rings = []
    for feature in features:
        geom = feature["geometry"]
        polygons = geom["coordinates"] if geom["type"] == "MultiPolygon" else [geom["coordinates"]]
        rings.extend(np.asarray(polygon[0]) for polygon in polygons)
    return rings


def plot_power_daily(data_energy_daily: pd.DataFrame) -> Figure:
    """Daily energy production per operator with a 30-day rolling mean."""
    fig, ax = plt.subplots(figsize=(8, 4.5))
    for operator, df in data_energy_daily.groupby("operator"):
        (line,) = ax.plot(df["date"], df["wind_energy"], lw=0.7, alpha=0.6, label=operator)
        ax.plot(
            df["date"], df["wind_energy"].rolling(30, center=True).mean(), color=line.get_color()
        )
    ax.set(
        title="Power Generation per Company", ylabel="Generated Electricity [MWh]", ylim=(0, None)
    )
    ax.legend(title="Operator", loc="upper center", ncols=4, bbox_to_anchor=(0.5, -0.1))
    fig.tight_layout()
    return fig


def plot_wind_daily(data_wind: pd.DataFrame) -> Figure:
    """Daily average wind speed over all grid cells."""
    daily = data_wind.groupby(data_wind["datetime"].dt.date)["wind_speed"].mean()
    fig, ax = plt.subplots(figsize=(8, 4.5))
    ax.plot(daily.index, daily.to_numpy(), lw=0.8)
    ax.plot(daily.index, daily.rolling(30, center=True).mean().to_numpy(), lw=2)
    ax.set(
        title="Daily Average Measured Wind Speeds",
        xlabel="Date",
        ylabel="Wind Speed [m/s]",
        ylim=(0, None),
    )
    fig.tight_layout()
    return fig


def _map(data_stations_raw: pd.DataFrame, grid: Grid, file_germany: str | Path):
    fig, ax = plt.subplots(figsize=(6, 7))
    for ring in read_outline(file_germany):
        ax.fill(ring[:, 0], ring[:, 1], facecolor="lightgrey", edgecolor="black", alpha=0.5, lw=0.5)
    ax.scatter(data_stations_raw["geoLaenge"], data_stations_raw["geoBreite"], s=8, color="black")
    ax.set(xticks=grid.bins_long, yticks=grid.bins_lat, xlabel="Longitude", ylabel="Latitude")
    ax.set_aspect("equal")
    return fig, ax


def _cell_corners(grid: Grid) -> list[tuple[float, float, float, float]]:
    """(x0, y0, width, height) of each cell, in the order of ``grid_id``."""
    return [
        (x0, y0, x1 - x0, y1 - y0)
        for x0, x1 in zip(grid.bins_long[:-1], grid.bins_long[1:], strict=True)
        for y0, y1 in zip(grid.bins_lat[:-1], grid.bins_lat[1:], strict=True)
    ]


def plot_wind_stations(
    data_stations_raw: pd.DataFrame, grid: Grid, file_germany: str | Path
) -> Figure:
    """Map of weather stations and the numbered grid cells."""
    fig, ax = _map(data_stations_raw, grid, file_germany)
    for x in grid.bins_long:
        ax.axvline(x, color="tab:blue")
    for y in grid.bins_lat:
        ax.axhline(y, color="tab:blue")
    for i, (x0, y0, w, h) in enumerate(_cell_corners(grid), start=1):
        ax.text(
            x0 + w / 2, y0 + h / 2, str(i), color="tab:blue", fontsize=18, ha="center", va="center"
        )
    ax.set_title("Weather Stations by Grid")
    fig.tight_layout()
    return fig


def col_square(
    coef: pd.Series,
    data_stations_raw: pd.DataFrame,
    grid: Grid,
    file_germany: str | Path,
    title: str = "Beta values of corresponding grid sections",
) -> Figure:
    """Map with grid cells colored by the model coefficient of the cell."""
    names = [f"grid_{i:02d}" for i in range(1, grid.n_cells + 1)]
    values = coef.reindex(names).fillna(0).to_numpy()
    fig, ax = _map(data_stations_raw, grid, file_germany)
    norm = CenteredNorm(vcenter=0, halfrange=max(np.abs(values).max(), 1e-12))
    cmap = plt.get_cmap("RdBu")
    for value, (x0, y0, w, h) in zip(values, _cell_corners(grid), strict=True):
        ax.add_patch(Rectangle((x0, y0), w, h, facecolor=cmap(norm(value)), alpha=0.6))
    fig.colorbar(plt.cm.ScalarMappable(norm=norm, cmap=cmap), ax=ax, label="Beta value", shrink=0.6)
    ax.set_title(title)
    fig.tight_layout()
    return fig


def plot_hourly(power_table_mw: pd.DataFrame, data_wind: pd.DataFrame) -> Figure:
    """Average power production and wind speed for each time of day (UTC)."""
    assert_columns(power_table_mw, ["datetime", "operator", "power_wind"])
    total = power_table_mw.groupby("datetime")["power_wind"].sum()
    power = total.groupby(get_time_of_day(total.index.to_series())).mean()
    wind = data_wind.groupby(get_time_of_day(data_wind["datetime"]))["wind_speed"].mean()

    fig, ax = plt.subplots(figsize=(8, 4.5))
    ax.plot(power.index, power.to_numpy(), color="tab:red", label="Power Production")
    ax.set(
        xlabel="Time of Day", ylabel="Power Production [MW]", xticks=range(0, 25, 2), ylim=(0, None)
    )
    ax2 = ax.twinx()
    ax2.plot(wind.index, wind.to_numpy(), color="tab:blue", label="Wind Speed")
    ax2.set(ylabel="Wind Speed [m/s]", ylim=(0, None))
    ax.set_title("Average Power Production and Wind Speed over the Day")
    fig.legend(loc="lower center", ncols=2)
    fig.tight_layout(rect=(0, 0.06, 1, 1))
    return fig


def plot_predicted_wind_energy(datetime: pd.Series, data: Mapping[str, np.ndarray]) -> Figure:
    """Overlay several prediction series (and the ground truth) over time."""
    fig, ax = plt.subplots(figsize=(9, 4.5))
    for name, values in data.items():
        if len(values) != len(datetime):
            raise ValueError(f"data[{name!r}] has length {len(values)}, expected {len(datetime)}")
        ax.plot(datetime, values, label=name)
    ax.set(
        xlabel="Date", ylabel="Wind Energy [MWh / 15 min]", title="Wind Power Production Prediction"
    )
    ax.legend(loc="upper center", ncols=len(data), bbox_to_anchor=(0.5, -0.12))
    fig.tight_layout()
    return fig
