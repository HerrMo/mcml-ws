"""Load and validate the YAML configuration of the analysis."""

from __future__ import annotations

import datetime as dt
from pathlib import Path

import yaml
from pydantic import BaseModel, ConfigDict, field_validator, model_validator

DEFAULT_CONFIG = Path("config.yaml")


class _Frozen(BaseModel):
    model_config = ConfigDict(frozen=True, extra="forbid")


class Paths(_Frozen):
    raw: Path
    intermediate: Path
    plots: Path
    stations: Path
    measure: Path
    power: Path
    germany: Path


class Dates(_Frozen):
    start_min: dt.date
    end_max: dt.date
    start_train: dt.date
    end_train: dt.date
    start_predict: dt.date
    end_predict: dt.date

    @model_validator(mode="after")
    def _check_ranges(self) -> Dates:
        for name in ("train", "predict"):
            start = getattr(self, f"start_{name}")
            end = getattr(self, f"end_{name}")
            if not (self.start_min <= start <= end <= self.end_max):
                raise ValueError(
                    f"Requested {name} range {start} - {end} invalid or outside of "
                    f"available data range {self.start_min} - {self.end_max}."
                )
        return self


class Grid(_Frozen):
    bins_long: tuple[float, ...]
    bins_lat: tuple[float, ...]

    @field_validator("bins_long", "bins_lat")
    @classmethod
    def _check_sorted(cls, bins: tuple[float, ...]) -> tuple[float, ...]:
        if len(bins) < 2 or list(bins) != sorted(set(bins)):
            raise ValueError("bins must be strictly increasing with at least 2 entries")
        return bins

    @property
    def n_cells(self) -> int:
        return (len(self.bins_long) - 1) * (len(self.bins_lat) - 1)


class Model(_Frozen):
    seed: int
    cv_folds: int = 10
    n_estimators: int = 500
    n_boot: int = 1000


class Config(_Frozen):
    paths: Paths
    dates: Dates
    grid: Grid
    model: Model


def load_config(path: str | Path = DEFAULT_CONFIG) -> Config:
    """Read a YAML file and validate it against the :class:`Config` schema.

    Args:
        path: Location of the YAML file.

    Returns:
        The validated configuration.

    Raises:
        pydantic.ValidationError: If the file does not match the schema.
    """
    with Path(path).open(encoding="utf-8") as f:
        return Config.model_validate(yaml.safe_load(f))
