"""Download the raw DWD wind data.

Do not run this for the exercises: the data on the server changes over time,
so the result will differ from the data shipped in ``data/raw`` (and from what
the tests expect). It is kept to document the provenance of the raw data.

The SMARD.de power data is not available through an API and has to be
downloaded manually from
<https://www.smard.de/home/downloadcenter/download-marktdaten/>.
"""

from __future__ import annotations

import io
import lzma
import re
import unicodedata
import urllib.request
import zipfile
from pathlib import Path

import pandas as pd

URL_WEATHER = "https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/hourly/wind/recent/"


def _fetch(url: str) -> bytes:
    with urllib.request.urlopen(url, timeout=60) as response:  # noqa: S310 (fixed https URL)
        return response.read()


def download_station_data(file_stations: str | Path) -> None:
    """Download the station description and write it as whitespace separated table.

    Station names containing spaces are joined with underscores and non-ASCII
    characters are transliterated.
    """
    text = _fetch(URL_WEATHER + "FF_Stundenwerte_Beschreibung_Stationen.txt").decode("latin-1")
    lines = text.splitlines()
    del lines[1]  # separator line "----- ---------"
    out = []
    for line in lines:
        line = unicodedata.normalize("NFKD", line).encode("ascii", "ignore").decode()
        x = re.split(r" +", line.strip())
        out.append(" ".join([*x[:6], "_".join(x[6:-1]), x[-1]]))
    Path(file_stations).write_text("\n".join(out) + "\n")


def download_measure_data(data_stations_grid: pd.DataFrame, file_measure: str | Path) -> None:
    """Download hourly wind measurements of all stations in ``data_stations_grid``.

    This takes a while.
    """
    tables = []
    for sid in data_stations_grid["stations_id"]:
        sid = f"{int(sid):05d}"
        archive = zipfile.ZipFile(io.BytesIO(_fetch(f"{URL_WEATHER}stundenwerte_FF_{sid}_akt.zip")))
        names = [n for n in archive.namelist() if re.match(rf"^produkt.*{sid}\.txt$", n)]
        if len(names) != 1:
            raise RuntimeError(f"station {sid}: found {len(names)} files matching the pattern")
        df = pd.read_csv(archive.open(names[0]), sep=";", na_values="-999", skipinitialspace=True)
        tables.append(df.drop(columns="eor"))
    with lzma.open(file_measure, "wt") as f:
        pd.concat(tables).to_csv(f, index=False)
