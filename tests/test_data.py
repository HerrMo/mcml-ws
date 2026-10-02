import datetime as dt

import pandas as pd
import pytest

from windpower import data_power, data_wind


@pytest.fixture
def energy():
    return pd.DataFrame(
        {
            "datetime": pd.to_datetime(
                ["2023-01-01 23:45", "2023-01-02 00:00", "2023-01-02 00:15", "2023-01-02 00:00"],
                utc=True,
            ),
            "operator": ["A", "A", "A", "B"],
            "wind_energy": [1.0, 2.0, 3.0, 10.0],
        }
    )


def test_convert_energy_to_power(energy):
    power = data_power.convert_energy_to_power(energy)
    assert "wind_energy" not in power
    assert power["power_wind"].tolist() == [4.0, 8.0, 12.0, 40.0]


def test_get_energy_daily(energy):
    daily = data_power.get_energy_daily(energy)
    expected = pd.DataFrame(
        {
            "date": [dt.date(2023, 1, 1), dt.date(2023, 1, 2), dt.date(2023, 1, 2)],
            "operator": ["A", "A", "B"],
            "wind_energy": [1.0, 5.0, 10.0],
        }
    )
    pd.testing.assert_frame_equal(daily, expected)


def test_missing_columns_raise(energy):
    with pytest.raises(ValueError, match="wind_energy"):
        data_power.get_energy_daily(energy.drop(columns="wind_energy"))


def test_get_stations_grid(cfg):
    stations = pd.DataFrame(
        {
            "Stations_id": [1, 2, 3, 4],
            "von_datum": dt.date(2000, 1, 1),
            "bis_datum": dt.date(2024, 1, 1),
            "Stationshoehe": 0,
            "geoBreite": [47.0, 48.0, 50.0, 55.0],  # 47.0: lower boundary is included
            "geoLaenge": [6.0, 6.5, 6.5, 14.0],
            "Stationsname": "x",
            "Bundesland": "y",
        }
    )
    grid = data_wind.get_stations_grid(stations, cfg.grid)
    assert grid.set_index("stations_id")["grid_id"].to_dict() == {1: 1, 2: 1, 3: 2, 4: 16}


def test_get_wind_in_grid():
    raw = pd.DataFrame(
        {
            "STATIONS_ID": [1, 2, 3, 1, 9],
            "MESS_DATUM": [2023010100, 2023010100, 2023010100, 2023010101, 2023010100],
            "F": [1.0, 3.0, 5.0, None, 7.0],
            "D": 0,
        }
    )
    stations_grid = pd.DataFrame({"stations_id": [1, 2, 3], "grid_id": [1, 1, 2]})
    wind = data_wind.get_wind_in_grid(raw, stations_grid)
    assert wind["wind_speed"].tolist() == [2.0, 5.0]  # NaN row and unknown station dropped
    assert (wind["datetime"] == pd.Timestamp("2023-01-01", tz="UTC")).all()


@pytest.mark.slow
def test_read_data_energy(cfg):
    energy = data_power.read_data_energy(cfg.paths.power)
    assert sorted(energy["operator"].unique()) == ["50Hertz", "Ampiron", "TenneT", "TransnetBW"]
    assert energy["wind_energy"].ge(0).all()
    # 15-minute resolution without duplicates, also across the end of daylight saving time
    assert not energy.duplicated(["datetime", "operator"]).any()
