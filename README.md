# Analysis of Wind Power in Germany

This project explores and models wind power generation in Germany.
It relates wind speed in different areas of Germany to generated power, visualizes the influence of each area on power output, and predicts power production from wind data with several models.

In reality, this is a mock project for teaching project organization and tools for reproducibility.
It is **not** meant to represent a good or statistically valid approach!

## Data

Power generation data of the four German transmission system operators (TSOs) was downloaded from [SMARD.de](https://www.smard.de); hourly wind measurements are compiled from the [DWD open data](https://opendata.dwd.de) server.
Station locations are binned into a 4 × 4 grid over Germany, and wind speed is averaged within each grid cell.

## Installation

The project uses [uv](https://docs.astral.sh/uv/) for dependency management.

```bash
uv sync            # creates .venv with the locked dependencies
uv run pre-commit install
```

## Usage

Run the full analysis (figures and `metrics.csv` are written to `plots/`):

```bash
uv run windpower --config config.yaml
```

Or step by step:

```python
from windpower import data_power, data_wind, model
from windpower.config import load_config

cfg = load_config("config.yaml")

# location data of weather stations
stations = data_wind.read_data_stations_raw(cfg.paths.stations, cfg.dates, cfg.grid)
stations_grid = data_wind.get_stations_grid(stations, cfg.grid)

# energy production data
energy = data_power.read_data_energy(cfg.paths.power)

# wind data, averaged within each grid cell
wind = data_wind.get_wind_in_grid(data_wind.read_data_wind_raw(cfg.paths.measure), stations_grid)

# merge wind data in wide format with energy data, restricted to the training period
modelinput = model.build_model_data(
    model.wind_to_wide(wind), energy, cfg.dates.start_train, cfg.dates.end_train
)

# fit a linear model
model.fit_model_lm(modelinput).coef
```

## Development

```bash
uv run pytest                 # tests
uv run ruff check . && uv run ruff format .
```

## Acknowledgements

We would like to thank Dick Brown for his report ['Analysis of German Wind Power Output'](https://www.kaggle.com/code/dickbrown/german-wind-power) on Kaggle for the idea to analyze wind power production data of Germany.

The power production data is courtesy of 'Bundesnetzagentur | SMARD.de'.

The wind data was compiled from 'https://opendata.dwd.de' of the 'Deutscher Wetterdienst'.

The outline of Germany is taken from [GADM](https://gadm.org) version 4.1.
