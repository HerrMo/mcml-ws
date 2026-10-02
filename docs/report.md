# Wind Power Generation in Germany

!!! note
    The figures on this page are produced by `uv run windpower` and copied to
    `docs/figures/` before the documentation is built (see
    `.github/workflows/docs.yml`).

## Introduction

Wind power is responsible for about 27 % of the electricity consumed in Germany in 2021.
We analyze wind power production data of the big four German transmission system operators (50Hertz, Amprion, TenneT TSO and TransnetBW) from June 2022 to October 2023.

![Daily average wind speed](figures/wind_speed.png)
![Daily wind energy per operator](figures/line_total_comp.png)

## Wind speed data

Only weather stations of the DWD that were active throughout the whole period are used.
Station locations are binned into a 4 × 4 grid; stations outside of the grid are moved to the closest cell.

![Weather stations by grid cell](figures/grid_on_map.png)

Averaged over the year, wind power production decreases during the day while wind speed increases.

![Power and wind speed over the day](figures/line_in_day_with_wind.png)

## Model

We model wind power production by the wind speed in each grid cell, with a separate intercept for each hour of the day:

$$
\text{Power}_i = \beta_{0,\text{TimeOfDay}(i)} + \beta_1 \text{Wind@Grid.01}_i + \dots + \beta_{16} \text{Wind@Grid.16}_i
$$

Negative coefficients of the least-squares fit indicate that the model misses important structure.
The penalized model constrains the grid coefficients to be non-negative and adds an L1 penalty chosen by 10-fold cross-validation.

![Least-squares coefficients](figures/squares_lm.png)
![Penalized coefficients](figures/squares_penalized.png)

Fitting the penalized model separately for each operator approximately recovers the control areas of the operators.

![50Hertz](figures/squares_50Hertz.png)
![Amprion](figures/squares_Ampiron.png)
![TenneT](figures/squares_TenneT.png)
![TransnetBW](figures/squares_TransnetBW.png)

## Prediction

![Predictions](figures/wind_power_prediction.png)

## Summary

This report only serves to teach reproducible project setup.
The analysis should not be seen as a good example of how to tackle the task.
