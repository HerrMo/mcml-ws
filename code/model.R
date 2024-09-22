suppressWarnings(require(penalized))  # penalized package requires us to do this
suppressWarnings(invisible(loadNamespace("ranger")))  # get ranger predict() function


# model fit functions

buildModelData <- function(data.wind.wide, data.energy, date.from, date.to, keep.datetime = FALSE) {

  assertDate(date.from)
  assertDate(date.to)

  data.energy.hr <- data.table::as.data.table(data.energy)[
    as.Date(datetime) >= date.from & as.Date(datetime) <= date.to]
  data.energy.hr <- data.energy.hr[,
    .(wind.energy = sum(wind.energy)), by = "datetime"]
  data.energy.hr <- data.energy.hr[,
    .(wind.energy = mean(wind.energy, na.rm = TRUE)),
    by = .(datetime = as.POSIXct(round(datetime, "hours")))]

  model.tbl <- data.table::as.data.table(data.wind.wide)[
    data.energy.hr[, .(datetime, wind.energy)], on = "datetime", nomatch = NULL]
  model.tbl[, time.of.day := as.factor(getTimeOfDay(datetime))]
  if (!keep.datetime) model.tbl[, datetime := NULL]
  data.table::setDF(model.tbl)
}



# Exercise 01_project_structure_i.R: 03
#
# We want to model power production from wind in Germany. For this, we scraped
# together wind speed data of numerous measuring stations and binned the
# location of these stations into a 4x4 grid across Germany, averaging the wind
# speed for every grid cell for each point in time.
#
# To fit a model that has the average wind speed in each grid cell as a
# covariate, the data needs to be reshaped in a "wide" format.
#
# Write a function, that takes a data.frame 'data.wind' with columns "grid.id",
# "datetime", and "wind.speed and returns a wide-format data.frame with columns
# 'datetime', 'grid.01', 'grid.02', 'grid.03', ... 'grid.16'.
#
# The input could, for example, look something like this:
example.data.wind <- data.frame(
  grid.id = c(12, 1, 7, 12, 12, 1, 7, 7, 12, 7, 1, 7, 1, 12, 1),
  datetime = as.POSIXct(c("2023-04-21 06:00:00", "2022-10-23 06:00:00", "2022-10-23 06:00:00",
                          "2023-02-14 12:00:00", "2022-12-19 12:00:00", "2022-11-11 12:00:00",
                          "2022-11-11 12:00:00", "2023-04-21 06:00:00", "2022-10-23 06:00:00",
                          "2023-02-14 12:00:00", "2023-02-14 12:00:00", "2022-12-19 12:00:00",
                          "2022-12-19 12:00:00", "2022-11-11 12:00:00", "2023-04-21 06:00:00"),
                        tz = "UTC"),
  wind.speed = c(2.388889, 1.500000, 1.250000, 1.888889, 2.611111, 4.200000, 2.333333, 2.866667,
                 2.000000, 1.437500, 1.000000, 1.500000, 1.764706, 1.636364, 2.200000)
)
# The result of this data excerpt input into 'ex01toWide' should then look like
# this:
example.data.wind.wide <- data.frame(
  datetime = as.POSIXct(c("2023-04-21 06:00:00", "2022-10-23 06:00:00", "2023-02-14 12:00:00",
                          "2022-12-19 12:00:00", "2022-11-11 12:00:00"),
                        tz = "UTC"),
  grid.01 = c(2.200000, 1.500000, 1.000000, 1.764706, 4.200000),
  grid.07 = c(2.866667, 1.250000, 1.437500, 1.500000, 2.333333),
  grid.12 = c(2.388889, 2.000000, 1.888889, 2.611111, 1.636364)
)
#
# You can also experiment with the actual `data.wind` created by
# `windpower_report.Rmd` when stepping through the first few chunks.
# `head(windToWide(data.wind))` should be a table with the columns `datetime`,
# as well as `grid.01`, `grid.02`, ... `grid.16`.
#
# The ordering of rows or columns does not matter in this exercise.
windToWide <- function(data.wind) {
  # your code
  data.table::setDF(data.table::dcast(data.table::as.data.table(data.wind),
    datetime ~ I(sprintf("grid.%02d", grid.id)), value.var = "wind.speed"))
}

fitModelLM <- function(data.modelinput) {
  lm(wind.energy ~ 0 + ., data.modelinput)
}

predictModelLm <- function(model.lm, data.predict) {
  predict(model.lm, data.predict)
}

fitModelPenalized <- cacheFunctionOnDisk(
  function(data.modelinput) {
    cvr <- optL1(
      wind.energy ~ 0 + time.of.day,
      penalized = as.formula(paste("~", paste(sprintf("grid.%02d", 1:16), collapse = " + "))),
      data = data.modelinput,
      positive = TRUE, model = "linear", fold = 10,
      trace = FALSE
    )
    cvr$fullfit
  },
  cache.path <- file.path(path.intermediate, "cache_fitModelPenalized")
)

predictModelPenalized <- function(model.penalized, data.predict) {
  predict(model.penalized, data = data.predict)[, 1]
}

# Exercise 01_project_structure_i.R: 03 (b)
#
# Next to the linear model and the penalized linear model, we also want to try
# fitting a random forest model on the given data.
#
# Fill out these functions so that they fit and predict a random forest using
# the `ranger::ranger()` function. You should set the `num.trees` to 2000 and
# the `always.split.variables` to `"time.of.day"`. Otherwise, the call to
# `ranger::ranger` is just like the call to `lm()`. Return the fitted random
# forest model.
fitModelRanger <- function(data.modelinput) {
  # your code
  ranger::ranger(wind.energy ~ 0 + .,
    data.modelinput, num.trees = 2000, always.split.variables = "time.of.day")
}
#
# Exercise 01_project_structure_i.R: 03 (b) Continued:
#
# Fill out this function as well. It gets the result from `fitModelRanger()` as
# input and should return a vector of predictions. The call to `predict()` for a
# "ranger" model is similar to the call for a `lm()` model, except that the
# returned object is a named list. You have to return the `$predictions` element
# of the result of `predict()`.
predictModelRanger <- function(model.ranger, data.predict) {
  # your code
  predict(model.ranger, data = data.predict)$predictions
}

fitPredictModelRanger <- cacheFunctionOnDisk(
  function(data.modelinput, data.predict) {
    model.ranger <- fitModelRanger(data.modelinput)
    predictModelRanger(model.ranger, data.predict)
  },
  file.path(path.intermediate, "cache_fitPredictModelRanger")
)
