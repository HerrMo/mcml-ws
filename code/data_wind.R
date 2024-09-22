#### weather data ####

if (start.date.train < start.date.min || end.date.train > end.date.max || start.date.train > end.date.train) {
  stop(sprintf("Requested date range invalid or outside of data possibilities.
Requested: %s  -  %s
Possible: %s  -  %s", start.date.train, end.date.train, start.date.min, end.date.max))
}

# creates a dt of all the active measure stations, their location and grid id
readDataStationsRaw <- function() {
  assertFile(file.stations, access = "r")
  stations <- data.table::fread(file.stations)

  # convert dates to Date
  toDate <- function(x) as.Date(as.character(x), format = "%Y%m%d")
  stations[, `:=`(von_datum = toDate(von_datum), bis_datum = toDate(bis_datum))]

  # select only stations that were active from start date and still measure till end date
  stations <- stations[von_datum <= start.date.min & bis_datum >= end.date.max]

  # bin the stations by coordinates
  xbins <- bins.long
  ybins <- bins.lat

    # include spots not in desired grid
  clip <- function(x, lower, upper) ifelse(x < lower, lower, ifelse(x > upper, upper, x))
  stations[, geoLaenge := clip(geoLaenge, min(xbins), max(xbins))]
  stations[, geoBreite := clip(geoBreite, min(ybins), max(ybins))]

  data.table::setDF(stations)
}

getStationsGrid <- function(data.stations.raw) {
  assertDataFrame(data.stations.raw)
  assertNames(names(data.stations.raw),
    must.include = c("Stations_id", "von_datum", "bis_datum",
      "Stationshoehe", "geoBreite", "geoLaenge",
      "Stationsname", "Bundesland"))
  # bin the stations by coordinates
  xbins <- bins.long
  ybins <- bins.lat

  stations <- data.table::as.data.table(data.stations.raw)
  stations[, `:=`(
    xcut = cut(geoLaenge, breaks = xbins, labels = FALSE),
    ycut = cut(geoBreite, breaks = ybins, labels = FALSE)
  )]

  data.table::setorder(stations, geoLaenge, geoBreite)

  grid.id.table <- stations[, data.table::CJ(xcut, ycut, sorted = TRUE, unique = TRUE)]
  grid.id.table[, grid.id := seq_len(nrow(grid.id.table))]

  data.stations.grid <- stations[grid.id.table, , on = .(xcut, ycut), nomatch = NULL][,
    .(stations.id = Stations_id, geoLaenge, geoBreite, grid.id)
  ]

  # pad station id
  data.table::setDF(data.stations.grid[, stations.id := sprintf("%05d", stations.id)])
}

# Use the measured data from the provided zip-file
readDataWindRaw <- function() {
  assertFile(file.measure, access = "r")
  read.csv(xzfile(file.measure))
}

getWindInGrid <- function(data.wind.raw, data.stations.grid) {

  assertDataFrame(data.wind.raw)
  assertNames(names(data.wind.raw), must.include = c("MESS_DATUM", "STATIONS_ID", "F", "D"))
  assertDataFrame(data.stations.grid)
  assertNames(names(data.stations.grid), must.include = "stations.id")


  toDateTime <- function(x) as.POSIXct(as.character(x), format = "%Y%m%d%H", tz = "UTC")
  data.wind.raw <- data.table::as.data.table(data.wind.raw)[,
    .(stations.id = STATIONS_ID, datetime = toDateTime(MESS_DATUM), wind.dir = D, wind.speed = get("F"))
  ]

  data.wind.raw <- data.table::as.data.table(data.stations.grid)[,
    stations.id := as.integer(stations.id)][data.wind.raw, on = "stations.id"]

  data.wind.raw <- na.omit(data.wind.raw)

  # average over grid
  data.table::setDF(data.wind.raw[, .(wind.speed = mean(wind.speed)), by = .(grid.id, datetime)])
}

# creates a data.frame to be used in the linear regression given the time scope
restrictWindData <- function(data.wind, beginning, end) {
  assertDataFrame(data.wind)
  assertNames(names(data.wind), must.include = "datetime")

  assertPOSIXct(beginning, lower = start.date.min)
  assertPOSIXct(end, lower = beginning, upper = end.date.max)

  as.data.frame(data.wind[data.table::between(data.wind$datetime, beginning, end), ])
}
