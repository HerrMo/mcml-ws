# This code prepares the raw weather data for the analysis.
# You should not run it for the exercise itself, since the raw data could be
# different from what the tests expect, but you can use it if you want to
# explore the underlying data more.
#
# The SMARD.de data is not available through an API, unfortunately, and needs to
# be downloaded manually. The data is available at
# <https://www.smard.de/home/downloadcenter/download-marktdaten/>

url.weather <- "https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/hourly/wind/recent/"

# following function downloads station documentation data from the dwd api Not
# needed for you students, as you will be supplied with this file preprocessed.
# Contains bash scripting so may not work on your OS
downloadStationData <- function() {
  download.file(paste0(url.weather, "FF_Stundenwerte_Beschreibung_Stationen.txt"), file.stations)
  stations <- readLines(file.stations)[-2]
  stations <- iconv(stations, from = "ISO-8859-1", to = "ASCII//TRANSLIT")
  stations <- vapply(strsplit(trimws(stations), " +"), function(x) {
    paste(c(
        x[1:6], paste(x[seq.int(7, length(x) - 1)], collapse = "_"), tail(x, 1)
    ), collapse = " ")
  }, character(1))
  writeLines(stations, file.stations)
}

# This function downloads wind measure data from the dwd api, given the list of
# active stations as given by 'makeStationGrid()'. This takes quite some time
# and should therefore only be run when necessary
downloadMeasureData <- function(data.stations.grid) {
  # download measure data
  assertDataFrame(data.stations.grid)
  assertNames(names(data.stations.grid), must.include = "stations.id")


  station.tables <- lapply(data.stations.grid$stations.id, function(sid) {
    surl <- paste0(url.weather, "stundenwerte_FF_", sid, "_akt.zip")

    temp <- tempfile()
    unpack.dir <- tempfile(pattern = "unzip")
    on.exit({
      unlink(temp, expand = FALSE)
      unlink(unpack.dir, recursive = TRUE, expand = FALSE)
    })
    download.file(surl, temp)

    unzip(temp, exdir = unpack.dir)
    file <- list.files(unpack.dir, pattern = sprintf("^produkt.*%s\\.txt$", sid), full.names = TRUE)
    if (length(file) != 1) stop(sprintf("Error unpacking %s: found != 1 file matching the pattern.", surl))
    result <- data.table::fread(file, na.strings = "-999")

    result
  })

  write.csv(rbindlist(station.tables)[, eor := NULL], file = xzfile(file.measure), row.names = FALSE)
}
