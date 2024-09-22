
# Load a provided dataset listing historical power generation for major German
# transmission system operators.
#
# Returns a data.frame with columns
# - `datetime` (POSIXct)
# - `operator` (character): company operating the transmission system
# - `wind.energy` (numeric): Energy produced in a given 15-minute-interval, in
#   units of MWh
readDataEnergy <- function() {
  # check that zip file exists
  assertFile(file.power, access = "r")
  # unzip into data folder
  unpack.dir <- tempfile(pattern = "unzip")
  on.exit({
    unlink(unpack.dir, recursive = TRUE, expand = FALSE)
  })

  unzip(file.power,
    exdir = unpack.dir, overwrite = TRUE
  )

  # create list of individual datatables
  power.dts <- lapply(list.files(unpack.dir, full.names = TRUE, pattern = ".*\\.csv"), function(x) {
    operator <- regmatches(x, regexpr("[[:alnum:]]+(?=\\.csv$)", x, perl = TRUE))
    data.table::fread(x, sep = ";", encoding = "UTF-8")[, operator := operator]
  })

  wind.power <- data.table::rbindlist(power.dts, fill = TRUE)

  wind.power[,
    datetime := as.POSIXct(
      as.POSIXct(paste(Datum, Anfang), format = "%d.%m.%Y %H:%M", tz = "Europe/Berlin"),
      tz = "UTC")]
  wind.power[, c("Datum", "Anfang", "Ende") := NULL]

  # ... i hate german number stuff, why do we do this to ourselves
  cols.mwh <- colnames(wind.power)[grepl("MWh", names(wind.power))]
  wind.power[,
    (cols.mwh) := lapply(.SD, function(x) as.numeric(gsub(",", ".", gsub("[^0-9,]", "", x)))),
    .SDcols = cols.mwh
  ]

  # NAs to 0 (no data)
  data.table::setnafill(wind.power, type = "const", fill = 0, cols = cols.mwh)

  data.table::setnames(wind.power, gsub(" Originalaufl.sungen$", "", names(wind.power)))

  # select only wind power, and sum over potentially duplicate times (happens when summer time ends)
  # We could do this more thoroughly if we cared.
  wind.power <- wind.power[,
    .(wind.energy = mean(`Wind Offshore [MWh]` + `Wind Onshore [MWh]`)),
    by = .(datetime, operator)]

  data.table::setDF(wind.power)
}

# Convert units in a `data.frame` as created by `readDataEnergy()`:
# Replace column `wind.energy` with `power.wind`, indicating power production in
# units of MW.
convertEnergyToPower <- function(table.power) {
  assertDataFrame(table.power)
  assertNames(names(table.power), must.include = c("datetime", "operator", "wind.energy"))
  # convert to MW instead of MWh
  # because it is in 15min intervals
  table.power$power.wind <- table.power$wind.energy * 4
  table.power$wind.energy <- NULL

  table.power
}

# Modify a `data.frame` as created by `readDataEnergy()` to give daily total
# energy production.
# Returns a data.frame with columns
# - date (`Date` class)
# - operator (`character`)
# - wind.energy (`numeric`) total energy, units of MWh
getEnergyDaily <- function(wind.power) {
  assertDataFrame(wind.power)
  assertNames(names(wind.power), must.include = c("datetime", "operator", "wind.energy"))

  #daily
  data.table::setDF(data.table::as.data.table(wind.power)[, .(date = as.Date(datetime), operator, wind.energy)][,
    .(wind.energy = sum(wind.energy)), by = .(date, operator)
  ])
}
