# code for plots

suppressWarnings(require(ggplot2))
suppressWarnings(require(raster))
# maybe don't take inspiration from this, I'm not great at ggplot2

# daily power production plot
plotPowerDaily <- function(data.energy.daily) {

  # by company
  ggplot(data = data.energy.daily, aes(date, wind.energy)) +
    geom_line(aes(color = operator)) +
    geom_smooth() +
    labs(title = "Power Generation per Company",
         color = "Operator",
         y = "Generated Electricity [MWh]") +
    scale_y_continuous(limits = c(0, max(data.energy.daily$wind.energy))) +
    theme(legend.position = "top")
}


# average daily wind plot
plotWindDaily <- function(data.wind) {

  data.wind <- data.table::as.data.table(data.wind)

  data.wind <- data.wind[, date := as.Date(datetime)][, .(wind.speed = mean(wind.speed)), by = "date"]
  ggplot(data.wind, aes(x = date, y = wind.speed)) +
    geom_line() +
    geom_smooth() +
    scale_y_continuous(limits = c(0, max(data.wind$wind.speed))) +
    labs(title = "Daily Average Measured Wind Speeds",
         x = "Date", y = "Wind Speed [m/s]")
}

# map of stations and grid squares in Germany
plotWindStations <- function(data.stations.raw) {
  xbins <- bins.long
  ybins <- bins.lat

  suppressWarnings({
    germany <- geodata::gadm("Germany", level = 0, path = path.raw)
    germany <- as(germany, "Spatial")
  })
  map <- ggplot() +
    geom_polygon(data = germany,
                 aes(x = long, y = lat, group = group),
                 color = "black", fill = "light grey", alpha = 0.3) +
    geom_point(data = data.stations.raw, aes(x = geoLaenge, y = geoBreite)) +
    scale_x_continuous(breaks = xbins, limits = c(5.8, 15.1)) +
    scale_y_continuous(breaks = ybins)

  labels <- data.table::CJ(
    lats = xbins[-length(xbins)] + diff(xbins) / 2,
    longs = ybins[-length(ybins)] + diff(ybins) / 2,
    sorted = TRUE)
  labels[, id := seq_len(nrow(labels))]

  map + geom_hline(yintercept = ybins, col = "blue") +
    geom_vline(xintercept = xbins, col = "blue") +
    geom_text(data = labels, aes(x = lats, y = longs, label = id),
              col = "blue", size = 8) +
    coord_fixed(ratio = 1) +
    labs(title = "Weather Stations by Grid", x = "Longitude", y = "Latitude")

}


### function for colored rectangles plot creation
colSquare <- function(coefs, data.stations.raw, title = "Beta values of corresponding Grid sections") {

  xbins <- bins.long
  ybins <- bins.lat

  coef.names.expected <- sprintf("grid.%02d", 1:16)
  coeff <- coefs[coef.names.expected]
  coeff[is.na(coeff)] <- 0

  reg.coef <- data.table::as.data.table(cbind(quadrant = names(coeff),
                                 value = as.numeric(coeff)))

  labels <- data.table::as.data.table(expand.grid(bins.long[-length(bins.long)],
                                      bins.lat[-length(bins.lat)]))
  data.table::setorder(labels, Var1, Var2)

  redDat <- cbind(reg.coef, labels)[, value := as.numeric(value)]

  suppressWarnings({
    germany <- geodata::gadm("Germany", level = 0, path = path.raw)
    germany <- as(germany, "Spatial")
  })
  map <- ggplot() +
    geom_polygon(data = germany,
                 aes(x = long, y = lat, group = group),
                 color = "black", fill = "light grey", alpha = 0.3) +
    geom_point(data = as.data.frame(data.stations.raw), aes(x = geoLaenge, y = geoBreite)) +
    scale_x_continuous(breaks = xbins, limits = c(5.8, 15.1)) +
    scale_y_continuous(breaks = ybins)

  map +
    geom_rect(data = redDat, aes(xmin = Var1, ymin = Var2,
                                 xmax = Var1 + 2, ymax = Var2 + 2,
                                 fill = value), alpha = 0.4) +
    scale_fill_gradient2(low = "red", mid = "white", high = "blue", name = "Beta value") +
    labs(title = title)

}


# some additional plots that werent used in the end for this exercise
plotHourly <- function(power.table.mw, data.wind) {

  assertDataFrame(power.table.mw)
  assertNames(colnames(power.table.mw), must.include = c("datetime", "operator", "power.wind"))
  wp <- data.table::as.data.table(power.table.mw)
  assertTRUE(anyDuplicated(wp, by = c("operator", "datetime")) == 0)
  wgrid <- data.table::as.data.table(data.wind)

  # typical power generation over the day

  # append a row at the end of the day that mirrors the beginning
  midnight2 <-  as.difftime(24, units = "hours")
  rbindMidnight <- function(t) rbind(t, t[hours.since.midnight == 0][, hours.since.midnight := midnight2])

  wp[, hours.since.midnight := getTimeOfDay(datetime)]

  gridpoints <- as.POSIXct(as.character(seq(0, 24, by = 2)), format = "%H", tz = "UTC")

  gridlabels <- format(gridpoints, format = "%H:%M")
  gridlabels[[length(gridlabels)]] <- "24:00"

  gridtimes <- getTimeOfDay(gridpoints)
  gridtimes[[length(gridtimes)]] <- midnight2  # would otherwise be 0

  # ingest data.wind
  wind <- wgrid[, .(wind.speed = mean(wind.speed)), by = .(hours.since.midnight = getTimeOfDay(datetime))]


  #######http://rstudio-pubs-static.s3.amazonaws.com/329613_f53e84d1a18840d5a1df55efb90739d9.html###
  #-----------------------------------------------------------------------------
  # Pre-specify some variables to make the following code more generic
  #-----------------------------------------------------------------------------
  d1 <- rbindMidnight(wp[, .(MWh = mean(power.wind)), by = "hours.since.midnight"])
  d2 <- rbindMidnight(wind)
  y1 <- "MWh"
  y2 <- "wind.speed"
  #-----------------------------------------------------------------------------
  # Rescale the second y axis by
  #   - subtracting its minimum value (to set it to start at 0)
  #   - scaling so that it has the same range as the 'y1' variable
  #   - offsettting it by the minimum value of y1
  #-----------------------------------------------------------------------------
  a            <- range(c(0, d1[[y1]]))
  b            <- range(c(0, d2[[y2]]))
  scale.factor <- diff(a) / diff(b)
  d2[[y2]]      <- ((d2[[y2]] - b[[1]]) * scale.factor) + a[[1]]
  #-----------------------------------------------------------------------------
  # Need to define the second axis transformation to be the inverse of the data
  # transformation to everything cancels out appropriately
  #-----------------------------------------------------------------------------
  trans <- ~ ((. - a[[1]]) / scale.factor) + b[[1]]
  #-----------------------------------------------------------------------------
  # tell the y axis to set up a scaled secondary axis with the given transform
  #-----------------------------------------------------------------------------
  ggplot(d1) +
    geom_line(aes(as.numeric(hours.since.midnight), MWh, color = "Power Production")) +
    geom_line(data = d2, aes(as.numeric(hours.since.midnight), wind.speed, color = "Wind Speed")) +
    scale_y_continuous(name = "Power Production [MW]", limits = a,
                       sec.axis = sec_axis(trans = trans, name = "Wind Speed [m/s]")) +
    scale_x_continuous(breaks = as.numeric(gridtimes), labels = gridlabels) +
    labs(x = "Time of Day", title = "Average Power Production and Wind Speed over the Day",
         color = "") +
    theme(legend.position = "top")
}

plotPredictedWindEnergy <- function(datetime, data) {
  assertPOSIXct(datetime)
  assertList(data, types = "numeric")
  assertNames(names(data), type = "unique")

  plotdata.long <- data.table::rbindlist(lapply(names(data), function(n) {
    assertNumeric(data[[n]], len = length(datetime), .var.name = sprintf("data[[%s]]", n))
    data.table::data.table(datetime = datetime, energy = data[[n]], source = n)
  }))

  ggplot(plotdata.long, aes(x = datetime, y = energy, color = source)) +
    geom_line() +
    labs(x = "Date", y = "Wind Power Production [MW]", color = "",
       title = "Wind Power Production Prediction") +
    theme(legend.position = "bottom")

}
