
require(checkmate)

# ggplot theme
ggplot2::theme_set(ggplot2::theme_minimal())

# data locations
path.raw <- "data/raw"
path.intermediate <- "data/intermediate"

file.stations <- file.path(path.raw, "stations.txt")
file.measure <- file.path(path.raw, "wind_raw.xz")
file.power <- file.path(path.raw, "PowerGen.zip")


end.date.max <- as.Date("2023-11-01")
start.date.min <- as.Date("2022-05-25")

# plotting parameters
bins.long <- c(6, 8, 10, 12, 14)
bins.lat <- c(47, 49, 51, 53, 55)
start.date.train <- as.Date("2022-06-01")
end.date.train <- as.Date("2023-06-01")
start.date.predict <- as.Date("2023-07-02")
end.date.predict <- as.Date("2023-07-12")
