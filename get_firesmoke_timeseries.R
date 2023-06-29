
library(tidyverse)
library(terra)
library(ncmeta)
library(ncdf4)

# Get firesmoke forecast from https://firesmoke.ca/forecasts/
get_fs_ts <- function(url, lat, lon) {
  fname <- basename(url)
  req <- curl::curl_fetch_disk(url, path = fname)
  fs <- terra::rast(fname)

  # Georeference the raster.
  crs(fs) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84" # lat/long coord system
  fmeta <- ncmeta::nc_atts(fname)
  fmeta <- structure(fmeta$value, .Names = fmeta$name)
  ext(fs) <- with(fmeta, c(XORIG, XORIG + XCELL*NCOLS, YORIG, YORIG + YCELL*NROWS))
  origin(fs) <- c(fmeta$XORIG, fmeta$YORIG)

  # Checking that it worked, looks right
  # fsb <-raster::brick(fs)
  # mapview::mapview(subset(fsb, 1))
  # Extract the time dimension
  # Found this at http://mazamascience.com/Classes/PWFSL_2014/Lesson_07_BlueSky_FirstSteps.html
  nc <- nc_open(fname)
  tflag <- ncvar_get(nc, 'TFLAG', start=c(1,1,1), count=c(-1,-1,-1))
  time_str <- paste0(tflag[1,], sprintf(fmt="%06d", tflag[2,]))
  # We use 'strptime()' to convert our character index to a "POSIXct" value.
  times <- strptime(x=time_str, format="%Y%j%H%M%S", tz="GMT") |>
    lubridate::with_tz("EST")
  nc_close(nc)
  # Get values for a coordinate (NYC)
  vals <- terra::extract(fs, cbind(lon, lat), method = "bilinear") |>
    unlist() |> na.omit()
  fcast <- tibble(
    time = times,
    pm25 = vals,
    timestamp = paste("Forcast as of", times[1]),
    series = "firesmoke.ca Model Forecast NYC"
  )
  fcast
}

url <- c("https://firesmoke.ca/forecasts/current/dispersion.nc")

fcasts <- get_fs_ts(url, lat = 42.2781, lon = -74.9160)
fcasts$time <- as.POSIXct(fcasts$time)

# Get NYC PM2.5 Data from https://a816-dohbesp.nyc.gov/IndicatorPublic/beta/key-topics/airquality/realtime/
# nyc_pm25_data = readr::read_csv("https://azdohv2staticweb.blob.core.windows.net/$web/nyccas_realtime_DEC.csv")
# pdat <- nyc_pm25_data |>
#   rename(time = starttime,
#          pm25 = Value) |>
#   group_by(time) |>
#   summarize(pm25 = mean(pm25)) |>
#   mutate(series = "NYC Community Air Survey Average") |>
#   bind_rows(fcasts) |>
#   filter(time >= Sys.time() - days(7))
#
#Plot
ggplot(fcasts, mapping = aes(x = time, y = pm25)) +
  geom_line() +
  scale_x_datetime(date_breaks = "day", date_labels = "%b %d %I%p") +
  labs(y = expression(PM[2.5]~(μg/m^3))) +
  theme_bw()

#TODO add colors

