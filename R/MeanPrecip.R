#' Function to find the average precipitation value in a daily rainfall dataset for a particular location during the given period
#'
#'
#' @param precip precipitation in inches/day
#' @param station describes the number or name of the precipitation gauge or station
#' @return the average of the recorded rainfall values
#' @return the data of the greatest recorded rainfall value
#' @return the selected rainfall station
#'

MeanPrecip = function(precip, station = "Cachuma") {
  library(tidyverse)

  precip_subset <- precip %>%
    dplyr::mutate(year_month_day = lubridate::parse_date_time(date, "ymd"),
                  rainfall = as.numeric(rainfall))

  if (station == "Cachuma")
    precip_subset <- precip_subset %>%
      dplyr::filter(station == "Cachuma") %>%
      dplyr::select(rainfall)

  if (station == "Gibraltar")
    precip_subset <- precip_subset %>%
      dplyr::filter(station == "Gibraltar") %>%
      dplyr::select(rainfall)

  if (station == "Jameson")
    precip_subset <- precip_subset %>%
      dplyr::filter(station == "Jameson") %>%
      dplyr::select(rainfall)

  if (station == "San Marcos")
    precip_subset <- precip_subset %>%
      dplyr::filter(station == "San Marcos") %>%
      dplyr::select(rainfall)

  precip_subset_mean <- apply(precip_subset, MARGIN = 2, FUN = mean)

return(list(Date = date, Station = station, Average_Precip = precip_subset_mean))

}

