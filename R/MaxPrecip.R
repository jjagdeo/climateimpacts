#' Function to find the greatest recorded precipitation values in a daily rainfall dataset for a particular location during the given period
#'
#'
#' @param precip precipitation in inches/day
#' @param station describes the number or name of the precipitation gauge or station
#' @return the greatest recorded rainfall value
#' @return the data of the greatest recorded rainfall value
#' @return the selected rainfall station

MaxPrecip = function(precip, station = "Cachuma") {
  library(tidyverse)
  precip_subset <- precip %>%
    dplyr::mutate(year_month_day = lubridate::parse_date_time(date, "ymd"),
           # year = lubridate::year(date),
           # month = lubridate::month(date),
           # day = lubridate::day(date),
           rainfall = as.numeric(rainfall)) %>%
    dplyr::filter(station == station) %>%
    dplyr::select(rainfall)

  precip_subset_max <- apply(precip_subset, MARGIN = 2, FUN = max)

return(list(Date = date, Station = station, Max_Precip = precip_subset_max))

}

