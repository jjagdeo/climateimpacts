#' Function to find the average precipitation value in a daily rainfall dataset for a particular location during the given period
#'
#'
#' @param precip precipitation in inches/day
#' @param station describes the name of the precipitation gauge or station
#' @return the average of the recorded rainfall values
#' @return the selected rainfall station
#'

MeanPrecip = function(precip, station = "Cachuma") {
  library(tidyverse)

  precip_subset <- precip %>%
    dplyr::mutate(year_month_day = lubridate::parse_date_time(date, "ymd"), # Convert date column to R recognized dates
                  rainfall = as.numeric(rainfall)) # Convert precip column to numeric

  # Use if statement to filter based on user-inputed station (4 choices: Cachuma, Gibraltar, Jameson, and San Marcos)

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

  # Apply mean function to station's precipitation subset

  precip_subset_mean <- apply(precip_subset, MARGIN = 2, FUN = mean)

return(list(Station = station, Average_Precip = precip_subset_mean))

}

