#'Volume of snow vs rain (inches)
#'
#'Takes precipitation (inches) and corresponding temperature (F) data and calculates the volume of precipitation that fell as rain vs snow
#'
#'@param climate data frame including precipitation (inches) and temperature (F) observations
#'@return precip_summary a table with volume of precipitation that fell as rain vs snow and average temperature when precipitation fell as rain vs snow.


snow_v_rain = function(climate){
  library(tidyverse)
  precip_summary = climate %>%
    mutate(precip_type = case_when(temp > 32 ~ "Rain",
                                   temp < 32 ~ "Snow")) %>%
             group_by(precip_type) %>%
             summarise(volume = sum(precip),
                       avg_temp = mean(temp))
      return(precip_summary)
}



