#How much snow vs rain

#if temp is greater than 32, but precip gets added to rain total.

snow_v_rain = function(climate){
  library(tidyverse)
  precip_summary<- climate %>%
    mutate(precip_type = case_when(temp > 32 == "Rain",
                                   temp < 32 == "Snow")) %>%
             group_by(precip_type) %>%
             summarise(volume = sum(precip),
                       avg_temp = mean(temp))
      return(precip_summary)
}



