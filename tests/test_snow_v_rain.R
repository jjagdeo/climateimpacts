#Tests for snow_v_rain function

library(testthat)

test_that("snow_v_rain_works" ,
          {climate_test = data.frame(precip = rnorm(mean = 0.05, sd = 0.08, n = 365), temp = rnorm(mean = 50, sd = 25, n = 365))

          #Test that the total volume of precipitation is not negative
          expect_true(sum(snow_v_rain(climate_test)[,"volume"], na.rm = T) > 0)

          #Test that there are the same number of precipitation and temperature observations
          expect_length(climate_test$precip, length(climate_test$temp))

          #Test that the average temperature of observations classified as rain is larger than the average temp of observations classified as snow
          expect_true(snow_v_rain(climate_test)[1,"avg_temp"] > snow_v_rain(climate_test)[2,"avg_temp"])

          })
