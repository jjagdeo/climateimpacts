# Tests for WatershedStorage() function input data
# Input data stored in the package as 'precip_waterstorage', 'runoff_waterstorage', and 'evap_waterstorage'

library(testthat)

# Test 1: Input precipitation values cannot be negative, so they must be greater than 0
expect_true(length(precip_waterstorage) > 0)

# Test 2: The number of observations in the precipitation data must match those in the number of observations for the runoff data
nrunoff = length(runoff_waterstorage)
expect_length(precip_waterstorage, nrunoff)

# Test 3: For a watershed absent of irrigation, the sum of precipitation should be greater than the sum of evaporation values in the same time period
expect_true(sum(precip_waterstorage) > sum(evap_waterstorage))
