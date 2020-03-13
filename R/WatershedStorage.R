#' Volume of Water Stored in Watershed
#'
#' Function describing water stored in a watershed/year using simplified inflow/outflow processes
#'
#'
#' @param precip volume of precipitation in inches/year
#' @param evap volume of water evaporated in inches/year
#' @param runoff volume of runoff in inches/year
#' @param watershed_size total area of watershed in sq miles
#' @return storage the volume of water stored in the watershed in cubic ft/year


WatershedStorage = function(precip, evap, runoff, watershed_size) {
  storage = ((precip - (evap + runoff)) * 0.0833333) * (watershed_size * 27880000)
  # Multiply by watershed size to get volumetric storage from rate inputs: precip, evap, runoff
  # Multiple watershed_size (given by user in square miles) by 27,880,000 to convert to square feet
  # Multiply precip - (evap + runoff) (given by user in inches) by 0.0833333 to convert to feet
  return(storage)
  # Storage is returned in units of cubic feet per year
}
