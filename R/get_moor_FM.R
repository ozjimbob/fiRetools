#' Calculates Moorland FDI Fuel Moisture
#'
#' @param Temp Temperature in degrees C
#' @param Humid Humidity percentage
#' @param RainHours Number of hours over which rainfall fell
#' @param RainAmount Amount of rainfall (mm)
#'
#' @return Moorland Fuel Moisture
#' @export
#'
#' @examples
#' get_moor_FM(12,87,24,22)
get_moor_FM<-function(Temp,Humid, RainHours,RainAmount){
  RF <- get_moor_RF(RainHours,RainAmount)
  HF <- get_moor_HF(Temp,Humid)
  return(max(RF+HF,0))
}
