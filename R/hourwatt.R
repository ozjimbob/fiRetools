#' Calculate radiation in W/m2 for a given latitude, hour of day and day of year
#'
#' @param dayofyear Julian Day of year
#' @param latitude Latitude of location
#' @param hour Hour after midnight
#'
#' @return Radiation (W/m2)
#' @export
#'
#' @examples
#' hourwatt(20,-43,17)
hourwatt <- function(dayofyear,latitude,hour){
  altitude_deg<-sunaltitude(latitude,dayofyear,hour)
  if(altitude_deg <= 0){
    radi <- 0
  }else{
    GetAirMassRatio <- (1/sin(radians(altitude_deg)))
    GetApparentExtraterrestrialFlux<-1160 + (75 * sin((360/365) * (dayofyear - 275)))
    GetOpticalDepth <- 0.174 + (0.035 * sin((360/365) * (dayofyear - 100)))
    radi <- GetApparentExtraterrestrialFlux * exp(-1 * GetOpticalDepth * GetAirMassRatio)
  }
  return(radi)
}
