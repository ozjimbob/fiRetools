#' Calculate sun altitude for given latitude, day and time
#'
#' @param latitude Latitude of location
#' @param dayofyear Julian day of year
#' @param hour Time of day (hours from midnight)
#'
#' @return Sunaltitude in degrees
#' @export
#'
#' @examples
#' sunaltitude(-43,20,15)
sunaltitude <- function(latitude,dayofyear,hour){
  decT <- 2.0 * pi * ((dayofyear-1.0)/365.0)
  declination <- 0.322003-22.971*cos(decT)-0.357898*cos(2*decT)-0.14398*cos(3*decT)+3.94638*sin(decT)+0.019334*sin(2*decT)+0.05928*sin(3*decT)
  hourangle <- radians((15*(hour-12.0)))
  altT <- sin(radians(declination))*sin(radians(latitude))
  altU <- cos(radians(declination))*cos(radians(latitude))*cos(hourangle)
  altcalc <- asin(altT + altU)
  altitude <- degrees(altcalc)
  altitude
}
