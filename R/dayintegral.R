#' Calculate number of hours of sunlight for a given day
#'
#' @param dayofyear Julian day of year
#' @param latitude Latitude of location
#' @param precision Precision of integral calculation (default 64)
#'
#' @return Hours of sunlight
#' @export
#'
#' @examples
#' dayintegral(20,-43)
dayintegral <- function(dayofyear,latitude,precision=64){
  solarint <- 0.0
  hour=(0:(24*precision))/precision
  altitude <- sunaltitude(latitude,dayofyear,hour)
  solarint<-sum(as.numeric(altitude>0)/precision)
  solarint
}
