#' Calculate a vector of day lengths for a given latitude, by julian day
#'
#' @param latitude Latitude of location
#' @param precision Optional, precision of day integral (default value 64)
#'
#' @return Vector of day lengths in hours
#' @export
#'
#' @examples
#' daylengths(-43)
daylengths <- function(latitude,precision=64){
  daylist<-rep(0,366)
  for(a in 1:366){
    daylist[a] <- dayintegral(a,latitude,precision)
  }
  daylist
}
