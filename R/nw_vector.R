#' Generate sf vector line in a north-west direction from point
#'
#' @param point sf point object
#' @param dist Distance in meters
#'
#' @return Data frame with slope, r2 value and angle in degrees
#' @export
#'
#' @examples
#' nw_vector(sf::st_as_sf(data.frame(x=521310,y=5244614),coords=c("x","y"),crs=28355),350)
nw_vector=function(point,dist=350){
  out = line_vector(point,315,dist)
  sf::st_sfc(out,crs=sf::st_crs(point))
}
