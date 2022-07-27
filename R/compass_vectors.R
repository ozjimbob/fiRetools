#' Construct 8 line vectors in cardinal directions from a point over a distance
#'
#' @param point sf object with a point
#' @param distance Distance in metres
#'
#' @return sf object with line vectors
#' @export
#' @examples
#'
#'  compass_vectors(sf::st_as_sf(data.frame(x=521310,y=5244614),coords=c("x","y"),crs=28355),50)

compass_vectors<-function(point,distance){
  out = list()
  aseq = seq(0,355,45)+1
  for(ang in seq_along(aseq)){
    out[[ang]] = line_vector(point,aseq[ang],distance)
  }
  sf::st_sfc(out,crs=sf::st_crs(point))
}
