#' Construct line vector from a point over a given distance
#'
#' @param point sf object with a point
#' @param angle Compass angle in degrees
#' @param distance Distance in metres
#'
#' @return sf LineString object of vector
#'
#' @examples
#' \dontrun{
#'
#' line_vector(point,15,50)
#' }
line_vector<-function(point,angle,distance){
  cord = sf::st_coordinates(point)
  X=cord[1]
  Y=cord[2]
  angle=angle*pi/180
  x=X+distance*sin(angle)
  y=Y+distance*cos(angle)
  pts=matrix(c(X,x,Y,y),ncol=2)
  lin=sf::st_linestring(pts)
  lin
}
