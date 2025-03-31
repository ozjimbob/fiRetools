#' Calculate slope based on regression over a given distance and angle across a DEM
#'
#' @param point sf object with a point
#' @param distance Distance in metres
#' @param angle Compass vector angle
#' @param dem RASTER dem object
#'
#' @return Slope
#' @export
#' @examples
#'
#'  data("COH_DEM")
#'  slope_vector(sf::st_as_sf(data.frame(x=526000,y=5248614),coords=c("x","y"),crs=28355),150,37,COH_DEM)

slope_vector<-function(point,distance,angle,dem){
    v1 <- line_vector(point,angle,distance)
    v1 <- sf::st_sfc(v1,crs=sf::st_crs(point))
    v1 <- sf::st_sf(v1,data.frame(n=0))
    ex <- raster::extract(dem,v1,along=TRUE,small=TRUE)[[1]]
    sslp(ex)
}
