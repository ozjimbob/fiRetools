#' Extend and shift a polygon by a given angle and distance
#'
#' @param poly A polygon sf object
#' @param dis Distance to extend in metres
#' @param base_ang Angle in which to extend polygon
#' @param var_ang Lateral angle range in which to shuffle polygon
#'
#' @return A sf polygon object
#' @export
#'
#' @examples
#' \dontrun{
#'
#' shift_poly(vegetation,50,135,6.5)
#' }
shift_poly <- function(poly,dis = 50,base_ang = 135, var_ang = 6.5){
  require(sf)
  #veg <- filter(veg,VEG_GROUP==type)
  base_ang <- 135
  var_ang <- 6.5
  dseq <- seq(10,dis,by=10)
  aseq <- seq(base_ang-var_ang,base_ang+var_ang,2)

  out <- list()
  idx=1
  for(dd in seq_along(dseq)){
   # print(dseq[dd])
    for(ang in aseq){
      #print(ang)
      rang <- ang * pi/180
      vec <- c(sin(rang),cos(rang))
      vec <- vec * dseq[dd]
      poly2 <- poly
      poly2g <-sf::st_geometry(poly2)
      poly2g <- poly2g + vec
      sf::st_geometry(poly2)<-poly2g
      sf::st_crs(poly2)<-sf::st_crs(poly)
      out[[idx]] <- poly2
      idx=idx+1
    }
  }

  out <- bind_rows(out)
  out <- summarise(out)
  out
}
