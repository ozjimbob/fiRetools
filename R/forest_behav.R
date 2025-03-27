#' Forest fire behaviour parameters from FFDI and Fuel Load
#'
#' @param FFDI McArthur Forest Fire Danger Index
#' @param FL Fuel Load in t/ha
#' @param GS Ground slope in degrees
#'
#' @return Data frame containing columns for flame height (m), spotting distance (km), rate of spread (km/h) and fireline intensity (kW/m)
#' @export
#'
#' @examples
#'
#' forest_behav(56,12.5,5)
forest_behav <- function(FFDI,FL,GS=0){

  j = (0.0012 * FFDI * FL)
  l = (13 * j + 0.24 * FL) /2
  z = (j * (4.17-(0.033*FL)))-0.36
  v = j * (exp(0.069 * GS))


  if (l<0){
    m=0
  } else{
    m =(l)
  }

  if (z<0){
    y = 0
  }else{
    y =(z)
  }

  if (v<0){
    u=0
  }else{
    u =(v)
  }

  ## FI = HC * WO * ROS
  FI = 18600 * FL * u/36

  data.frame(flame_height=m,
             spotting = y,
             ros = u,
             fireline = FI)

}
