#' Determine Maximum View Factor
#'
#' @param dist Distance (metres)
#' @param Lf Flame length
#' @param site_slope Slope of site (degrees)
#' @param h Elevation of receiver (m)
#' @param Wf Flame width
#'
#' @return MaxViewFactor
#' @export
#'
#' @examples
#'
#' determineMaxViewFactor(20,5,5,2,100)
determineMaxViewFactor = function(dist,Lf,site_slope,h,Wf){

  counter = 0
  if (dist <= 0.5 * Lf * cos(site_slope*pi/180)) {
    MaxViewFactor = 1
    FlameAngleWithMaxView =  atan( sqrt(1 - 4 * dist**2 / Lf**2)/ (2 * dist /Lf)) * 180 / pi

  } else {

    FlameAngle0 = site_slope
    ViewFactor0 = calc_view_factor(FlameAngle0, dist,site_slope,Lf,h,Wf)
    FlameAngleIncrement = 10



    while (FlameAngleIncrement > 0.1 && counter <= 10000) {
      FlameAngle1 = FlameAngle0 + FlameAngleIncrement
      ViewFactor1 = calc_view_factor(FlameAngle1,dist,site_slope,Lf,h,Wf)

      FlameAngle2 = FlameAngle1 + FlameAngleIncrement
      ViewFactor2 = calc_view_factor(FlameAngle2, dist,site_slope,Lf,h,Wf)

      while (ViewFactor1 < ViewFactor0 || ViewFactor1 <= ViewFactor2) {

        ViewFactor0 = ViewFactor1
        FlameAngle0 = FlameAngle1
        ViewFactor1 = ViewFactor2
        FlameAngle1 = FlameAngle2

        FlameAngle2 = FlameAngle1 + FlameAngleIncrement
        ViewFactor2 = calc_view_factor(FlameAngle2, dist,site_slope,Lf,h,Wf)

      }
      FlameAngleIncrement = FlameAngleIncrement / 10
      counter = counter + 1

    }



    MaxViewFactor = round(ViewFactor1*10000)/10000
    FlameAngleWithMaxView = FlameAngle1
  }

  return(MaxViewFactor)
}





