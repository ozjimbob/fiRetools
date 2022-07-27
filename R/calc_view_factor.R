#' Calculate View Factor
#'
#' @param FlameAngle Distance (metres)
#' @param dist Distance (metres)
#' @param site_slope Slope of site (degrees)
#' @param Lf Flame length
#' @param h Elevation of receiver (m)
#' @param Wf Flame width
#'
#' @return View Factor
#' @export
#'
#' @examples
#'
#' calc_view_factor(20,15,5,5,2,100)
calc_view_factor<-function(FlameAngle,dist,site_slope,Lf,h,Wf){

  View_factor =0
  flame_angle_c =0
  site_slope_c = 0
  X1=0.0
  Y1=0.0
  X2=0.0
  Y2=0.0

  site_slope_c = site_slope * 2 * pi / 360
  flame_angle_c = FlameAngle * 2 * pi / 360

  if ((dist - 0.5 * Lf * cos(flame_angle_c)) > 0 ) {

    X1 = (Lf * sin(flame_angle_c) - 0.5 * Lf * cos(flame_angle_c) * tan(site_slope_c) - dist * tan(site_slope_c) - h) / (dist - 0.5 * Lf * cos(flame_angle_c))
    Y1 = (0.5 * Wf) / (dist - 0.5 * Lf * cos(flame_angle_c))
    X2 = (h + (dist - 0.5 * Lf * cos(flame_angle_c)) * tan(site_slope_c)) / (dist - 0.5 * Lf * cos(flame_angle_c))
    Y2 = (0.5 * Wf) / (dist - 0.5 * Lf * cos(flame_angle_c))
    View_factor = (1 / pi) * (X1 / sqrt(1 + X1**2) * atan(Y1 / sqrt(1 + X1**2)) + Y1 / sqrt(1 + Y1**2) * atan(X1 / sqrt(1 + Y1**2)) + X2 / sqrt(1 + X2**2) * atan(Y2 / sqrt(1 + X2**2)) + Y2 / sqrt(1 + Y2**2) * atan(X2 / sqrt(1 + Y2**2)));

    return(View_factor)

  } else {

    return(1.0)
  }
}
