#' Calculate Landscape Risk
#'
#' @param prop_table Table with required fields created by genereate_property_fields()
#' @param datdf Data frame containing fuel data, including Vegetation_Category, Vegetaion_Type, Vegetation_Community, SFFL (Surface fine fuel load), OFFL (overall fine fuel load) and HGT (Height of elevated fuels in m)
#'
#' @return Data frame with fields FBW for fuel break width, FL for fireline intensity (kW), and DRisk for the risk index (difference between distance to vegetation and fuel break width)
#' @export
#'
#' @examples
#' \dontrun{
#' output <- calc_risk(data_points,fuel_types)
#'}
calc_risk <- function(prop_table,datdf){
  proc_fire <- function(i){
    this_adr <- prop_table[i,]
    vegetation_community = this_adr$VEG_GROUP
    fb_class <- this_adr$fb_class
    slope_type<-this_adr$slope_type
    max_fire_run_distance = this_adr$maxrun
    dist <- this_adr$dist
    effective_slope <- this_adr$effective_slope
    id <- this_adr$id
    ans<-FBW_calc(fb_class,vegetation_community,slope_type,effective_slope,max_fire_run_distance,datdf,id,dist)
  }
  out <- purrr::map_df(1:nrow(prop_table),proc_fire)
  DRisk <- ""
  id <- 0
  prop_table$FBW <- out$FBW
  prop_table$FL <- out$FL
  prop_table$id <- out$id
  prop_table$DRisk <- out$FBW - prop_table$dist
  prop_table <- dplyr::group_by(prop_table,id)
  prop_table <- dplyr::slice(prop_table,which.max(DRisk))
  return(prop_table)
}
