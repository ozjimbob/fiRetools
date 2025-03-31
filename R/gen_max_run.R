#' Precalculate max fire run distance
#'
#' @param locations sf point object containing an ID field
#' @param veg Vegetation polygon layer with vegetation type in VEG_GROUP field
#' @param urb_types List of vegetation types to treat as urban/no fire risk
#' @param urb_set Vegetation type string to associate with urban in output
#' @param id_field Name of field contianing ID
#'
#' @return sf point object with landscape fields added, ready for risk calculation
#' @export
#'
#' @examples
#' \dontrun{
#' output <- generate_max_run(data_points,veg,id_field="ID")
#'}
generate_max_run <- function(locations,
                                     veg,
                                     urb_types=c("Other natural environments", "Agricultural, urban and exotic vegetation"),
                                     urb_set="Agricultural, urban and exotic vegetation",
                                     id_field="RespondentID"){
  VEG_GROUP<-""
  angle <- ""
  vlen <- ""
  proc_idx <- function(main_idx){
    this_adr <- locations[main_idx,]
    #print(main_idx)
    # Generate compass vectors at 45-degree increments around point

      # Draw north-west vector
      nw_vec = nw_vector(this_adr)
      nw_vec <- sf::st_sf(nw_vec,dplyr::tibble(angle=315))
      veg_nw <- sf::st_intersection(nw_vec,veg)
      veg_nw <- dplyr::filter(veg_nw,!VEG_GROUP %in% urb_types)
      if(nrow(veg_nw)==0){
        maxrun="< 300m"
      }else{
        veg_nw$len = as.numeric(sf::st_length(veg_nw))
        if(max(veg_nw$len)>=300){
          maxrun="> 300m"
        }else{
          maxrun="< 300m"
        }
      }

    ##

    all_dir<-tibble::tibble(id=this_adr[[id_field]], maxrun=maxrun)

    all_dir
  }

  out_list <- purrr::map_df(1:nrow(locations),proc_idx)

  out_list
  return(out_list)
}

