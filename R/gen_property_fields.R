#' Precalculate landscape fields reqired for landscape risk calculations
#'
#' @param locations sf point object containing an ID field
#' @param dem Raster object with digtial elevation model of region
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
#' output <- generate_property_fields(data_points,dem,veg,id_field="ID")
#'}
generate_property_fields <- function(locations,
                                     dem,
                                     veg,
                                     urb_types=c("Other natural environments", "Agricultural, urban and exotic vegetation"),
                                     urb_set="Agricultural, urban and exotic vegetation",
                                     id_field="RespondentID"){

  proc_idx <- function(main_idx){
    this_adr <- locations[main_idx,]
    #print(main_idx)
    # Generate compass vectors at 45-degree increments around point
    p1 <- compass_vectors(this_adr,150)

    # Convert into a sf with angles listed
    p1 <- sf::st_sf(p1,dplyr::tibble(angle=seq(0,355,45)))

    # Extract elevation along each vector
    e <- raster::extract(dem,p1,along=TRUE,small=TRUE)

    # Calculate slope and r-squared along each vector
    slp_list <- purrr::map_df(e,sslp)
    slp_list$angle <- seq(0,355,45)

    # Intersect with broad veg group shapefile
    veg_e <- sf::st_intersection(p1,veg)

    # Remove any non-veg categories
    no_urb <- dplyr::filter(veg_e,!VEG_GROUP %in% urb_types)


    if(nrow(no_urb)==0){
      # If it's all urban, then just call this an urban site
      type<-"Urban"
      all_dir=dplyr::tibble(VEG_GROUP="Agricultural, urban and exotic vegetation",
                     angle=0,dist=150,slope=0,slope_type="flat",type=type,maxrun="< 300m")


    }else{
      # If there is some vegetation, go on to calculate
      type<-"Fringe"
      # Calculate length of each vegetation intercept line
      no_urb$length <- as.numeric(sf::st_length(no_urb))
      sf::st_geometry(no_urb)<-NULL
      # Find the total length of vegetation within each angle
      no_urb<- dplyr::group_by(no_urb,angle)
      no_urb<- dplyr::summarise(no_urb,vlen = sum(length))
      # Which angle has the most distance vegetation?
      max_veg<-max(no_urb$vlen)
      # Get a list of directions to test, in case there are multiple maxima
      which_max<-dplyr::filter(no_urb,vlen==max_veg)

      # Try each angle
      dir_list = list()
      for(angle_idx in seq_along(which_max$angle)){
        this_angle = which_max$angle[angle_idx]
        # Subset just this angle
        this_veg<-dplyr::filter(veg_e,angle==this_angle)
        # Calculate lengths for each veg type
        this_veg$vlen<-as.numeric(sf::st_length(this_veg))
        this_veg_ng<-this_veg
        sf::st_geometry(this_veg_ng)<-NULL
        # Aggregate incase vegetation is patchy and broken
        this_veg_ng <- dplyr::group_by(this_veg_ng,VEG_GROUP)
        this_veg_ng <- dplyr::summarise(this_veg_ng,vlen = sum(vlen))
        # Find the veg type with the longest length
        max_veg_group = this_veg_ng$VEG_GROUP[which.max(this_veg_ng$vlen)]
        # Isolate the lines representing this vegetation type
        this_veg_test = dplyr::filter(this_veg,VEG_GROUP==max_veg_group)
        # Calcualte distances
        dist = min(as.numeric(sf::st_distance(this_adr,this_veg_test)))

        # Now work out slope
        this_slp = dplyr::filter(slp_list,angle==this_angle)
        if(this_slp$r > 0.6){
          this_slope = this_slp$slang
          if(this_slope < 0){
            slope_type = "downslope"
          }else{
            slope_type="upslope"
          }
        }else{
          this_slope = 0
          slope_type = "level"
        }
        dir_list[[angle_idx]]=dplyr::tibble(VEG_GROUP=max_veg_group,angle=this_angle,dist=dist,slope=this_slope,slope_type=slope_type)

      }

      all_dir = dplyr::bind_rows(dir_list)
      all_dir$type = type


      # Draw north-west vector
      nw_vec = nw_vector(this_adr)
      nw_vec <- sf::st_sf(nw_vec,dplyr::tibble(angle=315))
      veg_nw <- sf::st_intersection(nw_vec,veg)
      veg_nw <- dplyr::filter(veg_nw,!VEG_GROUP %in% urb_types)
      if(nrow(veg_nw)==0){
        all_dir$maxrun="< 300m"
      }else{
        veg_nw$len = as.numeric(sf::st_length(veg_nw))
        if(max(veg_nw$len)>=300){
          all_dir$maxrun="> 300m"
        }else{
          all_dir$maxrun="< 300m"
        }
      }

    }

    all_dir$id = this_adr[[id_field]]

    all_dir
  }

  out_list <- purrr::map_df(1:nrow(locations),proc_idx)


}
