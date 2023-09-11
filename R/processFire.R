#' Process input fire history vector data to produce yearly history rasters
#'
#' @param outdir Directory to output fire history, must contains files produced by createDomain
#' @param fire_history SpatVect or SimpleFeatures object contianing the fire history polygons. Must contain a field with fire date as a four-digit year.
#' @param season_field Name of field in SpatVect containing year/season of fire
#' @param start_year Optional, start year for history calculation. Years before this will be excluded.
#' @param end_year Optional end year for history production. Years after this will be excluded. If this year is beyond the end year in the history, empty future projection rasters will be produced.
#' @param quiet Print progress information
#'
#' @return Path to outdir for piping.
#' @export
#'
#' @examples
#' \dontrun{
#' data("COH_Fire")
#' fire_history <- vect(COH_Fire)
#' processFire("output",fire_history=fire_data,season_field="Year")
#' }
processFire <- function(outdir,fire_history,season_field,start_year=NULL,end_year=NULL,quiet=TRUE){

  if(!file.exists(paste0(outdir,"/mask.tif"))){stop("Mask file missing in outdir.")}
  if(!file.exists(paste0(outdir,"/template.tif"))){stop("Template file missing in outdir.")}
  if(!file.exists(paste0(outdir,"/ROI.gpkg"))){stop("ROI vector file missing in outdir.")}

  # Load ROI
  ROI <- terra::vect(paste0(outdir,"/ROI.gpkg"))

  # Load Mask
  mask_raster <- terra::rast(paste0(outdir,"/mask.tif"))

  # Load Template
  template_raster <- terra::rast(paste0(outdir,"/template.tif"))

  # Convert fire history to vect if it is sf
  if("sf" %in% class(fire_history)){
    pq("Found sf object, converting to SpatVector.",quiet)
    fire_history <- terra::vect(fire_history)
  }

  # Find and set name of fire season
  name_find <- which(names(fire_history)==season_field)

  if(length(name_find)==0){stop("Fieldname not found in fire history")}

  # Rename field
  names(fire_history)[name_find] = "SEASON_ftr"
  fire_history$SEASON_ftr = as.numeric(fire_history$SEASON_ftr)

  # set start/end year
  if(is.null(start_year)){
    start_year <- min(fire_history$SEASON_ftr)
  }
  if(is.null(end_year)){
    end_year <- max(fire_history$SEASON_ftr)
  }
  if(end_year <= start_year){stop("End year less than start year, or not enough years.")}
  year_list <- start_year:end_year

  # project fire history to ROI
  fire_history <- terra::project(fire_history,ROI)

  # Clip history to ROI
  fire_history <- terra::intersect(fire_history,ROI)

  if(!dir.exists(paste0(outdir,"/fire_binary"))){dir.create(paste0(outdir,"/fire_binary"))}

  # Rasterize each year
  for(this_year in year_list){
    pq(paste0("Rasterizing year: ",this_year),quiet)
    this_year_fh <- terra::subset(fire_history,fire_history$SEASON_ftr==this_year)

    # Check if no fires
    if(nrow(this_year_fh)==0){
      blank_year <- mask_raster
      blank_year <- blank_year-1
      terra::writeRaster(blank_year,paste0(outdir,"/fire_binary/fire_",this_year,".tif"),overwrite=TRUE)
      next
    }

    this_year_fh$VALUE = 1
    out <- terra::rasterize(this_year_fh, template_raster,field="VALUE",background=0)
    out <- out * mask_raster
    terra::writeRaster(out,paste0(outdir,"/fire_binary/fire_",this_year,".tif"),overwrite=TRUE)
  }

  # Next, for each year, calculate last year burnt.
  if(!dir.exists(paste0(outdir,"/fire_LYB"))){dir.create(paste0(outdir,"/fire_LYB"))}

  idx <- 0
  for(this_year in year_list){
    pq(paste0("Calculating last year burnt, year: ",this_year),quiet)
    this_rast <- terra::rast(paste0(outdir,"/fire_binary/fire_",this_year,".tif"))
    this_rast <- this_rast * this_year
    if(idx > 0){
      last_rast <- terra::rast(paste0(outdir,"/fire_LYB/LYB_",this_year-1,".tif"))
      this_rast <- max(this_rast,last_rast)
    }
    idx <- idx+1
    terra::writeRaster(this_rast,paste0(outdir,"/fire_LYB/LYB_",this_year,".tif"),overwrite=TRUE)
  }

  # Next, for each year, calculate time since last.
  if(!dir.exists(paste0(outdir,"/fire_TSL"))){dir.create(paste0(outdir,"/fire_TSL"))}

  for(this_year in year_list){
    pq(paste0("Calculating time since last, year: ",this_year),quiet)
    this_rast <- terra::rast(paste0(outdir,"/fire_LYB/LYB_",this_year,".tif"))
    terra::values(this_rast)[terra::values(this_rast)==0]<-NA
    this_rast <- this_year - this_rast
    terra::writeRaster(this_rast,paste0(outdir,"/fire_TSL/TSL_",this_year,".tif"),overwrite=TRUE)
  }

  if(!dir.exists(paste0(outdir,"/fire_TB"))){dir.create(paste0(outdir,"/fire_TB"))}

  ### NEXT PROGRESSIVE TIMES BURNT
  idx<-1
  for(this_year in year_list){
    pq(paste0("Calculating times burnt, year: ",this_year),quiet)
    input_rast<-terra::rast(paste0(outdir,"/fire_binary/fire_",this_year,".tif"))
    if(idx==1){
      this_rast <- input_rast
    }else{
      this_rast <- this_rast + input_rast
    }
    idx<-idx+1
    terra::writeRaster(this_rast,paste0(outdir,"/fire_TB/TB_",this_year,".tif"),overwrite=TRUE)
  }

  year_list <- dplyr::tibble(year=year_list)
  readr::write_csv(year_list,paste0(outdir,"/year_list.csv"))
  return(outdir)

}
