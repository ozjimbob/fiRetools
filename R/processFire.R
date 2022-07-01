#' Process input fire history vector data to produce yearly history rasters
#'
#' @param outdir Directory to output fire history, must contains files produced by createDomain
#' @param fire_history SpatVect or SimpleFeatures object contianing the fire history polygons. Must contain a field with fire date as a four-digit year.
#' @param season_field Name of field in SpatVect containing year/season of fire
#' @param start_year Optional, start year for history calculation. Years before this will be excluded.
#' @param end_year Optional end year for history production. Years after this will be excluded. If this year is beyond the end year in the history, empty future projection rasters will be produced.
#' @param quiet Print progress information
#'
#' @return Status of processing.
#' @export
#'
#' @examples
#' \dontrun{
#' processFire("output",fire_history=fire_data,season_file="Year")
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
  name_find <- which(season_field %in% names(fire_history))

  if(length(name_find)==0){stop("Fieldname not found in fire history")}

  # Rename field
  names(fire_history)[name_find] = "SEASON"

  # set start/end year
  if(is.null(end_year)){
    start_year <- min(fire_history$SEASON)
  }
  if(is.null(end_year)){
    end_year <- max(fire_history$SEASON)
  }

  year_list <- start_year:end_year

  # project fire history to ROI
  fire_history <- terra::project(fire_history,ROI)

  # Clip history to ROI
  fire_history <- terra::intersect(fire_history,ROI)

  if(!dir.exists(paste0(outdir,"/fire_binary"))){dir.create(paste0(outdir,"/fire_binary"))}

  # Rasterize each year
  for(this_year in year_list){
    print(this_year)
    this_year_fh <- terra::subset(fire_history,fire_history$SEASON==this_year)

    # Check if no fires
    if(nrow(this_year_fh)==0){
      blank_year <- mask_raster
      blank_year <- blank_year-1
      terra::writeRaster(blank_year,paste0(outdir,"/fire_binary/fire_",this_year,".tif"),overwrite=TRUE)
      next
    }

    print("Set Value")
    this_year_fh$VALUE = 1
    print("rasterize")
    out <- terra::rasterize(this_year_fh, template_raster,field="VALUE",background=0)
    print("Mask")
    out <- out * mask_raster
    print("Write")
    terra::writeRaster(out,paste0(outdir,"/fire_binary/fire_",this_year,".tif"),overwrite=TRUE)
  }

  # Next, for each year, calculate last year burnt.
  if(!dir.exists(paste0(outdir,"/fire_LYB"))){dir.create(paste0(outdir,"/fire_LYB"))}

  idx <- 0
  for(this_year in year_list){
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
    this_rast <- terra::rast(paste0(outdir,"/fire_LYB/LYB_",this_year,".tif"))
    terra::values(this_rast)[terra::values(this_rast)==0]<-NA
    this_rast <- this_year - this_rast
    terra::writeRaster(this_rast,paste0(outdir,"/fire_TSL/TSL_",this_year,".tif"),overwrite=TRUE)
  }

  ### NEXT PROGRESSIVE TIMES BURNT


  return()

}
