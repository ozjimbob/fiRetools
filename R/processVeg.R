#' Process Vegetation polygon data to classified rasters
#'
#' @param outdir Directory to output vegetation maps, must contains files produced by createDomain
#' @param veg_polygon SpatVect or SimpleFeatures object containing the vegetation map. Must contain a field with unique vegetation ID.
#' @param veg_LUT Tibble or data.frame containing look-up table linking veg ID to MAX and MIN intervals
#' @param join_field Name fo field joining veg_polygon and veg_LUT
#' @param min_field Field name in look-up table containing minimum interval in years
#' @param max_field Field name in look-up table containing maximum interval in years
#' @param quiet Verbose logging
#'
#' @return Status of processing.
#' @export
#'
#' @examples
#' \dontrun{
#' data("COH_Veg")
#' COH_Veg <- vect(COH_Veg)
#' data("COH_Veg_LUT")
#' processVeg("output",veg_polygon=COH_Veg,veg_LUT=COH_Veg_LUT,join_field="VEGCODE")
#' }
processVeg <- function(outdir,veg_polygon, veg_LUT, join_field, min_field="MIN",max_field="MAX",quiet=TRUE){

  if(!file.exists(paste0(outdir,"/mask.tif"))){stop("Mask file missing in outdir.")}
  if(!file.exists(paste0(outdir,"/template.tif"))){stop("Template file missing in outdir.")}
  if(!file.exists(paste0(outdir,"/ROI.gpkg"))){stop("ROI vector file missing in outdir.")}

  # Load ROI
  ROI <- terra::vect(paste0(outdir,"/ROI.gpkg"))

  # Load Mask
  mask_raster <- terra::rast(paste0(outdir,"/mask.tif"))

  # Load Template
  template_raster <- terra::rast(paste0(outdir,"/template.tif"))

  # Convert veg vect if it is sf
  if("sf" %in% class(veg_polygon)){
    pq("Found sf object, converting to SpatVector.",quiet)
    veg_polygon <- terra::vect(veg_polygon)
  }

  if(!dir.exists(paste0(outdir,"/veg"))){dir.create(paste0(outdir,"/veg"))}


  # if ID is numeric, direct rasterize, otherwise convert to factor and rasterize
  if(!is.numeric(veg_polygon[[join_field]][1])){

    # Assuming LUT contains all classes, convert to factor
    veg_LUT[[join_field]] <- factor(veg_LUT[[join_field]])

    # Add numeric version of factor for raster creation
    veg_LUT$VC_NUM <- as.numeric(veg_LUT[[join_field]])

    # Extract data framt from veg_polygon because vect object are weird to work on
    vdf <- data.frame(veg_polygon)

    # Make polyghon veg field a factor, copy over attributes from LUT factor
    vdf[[join_field]] <- as.factor(vdf[[join_field]])
    attributes(vdf[[join_field]]) <- attributes(veg_LUT[[join_field]])

    # Add new numeric column
    veg_polygon$VC_NUM <- as.numeric(vdf[[join_field]])
    rm(vdf)

    # Make ID raster, mask
    vr <- terra::rasterize(veg_polygon, template_raster,field="VC_NUM",filename=paste0(outdir,"/veg/veg_code.tif"),overwrite=TRUE)
    vr <- vr * mask_raster
    terra::writeRaster(vr,paste0(outdir,"/veg/veg_code.tif"),overwrite=TRUE)

    # Reclassify
    max_mat <- cbind(veg_LUT$VC_NUM,veg_LUT[[max_field]])
    min_mat <- cbind(veg_LUT$VC_NUM,veg_LUT[[min_field]])

    cc_max <- terra::classify(vr,max_mat,othersNA=TRUE,filename=paste0(outdir,"/veg/veg_max.tif"),overwrite=TRUE)
    cc_min <- terra::classify(vr,min_mat,othersNA=TRUE,filename=paste0(outdir,"/veg/veg_min.tif"),overwrite=TRUE)

  }else{
    # Numeric ID field, we can directly raterize and assume LUT uses numeric ID
    vr <- terra::rasterize(veg_polygon, template_raster,field=join_field,filename=paste0(outdir,"/veg/veg_code.tif"),overwrite=TRUE)
    vr <- vr * mask_raster
    terra::writeRaster(vr,paste0(outdir,"/veg/veg_code.tif"),overwrite=TRUE)

    max_mat <- cbind(veg_LUT[[join_field]],veg_LUT[[max_field]])
    min_mat <- cbind(veg_LUT[[join_field]],veg_LUT[[min_field]])

    cc_max <- terra::classify(vr,max_mat,othersNA=TRUE,filename=paste0(outdir,"/veg/veg_max.tif"),overwrite=TRUE)
    cc_min <- terra::classify(vr,min_mat,othersNA=TRUE,filename=paste0(outdir,"/veg/veg_min.tif"),overwrite=TRUE)
  }
  return()

}
