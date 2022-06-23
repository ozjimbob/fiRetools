#' Create template and region of interest (ROI) rasters in an output directory based on input vector ROI, resolution and projection.
#'
#' @param domain SpatVect or SimpleFeatures object contianing the domain of the analysis.  If this contains multiple features, they will be aggregated.
#' @param res Resolution, in project units, of the output template and mask rasters. Defaults to 100.
#' @param proj Projection (CRS) object for the template and mask rasters to be produced, as epsg string eg. "epsg:3308". If none is provided, will use projection of input domain.
#' @param outdir Output directory to write the rasters
#' @param quiet Write progress diagnostics (TRUE/FALSE)
#' @param overwrite Overwrite files (TRUE/FALSE)
#'
#' @return template
#' @export
#'
#' @examples createDomain(myVect, 30, "EPSG:3308","./output")
createDomain <- function(domain,res=100,proj,outdir,quiet=TRUE,overwrite=TRUE){
  # Work out if input is vect or sf, convert to appropriate
  if("sf" %in% class(domain)){
    if(!quiet){
      print("Found sf object, converting to SpatVector.")
    }
    domain <- vect(domain)
  }

  # Do we have a projection? If not get it from input vect:
  if(!is.null(proj)){
    if(!quiet){
      print("Projection provided, projecting input domain.")
    }
    domain <- project(domain,proj)
  }else{
    if(!quiet){
      print("No projection provided, using projection from input domain.")
    }
  }

  proj <- paste0("epsg:",crs(domain,describe=TRUE)$code)
  if(!quiet){
    print(paste0("Projection is: ",proj))
  }

  # aggregate to single polygon
  if(!quiet){
    print("Aggregating domain polygons into single polygon.")
  }
  domain <- aggregate(domain)

  # create raster
  if(!quiet){
    print("Creating template raster.")
  }
  template <- rast(ext(domain),res=res,crs=proj)
  if(!quiet){
    print(paste0("Raster extent: ",ext(template)))
    print(paste0("Raster resolution: ",res(template)[1]," x ",res(template)[2]))
    print(paste0("Raster size: ",ncol(template)," x ",nrow(template)))
  }
  values(template)=1

  if(!quiet){
    print("Masking template by domain.")
  }
  mask <- mask(template,domain)

  if(!quiet){
    print("Writing template.")
  }
  writeRaster(template,paste0(outdir,"/template.tif"),overwrite=overwrite)
  if(!quiet){
    print("Writing mask")
  }
  writeRaster(mask,paste0(outdir,"/mask.tif"),overwrite=overwrite)
  if(!quiet){
    print("Writing vector ROI")
  }
  writeVector(domain,paste0(outdir,"/ROI.gpkg"),overwrite=overwrite)

  return(message)
}
