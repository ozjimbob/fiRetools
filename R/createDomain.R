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
#' @examples
#' \dontrun{
#' library(terra)
#' region <- vect("park.gpkg")
#' createDomain(region, res=30, proj="EPSG:3308",outdir="./output",quiet=FALSE)
#' }
createDomain <- function(domain,res=100,proj,outdir,quiet=TRUE,overwrite=TRUE){
  # Work out if input is vect or sf, convert to appropriate
  if("sf" %in% class(domain)){
    pq("Found sf object, converting to SpatVector.",quiet)
    domain <- terra::vect(domain)
  }

  # Do we have a projection? If not get it from input vect:
  if(!is.null(proj)){
    pq("Projection provided, projecting input domain.",quiet)
    domain <- terra::project(domain,proj)
  }else{
    pq("No projection provided, using projection from input domain.",quiet)
  }

  proj <- paste0("epsg:",crs(domain,describe=TRUE)$code)
  pq(paste0("Projection is: ",proj),quiet)

  # aggregate to single polygon
  pq("Aggregating domain polygons into single polygon.",quiet)
  domain <- terra::aggregate(domain)

  # create raster
  pq("Creating template raster.",quiet)
  template <- rast(terra::ext(domain),res=res,crs=proj)
  pq(paste0("Raster extent: ",terra::ext(template)),quiet)
  pq(paste0("Raster resolution: ",terra::res(template)[1]," x ",terra::res(template)[2]),quiet)
  pq(paste0("Raster size: ",terra::ncol(template)," x ",terra::nrow(template)),quiet)
  values(template)=1

  pq("Masking template by domain.",quiet)
  mask <- terra::mask(template,domain)


  pq("Writing template.",quiet)

  terra::writeRaster(template,paste0(outdir,"/template.tif"),overwrite=overwrite)
  pq("Writing mask",quiet)
  terra::writeRaster(mask,paste0(outdir,"/mask.tif"),overwrite=overwrite)
  pq("Writing vector ROI",quiet)
  terra::writeVector(domain,paste0(outdir,"/ROI.gpkg"),overwrite=overwrite)

  out$template <- terra::rast(paste0(outdir,"/template.tif"))
  out$mask <- terra::rast(paste0(outdir,"/mask.tif"))
  out$ROI <- terra::vect(paste0(outdir,"/ROI.gpkg"))
  return(out)
}
