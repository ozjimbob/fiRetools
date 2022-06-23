#' Create template and region of interest (ROI) rasters in an output directory based on input vector ROI, resolution and projection.
#'
#' @param domain SpatVect or SimpleFeatures object contianing the domain of the analysis.  If this contains multiple features, they will be aggregated.
#' @param res Resolution, in project units, of the output template and mask rasters
#' @param proj Projection (CRS) object for the template and mask rasters to be produced
#' @param outdir Output directory to write the rasters
#'
#' @return template
#' @export
#'
#' @examples createDomain(myVect, 30, "EPSG:3308","./output")
createDomain <- function(domain,res,proj,outdir){
  #domain <- aggregate(domain)

  message <- paste0("Hello ", domain, res,proj,outdir, "!")

  return(message)
}
