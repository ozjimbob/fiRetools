% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/processVeg.R
\name{processVeg}
\alias{processVeg}
\title{Process Vegetation polygon data to classified rasters}
\usage{
processVeg(
  outdir,
  veg_polygon,
  veg_LUT,
  join_field,
  min_field = "MIN",
  max_field = "MAX",
  form_field = NULL,
  quiet = TRUE
)
}
\arguments{
\item{outdir}{Directory to output vegetation maps, must contains files produced by createDomain}

\item{veg_polygon}{SpatVect or SimpleFeatures object containing the vegetation map. Must contain a field with unique vegetation ID.}

\item{veg_LUT}{Tibble or data.frame containing look-up table linking veg ID to MAX and MIN intervals}

\item{join_field}{Name fo field joining veg_polygon and veg_LUT}

\item{min_field}{Field name in look-up table containing minimum interval in years}

\item{max_field}{Field name in look-up table containing maximum interval in years}

\item{form_field}{(Optional) Field name in look-up table containing the broad vegetation formation or group to summarise results within.}

\item{quiet}{Verbose logging}
}
\value{
Path to outdir for piping.
}
\description{
Process Vegetation polygon data to classified rasters
}
\examples{
\dontrun{

data("COH_Veg")
COH_Veg <- vect(COH_Veg)
data("COH_Veg_LUT")
processVeg("output",veg_polygon=COH_Veg,veg_LUT=COH_Veg_LUT,
    join_field="VEGCODE",form_field="VEG_GROUP")
}
}
