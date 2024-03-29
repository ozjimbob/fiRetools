% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_risk.R
\name{calc_risk}
\alias{calc_risk}
\title{Calculate Landscape Risk}
\usage{
calc_risk(prop_table, datdf)
}
\arguments{
\item{prop_table}{Table with required fields created by genereate_property_fields()}

\item{datdf}{Data frame containing fuel data, including Vegetation_Category, Vegetaion_Type, Vegetation_Community, SFFL (Surface fine fuel load), OFFL (overall fine fuel load) and HGT (Height of elevated fuels in m)}
}
\value{
Data frame with fields FBW for fuel break width, FL for fireline intensity (kW), and DRisk for the risk index (difference between distance to vegetation and fuel break width)
}
\description{
Calculate Landscape Risk
}
\examples{
\dontrun{
output <- calc_risk(data_points,fuel_types)
}
}
