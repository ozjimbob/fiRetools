
# fiRetools

<!-- badges: start -->
[![R-CMD-check](https://github.com/ozjimbob/fiRetools/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ozjimbob/fiRetools/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

fiRetools is an R package version of the FireTools vegetation fire status tool.  Using vector data of fire history, vegetation type and a look-up table of fire attributes (Maximum/Minimum burn intervals), you can produce maps of vegetation status, as well as timeseries analyses of area within each class, and associated landscape metrics.

The fiRetools package also includes functions for radiant heat risk calculation, fire weather indices, fire behaviour and ember transport distance. 

## Installation

You can install the development version of fiRetools from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("ozjimbob/fiRetools")
```

## FFDI Calculation

Simple McArthur Forest Fire Danger Index calculation

``` r
library(fiRetools)
ffdi(Temperature=34,Humid=18, Wind=37,DF=9)
```

