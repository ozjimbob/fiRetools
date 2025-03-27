#' Calculator for single Forest Fire Danger Index calculation
#'
#' @param Temperature Temperature in C
#' @param Humid Relative Humidity in %
#' @param Wind Wind Speed in km/h
#' @param DF Drought factor (1-10)
#'
#' @return McArthur FFDI value
#' @export
#'
#' @examples
#'
#' ffdi(18,28,28,5.4)
ffdi <- function(Temperature, Humid,Wind, DF){
  2 * exp(-.45 + .987 * log(DF + .001) - .0345 * Humid + .0338 * Temperature + .0234 * Wind)
}
