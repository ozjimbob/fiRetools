#' Modelled maximum 5 ember/m-2 ember distance
#'
#' @param FFDI McArthur Forest Fire Danger Index
#' @param FL Fuel Load in t/ha
#'
#' @return Distance in metres of outer ember transport distance
#' @export
#'
#' @examples
#'
#' ember_dist(45,15.5)
ember_dist = function(FFDI,FL){
  exp(2.86250094 + 0.01932069 * FFDI + 0.06030336 * FL)
}
