#' Calculate moorland FDI humidity factor
#'
#' @param Temp Temperature in degrees C
#' @param Humid Humidity percentage
#'
#' @return Moorland FDI Humidity Factor
#' @export
#'
#' @examples
#' get_moor_HF(12,87)
get_moor_HF<-function(Temp,Humid){
  Temp<-max(Temp,8)
  Temp<-min(Temp,35)
  RH<-max(Humid,20)
  RH<-min(Humid,100)
  y <-exp(2.572e+00 + (-2.563e-02 * Temp) + (-3.288e-05 * Temp^2) + (5.218e-03 * Humid) + (6.673e-05 * Humid ^2))
  return(y)
}
