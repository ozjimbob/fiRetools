#' Convert Rate of Spread to Moorland FDI
#'
#' @param RS Rate of spread
#'
#' @return Moorland FDI
#' @export
#'
#' @examples
#' RS_to_MFI(4.5)
RS_to_MFI <- function(RS){
  FI<--0.465568 + 0.716654 * RS
  return(FI)
}
