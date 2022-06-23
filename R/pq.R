#' Print non-quiet information.
#'
#' @param text Text to be printed
#' @param quiet TRUE/FALSE
#' @keywords internal
#' @return NULL
#'
#' @examples pq("Hello",TRUE)
pq <- function(text,quiet){
  if(!quiet){
    print(text)
  }
}
