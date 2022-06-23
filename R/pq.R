#' Print non-quiet information.
#'
#' @param text Text to be printed
#' @param quiet TRUE/FALSE
#' @return NULL
#'
#' @examples pq("Hello",TRUE)
#' @NoRd
pq <- function(text,quiet){
  if(!quiet){
    print(text)
  }
}
