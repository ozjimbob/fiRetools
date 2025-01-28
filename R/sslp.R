#' Regress slope from vector of elevations
#'
#' @param y Vector of elevations spread over the distance
#' @param dist Distance in meters
#'
#' @return Data frame with slope, r2 value and angle in degrees
#' @export
#'
#' @examples
#' sslp(c(20,30,45,40,80,91))
sslp <- function(y,dist=150){
  if(all(is.na(y))){
    y=rep(0,length(y))
  }
  mny = mean(y,na.rm=TRUE)
  y[is.na(y)]=mny
  x=seq(0,dist,length.out=length(y))
  m = stats::lm(y~x)
  ang = tan(stats::coef(m)[2])/pi*180
  r=summary(m)$r.squared
  if(is.nan(r)){
    r=0
  }
  data.frame(slope=stats::coef(m)[2],r=r,slang=ang)
}
