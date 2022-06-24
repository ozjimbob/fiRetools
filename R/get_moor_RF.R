#' Calculate Moorland FDI Rainfall Factor
#'
#' @param RainHours Number of hours over which rainfall fell
#' @param RainAmount Amount of rain in mm
#'
#' @return Moorland FDI Rainfall Factor
#' @export
#'
#' @examples
#' get_moor_RF(24,20)
get_moor_RF<-function(RainHours,RainAmount){
  RF_Table<-matrix(c(0,10,18,31,53,64,
                    0,8,14,24,41,50,
                    0,6,11,19,32,38,
                    0,4,8,14,25,30,
                    0,3,6,11,19,23,
                    0,1,2,4,7,8,
                    0,0,1,1,1,1),nrow=7,ncol=6,byrow=T)
  RainAmountList<-c(0,0.05,0.1,0.2,0.5,1)
  RainHourList<-c(0,3,6,9,12,24,48)
  if(RainHours > 48){return(0)}
  RAcol <- max(which(RainAmount >= RainAmountList))
  RHrow <- max(which(RainHours >= RainHourList))
  RFval <- RF_Table[RHrow,RAcol]
  return(RFval)
}
