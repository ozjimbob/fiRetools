#' Get Evapotranspiration value for given Temperature and Soil Dryness Index
#'
#' @param Temp Temperature in Degrees C
#' @param SDI Mount's Soil Dryness Index
#'
#' @return Evapotranspiration value
#' @export
#'
#' @examples
#' get_ET(15,220)
get_ET<-function(Temp,SDI){
  ET_Table<-matrix(c(1,2,4,5,7,8,9,11,13,15,17,19,21,23,25,27,
                    1,2,3,3,4,5,6,8,10,12,14,16,18,20,22,24,
                    0,0,1,1,2,3,4,6,8,10,12,14,16,18,20,22,
                    0,0,1,1,2,2,3,3,4,4,5,5,6,6,7,7,
                    0,0,0,0,0,0,1,1,1,1,2,2,2,2,3,3),nrow=5,ncol=16,byrow=T)
  temp_list<-c(30,35,40,45,50,55,60,65,70,75,80,85,90,95,100,105)
  SDI_list<-c(0,100,200,550,650)
  if(Temp < 30){return(0)}
  if(SDI < 1){SDI<-1}
  temp_col<-max(which(Temp > temp_list))
  sdi_row<-max(which(SDI > SDI_list))
  ETval<-ET_Table[sdi_row,temp_col]
  return(ETval)
}
