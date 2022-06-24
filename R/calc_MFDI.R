#' Calculate Moorland Fire Danger Index
#'
#' @param Rain Rainfall amount (mm)
#' @param Temp Temperature (degrees C)
#' @param RH Relative humidity (RH)
#' @param Wind Wind speed
#' @param Age Age of fuel (time since fire)
#' @param Time Vector of time values in minutes or days
#' @param Daily If time data is daily set to TRUE, otherwise if it is in minutes set to FALSE
#'
#' @return Moorland FDI
#' @export
#'
#' @examples
#' calc_MFDI(Rain=c(0,20,10,4),
#' Temp=c(12,13,8,9),
#' RH=c(55,87,90,67),
#' Wind=c(5,8,9,9),
#' Age=c(25,25,25,25),
#' Time=c(1,2,3,4),
#' Daily=TRUE)
calc_MFDI<-function(Rain,Temp,RH,Wind,Age,Time,Daily=FALSE){
  MFDI <- rep(0,length(Rain))
  tdiff<-c(0,diff(Time))
  if(Daily==TRUE){
    tdiff<-tdiff/24
  }else{
    tdiff<-tdiff/3600
  }
  oframe <- data.frame(Rain=Rain,Temp=Temp,RH=RH,Wind=Wind,Age=Age)
  oframe$SinceRain <- 0
  SinceRain<-48
  for(i in seq_along(oframe$Rain)){
    if(oframe$Rain[i]>0){
      SinceRain<-0

    }else{
      SinceRain <- SinceRain + tdiff[i]

    }
    oframe$SinceRain[i]<-SinceRain
  }

  for(i in seq_along(MFDI)){
    FM<-get_moor_FM(oframe$Temp[i],oframe$RH[i], oframe$SinceRain[i],oframe$Rain[i])
    SR<-get_moor_RS(FM,oframe$Wind[i],oframe$Age[i])
    MFDI[i]<-RS_to_MFI(SR)
  }
  return(MFDI)
}

