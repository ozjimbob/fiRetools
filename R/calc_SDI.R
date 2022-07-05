#' Calculate Mount's Soil Dryness Index (SDI)
#'
#' @param Rain Daily precipitation in mm
#' @param Temperature Daily maximum temperature in degrees C
#'
#' @return Mount's SDI in mm
#' @export
#'
#' @examples
#' calc_SDI(c(2,5,0,0),c(26,17,18,20))
calc_SDI<-function(Rain,Temperature){

  SDI<-rep(1,length(Rain))
  Canopy_water<-rep(0,length(Rain))

  o_frame<-data.frame(SDI=SDI,Canopy_water=Canopy_water,
                     Rain=rep(0,length(SDI)),
                     ET=rep(0,length(SDI)),
                     runoff=rep(0,length(SDI)),
                     interception=rep(0,length(SDI)),
                     P_eff=rep(0,length(SDI)))

  R<-0.3
  Cc<-8
  W<-2
  FR <- 1/40


  for(idx in 2:length(Rain)){

    RainY<-Rain[idx-1]/.254
    TempF<-Temperature[idx] * (9/5) + 32
    SDIY<-SDI[idx-1]



    C_frac<-R * RainY + Canopy_water[idx-1]
    if(C_frac <= Cc){
      interception<-R * RainY
    }else{
      interception<-Cc-Canopy_water[idx-1]
    }

    if(RainY > 0){
      Canopy_water[idx]<- Canopy_water[idx-1] + interception - W
    }else{
      Canopy_water[idx] <- 0
    }

    runoff <- FR * RainY
    ET <- get_ET(TempF,SDIY)
    P_eff <- RainY - interception - runoff
    SDI[idx] <- max(0,SDIY - P_eff + ET)

  }
  SDI*.254

}
