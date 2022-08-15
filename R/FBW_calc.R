#' Fuel Break Width Calculator
#'
#' @param fb_class Fuel break class; string class1 = Managed fuel, class2 = Protective break only
#' @param vegetation_community Vegetation community from the Vegetation_Community field of fuel load data frame
#' @param slope_type Position of property; string upslope, downslope or none
#' @param effective_slope Slope in degrees
#' @param max_fire_run_distance Maximum fire run distance in metres
#' @param datdf Data frame with Vegetation_Community, SFFL (surface) and OFFL (overall) fine fuel loads, understory height in HGT, and Vegetation_Category ("Forest, Rainforest and Woodland", "Shrub, Scrub and Heath","Grassland" or "Tussock Moorland") to select fire model
#' @param id Point ID number
#' @param dist Distance field, unsure purpose
#'
#' @return Maximum fuel break width in m
#' @export
#'
#' @examples
#' \dontrun{
#'
#' FBW_calc("class1","DOB","downslope",5,340,fuel_data)
#' }
FBW_calc<-function(fb_class,
                  vegetation_community,
                  slope_type,
                  effective_slope,
                  max_fire_run_distance,
                  datdf,
                  id,dist){




  # Constants and defaults
print(id)
  vegetation_category = ""
  fuel_type = ""

  RHF_THRESHOLD = 12.5 # Radiant Heat Flux Threshold
  FFDI = 0
  GFDI = 0
  V = 0 # Average Wind Speed
  Ws=0.0 # Surface Fuel Load
  Wo =0.0 # Overall Fuel Load
  VH =0.0 # Average Height of vegetation_type
  ros = 0.0 # Rate of Spread
  fireline_intensity = 0.0;
  Lf = 0.0 # Flame Length
  Wf = 100 # Flame width
  moisture_factor = 5
  age = 20
  FlameAngle = 0.0
  FlameAngleWithMaxView = 0.0
  MaxViewFactor = 0.0000
  Tf = 1100 # Flame temperature
  e = 0.95 # Flame emissivity
  Sigama = 5.67 * 10**-11
  transmissivity = 1.0
  Hc = 18600 # Heat of combustion
  h = 2 # Elevation of receiver
  site_slope =0
  #dist = 0.0
  FBW = 0.0 # Fuel Break Width
  Ta =0 # Ambient Temperature
  RH =0.25; # Relative Humidity
  L = 0.0 # Path length



  # Set based on class
  if (fb_class == "class1") {
    FFDI = 24
    GFDI = 24
    V = 26
    Ta = 305

  } else {
    FFDI = 50
    GFDI = 70
    V = 45
    Ta = 308
  }

  # Set based on fire run

  if(max_fire_run_distance == "< 300m"){
    Wf = 50
  }else{
    Wf = 100
  }


  Ws = datdf$SFFL[datdf$Vegetation_Community == vegetation_community][1]
  Wo = datdf$OFFL[datdf$Vegetation_Community == vegetation_community][1]
  VH = max(0,datdf$HGT[datdf$Vegetation_Community == vegetation_community][1])

  fuel_type = datdf$Vegetation_Category[datdf$Vegetation_Community == vegetation_community][1]

  #print(paste0("veg com:",vegetation_community))
  #print(paste0("fuel type:",fuel_type))
  #print(paste0("Wf:",Wf))
  #print(paste0("FFDI:",FFDI))
  # Start of setback calculation

  n=0
  d=0
  d_increment=1
  Rd=0
  if(slope_type!="downslope"){
    effective_slope=0
  }

  ### Fire Behaviour model

  if (fuel_type == "Forest, Rainforest and Woodland"){

    if (slope_type == "downslope" ) {

      ros = 0.0012 * FFDI * Ws * exp(0.069 * effective_slope)

    } else {

      ros = 0.0012 * FFDI * Ws * exp(-0.069 * effective_slope)

    }

  }


  if (fuel_type == "Shrub, Scrub and Heath"){

    if (slope_type == "downslope" ) {

      ros = 0.023 * V**1.21 * VH**0.54 * exp(0.069 * effective_slope)


    } else {

      ros = 0.023 * V**1.21 * VH**0.54 * exp(-0.069 * effective_slope)

    }

  }

  if (fuel_type == "Grassland"){

    if (slope_type == "downslope" ) {

      ros = 0.13*GFDI * exp(0.069 * effective_slope)

    } else {

      ros = 0.13*GFDI * exp(-0.069 * effective_slope)
    }

  }

  if (fuel_type == "Tussock Moorland"){

    if (slope_type == "downslope" ) {

      ros = 0.024 * V**1.312 * exp(-0.0243 * moisture_factor) * (1 - exp(-0.116 * age)) * exp(0.069 * effective_slope)

    } else {

      ros = 0.024 * V**1.312 * exp(-0.0243 * moisture_factor) * (1 - exp(-0.116 * age)) * exp(-0.069 * effective_slope)

    }

  }

  #print(paste0("ros:",ros))

  fireline_intensity = Hc * Wo * ros/36

  #print(paste0("fireline intensity:",fireline_intensity))

  if (fuel_type == "Forest, Rainforest and Woodland"){

    Lf= (13*ros+0.24*Wo)/2

  }

  if (fuel_type == "Shrub, Scrub and Heath") {

    Lf=0.0775 * fireline_intensity**0.46

  }

  if (fuel_type == "Tussock Moorland") {

    Lf=0.0775 * fireline_intensity**0.46
  }

  if (fuel_type == "Grassland"){

    Lf=1.192 * (fireline_intensity/1000)**0.5

  }

  ros = round(ros*100)/100
  fireline_intensity = round(fireline_intensity)
  Lf = round(Lf*100)/100

  #print(paste0("Lf:",Lf))

  #######

  # minimum NSP
  d=6.0
  minFBW=6.0

  #print(site_slope)
  rec=c()
  while (abs(RHF_THRESHOLD - Rd)>0.01 && n<= 1000) {

    MaxViewFactor = determineMaxViewFactor(d,Lf,site_slope,h,Wf)

    Rd = transmissivity * e * Sigama * MaxViewFactor * Tf ** 4
    rec=c(rec,Rd)
    #print(d)
    #print(Rd)
    #print("")

    n = n + 1

    if (Rd > RHF_THRESHOLD) {

      d = d + d_increment

    } else {

      d = d - d_increment
      d_increment = d_increment / 10
    }
  }

  #print(paste0("RHFt:",RHF_THRESHOLD))
  #print(paste0("Rd:",Rd))
  #print(n)

  if (Rd > RHF_THRESHOLD) {

    d = d - d_increment

  } else {

    d = d + d_increment*10
  }


  if(minFBW > round(d)){
    theFBW = minFBW
  }else{
    theFBW = round(d)
  }
  #plot(rec)
  print(id)
  return(data.frame(id=id,dist=dist,FBW=theFBW,FL=fireline_intensity,Rd=rec[1]))

}
