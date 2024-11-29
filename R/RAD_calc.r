#' Radiant Heat Calculator
#'
#' @param FFDI Forest Fire Danger Index
#' @param GFDI Grassland Fire Danger Index
#' @param V 10m Wind Speed (kmh)
#' @param Ta Ambient Temperature (C)
#' @param Ws Surface Fuel Load (t/ha)
#' @param Wo Overall Fuel load (t/ha)
#' @param HGT Understory height (m, for heathland, grassland or shrub, all others set to -1)
#' @param fuel_type (string, Forest/Shrub/Grass/Moorland)
#' @param vegetation_community Vegetation community from the Vegetation_Community field of fuel load data frame
#' @param slope_type Position of property; string upslope, downslope or none
#' @param effective_slope Slope in degrees
#' @param max_fire_run_distance Maximum fire run distance in metres
#' @param dist Distance of house from fireline (m)
#' @return Radiant heat flux in kW/m2
#' @export
#'
#' @examples
#' \dontrun{
#'
#' RAD_calc()
#' }
RAD_calc<-function(FFDI = 24,
                   GFDI = 24,
                   V=26,
                   Ta=31,
                   Ws=20,
                   Wo=30,
                   HGT=-1,
                   fuel_type="Forest",
                   slope_type="upslope",
                   effective_slope=5,
                   max_fire_run_distance="< 300m",
                   dist = 50
                   ){




  # Constants and defaults
  #print(id)
  RHF_THRESHOLD = 12.5 # Radiant Heat Flux Threshold

  V = 0 # Average Wind Speed

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
  #dist = 0.0
  FBW = 0.0 # Fuel Break Width
  Ta =Ta + 273.15
  RH =0.25; # Relative Humidity
  L = 0.0 # Path length

    # Set based on fire run

  if(max_fire_run_distance == "< 300m"){
    Wf = 50
  }else{
    Wf = 100
  }

  VH = max(0,HGT)

  #fuel_type = datdf$Vegetation_Category[datdf$Vegetation_Community == vegetation_community][1]

  #print(paste0("veg com:",vegetation_community))
  #print(paste0("fuel type:",fuel_type))
  #print(paste0("Wf:",Wf))
  #print(paste0("FFDI:",FFDI))
  # Start of setback calculation

  n=0
  Rd=0
  if(slope_type!="downslope"){
    effective_slope=0
  }

  ### Fire Behaviour model

  if (fuel_type == "Forest"){

    if (slope_type == "downslope" ) {

      ros = 0.0012 * FFDI * Ws * exp(0.069 * effective_slope)

    } else {

      ros = 0.0012 * FFDI * Ws * exp(-0.069 * effective_slope)

    }

  }


  if (fuel_type == "Shrub"){

    if (slope_type == "downslope" ) {

      ros = 0.023 * V**1.21 * VH**0.54 * exp(0.069 * effective_slope)


    } else {

      ros = 0.023 * V**1.21 * VH**0.54 * exp(-0.069 * effective_slope)

    }

  }

  if (fuel_type == "Grass"){

    if (slope_type == "downslope" ) {

      ros = 0.13*GFDI * exp(0.069 * effective_slope)

    } else {

      ros = 0.13*GFDI * exp(-0.069 * effective_slope)
    }

  }

  if (fuel_type == "Moorland"){

    if (slope_type == "downslope" ) {

      ros = 0.024 * V**1.312 * exp(-0.0243 * moisture_factor) * (1 - exp(-0.116 * age)) * exp(0.069 * effective_slope)

    } else {

      ros = 0.024 * V**1.312 * exp(-0.0243 * moisture_factor) * (1 - exp(-0.116 * age)) * exp(-0.069 * effective_slope)

    }

  }

  #print(paste0("ros:",ros))

  fireline_intensity = Hc * Wo * ros/36

  #print(paste0("fireline intensity:",fireline_intensity))

  if (fuel_type == "Forest"){

    Lf= (13*ros+0.24*Wo)/2

  }

  if (fuel_type == "Shrub") {

    Lf=0.0775 * fireline_intensity**0.46

  }

  if (fuel_type == "Moorland") {

    Lf=0.0775 * fireline_intensity**0.46
  }

  if (fuel_type == "Grass"){

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
  MaxViewFactor = determineMaxViewFactor(dist,Lf,site_slope,h,Wf)

  Rd = transmissivity * e * Sigama * MaxViewFactor * Tf ** 4

  return(Rd)

}
