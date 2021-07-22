library(neonUtilities)
library(tidyverse)
library(lubridate)
options(stringsAsFactors=F)
#Wheeze
# unzip and stack variables
stackByTable("NEON_pressure-air.zip")
stackByTable("NEON_temp-soil.zip")
stackByTable("NEON_conc-co2-soil.zip")
stackByTable("NEON_conc-h2o-soil-salinity.zip")

positions <- c(501:503)


bp <- read_csv('NEON_pressure-air/stackedFiles/BP_30min.csv') %>%   # Barometric pressure
    select(startDateTime,staPresMean)

co2 <- read_csv('NEON_conc-co2-soil/stackedFiles/SCO2C_30_minute.csv')

# Pull in soil temperature, join on air pressure data
soil_temp <- read_csv('NEON_temp-soil/stackedFiles/ST_30_minute.csv') %>%  # Soil Temp
  left_join(bp,by="startDateTime")

soil_h20 <- read_csv('NEON_conc-h2o-soil-salinity/stackedFiles/SWS_30_minute.csv') # Soil Water content


# Group and nest with each sensor
soil_temp_n <- soil_temp %>%
  group_by(startDateTime,horizontalPosition,verticalPosition) %>% nest()

soil_h20_n <- soil_h20 %>%
  group_by(startDateTime,horizontalPosition,verticalPosition) %>% nest()

co2_n <- co2 %>% group_by(startDateTime,horizontalPosition,verticalPosition) %>% nest()




# We are going to nest these in order to compute them

diffusion_coeff <- function(temp,h20) {

if (is.null(temp) | is.null(h20)) {
  return(NA)
} else {
  # Default diffusion coefficient
  bp <- temp %>% pull(staPresMean)
  temp <- temp %>% pull(soilTempMean)
  h20 <- h20 %>% pull(VSWCMean)


  Da0 <- 14.7  # mm^2 s^-1
  Da0 <- Da0 /(1000^2)  # Convert to m^2 / s^1 (1000 mm = 1 m)
  rho_b <- 1.53315 # bulk density calculated from excel across all sites (units g cm-3)
  rho_m <- 2.65  # From Tang (density of mineral soil)

  phi <- 1 - rho_b/rho_m

  Da <- Da0*((temp+273.15)/293.15)^1.75*(bp/101.3)

  alpha = phi - h20
  xi = alpha^(10/3)/phi^2


  Ds = xi*Da

  return(Ds)
}

}


mole_fraction <- function(temp,co2) {

  if (is.null(temp) | is.null(co2)) {
    return(NA)
  } else {
    # Default diffusion coefficient
    bp <- temp %>% pull(staPresMean)
    temp <- temp %>% pull(soilTempMean)
    co2 <- co2 %>% pull(soilCO2concentrationMean)


    # See Tang 2005 for the conversion

  co2_m <- co2 * bp*1000 / (8.3144*(temp+273.15))  # Convert pressure from kPa to Pa, temp to Kelvins.  8.3144 is the universal gas constant

    return(co2_m)
  }

}


# Nest some of the data together

my_data_n <- soil_temp_n %>%
  left_join(soil_h20_n,by=c("startDateTime","horizontalPosition","verticalPosition")) %>%
  rename(data.temp=data.x,
         data.h20=data.y)

my_data_co2_n <- soil_temp_n %>%
  left_join(co2_n,by=c("startDateTime","horizontalPosition","verticalPosition")) %>%
  rename(data.temp=data.x,
         data.co2=data.y)

my_data_d <- my_data_n %>% mutate(ds = map2(data.temp,data.h20,.f=~diffusion_coeff(.x,.y)))

my_data_co2 <- my_data_co2_n %>% mutate(co2_m = map2(data.temp,data.co2,.f=~mole_fraction(.x,.y)))

# Now we need to compute these at the different depths

# So we have the diffusion at depth, now we want to compute the flux at each time
# Assume sensors are at 3, 9, and 21 cm below depth
# Negative sign because we have depth below the surface!
z1 <- -.03   # converted to meters
z2 <- -.09
z3 <- -.21



za <- z1/2
zb <- (z2 - z1)/2
zc <- (z3 - z2)/2

depths <- c(z1,z2,z3)
mean_depths <- c(za,zb,zc)

# Compute the co2 gradients, unnest, group by date and horizontal position
co2_g_n <- my_data_co2 %>% select(1:3,6) %>% unnest() %>% group_by(startDateTime,horizontalPosition) %>% nest() %>%
  mutate(co2_df = map(data,.f=function(.x) {
    in_data <- .x %>% filter(verticalPosition %in% 501:503) %>%
      pull(co2_m)

      data.frame(gradients= -diff(in_data) / (-diff(depths)),position=c(501,502)) })) %>%
  select(-data)

diffusion_coeffs <- my_data_d %>% select(startDateTime,horizontalPosition,verticalPosition,ds) %>% unnest() %>% group_by(startDateTime,horizontalPosition) %>% nest()

# Now we want to take the diffusion coeffs and multiply them by the fluxes at that given depth.



flux_n <- diffusion_coeffs %>% left_join(co2_g_n,by=c("startDateTime","horizontalPosition"))

# Now multiply the two together
flux_gradients <- flux_n %>% mutate(data = map2(data,co2_df,.f=function(.x,.y){
  if(!is.null(.x) & !is.null(.y) ) {
    mean_depths <- c(zc,zb)
    joined_data <- .x %>% left_join(.y,by=c("verticalPosition"="position")) %>%
    mutate(flux = -ds*gradients) %>%
      filter(verticalPosition %in% c(501,502)) %>%
      mutate(diff_f = mean_depths*flux)  %>%
      summarize(f0=-diff(diff_f)/(zb-za)) %>%
      pull(f0)

    return(joined_data)
  } else { return(NA)}


}))

# We just need to get the conversion from ppm to mole fraction, and then we should be done

diffusion_coeffs %>% unnest() %>% filter(horizontalPosition =="001") %>% ggplot(aes(x=startDateTime,y=ds,color=as.factor(verticalPosition))) + geom_point()

<- my_data_d %>% select(startDateTime,horizontalPosition,verticalPosition,ds) %>% unnest() %>% group_by(startDateTime,horizontalPosition) %>% nest()



co2 %>% filter(horizontalPosition =="001") %>% ggplot(aes(x=startDateTime,y=soilCO2concentrationMean,color=as.factor(verticalPosition))) + geom_point()

# Now let's compute the fluxes - yay!
flux_gradients %>% filter(horizontalPosition=="005") %>% select(-co2_df) %>% unnest() %>%
  ggplot(aes(x=startDateTime,y=data,color=horizontalPosition)) + geom_point() +
  labs(x='Time',y = 'Soil Efflux') +
  theme_bw(base_size = 16, base_family = "Helvetica") +
  theme(axis.title.x=element_text(face="bold"),axis.title.y=element_text(face="bold"),strip.background = element_rect(colour="white", fill="white"))+
  guides(color=FALSE) + ylim(c(0,7))


soil_h20 %>% ggplot(aes(x=startDateTime,y=VSWCMean,color=as.factor(verticalPosition))) + geom_line()

