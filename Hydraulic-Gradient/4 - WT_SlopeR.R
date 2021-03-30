library(tidyverse)
library(raster)
library(rgdal)
library(sf)
library(lubridate)
library(kableExtra)


# This script extracts the water table elevation data along a transect of points from interpolated water table rasters, calculates their slopes and saves into a datatable over time.


pts <- read_sf("Hydraulic-Gradient/gradient_pts.gpkg")
dataday = seq(from = as.Date("2019-05-01"), to = as.Date("2020-11-01"), by = "month")
# Test day dataday = "2019-05-01"

gradients <-function(dataday){
  
  gradient <- raster(paste0("Kriging-Watertable/kriged_tiffs/", dataday, ".tif"))
  
  pts$wt <- raster::extract(x = gradient, y = pts)
  
  ptsdat <- as_tibble(pts) %>% 
    summarise(date = dataday,
              slope = abs((last(wt) - first(wt))/(last(distance)-first(distance))), #m/m rise/run
              darcyq = slope*(2.798596e-05)*31557600) # value for conductivity from Theis script in cm/s. units here cm/y 
}

slopes <- lapply(dataday, gradients) %>% bind_rows()


slopes %>% 
  ggplot()+
  geom_line(aes(x = date, y = slope))+
  labs(title = "Water Table Gradient", x = "Date", y = "dh/dx")

slopes %>% 
  ggplot()+
  geom_line(aes(x = date, y = darcyq))+
  labs(title = "Groundwater Flow", x = "Date", y = "Darcy Flux (cm/y)")

gradsummary <- slopes %>% 
  summarize(mn.slope = mean(slope),
            mn.darcyq = mean(darcyq),
            max.velo = mn.darcyq/.25,
            min.velo = mn.darcyq/.5,
            min.time = 180000/max.velo,
            max.time = 180000/min.velo)

kbl(x = gradsummary, digits = 3,col.names = c("dh/dx","q (cm/y)","Max velo","Min velo", "min residence", "max residence"))

# REMEMBER THIS IS NOT VELOCITY. NEED TO DIVIDE BY POROSITY FOR AVG LINEAR VELOCITY.

# Porosity in sandy silt can range from 25%-50%, so avg linear velo could be ~60-30 cm/yr
