library(tidyverse)
library(raster)
library(rgdal)
library(sf)
library(lubridate)
library(kableExtra)


# This script extracts the water table elevation data along a transect of points from interpolated water table rasters, calculates their slopes and saves into a datatable over time.


pts <- read_sf("Hydraulic-Gradient/gradient_pts.gpkg")
dataday = seq(from = as.Date("2019-04-08"), to = as.Date("2020-11-01"), by = "week")
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



slopes.prep <- slopes %>% 
  mutate(year = year(date),
         yday = yday(date),
         month = month(date))

p <- slopes.prep %>% 
  filter(month(date)>= 03 & month(date) <=10) %>% 
  ggplot()+
  geom_line(aes(x = yday, y = slope, colour = as.factor(year)))+
  geom_hline(yintercept = 0.0087, linetype = "dashed")+
  scale_x_continuous(breaks = c(60,91,121,152,182,213,244,274,305),
                     labels = c("Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov"))+
  scale_colour_discrete(name = "Year")+
  labs(x = "Month", y = "dh/dx") +
  theme(legend.position = c(.9,.2))

ggplotly(p)

ggsave(p, filename = "Gradients.png", "png", width = 6.5, units = "in")

p <- slopes %>% 
  ggplot()+
  geom_line(aes(x = date, y = darcyq))+
  labs(title = "Groundwater Flow", x = "Date", y = "Darcy Flux (cm/y)")

ggplotly(p)
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
