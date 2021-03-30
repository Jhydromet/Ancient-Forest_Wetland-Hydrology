library(tidyverse)
library(lubridate)
library(broom)
library(purrr)
library(sf)
library(rgdal) # Required for raster I think)
library(tmap)  # plot spatially
library(raster)# make raster
library(gstat) # Use gstat's idw routine
library(sp)    # Used for the spsample function
library(mapview)
library(rasterVis)


date <- seq.Date(ymd("2020-05-01"),ymd("2020-11-01"), by = "month")


krigeplottR <- function(date, x, y, wl) {
  site <- st_read(paste0("Kriging-Watertable/krige_sites/",date,".gpkg"))
  tif <- raster(paste0("Kriging-Watertable/kriged_tiffs/",date,".tif"))
  
  raster.df <- as.data.frame(as(tif,"SpatialPixelsDataFrame"))
  colnames(raster.df) <- c("wl","x","y")
  
  
  p <- ggplot()+
    geom_tile(data = raster.df,aes(x=x,y=y,fill=wl))+
    geom_sf(data = site)+
    scale_fill_viridis_c()+
    labs(title = date)+
    theme(axis.text.x = element_text(angle =45, hjust = 1),
          axis.text.y = element_text(angle =45),
          axis.title = element_blank(),
          legend.title = element_blank(),
          legend.key.height = unit(40,"pt"))
  
  write_rds(p, paste0("Kriging-Watertable/krige_plots/",date))
}

date %>% 
  map(krigeplottR)





