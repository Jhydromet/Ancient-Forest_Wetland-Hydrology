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
library(magick)

# date <- ymd("2019-05-01")


date <- seq.Date(ymd("2019-05-01"),ymd("2020-11-01"), by = "month")


krigeplottR <- function(date) {
  site <- st_read(paste0("Level-Logger-Data-Compiling/daily_wl_gpkgs/",date,".gpkg"))
  tif <- raster(paste0("Kriging-Watertable/kriged_tiffs/",date,".tif"))
  
  raster.df <- as.data.frame(as(tif,"SpatialPixelsDataFrame"))
  colnames(raster.df) <- c("wl","x","y")
  
  
  p <- ggplot()+
    geom_tile(data = raster.df,aes(x=x,y=y,fill=wl))+
    geom_sf(data = site)+
    scale_fill_viridis_c(limits = c(625,650))+
    labs(title = date)+
    theme(axis.text.x = element_text(angle =45, hjust = 1),
          axis.text.y = element_text(angle =45),
          axis.title = element_blank(),
          legend.title = element_blank(),
          legend.key.height = unit(40,"pt"))
  
  write_rds(p, paste0("Kriging-Watertable/krige_plots/rds/",date))
  ggsave(p, filename = paste0("Kriging-Watertable/krige_plots/png/",date, ".png"), device = "png")
}

date %>% 
  map(krigeplottR)


# make a gif

list.files(path = "Kriging-Watertable/krige_plots/png/", pattern = "*.png", full.names = T) %>% 
  map(image_read) %>% # reads each path file
  image_join() %>% # joins image
  image_animate(fps=2) %>% # animates, can opt for number of loops
  image_write("Kriging-Watertable/watertable.gif")


