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
library(gridExtra)

# date <- ymd("2019-05-06")


date <- sub(x = dir(path = "Kriging-Watertable/kriged_tiffs/", pattern = "*.tif"), pattern = "*.tif", replacement = "")

date <- seq.Date(from = ymd("2019-03-25"), to = ymd("2020-11-01"), by = "week")



krigeplottR <- function(date) {
  site <- st_read(paste0("Level-Logger-Data-Compiling/daily_wl_gpkgs/",date,".gpkg"))
  tif <- raster(paste0("Kriging-Watertable/kriged_tiffs/",date,".tif"))
  
  raster.df <- as.data.frame(as(tif,"SpatialPixelsDataFrame"))
  colnames(raster.df) <- c("wl","x","y")
  
  
  p <- ggplot()+
    geom_tile(data = raster.df,aes(x=x,y=y,fill=wl))+
    geom_sf(data = site)+
    scale_fill_viridis_b(breaks = seq(625,650,by = 2.5),limits = c(625,650), name = "Water Elevation (m)")+
    labs(title = date)+
    theme(
          # axis.text = element_blank(),
          axis.text.x = element_text(angle =45, hjust = 1),
          axis.text.y = element_text(angle =45),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          # legend.position = "none",
          legend.key.height = unit(2, "cm"),
          plot.margin = unit(c(.01,.01,.01,.01), "cm")
          )
  
  write_rds(p, paste0("Kriging-Watertable/krige_plots/rds/",date))
  ggsave(p, filename = paste0("Kriging-Watertable/krige_plots/png/",date, ".png"), device = "png", width = 6.5, units = "in")
}

date %>% 
  map(krigeplottR)


# make a gif

# list.files(path = "Kriging-Watertable/krige_plots/png/", pattern = "*.png", full.names = T) %>% 
#   map(image_read) %>% # reads each path file
#   image_join() %>% # joins image
#   image_animate(fps=10) %>% # animates, can opt for number of loops
#   image_write("Kriging-Watertable/watertable.gif")

# plots <- list.files(path = "Kriging-Watertable/krige_plots/rds/", pattern = "*", full.names = T) %>% 
#   map(read_rds)
# p
# 
# arrangeGrob(grobs = plots,nrow = 2)
# 
# grid.arrange(grobs = plots,nrow = 2,)
# 
# ggsave("Plots/KrigeGrid.png", plot = grid.arrange(grobs = plots,nrow = 2), width = 6.5,height = 5, units = "in")


