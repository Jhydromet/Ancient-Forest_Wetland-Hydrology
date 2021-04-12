library(tidyverse)
library(lubridate)
library(purrr)
library(sf)
library(rgdal) # Required for raster I think)
library(tmap)  # plot spatially
library(raster)# make raster
library(gstat) # Use gstat's idw routine
library(sp)    # Used for the spsample function



dataday = seq(ymd("2019-05-01"), ymd("2020-11-01"), by = "week")
# Test day dataday = as.Date("2019-05-01")

crs_utm10 <- CRS(SRS_string = "EPSG:32610")

KrigR <- function(dataday) {
  
  WL <- as_Spatial(st_read(paste0("Level-Logger-Data-Compiling/daily_wl_gpkgs/", dataday, ".gpkg")))
  proj4string(WL) <- crs_utm10
  B <-  as_Spatial(st_read("Kriging-Watertable/AOI.gpkg"))
  proj4string(B) <- crs_utm10
  WL@bbox <- B@bbox
  
  
  f.1 <- as.formula(head ~ elevation_m)
  
  var.smpl <- variogram(object = head ~ elevation_m, data = WL,locations = WL, cutoff = 2000, width = 100)
  dat.fit <-  fit.variogram(var.smpl, vgm(model = "Sph"))
  
  dem <- raster("Kriging-Watertable/AF_DEM-lowres.tif")
  proj4string(dem) <- crs_utm10
  demdf <- as.data.frame(dem,xy=T) %>% 
    rename(elevation_m = "AF_DEM.lowres")
  demdf$elevation_m[demdf$elevation_m == 0] <- NA
  coordinates(demdf) = ~x+y
  proj4string(demdf) = crs_utm10
  gridded(demdf) = T
  demdf <- as(demdf,"SpatialPixelsDataFrame")
  
  dat.krg <- krige(formula = f.1, locations = WL, newdata = demdf, model = dat.fit)
  r <- raster(dat.krg)
  r.m <- mask(r, B)
  
  writeRaster(r.m, filename= paste0("Kriging-Watertable/kriged_tiffs/", dataday, ".tif"), format="GTiff", overwrite=TRUE)
}


dataday %>% 
  map(.f = KrigR)


# basemap <- raster::raster("E:/Jeremy's MSc Research/Hydrometric and GIS/GIS Data/Raster Data/Ancient Forest Wetland LIDAR/AF_DEM.tif")
# 
plt <-   tm_shape(r.m) +
  tm_raster(n=10, palette="Blues", auto.palette.mapping=FALSE,
            title="Water Table Elevation \n(m.a.s.l)", midpoint = NA) +
  tm_shape(WL) + tm_markers(size=0.2) +
  tm_legend(legend.outside=TRUE)

plt


# Variance plotting, could be added later to loop? maybe doesnt matter -------------------------



# Plot variance data. This is generated when Krige() is used
# 
# r   <- raster(dat.krg, layer="var1.var")
# r.m <- mask(r, B)
# 
# tm_shape(r.m) + 
#   tm_raster(n=7, palette ="Reds",
#             title="Variance map \n(m)") +tm_shape(WL) + tm_dots(size=0.2) +
#   tm_legend(legend.outside=TRUE)
# 
# # Plot the 95% confidence interval. Interpret this as the m above or below estimated).
# 
# r   <- sqrt(raster(dat.krg, layer="var1.var")) * 1.96
# r.m <- mask(r, B)
# 
# tm_shape(r.m) + 
#   tm_raster(n=7, palette ="Reds",
#             title="95% CI map \n(m)") +tm_shape(WL) + tm_dots(size=0.2) +
#   tm_legend(legend.outside=TRUE)
