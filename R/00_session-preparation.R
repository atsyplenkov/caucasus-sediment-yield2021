library(XML)
library(rgdal)
library(gdalUtils)
library(raster)
library(sf)
library(stars)
library(tidyverse)

# 1) Load Caucasus boundary -------------------------------------------------
caucasus <- st_read("data/spatial/caucasus_aoi_6931.shp")

# Transform to IGH projection
igh <- '+proj=igh +lat_0=0 +lon_0=0 +datum=WGS84 +units=m +no_defs'

caucasus_igh <- st_transform(caucasus,
                             igh)

# Boundary box
(bbox <- st_bbox(caucasus_igh))

## ul means upper left
## lr means lower right
ulx = bbox$xmin
uly = bbox$ymax
lrx = bbox$xmax
lry = bbox$ymin
(bb <- c(ulx, uly, lrx, lry))

# 2) Download layers ------------------------------------------------------
sg_url <- "/vsicurl/https://files.isric.org/soilgrids/latest/data/"
# props <- c("sand", "silt", "clay", "soc")
props <- c("bdod")
layers <- c("0-5", "5-15", "15-30")

(vrt <- paste0(props, "/", props, "_", rep(layers, 1),
               "cm_mean.vrt"))

(lfile <- paste0("data/soilgrid/", props, "_", rep(layers, 1), ".tif"))

map2(vrt, lfile,
     ~( gdal_translate(paste0(sg_url, .x), .y,
                       tr = c(250,250),
                       projwin = bb,
                       projwin_srs = igh,
                       verbose = TRUE)))

# 3) Sand fraction calc ---------------------------------------------------
(sand_files <- list.files("data/soilgrid/",
                          pattern = "sand*",
                          full.names = T))

sand_stars <- sand_files %>% 
  map(read_stars) %>% 
  reduce(c) %>% 
  merge() %>% 
  set_names("sand") %>% 
  st_set_dimensions(names = c("x", "y", "layer"))

sand <- st_apply(sand_stars/10,
                 c("x", "y"),
                 mean) %>% 
  # st_transform(4326) %>% 
  set_names("sand")

sand_raster <- sand %>% 
  as.data.frame() %>% 
  rasterFromXYZ(crs = st_crs(sand)$proj4string) %>% 
  raster::projectRaster(crs = 4326) 


# 4) Bulk density of the fine earth fraction ------------------------------
(bulk_files <- list.files("data/soilgrid/",
                          pattern = "bdod*",
                          full.names = T))

bulk_stars <- bulk_files %>% 
  map(read_stars) %>% 
  reduce(c) %>% 
  merge() %>% 
  set_names("bulk") %>% 
  st_set_dimensions(names = c("x", "y", "layer"))

bulk <- st_apply(bulk_stars/100,
                 c("x", "y"),
                 mean) %>% 
  # st_transform(4326) %>% 
  set_names("bulk")

bulk_raster <- bulk %>% 
  as.data.frame() %>% 
  rasterFromXYZ(crs = st_crs(bulk)$proj4string) %>% 
  raster::projectRaster(crs = 4326) 

# TerraClimate ---------------------------------------------------------------
# Evapotranspiration
download.file(url = 'http://thredds.northwestknowledge.net:8080/thredds/fileServer/TERRACLIMATE_ALL/data/TerraClimate_pet_2019.nc',
              destfile = 'pet.nc')

terraclim_base_url <- "http://thredds.northwestknowledge.net:8080/thredds/fileServer/TERRACLIMATE_ALL/data/TerraClimate_"

pet <- paste0(terraclim_base_url,
              'pet_',
              seq(1958,
                  2018,
                  1),
              ".nc") 

pet_dest <- paste0("data/terraclim/TerraClimate_pet_",
                   seq(1958,
                       2018,
                       1),
                   ".nc") 

map2(pet, pet_dest,
     ~download.file(.x, .y))


# SAVE --------------------------------------------------------------------
writeRaster(sand_raster,
            "data/soilgrid/caucasus_sand-average.tif")

writeRaster(bulk_raster,
            "data/soilgrid/caucasus_bulk-average.tif")

