library(tidyverse)
library(sf)
library(raster)
library(exactextractr)
library(mapview)
library(stars)
library(units)

# 1) Load data ------------------------------------------------------------
ws <- st_read("data/spatial/ws_caucasus-b.shp") %>% 
  mutate(area = st_area(.)/10^6,
         area = as.numeric(area))

df <- st_read("data/spatial/sy_caucasus_6931.shp",
              options = "ENCODING=WINDOWS-1251") %>% 
  rename(River = River_name,
         Name = Station_na,
         A = Catchment_,
         sy = SSY_averag,
         MP = Measuring_,
         runoff = Runoff_ave)

ext2 <- ws %>% 
  st_transform(4326) %>% 
  st_bbox() %>% 
  extent()

# Dams --------------------------------------------------------------------
dams <- st_read("E:/WORK/00_GLOBAL/GRAND/GRanD_dams_v1_1.shp") %>% 
  st_crop(st_bbox(st_transform(ws, 4326)))

dams_intersects <- st_intersects(
  dams,
  st_transform(ws, 4326)) %>%
  unlist() %>%
  unique()

dams_ws <- ws %>% 
  filter(row_number() %in% dams_intersects) %>% 
  pull(id)

# Koppen-Geiger class -----------------------------------------------------
kg_ras <- raster("E:/WORK/00_GLOBAL/METEO/Map_KG-Global/KG_1986-2010.grd")

kg <- tibble(
  KG1 = seq(1,32,1),
  KG = c('Af', 'Am', 'As', 'Aw', 'BSh', 'BSk',
         'BWh', 'BWk', 'Cfa', 'Cfb','Cfc', 'Csa',
         'Csb', 'Csc', 'Cwa','Cwb', 'Cwc', 'Dfa',
         'Dfb', 'Dfc','Dfd', 'Dsa', 'Dsb', 'Dsc',
         'Dsd','Dwa', 'Dwb', 'Dwc', 'Dwd', 'EF','ET',
         'Ocean')
)

major_kg <- exact_extract(kg_ras,
                          ws %>% 
                            st_transform(projection(kg_ras)),
                          "majority")

# Landscape province ------------------------------------------------------
lz_ras <- raster("data/spatial/isachenko_caucasus_id.tif")

lz <- st_read("data/spatial/isachenko_caucasus.shp") %>% 
  st_drop_geometry() %>% 
  dplyr::select(LZ1 = id,
                LZ = type_en)

major_lz <- exact_extract(lz_ras,
                          ws %>% 
                            st_transform(projection(lz_ras)),
                          "majority")

# TEMP --------------------------------------------------------------------
kglz <- bind_cols(major_kg, major_lz) %>% 
  rename(KG1 = 1, LZ1 = 2) %>% 
  left_join(kg, by = "KG1") %>% 
  left_join(lz, by = "LZ1") %>% 
  dplyr::select(-contains("1"))

# Elevation ---------------------------------------------------------------
dem <- raster("E:/WORK/00_GLOBAL/ASTER/Caucasus/AW3D30_Caucasus_aoi_fill.tif")

mean_alt <- exact_extract(dem,
                          ws %>% 
                            st_transform(projection(dem)),
                          c("min",
                            "mean",
                            "max",
                            "stdev")) %>% 
  rename_with(~paste0("DEM_", .x))

# 2) PGA ------------------------------------------------------------------
pga <- raster("E:/WORK/00_GLOBAL/PGA/gshap_arcgis/gshap_globe/hdr.adf")

mean_pga <- 
  exact_extract(pga,
                ws %>% 
                  st_transform(projection(pga)),
                c("mean",
                  # "median",
                  # "max",
                  "stdev"
                  )) %>% 
  rename_with(~paste0("PGA_", .x))

# 3) Landuse -------------------------------------------------------------
landuse <- raster("E:/WORK/00_GLOBAL/Caucasus landuse/Caucasus_classifcation_2015_topographically_corrected.tif")

freqs <- exact_extract(landuse,
                       ws %>% 
                         st_transform(projection(landuse)),
                       function(value, coverage_fraction) {
                         data.frame(value = value,
                                    frac = coverage_fraction / sum(coverage_fraction, na.rm = T)) %>%
                           group_by(value) %>%
                           summarize(freq = sum(frac, na.rm = T))
                       })

landuse_type2015 <- freqs %>% 
  mutate(cu = cumsum(freq),
         cu = round(cu, 5)) %>% 
  mutate(br = (cu - 1),
         brf = (br %% 1)==0) %>% 
  mutate(river = ifelse(brf == T,
                        as.integer(cu) + 1,
                        NA_integer_)) %>% 
  mutate(river = lag(river)) %>% 
  mutate(river = ifelse(row_number() == 1, 1, river)) %>% 
  mutate(river = zoo::na.locf(river)) %>%
  left_join(st_drop_geometry(ws) %>% 
              rowid_to_column(var = "river"),
            by = "river") %>% 
  dplyr::select(id, value, freq, area) %>% 
  mutate(type = case_when(
    value == 1 ~ "coniferous forest",
    value == 2 ~ "mixed forest",
    value == 3 ~ "deciduous forest",
    value == 4 ~ "barren",
    value == 5 ~ "rangeland",
    value == 6 ~ "cropland",
    value == 7 ~ "built-up",
    value == 8 ~ "wetlands",
    value == 9 ~ "water",
    value == 10 ~ "snow and ice",
    TRUE ~ "other"
  )) %>% 
  mutate(area_type = area * freq)

ws_landuse <- landuse_type2015 %>% 
  dplyr::select(id, freq, type) %>% 
  mutate(type2 = case_when(
    str_detect(type, "forest") ~ "forest",
    str_detect(type, "crop") ~ "cropland",
    TRUE ~ type
  )) %>% 
  group_by(id, type2) %>% 
  summarise(freq = 100 * sum(freq,
                             na.rm = T)) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = id,
              names_from = type2,
              names_prefix = "area_",
              values_from = freq)

ws_shdi <- landuse_type2015 %>% 
  drop_na(value) %>%
  group_by(id) %>% 
  summarise(SHDI = -sum(freq * log(freq)),
            .groups = "drop") 

# 4) Lithology ------------------------------------------------------------
glim <- raster("data/spatial/lithology/glim_weathering-rate.tif")

mean_weathering <- 
  exact_extract(glim,
                ws %>% 
                  st_transform(projection(glim)),
                c("mean",
                  # "min",
                  # "max",
                  "stdev")) %>% 
  rename_with(~paste0("CWR_", .x))

# 5) Sand content ---------------------------------------------------------
sand_raster <- raster("data/soilgrid/caucasus_sand-average.tif")

mean_sand <- 
  exact_extract(sand_raster,
                ws %>% 
                  st_transform(projection(sand_raster)),
                c("mean",
                  # "median",
                  # "max",
                  "stdev")) %>% 
  rename_with(~paste0("SAND_", .x))

# 6) Bulk density content ---------------------------------------------------------
# bulk_raster <- raster("data/soilgrid/caucasus_bulk-average.tif")
# 
# mean_bulk <- 
#   exact_extract(bulk_raster,
#                 ws %>% 
#                   st_transform(projection(sand_raster)),
#                 c("mean",
#                   # "median",
#                   # "max",
#                   "stdev")) %>% 
#   rename_with(~paste0("BULK_", .x))

# 7) HAND -----------------------------------------------------------------
hand <- raster("data/spatial/hand/hand_merit_clip.tif")

mean_hand <- 
  exact_extract(hand,
                ws %>% 
                  st_transform(projection(hand)),
                c("mean",
                  # "median",
                  # "max",
                  "stdev")) %>% 
  rename_with(~paste0("HAND_", .x))

# 8) Normalized steepness ksn ---------------------------------------------
ksn <- st_read("data/spatial/temp/ksn_all.shp") %>% 
  st_set_crs(6931) %>% 
  filter(ksn > 0)

ws_split <- ws %>%
  group_split(id, .keep = T)

names(ws_split) <- 
  ws_split %>% 
  purrr::map(~pull(., id)) %>% 
  purrr::map(~as.character(.))

mean_ksn <- ws_split %>% 
  map(~st_intersection(ksn, .)) %>% 
  map(~mean(.$ksn, na.rm = T)) %>% 
  bind_rows() %>% 
  gather(id, ksn)

# 9) Glaciers -------------------------------------------------------------
glaciers <- st_read("data/spatial/glims/glims_polygons.shp") %>% 
  st_transform(6931) %>% 
  filter(year == 1960)

glacier1960 <- ws_split %>%
  map(~st_intersection(glaciers, .)) %>% 
  map(~sum(st_area(.))) %>% 
  map(~set_units(., "km2")) %>% 
  map(~as.numeric(.)) %>% 
  bind_rows() %>% 
  gather(id, glacier)

# 10) Rainfall erosivity ---------------------------------------------------
glored <- raster("E:/WORK/00_GLOBAL/GlobalR/GlobalR_NoPol.tif")

mean_glored <- 
  exact_extract(glored,
                ws %>% 
                  st_transform(projection(glored)),
                c("mean",
                  # "median",
                  # "max",
                  "stdev")) %>% 
  rename_with(~paste0("GLORED_", .x))

# 11) Mean Canopy Height --------------------------------------------------------
canopy <- raster("/Users/atsyp/Downloads/Forest_height_2019_NAFR.tif")

mean_canopy <- 
  exact_extract(canopy,
                ws %>% 
                  st_transform(projection(canopy)),
                c("mean",
                  # "median",
                  # "max",
                  "stdev")) %>% 
  rename_with(~paste0("CANOPY_", .x))

# 12) Mean precipitation --------------------------------------------------
years <- list.files(path = '/Users/atsyp/YandexDisk/GIT/ssy-rtop/data/terraclim/',
                    pattern = '.ppt.') %>% 
  str_extract("\\d+") %>% 
  as.integer()

precipitaition_rasters <- list.files(path = '/Users/atsyp/YandexDisk/GIT/ssy-rtop/data/terraclim/',
                                     pattern = '.ppt.',
                                     full.names = T) %>%
  map(~stack(.x)) %>%
  map(~crop(.x, ext2))

precipitation_sum <- map(.x = precipitaition_rasters,
                         ~calc(.x, 
                               fun = sum, 
                               na.rm = TRUE))

p_sum_over <- map(.x = precipitation_sum,
                  ~exact_extract(.x,
                                 ws %>% 
                                   st_transform(projection(.x)),
                                 "mean"))

p_sum_db <- bind_cols(p_sum_over) %>% 
  magrittr::set_colnames(years) %>% 
  mutate(id = ws$id) %>% 
  gather(year, p, -id) %>% 
  mutate(year = as.integer(year))

p_mean <- p_sum_db %>% 
  group_by(id) %>% 
  summarise(P_mean = mean(p, na.rm = T))

# 13) Mean temperature ----------------------------------------------------
aet_rasters <- list.files(path = '/Users/atsyp/YandexDisk/GIT/ssy-rtop/data/terraclim/',
                                     pattern = '.aet.',
                                     full.names = T) %>%
  map(~stack(.x)) %>%
  map(~crop(.x, ext2))

aet_sum <- map(.x = aet_rasters,
                         ~calc(.x, 
                               fun = mean, 
                               na.rm = TRUE))

aet_over <- map(.x = aet_sum,
                ~exact_extract(.x,
                               ws %>% 
                                 st_transform(projection(.x)),
                               "mean"))

aet_sum_db <- bind_cols(aet_over) %>% 
  magrittr::set_colnames(years) %>% 
  mutate(id = ws$id) %>% 
  gather(year, p, -id) %>% 
  mutate(year = as.integer(year))

aet_mean <- aet_sum_db %>% 
  group_by(id) %>% 
  summarise(AET_mean = mean(p, na.rm = T))

# 14) Solar radiation -----------------------------------------------------
srad_rasters <- list.files(path = '/Users/atsyp/YandexDisk/GIT/ssy-rtop/data/terraclim/',
                          pattern = '.srad.',
                          full.names = T) %>%
  map(~stack(.x)) %>%
  map(~crop(.x, ext2))

srad_sum <- map(.x = srad_rasters,
               ~calc(.x, 
                     fun = mean, 
                     na.rm = TRUE))

srad_over <- map(.x = srad_sum,
                ~exact_extract(.x,
                               ws %>% 
                                 st_transform(projection(.x)),
                               "mean"))

srad_sum_db <- bind_cols(srad_over) %>% 
  magrittr::set_colnames(years) %>% 
  mutate(id = ws$id) %>% 
  gather(year, p, -id) %>% 
  mutate(year = as.integer(year))

srad_mean <- srad_sum_db %>% 
  group_by(id) %>% 
  summarise(SRAD_mean = mean(p, na.rm = T))

# 15) Water runoff -----------------------------------------------------
q_rasters <- list.files(path = '/Users/atsyp/YandexDisk/GIT/ssy-rtop/data/terraclim/',
                          pattern = '.q.',
                          full.names = T) %>%
  map(~stack(.x)) %>%
  map(~crop(.x, ext2))

q_sum <- map(.x = q_rasters,
               ~calc(.x, 
                     fun = mean, 
                     na.rm = TRUE))

q_over <- map(.x = q_sum,
                ~exact_extract(.x,
                               ws %>% 
                                 st_transform(projection(.x)),
                               "mean"))

q_sum_db <- bind_cols(q_over) %>% 
  magrittr::set_colnames(years) %>% 
  mutate(id = ws$id) %>% 
  gather(year, q, -id) %>% 
  mutate(year = as.integer(year))

q_mean <- q_sum_db %>% 
  group_by(id) %>% 
  summarise(Q_mean = mean(q, na.rm = T))

# 16) Population density --------------------------------------------------
pop_raster <- stack("E:/WORK/00_GLOBAL/Population density/gpw_v4_population_density_rev11_2pt5_min.nc") %>% 
  subset(1:5) %>% 
  crop(ext2) %>% 
  calc(fun = mean, 
       na.rm = TRUE)

pop_raster[pop_raster > 500] <- NA

mean_pop <- 
  exact_extract(pop_raster,
                ws %>% 
                  st_transform(projection(pop_raster)),
                c("mean",
                  # "median",
                  # "max",
                  "stdev")) %>% 
  rename_with(~paste0("POP_", .x))

# XX) Join together --------------------------------------------------------
load("data/tidy/ksn.Rdata")

ws_db <- ws %>% 
  st_drop_geometry() %>% 
  as_tibble() %>% 
  bind_cols(kglz,
            mean_weathering,
            mean_alt,
            mean_pga,
            mean_sand,
            mean_pop,
            mean_glored,
            mean_canopy,
            mean_hand) %>% 
  left_join(ws_landuse,
            by = "id") %>% 
  left_join(mean_ksn %>% 
              mutate(id = as.numeric(id)),
            by = "id") %>% 
  left_join(glacier1960 %>% 
              mutate(id = as.numeric(id)),
            by = "id") %>%  
  left_join(p_mean, 
            by = "id") %>% 
  left_join(aet_mean, 
            by = "id") %>%  
  left_join(srad_mean, 
            by = "id") %>% 
  left_join(q_mean, 
            by = "id") %>%
  left_join(ws_shdi, 
            by = "id") %>% 
  # remove dammed watersheds
  filter(!id %in% dams_ws)

# SAVE --------------------------------------------------------------------
save("ws_db",
     file = "data/tidy/ws_db.Rdata")

save("mean_ksn",
     file = "data/tidy/ksn.Rdata")
