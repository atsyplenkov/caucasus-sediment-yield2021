library(tidyverse)
library(sf)

# 1) Load data ------------------------------------------------------------
df <- st_read("data/spatial/sy_caucasus_6931.shp",
               options = "ENCODING=WINDOWS-1251") %>% 
  st_drop_geometry() %>% 
  as_tibble() %>% 
  rename(A = Catchment_)

ws <- st_read("data/spatial/ws_caucasus-b.shp") %>% 
  mutate(F = st_area(.)/10^6,
         F = as.numeric(F))


ws %>% 
  left_join(df,
            by = "id") %>% 
  mutate(diff = 100 * F / A) %>% 
  arrange(diff)

st_read("data/spatial/sy_caucasus_6931.shp",
        options = "ENCODING=WINDOWS-1251") %>% 
  filter(id %in% c(2046, 2052, 2131, 2144)) %>% 
  st_write("data/spatial/temp/pour_mistakes27Jul.shp")
