library(tidyverse)
library(corrr)
library(atslib)
library(see)
library(extrafont)
library(readxl)
library(sf)

theme_set(theme_hp())

# 0) Load data ------------------------------------------------------------
load("data/tidy/df_unc.Rdata")
load("data/tidy/ws_db.Rdata")

# 1) Uncertainty assessment --------------------------------------------------

# Chemical weathering rate
ucwr <- function(x){
  
  u <- rnorm(1000,
             mean = 1,
             sd = .05)
  
  sim <- x * u
  
  quantile(sim,
           probs = c(0.025, 0.975),
           na.rm = T) %>% 
    as.data.frame() %>% 
    rownames_to_column() %>%
    mutate(rowname = paste0("cwr_", rowname)) %>% 
    rename(val = 2) %>% 
    pivot_wider(names_from = rowname,
                values_from = val) %>% 
    mutate(cwr_sim = list(data.frame(cwr_sim = sim,
                                     rowid = 1:1000)))
}

# Peak Ground Acceleration
upga <- function(x){
  
  u <- rnorm(1000,
             mean = 1,
             sd = .20)
  
  sim <- x * u
  
  quantile(sim,
           probs = c(0.025, 0.975),
           na.rm = T) %>% 
    as.data.frame() %>% 
    rownames_to_column() %>%
    mutate(rowname = paste0("pga_", rowname)) %>% 
    rename(val = 2) %>% 
    pivot_wider(names_from = rowname,
                values_from = val) %>% 
    mutate(pga_sim = list(data.frame(pga_sim = sim,
                                     rowid = 1:1000)))
}

# Sand fraction
usand <- function(x){
  
  u <- rnorm(1000,
             mean = 1,
             sd = .09)
  
  sim <- x * u
  
  quantile(sim,
           probs = c(0.025, 0.975),
           na.rm = T) %>% 
    as.data.frame() %>% 
    rownames_to_column() %>%
    mutate(rowname = paste0("sand_", rowname)) %>% 
    rename(val = 2) %>% 
    pivot_wider(names_from = rowname,
                values_from = val) %>% 
    mutate(sand_sim = list(data.frame(sand_sim = sim,
                                     rowid = 1:1000)))
}

# Forest canopy height
ucanopy <- function(x){
  
  u <- rnorm(1000,
             mean = 1,
             sd = .12)
  
  sim <- x * u
  
  quantile(sim,
           probs = c(0.025, 0.975),
           na.rm = T) %>% 
    as.data.frame() %>% 
    rownames_to_column() %>%
    mutate(rowname = paste0("canopy_", rowname)) %>% 
    rename(val = 2) %>% 
    pivot_wider(names_from = rowname,
                values_from = val) %>% 
    mutate(canopy_sim = list(data.frame(canopy_sim = sim,
                                        rowid = 1:1000)))
}

# Height above the nearest drainage
uhand <- function(x){
  
  u <- rnorm(1000,
             mean = 1,
             sd = .05)
  
  sim <- x * u
  
  quantile(sim,
           probs = c(0.025, 0.975),
           na.rm = T) %>% 
    as.data.frame() %>% 
    rownames_to_column() %>%
    mutate(rowname = paste0("hand_", rowname)) %>% 
    rename(val = 2) %>% 
    pivot_wider(names_from = rowname,
                values_from = val) %>% 
    mutate(hand_sim = list(data.frame(hand_sim = sim,
                                      rowid = 1:1000)))
}

# Normalized steepness index
uksn <- function(x){
  
  u <- rnorm(1000,
             mean = 1,
             sd = .01)
  
  sim <- x * u
  
  quantile(sim,
           probs = c(0.025, 0.975),
           na.rm = T) %>% 
    as.data.frame() %>% 
    rownames_to_column() %>%
    mutate(rowname = paste0("ksn_", rowname)) %>% 
    rename(val = 2) %>% 
    pivot_wider(names_from = rowname,
                values_from = val) %>% 
    mutate(ksn_sim = list(data.frame(ksn_sim = sim,
                                      rowid = 1:1000)))
}

# Barren
ubarren <- function(x){
  
  u <- rnorm(1000,
             mean = 1,
             sd = .29)
  
  sim <- x * u
  
  quantile(sim,
           probs = c(0.025, 0.975),
           na.rm = T) %>% 
    as.data.frame() %>% 
    rownames_to_column() %>%
    mutate(rowname = paste0("barren_", rowname)) %>% 
    rename(val = 2) %>% 
    pivot_wider(names_from = rowname,
                values_from = val) %>% 
    mutate(barren_sim = list(data.frame(barren_sim = sim,
                                      rowid = 1:1000)))
}

# Cropland
ucrop <- function(x){
  
  u <- rnorm(1000,
             mean = 1,
             sd = .08)
  
  sim <- x * u
  
  quantile(sim,
           probs = c(0.025, 0.975),
           na.rm = T) %>% 
    as.data.frame() %>% 
    rownames_to_column() %>%
    mutate(rowname = paste0("crop_", rowname)) %>% 
    rename(val = 2) %>% 
    pivot_wider(names_from = rowname,
                values_from = val) %>% 
    mutate(crop_sim = list(data.frame(crop_sim = sim,
                                      rowid = 1:1000)))
}

# Forest
uforest <- function(x){
  
  u <- rnorm(1000,
             mean = 1,
             sd = .15)
  
  sim <- x * u
  
  quantile(sim,
           probs = c(0.025, 0.975),
           na.rm = T) %>% 
    as.data.frame() %>% 
    rownames_to_column() %>%
    mutate(rowname = paste0("forest_", rowname)) %>% 
    rename(val = 2) %>% 
    pivot_wider(names_from = rowname,
                values_from = val) %>% 
    mutate(forest_sim = list(data.frame(forest_sim = sim,
                                        rowid = 1:1000)))
}

# Glacier
uglacier <- function(x){
  
  u <- rnorm(1000,
             mean = 1,
             sd = .05)
  
  sim <- x * u
  
  quantile(sim,
           probs = c(0.025, 0.975),
           na.rm = T) %>% 
    as.data.frame() %>% 
    rownames_to_column() %>%
    mutate(rowname = paste0("glacier_", rowname)) %>% 
    rename(val = 2) %>% 
    pivot_wider(names_from = rowname,
                values_from = val) %>% 
    mutate(glacier_sim = list(data.frame(glacier_sim = sim,
                                        rowid = 1:1000)))
}

# P
up <- function(x){
  
  u <- rnorm(1000,
             mean = 1,
             sd = .091)
  
  sim <- x * u
  
  quantile(sim,
           probs = c(0.025, 0.975),
           na.rm = T) %>% 
    as.data.frame() %>% 
    rownames_to_column() %>%
    mutate(rowname = paste0("p_", rowname)) %>% 
    rename(val = 2) %>% 
    pivot_wider(names_from = rowname,
                values_from = val) %>% 
    mutate(p_sim = list(data.frame(p_sim = sim,
                                        rowid = 1:1000)))
}

# AET
uaet <- function(x){
  
  u <- rnorm(1000,
             mean = 1,
             sd = .03)
  
  sim <- x * u
  
  quantile(sim,
           probs = c(0.025, 0.975),
           na.rm = T) %>% 
    as.data.frame() %>% 
    rownames_to_column() %>%
    mutate(rowname = paste0("aet_", rowname)) %>% 
    rename(val = 2) %>% 
    pivot_wider(names_from = rowname,
                values_from = val) %>% 
    mutate(aet_sim = list(data.frame(aet_sim = sim,
                                        rowid = 1:1000)))
}

# SRAD
usrad <- function(x){
  
  u <- rnorm(1000,
             mean = 1,
             sd = .083)
  
  sim <- x * u
  
  quantile(sim,
           probs = c(0.025, 0.975),
           na.rm = T) %>% 
    as.data.frame() %>% 
    rownames_to_column() %>%
    mutate(rowname = paste0("srad_", rowname)) %>% 
    rename(val = 2) %>% 
    pivot_wider(names_from = rowname,
                values_from = val) %>% 
    mutate(srad_sim = list(data.frame(srad_sim = sim,
                                        rowid = 1:1000)))
}

# GLORED
uglored <- function(x){
  
  u <- rnorm(1000,
             mean = 1,
             sd = .1)
  
  sim <- x * u
  
  quantile(sim,
           probs = c(0.025, 0.975),
           na.rm = T) %>% 
    as.data.frame() %>% 
    rownames_to_column() %>%
    mutate(rowname = paste0("glored_", rowname)) %>% 
    rename(val = 2) %>% 
    pivot_wider(names_from = rowname,
                values_from = val) %>% 
    mutate(glored_sim = list(data.frame(glored_sim = sim,
                                        rowid = 1:1000)))
}

# POP
upop <- function(x){
  
  u <- rnorm(1000,
             mean = 1,
             sd = .15)
  
  sim <- x * u
  
  quantile(sim,
           probs = c(0.025, 0.975),
           na.rm = T) %>% 
    as.data.frame() %>% 
    rownames_to_column() %>%
    mutate(rowname = paste0("pop_", rowname)) %>% 
    rename(val = 2) %>% 
    pivot_wider(names_from = rowname,
                values_from = val) %>% 
    mutate(pop_sim = list(data.frame(pop_sim = sim,
                                     rowid = 1:1000)))
}

# Q
uq <- function(x){
  
  u <- rnorm(1000,
             mean = 1,
             sd = .36)
  
  sim <- x * u
  
  quantile(sim,
           probs = c(0.025, 0.975),
           na.rm = T) %>% 
    as.data.frame() %>% 
    rownames_to_column() %>%
    mutate(rowname = paste0("q_", rowname)) %>% 
    rename(val = 2) %>% 
    pivot_wider(names_from = rowname,
                values_from = val) %>% 
    mutate(q_sim = list(data.frame(q_sim = sim,
                                     rowid = 1:1000)))
}

# SHDI
ushdi <- function(x){
  
  u <- rnorm(1000,
             mean = 1,
             sd = .19)
  
  sim <- x * u
  
  quantile(sim,
           probs = c(0.025, 0.975),
           na.rm = T) %>% 
    as.data.frame() %>% 
    rownames_to_column() %>%
    mutate(rowname = paste0("shdi_", rowname)) %>% 
    rename(val = 2) %>% 
    pivot_wider(names_from = rowname,
                values_from = val) %>% 
    mutate(shdi_sim = list(data.frame(shdi_sim = sim,
                                   rowid = 1:1000)))
}


# 2) Simulate alternative values ------------------------------------------
set.seed(2021)
ws_sim_nest <- ws_db %>% 
  mutate(dem = DEM_stdev/DEM_mean) %>% 
  group_by(id, area, dem) %>% 
  nest() %>% 
  mutate(
    cwr = map(data,~ucwr(.x$CWR_mean)),
    pga = map(data, ~upga(.x$PGA_mean)),
    sand = map(data, ~usand(.x$SAND_mean)),
    canopy = map(data, ~ucanopy(.x$CANOPY_mean)),
    hand = map(data, ~uhand(.x$HAND_mean)),
    ksn = map(data, ~uksn(.x$ksn)),
    barren = map(data, ~ubarren(.x$area_barren)),
    crop = map(data, ~ucrop(.x$area_cropland)),
    forest = map(data, ~uforest(.x$area_forest)),
    glacier = map(data, ~uglacier(.x$glacier)),
    p = map(data, ~up(.x$P_mean)),
    q = map(data, ~uq(.x$Q_mean)),
    pop = map(data, ~upop(.x$POP_mean)),
    aet = map(data, ~uaet(.x$AET_mean)),
    srad = map(data, ~usrad(.x$SRAD_mean)),
    glored = map(data, ~uglored(.x$GLORED_mean)),
    shdi = map(data, ~ushdi(.x$SHDI))
  )

ws_sim <- ws_sim_nest %>% 
  dplyr::select(-data) %>% 
  unnest() %>% 
  ungroup()

# save("ws_sim",
#      file = "data/tidy/ws_sim.Rdata")

# 3) Merge with SY data ---------------------------------------------------
df_ws <- df_unc %>% 
  filter(MP_length >=7) %>% 
  unnest(cols = c(sim)) %>% 
  dplyr::select(id, rowid, sim) %>% 
  left_join(
    ws_sim %>% 
      select(id, area,
             contains("sim")) %>% 
      unnest(),
    by = c("id", "rowid")
  ) %>% 
  left_join(df_alt %>% 
              st_drop_geometry() %>% 
              dplyr::select(id, alt_group),
            by = "id") %>% 
  select(-matches("rowid\\d")) %>% 
  drop_na(area)

# 4) Correlate ------------------------------------------------------------
df_cor_alt <- df_ws %>% 
  select(-id) %>% 
  group_by(alt_group, rowid) %>% 
  nest() %>%
  mutate(cor = map(data,
                   ~corrr::correlate(.x,
                                     method = "spearman",
                                     use = "pairwise.complete.obs",
                                     quiet = T) %>% 
                     corrr::focus(sim))) %>% 
  unnest(cols = c(cor))

df_cor <- df_ws %>% 
  select(-id, -alt_group) %>% 
  group_by(rowid) %>% 
  nest() %>%
  mutate(cor = map(data,
                   ~corrr::correlate(.x,
                                     method = "spearman",
                                     use = "pairwise.complete.obs",
                                     quiet = T) %>% 
                     corrr::focus(sim))) %>% 
  unnest(cols = c(cor))

save("ws_sim", "df_cor_alt", "df_cor",
     file = "data/tidy/ws_sim.Rdata")

#####################################################
boot_cor %>% 
  filter(term != "id") %>% 
  select(-data) %>% 
  ungroup() %>% 
  bind_rows(
    tt_cor %>% 
      select(-data) %>% 
      ungroup()
  ) %>% 
  ggplot(aes(x = term,
             y = sim)) +
  geom_hline(yintercept = c(-0.5, 0.5),
             color = "grey80") +
  # geom_jitter(
  #   width = .3,
  #   alpha = .05,
  #   show.legend = F) +
  stat_boxplot(geom ='errorbar',
               width = .25) +
  geom_boxplot(outlier.shape = NA) +
  stat_summary(fun = median,
               colour = "#ED0000FF",
               geom = "text",
               family = "Roboto Light",
               show.legend =F, 
               vjust = -0.6,
               aes(label = round(..y..,
                                 digits=2))) +
  facet_wrap(~alt_group,
             scales = "free_y")

# 1) Partial correlation analysis -----------------------------------------
db_cor <- ws_db %>% 
  filter(!id %in% dams_ws) %>% 
  dplyr::select(-contains("snow"),
                -contains("water"),
                -contains("other")
                ) %>% 
  mutate(WR_cv = WR_stdev/WR_mean) %>% 
  mutate(HAND_cv = HAND_stdev/HAND_mean) %>% 
  mutate(GLORED_cv = GLORED_stdev/GLORED_mean) %>% 
  left_join(df_alt %>% 
              st_drop_geometry() %>% 
              dplyr::select(id, sy),
            by = "id")

part_corr <- db_cor %>% 
  dplyr::select(-id) %>% 
  correlate(method = "spearman") %>% 
  shave() %>% 
  fashion()

db_cor %>% 
  dplyr::select(-id) %>% 
  correlate(method = "spearman") %>% 
  focus(sy) %>% 
  arrange(-sy) %>% 
  print(n = 100)

cor.test(db_cor$sy,
         db_cor$GLORED_cv,
         method = "spearman")

# 2) Correlation --------------------------------------------------------------------
boot_cor <-
  df_unc %>% 
  filter(MP_length >=7) %>% 
  unnest(cols = c(sim)) %>% 
  dplyr::select(id, rowid, sim) %>% 
  left_join(ws_db %>% 
              mutate(WR_cv = WR_stdev/WR_mean),
            by = "id") %>% 
  left_join(df_alt %>% 
              st_drop_geometry() %>% 
              dplyr::select(id, alt_group),
            by = "id") %>% 
  dplyr::select(-id, area) %>% 
  group_by(alt_group, rowid) %>% 
  nest() %>%
  mutate(cor = map(data,
                   ~corrr::correlate(.x,
                                     method = "spearman", 
                                     use = "pairwise.complete.obs",
                                     quiet = T) %>% 
                     corrr::focus(sim))) %>% 
  unnest(cols = c(cor))

# 3) Landuse --------------------------------------------------------------
boot_cor_landuse <-
  boot_cor %>%
  drop_na(alt_group) %>% 
  filter(str_detect(term, "area")) %>% 
  filter(!str_detect(term, "water|snow")) %>% 
  mutate(term = str_remove(term, "area_")) %>%
  filter(term != "area") %>% 
  mutate(term = str_to_sentence(term)) %>% 
  group_by(alt_group, term) %>% 
  mutate(outlier.high = sim > quantile(sim, .75) + 1.50*IQR(sim),
         outlier.low = sim < quantile(sim, .25) - 1.50*IQR(sim),
         outlier.color = case_when(outlier.high ~ "red",
                                   outlier.low ~ "red",
                                   outlier.low == F | outlier.high == F ~ "black")) %>% 
  ggplot(aes(y = sim,
             x = term)) +
  geom_hline(yintercept = c(-0.5, 0.5),
             color = "grey80") +
  geom_jitter(
    aes(color = outlier.color),
    width = .3,
    alpha = .05,
    show.legend = F) +
  stat_boxplot(geom ='errorbar',
               width = .25) +
  geom_boxplot(outlier.shape = NA) +
  stat_summary(fun = median,
               colour = "#ED0000FF",
               geom = "text",
               show.legend =F, 
               vjust = -0.6,
               aes(label = round(..y.., digits=2))) +
  scale_y_continuous(breaks = seq(-1, 1, .2),
                     minor_breaks = NULL) +
  labs(x = NULL,
       y = "Spearman r") +
  ggsci::scale_color_lancet() +
  facet_wrap(~alt_group,
             nrow = 2, ncol = 2) 

# PGA and other ---------------------------------------------------------------------
boot_cor_pga <-
  boot_cor %>%
  drop_na(alt_group) %>% 
  filter(!str_detect(term, "area")) %>% 
  filter(str_detect(term, "mean|cv")) %>% 
  mutate(term = str_replace(term, "WR", "CWR")) %>% 
  group_by(alt_group, term) %>% 
  mutate(outlier.high = sim > quantile(sim, .75) + 1.50*IQR(sim),
         outlier.low = sim < quantile(sim, .25) - 1.50*IQR(sim),
         outlier.color = case_when(outlier.high ~ "red",
                                   outlier.low ~ "red",
                                   outlier.low == F | outlier.high == F ~ "black")) %>% 
  ggplot(aes(y = sim,
             x = term)) +
  geom_hline(yintercept = c(-0.5, 0.5),
             color = "grey80") +
  geom_jitter(
    aes(color = outlier.color),
    width = .3,
    alpha = .05,
    show.legend = F) +
  stat_boxplot(geom ='errorbar',
               width = .25) +
  geom_boxplot(outlier.shape = NA) +
  stat_summary(fun = median,
               colour = "#ED0000FF",
               geom = "text",
               show.legend =F, 
               vjust = -0.6,
               aes(label = round(..y.., digits=2))) +
  scale_y_continuous(breaks = seq(-1, 1, .2),
                     minor_breaks = NULL) +
  labs(x = NULL,
       y = "Spearman r") +
  ggsci::scale_color_lancet() +
  facet_wrap(~alt_group,
             nrow = 2, ncol = 2) 

# Scatter plots -----------------------------------------------------------
df_plot <- df_unc %>% 
  filter(MP_length >=7) %>% 
  dplyr::select(-sim) %>% 
  left_join(df_alt %>% 
              st_drop_geometry() %>% 
              dplyr::select(id, alt_group),
            by = "id") %>% 
  left_join(ws_db,
            by = "id")

hand_plot <-
  df_plot %>% 
  ggplot(aes(y = sy,
             x = HAND_mean,
             color = alt_group)) +
  geom_errorbar(aes(ymin = `2.5%`,
                    ymax = `97.5%`),
                color = "grey70") +
  geom_point(aes(fill = alt_group),
             size = 1.7,
             color = "grey10",
             shape = 21) +
  geom_smooth(method = "lm",
              se = F,
              show.legend = F,
              linetype = "dashed") +
  ggpmisc::stat_poly_eq(formula = "y~x",
                        parse = T) +
  scale_fill_metro() +
  scale_color_metro() +
  scale_y_log10() +
  scale_x_log10() +
  ggplot2::annotation_logticks(sides = "bl",
                               colour = "grey50",
                               outside = T,
                               short = unit(0.07, "cm"),
                               mid = unit(0.1, "cm"),
                               long = unit(0.15, "cm")) +
  coord_cartesian(clip = "off") +
  labs(subtitle = "e",
       x = "Height Above Nearest Drainage, m",
       y = expression("Sediment yield, t"%.%"km"^-2%.%"yr"^-1),
       color = "Altitude group: ",
       fill = "Altitude group: ")

cwr_plot <-
  df_plot %>% 
  ggplot(aes(y = sy,
             x = WR_mean,
             color = alt_group)) +
  geom_errorbar(aes(ymin = `2.5%`,
                    ymax = `97.5%`),
                color = "grey70") +
  geom_point(aes(fill = alt_group),
             size = 1.7,
             color = "grey10",
             shape = 21) +
  geom_smooth(method = "lm",
              se = F,
              show.legend = F,
              linetype = "dashed") +
  ggpmisc::stat_poly_eq(formula = "y~x",
                        parse = T,
                        label.x = 0.9) +
  scale_fill_metro() +
  scale_color_metro() +
  scale_y_log10() +
  scale_x_log10() +
  ggplot2::annotation_logticks(sides = "bl",
                               colour = "grey50",
                               outside = T,
                               short = unit(0.07, "cm"),
                               mid = unit(0.1, "cm"),
                               long = unit(0.15, "cm")) +
  coord_cartesian(clip = "off") +
  labs(subtitle = "d",
       x = expression("Chemical weathering rate, t"%.%"km"^-2%.%"yr"^-1),
       y = expression("Sediment yield, t"%.%"km"^-2%.%"yr"^-1),
       color = "Altitude group: ",
       fill = "Altitude group: ")

pga_plot <-
  df_plot %>% 
  ggplot(aes(y = sy,
             x = PGA_mean,
             color = alt_group)) +
  geom_errorbar(aes(ymin = `2.5%`,
                    ymax = `97.5%`),
                color = "grey70") +
  geom_point(aes(fill = alt_group),
             size = 1.7,
             color = "grey10",
             shape = 21) +
  geom_smooth(method = "lm",
              se = F,
              show.legend = F,
              linetype = "dashed") +
  ggpmisc::stat_poly_eq(formula = "y~x",
                        parse = T) +
  scale_fill_metro() +
  scale_color_metro() +
  scale_y_log10() +
  # scale_x_log10() +
  ggplot2::annotation_logticks(sides = "l",
                               colour = "grey50",
                               outside = T,
                               short = unit(0.07, "cm"),
                               mid = unit(0.1, "cm"),
                               long = unit(0.15, "cm")) +
  coord_cartesian(clip = "off") +
  labs(subtitle = "c",
       x = expression("Peak Ground Acceleration, m"%.%"s"^-2),
       y = expression("Sediment yield, t"%.%"km"^-2%.%"yr"^-1),
       color = "Altitude group: ",
       fill = "Altitude group: ")

crop_plot <-
  df_plot %>% 
  filter(area_cropland > 1) %>% 
  ggplot(aes(y = sy,
             x = area_cropland,
             color = alt_group)) +
  geom_errorbar(aes(ymin = `2.5%`,
                    ymax = `97.5%`),
                color = "grey70") +
  geom_point(aes(fill = alt_group),
             size = 1.7,
             color = "grey10",
             shape = 21) +
  geom_smooth(method = "lm",
              se = F,
              show.legend = F,
              linetype = "dashed") +
  ggpmisc::stat_poly_eq(formula = "y~x",
                        parse = T) +
  scale_fill_metro() +
  scale_color_metro() +
  scale_y_log10() +
  # scale_x_log10() +
  ggplot2::annotation_logticks(sides = "l",
                               colour = "grey50",
                               outside = T,
                               short = unit(0.07, "cm"),
                               mid = unit(0.1, "cm"),
                               long = unit(0.15, "cm")) +
  coord_cartesian(clip = "off",
                  xlim = c(0, 100)) + # Added!
  labs(subtitle = "b",
       x = "Cropland area in 2015, %",
       y = expression("Sediment yield, t"%.%"km"^-2%.%"yr"^-1),
       color = "Altitude group: ",
       fill = "Altitude group: ")

barren_plot <-
  df_plot %>% 
  filter(area_barren > 1) %>% 
  ggplot(aes(y = sy,
             x = area_barren ,
             color = alt_group)) +
  geom_errorbar(aes(ymin = `2.5%`,
                    ymax = `97.5%`),
                color = "grey70") +
  geom_point(aes(fill = alt_group),
             size = 1.7,
             color = "grey10",
             shape = 21) +
  geom_smooth(method = "lm",
              se = F,
              show.legend = F,
              linetype = "dashed") +
  ggpmisc::stat_poly_eq(formula = "y~x",
                        parse = T) +
  scale_fill_metro() +
  scale_color_metro() +
  scale_y_log10() +
  scale_x_log10() +
  ggplot2::annotation_logticks(sides = "l",
                               colour = "grey50",
                               outside = T,
                               short = unit(0.07, "cm"),
                               mid = unit(0.1, "cm"),
                               long = unit(0.15, "cm")) +
  coord_cartesian(clip = "off") +
  labs(subtitle = "a",
       x = "Barren area in 2015, %",
       y = expression("Sediment yield, t"%.%"km"^-2%.%"yr"^-1),
       color = "Altitude group: ",
       fill = "Altitude group: ")

# SAVE --------------------------------------------------------------------
writexl::write_xlsx(part_corr,
                    "analysis/table1.xlsx")


scatter_plots <- ggpubr::ggarrange(barren_plot, crop_plot,
                                   pga_plot, cwr_plot,
                                   hand_plot,
                                   nrow = 2,
                                   ncol = 3,
                                   common.legend = T,
                                   legend = "bottom")

ggsave("figures/fig6_scatter_unc_plots.png",
       scatter_plots,
       dpi = 500,
       w = 11, h = 9)

ggsave("figures/fig4_boot_cor_plot1.png",
       boot_cor_landuse,
       dpi = 500,
       w = 8, h = 6)

ggsave("figures/fig5_boot_cor_plot2.png",
       boot_cor_pga,
       dpi = 500,
       w = 10, h = 6)
