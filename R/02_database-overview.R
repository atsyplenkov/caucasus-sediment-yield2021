library(tidyverse)
library(sf)
library(raster)
library(exactextractr)
library(mapview)
library(atslib)
library(extrafont)
library(imputeTS)
library(ggpubr)
library(rstatix)
library(signs)

theme_set(theme_hp())

load("data/tidy/ws_db.Rdata")

# 1) Read database --------------------------------------------------------
df <- st_read("data/spatial/sy_caucasus_6931.shp",
              options = "ENCODING=WINDOWS-1251") %>% 
  rename(River = River_name,
         Name = Station_na,
         A = Catchment_,
         sy = SSY_averag,
         MP = Measuring_,
         runoff = Runoff_ave)

# 2) Station elevation ----------------------------------------------------
aster <- raster("D:/WORK/00_GLOBAL/ASTER/Caucasus/AW3D30_Caucasus_aoi_fill.tif")

df_buff <- df %>% 
  st_buffer(1000)

df_alt <- df %>% 
  mutate(H_station = exact_extract(aster,
                                   df_buff,
                                   "mean")) %>% 
  mutate(alt_group = case_when(
    H_station <= 500 ~ "< 500",
    dplyr::between(H_station, 500, 1000) ~ "500–1000",
    dplyr::between(H_station, 1000, 1500) ~ "1000–1500",
    H_station >= 1500 ~ "> 1500"
  )) %>% 
  mutate(alt_group = as_factor(alt_group),
         alt_group = fct_relevel(alt_group,
                                 c("< 500",
                                   "500–1000",
                                   "1000–1500",
                                   "> 1500")))

df_alt %>% 
  st_drop_geometry() %>% 
  group_by(alt_group) %>% 
  count()

ws_db %>% 
  left_join(df_alt %>% 
              dplyr::select(id, sy),
            by = "id") %>% 
  dplyr::select(area) %>% 
  summary()

# source:
# https://gscheithauer.medium.com/how-to-add-number-of-observations-to-a-ggplot2-boxplot-b22710f7ef80
stat_box_data <- function(y) {
  return( 
    data.frame(
      y = log10(10000),
      family = "Roboto Condensed",
      size = 3,
      label = paste('n =', length(y))
    )
  )
}

# Boxplot by elevation
df_overview_plot <-
  df_alt %>% 
  filter(id %in% ws_db$id) %>% 
  mutate(sy = log10(sy)) %>% 
  ggplot(aes(x = alt_group,
             y = 10^sy)) +
  geom_jitter(
    color = "#00468BFF",
    width = .3,
    alpha = .3,
    show.legend = F) +
  stat_boxplot(geom ='errorbar',
               width = .25) +
  geom_boxplot(outlier.shape = NA,
               alpha = .7) +
  stat_summary(
    fun.data = stat_box_data,
    geom = "text", 
    hjust = 0.5,
    vjust = 0.9
  ) +
  scale_y_log10(
    labels = scales::comma_format()
  ) +
  ggplot2::annotation_logticks(sides = "l",
                               colour = "grey50",
                               outside = T,
                               short = unit(0.07, "cm"),
                               mid = unit(0.1, "cm"),
                               long = unit(0.15, "cm")) +
  coord_cartesian(clip = "off") +
  ggsci::scale_color_lancet() +
  labs(y = expression("SSY, t"%.%"km"^-2%.%"yr"^-1),
       x = "Gauging station altitude, m",
       subtitle = "a")

df_overview_plot

# Median measuring period length?
df_alt %>% 
  filter(id %in% ws_db$id) %>% 
  st_drop_geometry() %>% 
  as_tibble() %>% 
  mutate(MP_length = as.numeric(MP_length)) %>% 
  drop_na(MP_length) %>% 
  summary()

mplength_plot <- df_alt %>% 
  filter(id %in% ws_db$id) %>% 
  st_drop_geometry() %>% 
  as_tibble() %>% 
  mutate(MP_length = as.numeric(MP_length)) %>% 
  drop_na(MP_length) %>%
  ggplot(aes(MP_length)) +
  geom_histogram(binwidth = 5,
                 color = "grey20",
                 alpha = .3,
                 fill = "#00468BFF") +
  scale_x_continuous(breaks = seq(0, 80, by = 10),
                     labels = seq(0, 80, by = 10),
                     expand = c(0.005, 0)) +
  scale_y_continuous(breaks = seq(0, 50, 10),
                     limits = c(0, 50),
                     expand = c(0.01, 0),
                     position = "right") +
  labs(x = "Measuring period, years",
       y = "Count",
       subtitle = "b")

df_kg_plot <- ws_db %>% 
  left_join(df_alt %>% 
              dplyr::select(id, sy),
            by = "id") %>% 
  mutate(sy = log10(sy)) %>% 
  ggplot(aes(x = KG,
             y = 10^sy)) +
  geom_jitter(
    color = "#00468BFF",
    width = .3,
    alpha = .3,
    show.legend = F) +
  stat_boxplot(geom ='errorbar',
               width = .25) +
  geom_boxplot(outlier.shape = NA, alpha = .7) +
  stat_summary(
    fun.data = stat_box_data, 
    geom = "text", 
    hjust = 0.5,
    vjust = 0.9
  ) +
  scale_y_log10(
    labels = scales::comma_format()
  ) +
  ggplot2::annotation_logticks(sides = "l",
                               colour = "grey50",
                               outside = T,
                               short = unit(0.07, "cm"),
                               mid = unit(0.1, "cm"),
                               long = unit(0.15, "cm")) +
  coord_cartesian(clip = "off") +
  ggsci::scale_color_lancet() +
  labs(y = expression("SSY, t"%.%"km"^-2%.%"yr"^-1),
       x = "Köppen-Geiger climate zone",
       subtitle = "c")

df_lz_plot <- ws_db %>% 
  left_join(df_alt %>% 
              dplyr::select(id, sy),
            by = "id") %>% 
  group_by(LZ) %>% 
  mutate(sy = log10(sy)) %>% 
  ggplot(aes(x = LZ,
             y = 10^sy)) +
  geom_jitter(
    color = "#00468BFF",
    width = .3,
    alpha = .3,
    show.legend = F) +
  stat_boxplot(geom ='errorbar',
               width = .25) +
  geom_boxplot(outlier.shape = NA, alpha = .7) +
  stat_summary(
    fun.data = stat_box_data, 
    geom = "text", 
    hjust = 0.5,
    vjust = 0.9
  ) +
  scale_y_log10(position = "right",
                labels = scales::comma_format()) +
  ggplot2::annotation_logticks(sides = "r",
                               colour = "grey50",
                               outside = T,
                               short = unit(0.07, "cm"),
                               mid = unit(0.1, "cm"),
                               long = unit(0.15, "cm")) +
  coord_cartesian(clip = "off") +
  ggsci::scale_color_lancet() +
  labs(y = expression("SSY, t"%.%"km"^-2%.%"yr"^-1),
       x = "Isachenko landscape province",
       subtitle = "d")


database_overview <- ggpubr::ggarrange(df_overview_plot,
                                       mplength_plot,
                                       df_kg_plot,
                                       df_lz_plot,
                                       nrow = 2,
                                       ncol = 2)

# 3) Uncertainties --------------------------------------------------------
fbl <- df_alt %>% 
  st_drop_geometry() %>% 
  as_tibble() %>% 
  mutate(fbl = 0.45 - 0.04 * log10(A)) %>% 
  summarise(mean_fbl = mean(fbl),
            sd_fbl = sd(fbl))

unc_final <- function(x, period){
  
  ume <- rnorm(1000,
               mean = 1,
               sd = .2)
  
  # ubl <- truncnorm::rtruncnorm(1000,
  #                              a = 0,
  #                              b = 1,
  #                              mean = fbl$mean_fbl, 
  #                              sd = fbl$sd_fbl)
  
  wei <- rweibull(1000,
                  shape = 1.22,
                  scale = 172.7)
  
  wei_sample <- sample(wei, period)
  
  ump <- mean(wei_sample)/mean(wei)
  
  # sim <- (x * ume + x/(1/ubl - 1)) / ump
  sim <- (x * ume) / ump
  
  quantile(sim, probs = c(0.025, 0.975)) %>% 
    as.data.frame() %>% 
    rownames_to_column() %>%
    rename(val = 2) %>% 
    pivot_wider(names_from = rowname,
                values_from = val) %>% 
    mutate(sim = list(data.frame(sim,
                                 rowid = 1:1000)))
}

set.seed(2021)
df_unc <- df_alt %>%
  st_drop_geometry() %>% 
  as_tibble() %>%
  mutate(MP_length = na_mean(as.numeric(MP_length))) %>% 
  dplyr::select(id, A, sy, MP_length) %>% 
  group_by(id) %>% 
  nest() %>% 
  mutate(sy_sim = map(data, ~unc_final(.x$sy, .x$MP_length))) %>%
  unnest(cols = c(data, sy_sim)) %>% 
  ungroup()

# Summary statistics ------------------------------------------------------
load("data/tidy/df_unc.Rdata")

just_stat <- ws_db %>% 
  left_join(df_alt %>% 
              st_drop_geometry() %>% 
              dplyr::select(id, sy, alt_group),
            by = "id") %>%
  get_summary_stats(sy,
                    type = "common") %>% 
  mutate_if(is.numeric,
            ~smart_round(.))

ws_db %>% 
  left_join(df_alt %>% 
              st_drop_geometry() %>% 
              dplyr::select(id, sy, alt_group),
            by = "id") %>%
  mutate(sy = sy/2.65/1000) %>% 
  get_summary_stats(sy,
                    type = "common") %>% 
  mutate_if(is.numeric,
            ~smart_round(.))

kg_stat <- ws_db %>% 
  left_join(df_alt %>% 
              st_drop_geometry() %>% 
              dplyr::select(id, sy, alt_group),
            by = "id") %>%
  group_by(KG) %>% 
  get_summary_stats(sy,
                    type = "common") %>% 
  mutate_if(is.numeric,
            ~smart_round(.)) %>% 
  dplyr::select(-variable)

lz_stat <- ws_db %>% 
  left_join(df_alt %>% 
              st_drop_geometry() %>% 
              dplyr::select(id, sy, alt_group),
            by = "id") %>%
  group_by(LZ) %>% 
  get_summary_stats(sy,
                    type = "common") %>% 
  mutate_if(is.numeric,
            ~smart_round(.)) %>% 
  dplyr::select(-variable)

alt_stat <- ws_db %>% 
  left_join(df_alt %>% 
              st_drop_geometry() %>% 
              dplyr::select(id, sy, alt_group),
            by = "id") %>%
  group_by(alt_group) %>% 
  get_summary_stats(sy,
                    type = "common") %>% 
  mutate_if(is.numeric,
            ~smart_round(.)) %>% 
  dplyr::select(-variable)

table2 <- list(just_stat,
  alt_stat,
  kg_stat,
  lz_stat) %>% 
  map(~rename(.x,
              group = 1)) %>% 
  bind_rows()

ws_db %>% 
  left_join(df_alt %>% 
              st_drop_geometry() %>% 
              dplyr::select(id, sy, alt_group),
            by = "id") %>% 
  pairwise_wilcox_test(sy ~ LZ,
                       p.adjust.method="holm") %>% 
  filter(p.adj < 0.05) %>% 
  print(n = 100)


# SAVE --------------------------------------------------------------------
ggsave("figures/fig02_database-overview.png",
       database_overview,
       dpi = 500,
       w = 10.5,
       h = 7)

save("df_unc", "df_alt",
    file = "data/tidy/df_unc.Rdata")

writexl::write_xlsx(table2,
                    "analysis/table2.xlsx")
