library(tidyverse)
library(corrr)
library(atslib)
library(see)
library(extrafont)
library(readxl)
library(sf)
library(signs)

theme_set(theme_hp())

# 0) Load data ------------------------------------------------------------
load("data/tidy/df_unc.Rdata")
load("data/tidy/ws_sim.Rdata")
load("data/tidy/ws_db.Rdata")

# 1) Correlation matrix ---------------------------------------------------
cor_mat <- df_cor %>% 
  ungroup() %>% 
  group_by(term) %>% 
  summarise(mean = mean(sim),
            low = quantile(sim,
                     probs = c(0.025),
                     na.rm = T),
            up = quantile(sim,
                          probs = c(0.975),
                          na.rm = T)) %>% 
  arrange(-(mean)) %>% 
  mutate(term = str_remove(term, "_sim"),
         term = str_to_upper(term)) %>% 
  mutate_if(is.numeric,
            ~round(., 2))
  
writexl::write_xlsx(cor_mat,
                    "analysis/table3.xlsx")

df_cor %>% 
  filter(term != "id") %>% 
  select(-data) %>% 
  ungroup() %>% 
  mutate(term = str_remove(term, "_sim"),
         term = str_to_upper(term)) %>% 
  ggplot(aes(x = term,
             y = sim)) +
  geom_hline(yintercept = 0,
             color = "grey90",
             linetype = "dotted") +
  geom_hline(yintercept = c(-0.5,0.5),
             color = "grey80") +
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
  scale_y_continuous(breaks = c(-.8, -.5, -.2, 0, .2, .5, .8)) +
  labs(x = NULL,
       y = "Spearman r") +
  theme(axis.text.x.bottom = element_text(angle = 90,
                                          vjust = .5,
                                          hjust = 0.95))

# 1) Relief factors -------------------------------------------------------
relief_cor_plot <- df_cor_alt %>% 
  mutate(alt_group = fct_relabel(
    alt_group,
    ~ gsub("-", "–", .x)
  )) %>% 
  filter(term != "id") %>% 
  select(-data) %>% 
  ungroup() %>% 
  filter(term %in% c("area",
                     "cwr_sim",
                     "pga_sim",
                     "hand_sim",
                     "ksn_sim")) %>% 
  mutate(term = str_remove(term, "_sim"),
         term = str_to_upper(term)) %>% 
  ggplot(aes(x = term,
             y = sim)) +
  geom_hline(yintercept = 0,
             color = "grey90",
             linetype = "dotted") +
  geom_hline(yintercept = c(-0.5,0.5),
             color = "grey80") +
  stat_boxplot(geom ='errorbar',
               width = .25) +
  geom_boxplot(outlier.shape = NA) +
  stat_summary(fun = median,
               colour = "#ED0000FF",
               geom = "text",
               family = "Roboto Light",
               show.legend =F, 
               vjust = -0.6,
               aes(label = signs(..y..,
                                 accuracy=.01))) +
  scale_y_continuous(breaks = c(-.8, -.5, -.2, 0, .2, .5, .8),
                     labels = signs_format(accuracy = .1)) +
  facet_wrap(~alt_group,
             labeller = labeller(
               alt_group = function(x) paste0(x, " m a.s.l.")
             )) +
  labs(x = NULL,
       y = "Spearman r") +
  theme(axis.text.x.bottom = element_text(angle = 90,
                                          vjust = .5,
                                          hjust = 0.95))

relief_cor_plot

ggsave("figures/fig03_relief_cor_plot.png",
       relief_cor_plot,
       dpi = 500,
       w = 8, h = 6)

# 2) Landuse factors -------------------------------------------------------
landuse_cor_plot <- df_cor_alt %>% 
  mutate(alt_group = fct_relabel(
    alt_group,
    ~ gsub("-", "–", .x)
  )) %>% 
  filter(term != "id") %>% 
  select(-data) %>% 
  ungroup() %>% 
  filter(term %in% c("canopy_sim",
                     "barren_sim",
                     "crop_sim",
                     "pop_sim",
                     "shdi_sim",
                     "forest_sim",
                     "glacier_sim")) %>% 
  mutate(term = str_remove(term, "_sim"),
         term = str_to_upper(term)) %>% 
  ggplot(aes(x = term,
             y = sim)) +
  geom_hline(yintercept = 0,
             color = "grey90",
             linetype = "dotted") +
  geom_hline(yintercept = c(-0.5,0.5),
             color = "grey80") +
  stat_boxplot(geom ='errorbar',
               width = .25) +
  geom_boxplot(outlier.shape = NA) +
  stat_summary(fun = median,
               colour = "#ED0000FF",
               geom = "text",
               family = "Roboto Light",
               show.legend =F, 
               vjust = -0.6,
               aes(label = signs(..y..,
                                 accuracy=.01))) +
  scale_y_continuous(breaks = c(-.8, -.5, -.2, 0, .2, .5, .8),
                     limits = c(-.8, .8),
                     labels = signs_format(accuracy = .1)) +
  facet_wrap(~alt_group,
             labeller = labeller(
               alt_group = function(x) paste0(x, " m a.s.l.")
             )) +
  labs(x = NULL,
       y = "Spearman r") +
  theme(axis.text.x.bottom = element_text(angle = 90,
                                          vjust = .5,
                                          hjust = 0.95))

landuse_cor_plot

ggsave("figures/fig04_landuse_cor_plot.png",
       landuse_cor_plot,
       dpi = 500,
       w = 8, h = 6)

# 3) Climate factors -------------------------------------------------------
climate_cor_plot <- df_cor_alt %>% 
  mutate(alt_group = fct_relabel(
    alt_group,
    ~ gsub("-", "–", .x)
  )) %>% 
  filter(term != "id") %>% 
  select(-data) %>% 
  ungroup() %>% 
  filter(term %in% c("p_sim",
                     "q_sim",
                     "aet_sim",
                     "srad_sim",
                     "glored_sim")) %>% 
  mutate(term = str_remove(term, "_sim"),
         term = str_to_upper(term)) %>% 
  ggplot(aes(x = term,
             y = sim)) +
  geom_hline(yintercept = 0,
             color = "grey90",
             linetype = "dotted") +
  geom_hline(yintercept = c(-0.5,0.5),
             color = "grey80") +
  stat_boxplot(geom ='errorbar',
               width = .25) +
  geom_boxplot(outlier.shape = NA) +
  stat_summary(fun = median,
               colour = "#ED0000FF",
               geom = "text",
               family = "Roboto Light",
               show.legend =F, 
               vjust = -0.6,
               aes(label = signs(..y..,
                                 accuracy=.01))) +
  scale_y_continuous(breaks = c(-.8, -.5, -.2, 0, .2, .5, .8),
                     limits = c(-.8, .8),
                     labels = signs_format(accuracy = .1)) +
  facet_wrap(~alt_group,
             labeller = labeller(
               alt_group = function(x) paste0(x, " m a.s.l.")
             )) +
  labs(x = NULL,
       y = "Spearman r") +
  theme(axis.text.x.bottom = element_text(angle = 90,
                                          vjust = .5,
                                          hjust = 0.95))

climate_cor_plot

ggsave("figures/fig05_climate_cor_plot.png",
       climate_cor_plot,
       dpi = 500,
       w = 8, h = 6)


# 4) Scatter plots --------------------------------------------------------
df_plot <- df_unc %>% 
  filter(MP_length >=7) %>% 
  dplyr::select(-sim) %>% 
  left_join(df_alt %>% 
              st_drop_geometry() %>% 
              dplyr::select(id, alt_group),
            by = "id") %>% 
  left_join(ws_sim %>% 
              select(-contains("sim")),
            by = "id") %>% 
  left_join(ws_db %>% 
              select(id, area_cropland,
                     area_forest,
                     area_barren,
                     ksn,
                     glacier,
                     contains("mean")),
            by = "id") %>% 
  mutate(alt_group = fct_relabel(
    alt_group,
    ~ gsub("-", "–", .x)
  ))

hand_plot <-
  df_plot %>% 
  ggplot(aes(y = sy,
             x = HAND_mean,
             color = alt_group)) +
  geom_errorbar(aes(xmin = `hand_2.5%`,
                    xmax = `hand_97.5%`),
                color = "grey80") + 
  geom_errorbar(aes(ymin = `2.5%`,
                    ymax = `97.5%`),
                color = "grey80") +
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
  labs(subtitle = "c",
       x = "Height Above Nearest Drainage, m",
       y = expression("Sediment yield, t"%.%"km"^-2%.%"yr"^-1),
       color = "Altitude group: ",
       fill = "Altitude group: ")

cwr_plot <-
  df_plot %>% 
  ggplot(aes(y = sy,
             x = CWR_mean,
             color = alt_group)) +
  # geom_errorbar(aes(xmin = `cwr_2.5%`,
                    # xmax = `cwr_97.5%`),
                # color = "grey80") + 
  geom_errorbar(aes(ymin = `2.5%`,
                    ymax = `97.5%`),
                color = "grey80") +
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
  labs(subtitle = "a",
       x = expression("Chemical weathering rate, t"%.%"km"^-2%.%"yr"^-1),
       y = expression("Sediment yield, t"%.%"km"^-2%.%"yr"^-1),
       color = "Altitude group: ",
       fill = "Altitude group: ")

pga_plot <-
  df_plot %>% 
  ggplot(aes(y = sy,
             x = PGA_mean,
             color = alt_group)) +
  # geom_errorbar(aes(xmin = `pga_2.5%`,
  #                   xmax = `pga_97.5%`),
  #               color = "grey80") + 
  geom_errorbar(aes(ymin = `2.5%`,
                    ymax = `97.5%`),
                color = "grey80") +
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
  labs(subtitle = "b",
       x = expression("Peak Ground Acceleration, m"%.%"s"^-2),
       y = expression("Sediment yield, t"%.%"km"^-2%.%"yr"^-1),
       color = "Altitude group: ",
       fill = "Altitude group: ")

crop_plot <-
  df_plot %>% 
  filter(area_cropland > 2) %>% 
  ggplot(aes(y = sy,
             x = area_cropland,
             color = alt_group)) +
  geom_errorbar(aes(xmin = `crop_2.5%`,
                    xmax = `crop_97.5%`),
                color = "grey80") + 
  geom_errorbar(aes(ymin = `2.5%`,
                    ymax = `97.5%`),
                color = "grey80") +
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
                        label.y = seq(0.1, 0.25,
                                      by = .05)
                        ) +
  scale_fill_metro() +
  scale_color_metro() +
  scale_y_log10() +
  scale_x_log10() +
  ggplot2::annotation_logticks(sides = "lb",
                               colour = "grey50",
                               outside = T,
                               short = unit(0.07, "cm"),
                               mid = unit(0.1, "cm"),
                               long = unit(0.15, "cm")) +
  coord_cartesian(clip = "off") +
  labs(subtitle = "f",
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
  geom_errorbar(aes(xmin = `barren_2.5%`,
                    xmax = `barren_97.5%`),
                color = "grey80") + 
  geom_errorbar(aes(ymin = `2.5%`,
                    ymax = `97.5%`),
                color = "grey80") +
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
  ggplot2::annotation_logticks(sides = "lb",
                               colour = "grey50",
                               outside = T,
                               short = unit(0.07, "cm"),
                               mid = unit(0.1, "cm"),
                               long = unit(0.15, "cm")) +
  coord_cartesian(clip = "off") +
  labs(subtitle = "e",
       x = "Barren area in 2015, %",
       y = expression("Sediment yield, t"%.%"km"^-2%.%"yr"^-1),
       color = "Altitude group: ",
       fill = "Altitude group: ")

ksn_plot <-
  df_plot %>% 
  ggplot(aes(y = sy,
             x = ksn ,
             color = alt_group)) +
  geom_errorbar(aes(xmin = `ksn_2.5%`,
                    xmax = `ksn_97.5%`),
                color = "grey80") + 
  geom_errorbar(aes(ymin = `2.5%`,
                    ymax = `97.5%`),
                color = "grey80") +
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
  ggplot2::annotation_logticks(sides = "lb",
                               colour = "grey50",
                               outside = T,
                               short = unit(0.07, "cm"),
                               mid = unit(0.1, "cm"),
                               long = unit(0.15, "cm")) +
  coord_cartesian(clip = "off") +
  labs(subtitle = "d",
       x = expression("Normalized steepness index, m"^-0.9),
       y = expression("Sediment yield, t"%.%"km"^-2%.%"yr"^-1),
       color = "Altitude group: ",
       fill = "Altitude group: ")

# SAVE --------------------------------------------------------------------

scatter_plots <- ggpubr::ggarrange(
  cwr_plot,
  pga_plot,
  hand_plot,
  ksn_plot,
  barren_plot,
  crop_plot,
  nrow = 2,
  ncol = 3,
  common.legend = T,
  legend = "bottom")

ggsave("figures/fig06_scatter_unc_plots.png",
       scatter_plots,
       dpi = 500,
       w = 12, h = 9)
