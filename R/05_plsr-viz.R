library(tidyverse)
library(tidytext)
library(readxl)
library(extrafont)
library(atslib)
library(sf)
library(imputeTS)
library(signs)

theme_set(theme_hp())

# Original PLS-R analysis was performed in XLSTAT 2021.3.1
# https://www.xlstat.com/en/

# 0) Additional data ------------------------------------------------------
load("data/tidy/df_unc.Rdata")
load("data/tidy/ws_db.Rdata")

sy_db <-  
  ws_db %>% 
  left_join(df_alt %>% 
              st_drop_geometry(),
            by = "id") %>% 
  mutate(MP_length = as.numeric(MP_length),
         MP_length = imputeTS::na_replace(MP_length, 10)) %>% 
  filter(MP_length > 5) %>% 
  transmute(sy = log10(sy),
            # id,
            # alt_group,
            A,
            H_station,
            dem = DEM_stdev/DEM_mean,
            area_barren,
            area_cropland,
            area_forest,
            ksn,
            pop = POP_mean,
            glacier,
            glored = GLORED_mean,
            q = Q_mean,
            p = P_mean,
            srad = SRAD_mean,
            aet = AET_mean,
            canopy = CANOPY_mean,
            sand = SAND_mean,
            hand = HAND_mean,
            pga = PGA_mean,
            cwr = CWR_mean,
            SHDI,
            KG,
            LZ) %>% 
  mutate(area_barren = imputeTS::na_replace(area_barren, fill = 1)) %>%  
  mutate(area_cropland = imputeTS::na_replace(area_cropland, fill = 1)) %>%
  # mutate_at(vars(hand:cwr),
  #           ~log10(.)) %>%
  drop_na()

clipr::write_clip(sy_db)

set.seed(2021)
pls  <-  plsr(sy ~ .,
              data = sy_db %>% 
                select_if(is.numeric),
              scale = TRUE,
              center = TRUE,
              validation = "LOO")

comp <- which.min(pls$validation$PRESS)
X    <- sy_db[,-1]
vip <- VIP(pls2, comp, length(X))

vip[vip>1]

summary(pls)

pls2 <- plsr(sy ~ .,
             data = sy_db %>% 
               select_if(is.numeric),
             ncomp = 5,
             scale = TRUE,
             center = TRUE,
             validation = "none")
summary(pls2)

sy_db %>% 
  select_if(is.numeric) %>%
  lm(sy ~ .,
     data = .) %>% 
  summary()

library(corrr)

sy_db %>% 
  select_if(is.numeric) %>% 
  correlate(method = "spearman") %>% 
  shave()
  

# 1) Read data ------------------------------------------------------------
# Original dataset
df <- read_excel("analysis/pls-analysis_dummy.xlsm",
                 sheet = 1)

# PLS-R model quality
model_quality <- read_excel("analysis/pls-analysis_dummy.xlsm",
                            sheet = 2,
                            range = "B120:F123")

# Correlation matrix of the variables with the t and u~ components:
corrmat <- read_excel("analysis/pls-analysis_dummy.xlsm",
                      sheet = 2,
                      range = "B148:J191") %>% 
  rename(var = 1)

# corscores <- read_excel("analysis/pls-analysis.xlsm",
#                         sheet = 2,
#                         range = "B506:F748")

# Variable Importance in the Projection (VIP):
vips <- read_excel("analysis/pls-analysis_dummy.xlsm",
                   sheet = 2,
                   range = "B1413:R1455")

# 2) Correlation circle ---------------------------------------------------
pls_corplot <- corrmat %>% 
  mutate(deps = ifelse(var == "sy", "Y", "X")) %>% 
  ggplot() +
  geom_hline(yintercept = 0,
             color = "grey90") +
  geom_vline(xintercept = 0,
             color = "grey90") +
  ggforce::geom_circle(
    data = tibble(x = c(0, 0),
                  y = c(0, 0),
                  r = c(0.7, 1),
                  l = c("a", "b")),
    aes(x0 = x,
        y0 = y,
        r = r,
        linetype = l),
    color = "grey70",
    show.legend = F
  ) +
  geom_segment(aes(
    x = rep(0, 43),
    y = rep(0, 43),
    xend = t1,
    yend = t2,
    color = deps),
    size = .1,
    show.legend = F) +
  geom_point(aes(t1,t2,
                 color = deps),
             size = .7) +
  ggrepel::geom_text_repel(
    aes(t1,t2,
        color = deps,
        label = var),
    show.legend = F,
    family = "Roboto Condensed",
    size = 3
  ) +
  see::scale_color_metro() +
  scale_linetype_manual(
    values = c("dashed", "solid")
  ) +
  scale_y_continuous(limits = c(-1, 1),
                     labels = signs_format(accuracy = .1)) +
  scale_x_continuous(limits = c(-1, 1),
                     labels = signs_format(accuracy = .1)) +
  labs(x = "Component  1",
       y = "Component 2",
       subtitle = "a",
       color = NULL) +
  theme(legend.position = c(0.93, 0.12))

pls_corplot

# 3) VIP ------------------------------------------------------------------
pls_vip <- vips %>% 
  transmute(vars = Variable,
            `Component 1` = `VIP(1)`,
            `Component 2` = `VIP(2)`) %>%
  mutate(vars = str_remove(vars, "area_")) %>% 
  mutate(vars = fct_inorder(vars)) %>% 
  gather(val, vip, -vars) %>% 
  mutate(major = vip > 1) %>% 
  ggplot(aes(vars, vip,
             # color = major,
             # alpha = major,
             fill = val)) +
  geom_col(position = "dodge2") +
  geom_hline(yintercept = 1,
             color = "black",
             linetype = "dashed") + 
  geom_hline(yintercept = 0.8,
             color = "grey70",
             linetype = "dashed") +
  see::scale_fill_metro() +
  scale_color_manual(values = c("grey", "black")) +
  scale_alpha_manual(values = c(0, 1)) +
  theme(axis.text.x.bottom = element_text(angle = 90,
                                          vjust = .5,
                                          hjust = 0.95)) +
  labs(color = NULL,
       fill = NULL,
       y = "VIP",
       x = NULL,
       subtitle = "b") +
  theme(legend.position = c(0.65,
                            0.85))

pls_vip

pls_plots <- ggpubr::ggarrange(pls_corplot,
                  pls_vip)


ggsave("figures/fig07_pls_corplot.png",
       pls_plots,
       dpi = 500,
       w = 12, h = 6)

cor.test(df$sy,
         df$aet,
         method = "spearman")
