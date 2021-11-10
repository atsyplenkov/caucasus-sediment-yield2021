library(tidyverse)
library(atslib)
library(extrafont)
library(readxl)
library(sf)
library(easystats)
library(skimr)

theme_set(theme_hp())

# 1) Load data ------------------------------------------------------------
load("data/tidy/df_unc.Rdata")
load("data/tidy/ws_db.Rdata")

sy_db <-  
  ws_db %>% 
  left_join(df_alt %>% 
              st_drop_geometry(),
            by = "id") %>% 
  transmute(id,
            alt_group,
            sy,
            # A,
            # H_station,
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
            cwr = CWR_mean) %>% 
  mutate(area_barren = imputeTS::na_replace(area_barren, fill = 1)) %>%  
  mutate(area_cropland = imputeTS::na_replace(area_cropland, fill = 1)) %>%
  # mutate_at(vars(hand:cwr),
  #           ~log10(.)) %>%
  drop_na() %>% 
  # mutate_at(vars(-id, -H_station, -glacier),
  #           ~log10(.))
  mutate(lamda_pga = forecast::BoxCox.lambda(pga,
                                         method = "loglik",
                                         lower = -3,
                                         upper = 3)) %>%
  mutate(pga = pga^lamda_pga) %>%
  mutate(lamda_ksn = forecast::BoxCox.lambda(ksn,
                                         method = "loglik",
                                         lower = -3,
                                         upper = 3)) %>%
  mutate(ksn = ksn^lamda_ksn) %>%
  # # mutate(lamda_bar = forecast::BoxCox.lambda(area_barren,
  # #                                        method = "loglik",
  # #                                        lower = -3,
  # #                                        upper = 3)) %>%
  # # mutate(area_barren = area_barren^lamda_bar) %>%
  dplyr::select(-contains("lamda"))

# 2) Model! ---------------------------------------------------------------
sy_mod <- lm(log10(sy) ~ ., data = sy_db[,-1])

sy_mod <- lm(log10(sy) ~ pga + hand + area_barren + ksn + q + sand + cwr, data = sy_db[,-1])

sy_mod2 <- parameters::select_parameters(sy_mod,
                                         method = "L1",
                                         cross_validation = TRUE)

compare_performance(sy_mod, sy_mod2)
compare_parameters(sy_mod, sy_mod2)

summary(sy_mod)

r2(sy_mod2)  
check_collinearity(sy_mod2)
check_heteroscedasticity(sy_mod2)
check_autocorrelation(sy_mod2)

check_model(sy_mod2)

sy_db %>% 
  mutate(Predicted = predict(sy_mod, sy_db)) %>% 
  ggplot(aes(x = log10(sy), y = Predicted)) +
  geom_point() +
  atslib::Add_1_line() +
  atslib::Add_R2() +
  labs(x = "Observed")

# PCA ---------------------------------------------------------------------
skimr::skim(sy_db)

res.pca <- prcomp(sy_db[,-1:-2],
                  scale = TRUE)

library(factoextra)
fviz_pca_contrib(res.pca, choice = "var",
                ggtheme = theme_hp())

