library(tidyverse)
library(tidymodels)
library(tidytext)
library(ggforce)
library(plsVarSel)
library(sf)
library(extrafont)
library(atslib)

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
            # alt_group,
            sy,
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
            cwr = CWR_mean) %>% 
  mutate(area_barren = imputeTS::na_replace(area_barren, fill = 1)) %>%  
  mutate(area_cropland = imputeTS::na_replace(area_cropland, fill = 1)) %>%
  # mutate_at(vars(hand:cwr),
  #           ~log10(.)) %>%
  drop_na() %>% 
  mutate(sy = log10(sy)) %>%
  dplyr::select(-id)

sy_db %>% 
  left_join(tt, by = "id") %>% 
  dplyr::select(-id) %>% 
  ggplot(aes(KG, 10^sy)) +
  stat_boxplot(geom ='errorbar',
               width = .25) +
  geom_boxplot(outlier.shape = NA) +
  stat_summary(fun = median,
               colour = "#ED0000FF",
               geom = "text",
               family = "Roboto Light",
               show.legend =F, 
               vjust = -0.4,
               aes(label = round(..y..,
                                 digits=2))) +
  scale_y_log10()



glimpse(sy_db)

# 2) Recipe ---------------------------------------------------------------
sy_rec <-
  recipe(sy ~ ., data = sy_db[,-1]) %>%
  # update_role(id, new_role = "id variable") %>% 
  step_zv(all_numeric_predictors()) %>%
  step_normalize(all_numeric_predictors())

rec_trained <- prep(sy_rec)
rec_trained

plot_test_results <- function(recipe,
                              dat = sy_db) {
  recipe %>%
    prep() %>%
    bake(new_data = dat) %>%
    mutate(alt_group = sy_db$alt_group) %>% 
    ggplot() +
    geom_autopoint(aes(color = sy),
                   alpha = 0.4,
                   size = 0.5) +
    geom_autodensity(alpha = .3) +
    facet_matrix(vars(-sy, -alt_group),
                 layer.diag = 2) +
    # see::scale_color_metro() +
    scale_color_distiller(palette = "BuPu",
                          direction = 1) +
    labs(color = "Altitude, m a.s.l.")
}

# 3) PCA ------------------------------------------------------------------
rec_trained %>%
  step_pca(all_numeric_predictors(),
           num_comp = 5) %>%
  plot_test_results() +
  ggtitle("Principal Component Analysis")

rec_trained %>%
  step_pca(all_numeric_predictors(),
           num_comp = 4) %>%
  prep() %>%
  tidy(number = 3) %>%
  filter(component %in% paste0("PC", 1:5)) %>%
  group_by(component) %>%
  slice_max(abs(value), n = 5) %>%
  ungroup() %>%
  ggplot(aes(abs(value),
             terms,
             fill = value > 0)) +
  geom_col(alpha = 0.8) +
  facet_wrap(vars(component),
             scales = "free_y") +
  labs(x = "Contribution to principal component",
       y = NULL,
       fill = "Positive?")

# 4) PLS ------------------------------------------------------------------
rec_trained %>%
  step_pls(all_numeric_predictors(),
           outcome = "sy", num_comp = 4) %>%
  plot_test_results() +
  ggtitle("Partial Least Squares")

rec_trained %>%
  step_pls(all_numeric_predictors(),
           outcome = "sy",
           num_comp = 4) %>%
  prep() %>%
  tidy(number = 3) %>%
  filter(component %in% paste0("PLS", 1:4)) %>%
  group_by(component) %>%
  slice_max(abs(value),
            n = 5) %>%
  arrange(-abs(value),
          .by_group = T) %>% 
  ungroup() %>%
  mutate(terms = reorder_within(terms,
                                abs(value),
                                component)) %>% 
  ggplot(aes(abs(value),
             terms,
             fill = value > 0)) +
  geom_col(alpha = 0.8) +
  facet_wrap(vars(component),
             scales = "free_y") +
  tidytext::scale_y_reordered() +
  labs(x = "Contribution to PLS component",
       y = NULL,
       fill = "Positive?")

set.seed(2021)
pls  <-  plsr(sy ~ .,
              data = sy_db[,-1], 
              ncomp = 4,
              scale = TRUE,
              validation = "LOO")
comp <- which.min(pls$validation$PRESS)
vip <- VIP(pls,
           comp,
           # 2,
           length(sy_db[,-1]))

summary(pls)
validationplot(pls,
               val.type="R2")

pls$projection %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  ggplot(aes(`Comp 1`, `Comp 2`)) +
  geom_point() +
  ggrepel::geom_text_repel(aes(label = rowname)) +
  lims(x = c(-1, 1),
       y = c(-1, 1))

vip %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "term") %>% 
  as_tibble() %>% 
  dplyr::rename(Overall = 2) %>% 
  arrange(Overall) %>% 
  mutate(term = fct_inorder(term)) %>% 
  # filter(Overall > 1) %>% 
  ggplot(aes(x = term,
             y = Overall)) +
  geom_segment(aes(y = 0,
                   yend = Overall,
                   xend = term)) +
  geom_point() +
  geom_point(data = . %>% 
               filter(Overall > 1),
             color = "coral",
             size = 2) +
  coord_flip()


plot(pls, "biplot")

library(mdatools)

Xc <- sy_db %>% select(-alt_group:-sy)
yc <- sy_db$sy

mjk <- pls(Xc, yc,
           ncomp = 4,
           scale = TRUE,
           cv = 10)

plotRegcoeffs(mjk, type = "h", show.ci = TRUE, show.labels = TRUE)

getRegcoeffs(mjk,
             ncomp = 4,
             full = T)

summary(mjk$coeffs, ncomp = 4)

summary(mjk)

exclcols = mjk$coeffs$p.values[, 2, 1] > 0.05

mjk <- pls(Xc, yc,
           ncomp = 4,
           scale = TRUE,
           cv = 10,
           exclcols = exclcols)

summary(mjk)
