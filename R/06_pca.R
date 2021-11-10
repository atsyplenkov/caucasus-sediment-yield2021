library(tidyverse)
library(tidymodels)
library(tidytext)
library(atslib)
library(extrafont)
library(readxl)
library(sf)
library(skimr)
library(embed)
library(dbscan)
library(factoextra)
library(mapview)
library(ggmap)

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
  drop_na()

# 2) PCA recipe -----------------------------------------------------------
sy_recipe <- sy_db %>% 
  recipe(sy ~ ., data = .) %>%
  update_role(id, alt_group, new_role = "id") %>%
  step_log(sy) %>% 
  # step_BoxCox(all_predictors()) %>%
  step_normalize(all_predictors()) %>%
  step_pca(all_predictors())

sy_prep <- prep(sy_recipe)

(tidied_pca <- tidy(sy_prep, 3))

# tidied_pca <- as.data.frame(sy_prep$steps[[4]]$res$rotation) %>% 
#   rownames_to_column(var = "terms") %>% 
#   gather(component, value, -terms)

# 3) PCA components -------------------------------------------------------
(pca_components <- tidied_pca %>%
   filter(component %in% paste0("PC", 1:6)) %>% 
   group_by(component) %>%
   top_n(6, abs(value)) %>%
   ungroup() %>%
   mutate(terms = str_to_upper(str_remove(terms, "area_"))) %>%  
   mutate(terms = reorder_within(terms, abs(value), component),
          positive = ifelse(value > 0, "Positive", "Negative")) %>%
   ggplot(aes(abs(value),
              terms,
              fill = positive)) +
   geom_col(alpha = .7) +
   facet_wrap(~component, scales = "free_y") +
   scale_y_reordered() +
   see::scale_fill_metro(palette = "full",
                         reverse = T) +
   labs(
     # subtitle = "a",
     x = "Absolute value of contribution",
     y = NULL, fill = ""
   ))

# How much variation are we capturing?
sdev <- sy_prep$steps[[3]]$res$sdev

(percent_variation <- sdev^2 / sum(sdev^2))

(pca_variance <- tibble(
  component = unique(tidied_pca$component),
  percent_var = cumsum(percent_variation)) %>%  ## use cumsum() to find cumulative, if you prefer
    mutate(component = fct_inorder(component)) %>%
    ggplot(aes(component, percent_variation)) +
    geom_col() +
    geom_text(aes(label = scales::percent(cumsum(percent_variation),
                                          accuracy = 0.1)),
              vjust = -.8, color = "black") +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(x = NULL,
         y = "Percent variance explained by each PCA component")
)

# 4) PCA viz --------------------------------------------------------------
pca_fit <- juice(sy_prep) %>%
  select(-id, -alt_group) %>%
  lm(sy ~ ., data = .)

summary(pca_fit)

juice(sy_prep) %>%
  ggplot(aes(PC1, PC2,
             label = id,
             color = alt_group)) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             alpha = .5) +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             alpha = .5) +
  geom_point(alpha = 0.4) 

res.pca <- sy_db %>% 
  select(-id:-sy) %>% 
  rename_at(vars(contains("area")),
            ~str_remove(., "area_")) %>%
  rename_all(~str_to_upper(.)) %>% 
  prcomp(scale = TRUE)

(pca_plot <-
  fviz_pca_biplot(res.pca,
                addEllipses = T,
                col.ind = sy_db$alt_group,
                col.var = "gray20",
                repel = T,
                palette = see::palette_metro()(4),
                label = "var",
                alpha = .3) +
  labs(title = "",
       # subtitle = "b",
       color = "Altitude group",
       shape = "Altitude group",
       fill = "Altitude group") +
  theme_hp() +
  theme(plot.margin = margin(rep(1, 4))))

ggsave("figures/pca_components.png",
       pca_components,
       dpi = 500,
       w = 10, h = 7)

ggsave("figures/pca_plot.png",
       pca_plot,
       dpi = 500,
       w = 8, h = 6.5)

# 5) PCA map --------------------------------------------------------------
library(tricolore)
library(ggtern)
library(cowplot)

ws <- st_read("data/spatial/ws_caucasus-b.shp") %>% 
  st_transform(4326) %>% 
  left_join(juice(sy_prep),
            by = "id") %>% 
  drop_na(sy) %>% 
  rmapshaper::ms_simplify(0.4)

# 5.1) Tricolor color scheme
range01 <- function(x){(x-min(x))/(max(x)-min(x))}

don_tric <- ws %>%
  st_drop_geometry() %>% 
  mutate_at(vars(PC1:PC3),
            ~scale(.)) %>% 
  mutate_at(vars(PC1:PC3),
            ~range01(.)) %>% 
  select(id,
         PC1:PC3)

tric <- Tricolore(
  don_tric,
  p1 = 'PC1',
  p2 = 'PC2',
  p3 = 'PC3',
  center = NA,
  breaks = 16,
  hue = 0.2,
  chroma = 0.9,
  lightness = 1,
  contrast = 0.3,
  spread = 1.4,
  legend = TRUE,
  show_data = FALSE,
  show_center = TRUE,
  label_as = "pct",
  crop = FALSE
)

tricolor_legend <- tric$key +
  geom_point(data = don_tric,
             aes(PC1, PC2, z = PC3), 
             shape = 46, color = "grey20", size = 3) +
  # geom_point(data = don_tric %>% filter(id == 2108), 
  #            aes(PC1, PC2, z = PC3), 
  #            shape = 1, color = "darkblue", size = 3) +
  Larrowlab("% PC1") +
  Tarrowlab("% PC2") +
  Rarrowlab("% PC3") +
  labs(x = "", y = "", z = "") +
  theme_hp() +
  theme(tern.axis.arrow.show = TRUE,
        tern.axis.text = element_text(size = 9,
                                      family = "Noto Sans",
                                      face = 2,
                                      colour = "grey20"),
        plot.background = element_rect(fill = NA,
                                       colour = NA))

ws$rgb <- tric$rgb

# 5.2) Tricolor map
# Merge rgb and point shapefile
sy_plot <- df_alt %>% 
  left_join(ws %>% 
              st_drop_geometry(),
            by = "id") %>% 
  mutate(rgb = ifelse(is.na(rgb),
                      "white",
                      rgb))

# Basemap
# map <- get_stamenmap(bbox = c(left = 37.4, bottom = 38.4,
#                               right = 49.7, top = 45.5),
#                      zoom = 7,
#                      maptype = "terrain-background",
#                      color = "bw")

load("data/tidy/stamen-map.Rdata")

tricolor_map <- ggmap(map,
                      darken = c(0.6, "white")) +
  annotate("text", x = 39, y = 42.6,
           label = "B l a c k  s e a",
           color = "gray80",
           size = 7,
           angle = -30,
           family = "Noto Sans",
           fontface = "italic") +
  geom_sf(data = ws,
          # aes(fill = rgb, geometry = geometry),
          aes(geometry = geometry),
          fill = NA,
          color = "grey70",
          inherit.aes = FALSE) +
  geom_sf(data = sy_plot,
          aes(fill = rgb,
              geometry = geometry),
          size = 3, 
          stroke = .1,
          alpha = .9,
          shape = 21,
          color = "grey60",
          inherit.aes = FALSE) +
  scale_fill_identity() +
  atslib::theme_map()

map_legend <- 
  tricolor_map +
  annotation_custom(ggplotGrob(tricolor_legend),
                    xmin = 37, xmax = 43,
                    ymin = 37.2, ymax = 43.2)

ggsave("figures/caucasus_tricolor.png",
       map_legend,
       dpi = 700,
       width = 12, height = 7)

# 5) UMAP -----------------------------------------------------------------
umap_rec <- sy_db %>% 
  # select(-glacier) %>% 
  recipe(sy ~ ., data = .) %>%
  update_role(id, alt_group, new_role = "id") %>%
  step_log(sy) %>% 
  step_normalize(all_predictors()) %>%
  # step_BoxCox(all_predictors()) %>% 
  step_umap(all_predictors(),
            seed = c(2021, 21),
            neighbors = 8,
            num_comp = 2)

umap_prep <- prep(umap_rec)

# umap_prep$steps[[2]]$sds

juice(umap_prep) %>%
  ggplot(aes(umap_1, umap_2, label = id,
             color = alt_group)) +
  geom_point(alpha = 0.7, size = 2) +
  # geom_text(check_overlap = TRUE, hjust = "inward") +
  labs(color = NULL)

db <- juice(umap_prep) %>% 
  select(contains("umap")) %>% 
  hdbscan(minPts = 20) 

juice(umap_prep) %>%
  mutate(cluster = db$cluster) %>% 
  mutate(prob = db$membership_prob) %>% 
  ggplot(aes(umap_1, umap_2,
             label = id,
             alpha = prob,
             color = as_factor(cluster))) +
  geom_point( size = 2) +
  # geom_text(check_overlap = TRUE, hjust = "inward") +
  labs(color = "Cluster",
       alpha = "Probability")

df_alt %>% 
  left_join(juice(umap_prep) %>%
              mutate(cluster = db$cluster) %>% 
              mutate(prob = db$membership_prob) %>% 
              select(id, cluster),
             by = "id") %>% 
  drop_na(cluster) %>% 
  mapview(zcol = "cluster", burst = T, hide = T)


