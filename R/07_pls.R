#Load the required libraries
library(tidyverse)
library(caret)
library(pls)
library(e1071)
library(corrplot)
library(rgdal)
library(tmap)
library(plyr)
library(gridExtra)
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

# 2) Principal Component Analysis -----------------------------------------
#Create PCs based on correlated blocks
sy.trans <- preProcess(x = sy_db[,-1:-2],
                         method = c("center", "scale", "pca"),
                         thresh = 95)

pca.data <- predict(sy.trans, sy_db)

#Look into PC1 loadings
head(sort(sy.trans$rotation[, 1], decreasing = TRUE))

sy_pca <- sy_db %>% 
  inner_join(pca.data,
             by = "id")

# 3) Partial Least Squares ------------------------------------------------
folds <- createMultiFolds(sy_pca$sy,
                          k = 10,
                          times = 50)

tr.ctrl <- trainControl("repeatedcv",
                        index = folds,
                        selectionFunction = "best")

#Train SY model
set.seed(2021)

sy.fit <- train(y = sy_db$sy,
                x = sy_db[, -1:-3],
                method = "pls",
                metric = "Rsquared",
                tuneLength = 20,
                trControl = tr.ctrl,
                preProc = c("zv", "center", "scale"))

plot(sy.fit)

plot(varImp(sy.fit), 16)

varImp(sy.fit)$importance %>%
  rownames_to_column(var = "term") %>% 
  as_tibble() %>% 
  arrange(Overall) %>% 
  mutate(term = fct_inorder(term)) %>% 
  ggplot(aes(x = term,
             y = Overall)) +
  geom_segment(aes(y = 0,
                   yend = Overall,
                   xend = term)) +
  geom_point() +
  coord_flip()

# 4) PLSR -----------------------------------------------------------------
library(plsVarSel)

sy_pls <- as.data.frame(sy_db[,-1:-2])

set.seed(1) 
pls.fit <-
  # plsr(log10(sy) ~ .,
  plsr(sy ~ .,
       data = sy_pls,
       scale = TRUE,
       validation = "CV")

summary (pls.fit)
cv = RMSEP(pls.fit)
(best.dims = which.min(cv$val[estimate = "adjCV", , ]) - 1)

set.seed(1) 
pls.fit2 <-
  # plsr(log10(sy) ~ .,
  plsr(sy ~ .,
       data = sy_pls,
       scale = TRUE,
       ncomp = best.dims)

coefficients = coef(pls.fit2)
sum.coef = sum(sapply(coefficients, abs))
coefficients = coefficients * 100 / sum.coef

coefficients %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "term") %>% 
  as_tibble() %>% 
  dplyr::rename(Overall = 2) %>% 
  arrange(Overall) %>% 
  mutate(term = fct_inorder(term)) %>% 
  ggplot(aes(x = term,
             y = Overall)) +
  geom_segment(aes(y = 0,
                   yend = Overall,
                   xend = term)) +
  geom_point() +
  coord_flip()

# Variable selection ------------------------------------------------------
set.seed(2021)
pls  <-  plsr(log10(sy) ~ .,
              data = sy_pls,
              scale = TRUE,
              validation = "CV")
comp <- which.min(pls$validation$PRESS)
X    <- sy_pls[,-1]
vip <- VIP(pls, comp, length(X))

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
