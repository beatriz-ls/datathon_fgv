# libs

library(tidyverse)
library(rsample)
library(forecast)
library(janitor)

# load data

dt <- readRDS("data.rds")

# data pre processing

df <- dt[complete.cases(dt),]

dt <- dt %>% 
  select(-symbol) %>%
  mutate(date = as.POSIXct(date, format = "%Y-%m-%d %H:%M:%S"),
         name = as.factor(name),
         cluster = as.factor(cluster)) %>%
  clean_names()

# split data

set.seed(123)

dt_split <- dt %>%
  initial_split(prop = .75)

trn_dt <- dt_split %>%
  training()

tst_dt <- dt_split %>%
  testing()

set.seed(123)
dt_cv <- trn_dt %>%
  vfold_cv(v = 5)

###### models ------------------------------------------------------------------

# modelo adam

model_adam <- adam_reg() %>%
  set_engine("adam") %>%
  set_mode("regression")

model_adam %>%
  fit(close ~ open+high+low+volume, data = trn_dt)

# modelo prophet

model_prophet <- prophet_reg() %>%
  set_engine("prophet") %>%
  set_mode("regression")

model_prophet %>%
  fit(close ~ open+high+low+volume, data = trn_dt)

# ETS model

model_ets <- exp_smoothing() %>%
  set_engine("ets") %>%
  set_mode("regression")

model_ets %>%
  fit(close ~ open+high+low+volume, data = trn_dt)

####### workflows --------------------------------------------------------------

fit_adam <- workflow() %>%
  add_formula(close ~ open+high+low+volume) %>%
  add_model(model_adam) %>%
  fit(data = trn_dt)

fit_prophet <- workflow() %>%
  add_formula(close ~ open+high+low+volume) %>%
  add_model(model_prophet) %>%
  fit(data = trn_dt)

fit_ets <- workflow() %>%
  add_formula(close ~ open+high+low+volume) %>%
  add_model(model_ets) %>%
  fit(data = trn_dt)

####### predictions ------------------------------------------------------------

predictions_adam <- fit_adam %>% augment(new_data = tst_dt)

predictions_prophet <- fit_prophet %>% augment(new_data = tst_dt)

predictions_ets <- fit_ets %>% augment(new_data = tst_dt)


pred_dt <- tst_dt %>%
  mutate(
    pred_adam = predictions_adam$.pred,
    pred_prophet = predictions_prophet$.pred,
    pred_ets = predictions_ets$.pred
  )

results <- pred_dt %>%
  pivot_longer(cols = starts_with("pred_"), 
               names_to = "model", 
               values_to = "prediction")

metrics <- results %>%
  group_by(model) %>%
  summarise(
    rmse = rmse_vec(truth = close, estimate = prediction),
    mae = mae_vec(truth = close, estimate = prediction),
    rsq = rsq_vec(truth = close, estimate = prediction)
  )








