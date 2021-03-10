## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(mixpoissonreg)

fit_nb_full <- mixpoissonregML(daysabs ~ gender + math + prog | gender + math + prog,
                             model = "NB", data = Attendance)

fit_pig_full <- mixpoissonregML(daysabs ~ gender + math + prog | gender + math + prog,
                             model = "PIG", data = Attendance)

## -----------------------------------------------------------------------------
summary(fit_nb_full)

## -----------------------------------------------------------------------------
summary(fit_pig_full)

## -----------------------------------------------------------------------------
fit_nb_red <- mixpoissonregML(daysabs ~ gender + math + prog | prog,
                             model = "NB", data = Attendance)

fit_pig_red <- mixpoissonregML(daysabs ~ gender + math + prog | prog,
                             model = "PIG", data = Attendance)

## -----------------------------------------------------------------------------
summary(fit_nb_red)

## -----------------------------------------------------------------------------
summary(fit_pig_red)

## -----------------------------------------------------------------------------
lmtest::waldtest(fit_nb_red, fit_nb_full)

## -----------------------------------------------------------------------------
lmtest::waldtest(fit_pig_red, fit_pig_full)

## -----------------------------------------------------------------------------
lmtest::lrtest(fit_nb_red, fit_nb_full)

## -----------------------------------------------------------------------------
lmtest::lrtest(fit_pig_red, fit_pig_full)

## -----------------------------------------------------------------------------
library(tidyr)
library(dplyr)

gender <- c("female", "male")
prog <- c("Academic", "General", "Vocational")
math <- c(0, 99)

new_cov <- crossing(gender, prog, math)

pred_values_nb <- predict(fit_nb_red, newdata = new_cov)

bind_cols(new_cov, "Predicted_Means_NB" = pred_values_nb)

## -----------------------------------------------------------------------------
pred_values_pig <- predict(fit_pig_red, newdata = new_cov)

bind_cols(new_cov, "Predicted_Means_PIG" = pred_values_pig)

## ----warning = FALSE----------------------------------------------------------
set.seed(2021) 
# We are fixing the seed for reproducibility
fit_nb_red_env <- mixpoissonregML(daysabs ~ gender + math + prog | prog,
                             model = "NB", data = Attendance, envelope = 20)

fit_pig_red_env <- mixpoissonregML(daysabs ~ gender + math + prog | prog,
                             model = "PIG", data = Attendance, envelope = 20)

## -----------------------------------------------------------------------------
summary(fit_nb_red_env)

## -----------------------------------------------------------------------------
summary(fit_pig_red_env)

## -----------------------------------------------------------------------------
autoplot(fit_nb_red_env, which = c(1,2))

## -----------------------------------------------------------------------------
autoplot(fit_pig_red_env, which = c(1,2))

## -----------------------------------------------------------------------------
autoplot(fit_nb_red, which = c(3,4,5))

## -----------------------------------------------------------------------------
autoplot(fit_pig_red, which = c(3,4,5))

## -----------------------------------------------------------------------------
# Relative change for mean-related coefficients
(influence(fit_nb_red)$coefficients.mean[94,] - 
   coefficients(fit_nb_red, parameters = "mean"))/
  coefficients(fit_nb_red, parameters = "mean")

# Relative change for precision-related coefficients
(influence(fit_nb_red)$coefficients.precision[94,] - 
    coefficients(fit_nb_red, parameters = "precision"))/
  coefficients(fit_nb_red, parameters = "precision")

## -----------------------------------------------------------------------------
# Relative change for mean-related coefficients
(influence(fit_pig_red)$coefficients.mean[94,] - 
   coefficients(fit_pig_red, parameters = "mean"))/
  coefficients(fit_pig_red, parameters = "mean")

# Relative change for precision-related coefficients
(influence(fit_pig_red)$coefficients.precision[94,] - 
    coefficients(fit_pig_red, parameters = "precision"))/
  coefficients(fit_pig_red, parameters = "precision")

## -----------------------------------------------------------------------------
local_influence_autoplot(fit_nb_red)

## -----------------------------------------------------------------------------
local_influence_autoplot(fit_pig_red)

## ----warning=FALSE, message=FALSE---------------------------------------------
inf_nb_tbl <- tidy_local_influence(fit_nb_red) %>% mutate(.index = row_number()) %>% 
  pivot_longer(!.index, names_to = "perturbation", values_to = "curvature") 
bench_nb_tbl <- local_influence_benchmarks(fit_nb_red) %>% 
  pivot_longer(everything(), names_to = "perturbation", values_to = "benchmark")

inf_nb_bench_tbl <- left_join(inf_nb_tbl, bench_nb_tbl, by = "perturbation") %>% 
  mutate(influential = curvature > benchmark) %>% filter(influential == TRUE) %>%
  select(-influential, -benchmark, -curvature)

data_nb_tbl <- augment(fit_nb_red) %>% mutate(.index = row_number()) %>% 
  select(.index, daysabs, gender, math, prog)

influential_nb <- left_join(inf_nb_bench_tbl, data_nb_tbl, by = ".index")

influential_nb

## -----------------------------------------------------------------------------
influential_nb %>% select(.index) %>% unique() %>% count()

## ----warning=FALSE, message=FALSE---------------------------------------------
inf_pig_tbl <- tidy_local_influence(fit_pig_red) %>% mutate(.index = row_number()) %>% 
  pivot_longer(!.index, names_to = "perturbation", values_to = "curvature") 
bench_pig_tbl <- local_influence_benchmarks(fit_pig_red) %>% 
  pivot_longer(everything(), names_to = "perturbation", values_to = "benchmark")

inf_pig_bench_tbl <- left_join(inf_pig_tbl, bench_pig_tbl, by = "perturbation") %>% 
  mutate(influential = curvature > benchmark) %>% filter(influential == TRUE) %>%
  select(-influential, -benchmark, -curvature)

data_pig_tbl <- augment(fit_pig_red) %>% mutate(.index = row_number()) %>% 
  select(.index, daysabs, gender, math, prog)

influential_pig <- left_join(inf_pig_bench_tbl, data_pig_tbl, by = ".index")

influential_pig

## -----------------------------------------------------------------------------
influential_pig %>% select(.index) %>% unique() %>% count()

## -----------------------------------------------------------------------------
influential_nb %>% filter(.index == 94)

influential_pig %>% filter(.index == 94)

## -----------------------------------------------------------------------------
ind_nb <- influential_nb %>% select(.index) %>% unique()

ind_pig <- influential_pig %>% select(.index) %>% unique()

ind_common <- intersect(ind_nb, ind_pig)

influential_nb %>% filter(.index %in% ind_common$.index) %>% select(-perturbation) %>% unique()

## -----------------------------------------------------------------------------
ind_nb_pig <- setdiff(ind_nb, ind_pig)

influential_nb %>% filter(.index %in% ind_nb_pig$.index) %>% select(-perturbation) %>% unique()

## -----------------------------------------------------------------------------
ind_pig_nb <- setdiff(ind_pig, ind_nb)

influential_pig %>% filter(.index %in% ind_pig_nb$.index) %>% select(-perturbation) %>% unique()

