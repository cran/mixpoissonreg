## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----warning=FALSE------------------------------------------------------------
library(mixpoissonreg)

fit <- mixpoissonreg(daysabs ~ gender + math + prog | prog, 
                       model = "PIG", data = Attendance,
                     em_controls = list(maxit=1))

augment(fit)

## -----------------------------------------------------------------------------
augment(fit, type.predict = "link")

## -----------------------------------------------------------------------------
augment(fit, type.predict = "link", se_fit = TRUE)

## -----------------------------------------------------------------------------
augment(fit, type.residuals = "score")

## -----------------------------------------------------------------------------
augment(fit, newdata = data.frame(gender = c("male", "female"), math = c(34, 85), 
                                  prog = factor(c("General", "Academic"), 
                                  levels = c("General", "Academic", "Vocational"))))

## -----------------------------------------------------------------------------
augment(fit, newdata = data.frame(gender = c("male", "female"), math = c(34, 85), 
                                  prog = factor(c("General", "Academic"), 
                                  levels = c("General", "Academic", "Vocational"))),
        pred_int = TRUE, level = 0.99,
        nsim_pred = 50, nsim_pred_y  =50,
        conf_int = FALSE)

## ----message=FALSE------------------------------------------------------------
library(dplyr)

augment(fit) %>% mutate(.index = row_number()) %>% arrange(desc(.cooksd)) %>% 
  select(.index, daysabs, gender, math,prog,.cooksd)

## -----------------------------------------------------------------------------
augment(fit) %>% mutate(.index = row_number()) %>% arrange(desc(.gencooksd)) %>% 
    select(.index, daysabs, gender, math,prog,.gencooksd)

## ----message=FALSE, warning=FALSE---------------------------------------------
library(ggplot2)

fit_math <- mixpoissonreg(daysabs ~ math, 
                        data = Attendance,
                        em_controls = list(maxit=1))

fit_data <- augment(fit_math) %>% dplyr::select(math, daysabs) %>% 
  dplyr::rename("Math Score" = math, "Days Abscent" = daysabs)

new_math <- seq(0, 100, by=0.25)

fit_int <- augment(fit_math, newdata = data.frame(math = new_math)) %>% 
  dplyr::rename("Math Score" = math) %>% mutate("Days Abscent" = 0)

ggplot(fit_data, aes(x = `Math Score`, y = `Days Abscent`)) + geom_point() +
  geom_function(fun = function(x){exp(fit_math$coefficients$mean[1] + 
                                        fit_math$coefficients$mean[2]*x)}, colour = "blue") +
  geom_ribbon(data = fit_int, aes(ymin = .fittedlwrconf, ymax = .fitteduprconf), 
              fill = "grey70", alpha = 0.7)

## ----message=FALSE, warning=FALSE---------------------------------------------
fit_pred <- augment(fit_math, newdata = data.frame(math = new_math), 
                   pred_int = TRUE, nsim_pred = 50, nsim_pred_y = 50) %>% 
  dplyr::rename("Math Score" = math) %>% mutate("Days Abscent" = 0)

ggplot(fit_data, aes(x = `Math Score`, y = `Days Abscent`)) + geom_point() +
  geom_function(fun = function(x){exp(fit_math$coefficients$mean[1] + 
                                        fit_math$coefficients$mean[2]*x)}, colour = "blue") +
  geom_ribbon(data = fit_pred, aes(ymin = .fittedlwrpred, ymax = .fitteduprpred), 
              fill = "grey70", alpha = 0.7)

## -----------------------------------------------------------------------------
glance(fit)

## -----------------------------------------------------------------------------
tidy(fit)

## -----------------------------------------------------------------------------
tidy(fit, conf.int = TRUE)

## -----------------------------------------------------------------------------
tidy(fit, conf.int = TRUE) %>%
  ggplot(aes(x = term, y = estimate)) + geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  theme(axis.text.x = element_text(angle=45)) + 
  scale_x_discrete(name = "Coefficient") + 
  scale_y_continuous(name = "Estimate") +
  facet_wrap(~ component)

## ----message=FALSE------------------------------------------------------------
library(gridExtra)

pmean <- tidy(fit, conf.int = TRUE) %>% filter(component == "mean") %>%
  ggplot(aes(x = term, y = estimate)) + geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  theme(axis.text.x = element_text(angle=45)) + 
  scale_x_discrete(name = "Coefficient") + 
  scale_y_continuous(name = "Estimate") + 
  facet_wrap(~ component)

pprecision <- tidy(fit, conf.int = TRUE) %>% filter(component == "precision") %>%
  ggplot(aes(x = term, y = estimate)) + geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  theme(axis.text.x = element_text(angle=45)) + 
  scale_x_discrete(name = "Coefficient") + 
  scale_y_continuous(name = element_blank()) + 
  facet_wrap(~ component)

grid.arrange(pmean, pprecision, ncol=2)

## -----------------------------------------------------------------------------
tidy_local_influence(fit)

## -----------------------------------------------------------------------------
tidy_local_influence(fit, curvature = "normal",
                     direction = "max.eigen")

## -----------------------------------------------------------------------------
local_influence_benchmarks(fit)

## ----warning=FALSE, message=FALSE---------------------------------------------
library(tidyr)

inf_tbl <- tidy_local_influence(fit) %>% mutate(.index = row_number()) %>% 
  pivot_longer(!.index, names_to = "perturbation", values_to = "curvature") 
bench_tbl <- local_influence_benchmarks(fit) %>% 
  pivot_longer(everything(), names_to = "perturbation", values_to = "benchmark")

inf_bench_tbl <- left_join(inf_tbl, bench_tbl, by = "perturbation") %>% 
  mutate(influential = curvature > benchmark) %>% filter(influential == TRUE) %>%
  select(-influential, -benchmark, -curvature)

data_tbl <- augment(fit) %>% mutate(.index = row_number()) %>% 
  select(.index, daysabs, gender, math, prog)

influential <- left_join(inf_bench_tbl, data_tbl, by = ".index")

influential

## -----------------------------------------------------------------------------
influential %>% select(.index) %>% unique() %>% count()

## -----------------------------------------------------------------------------
autoplot(fit)

## ----warning=FALSE------------------------------------------------------------
fit_env <- mixpoissonregML(daysabs ~ gender + math + prog | prog, 
                       model = "PIG", data = Attendance,
                       envelope = 10, optim_controls = list(maxit=1))

autoplot(fit_env)

## ----warning=FALSE------------------------------------------------------------
autoplot(fit, which = c(3,4))

## -----------------------------------------------------------------------------
local_influence_autoplot(fit)

## -----------------------------------------------------------------------------
local_influence_autoplot(fit, curvature = "normal")

## -----------------------------------------------------------------------------
local_influence_autoplot(fit, which = c(1,2))

## -----------------------------------------------------------------------------
local_influence_autoplot(fit, draw.benchmark = TRUE)

