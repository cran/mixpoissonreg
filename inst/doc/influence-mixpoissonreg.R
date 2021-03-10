## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----warning=FALSE------------------------------------------------------------
library(mixpoissonreg)

fit <- mixpoissonreg(daysabs ~ gender + math + prog | gender + math + prog, 
                        data = Attendance, em_controls = list(maxit=1))

head(hatvalues(fit))

## -----------------------------------------------------------------------------
head(hatvalues(fit, parameters = "precision"))

## -----------------------------------------------------------------------------
head(cooks.distance(fit))

## -----------------------------------------------------------------------------
head(cooks.distance(fit, hat = "precision"))

## -----------------------------------------------------------------------------
head(cooks.distance(fit, type = "GCD"))

## -----------------------------------------------------------------------------
head(cooks.distance(fit, type = "GCDmean"))

## -----------------------------------------------------------------------------
head(cooks.distance(fit, type = "GCDprecision"))

## -----------------------------------------------------------------------------
head(cooks.distance(fit, type = "QD"))

## -----------------------------------------------------------------------------
head(cooks.distance(fit, type = "LD"))

## -----------------------------------------------------------------------------
influence_fit <- influence(fit)

head(influence_fit$coefficients.mean)

## -----------------------------------------------------------------------------
plot(fit, which = c(3,4,5))

## -----------------------------------------------------------------------------
autoplot(fit, which = c(3,4,5))

## -----------------------------------------------------------------------------
plot(fit, which = c(3,4,5), id.n = 5)

## -----------------------------------------------------------------------------
autoplot(fit, which = c(3,4,5), label.n = 5)

## -----------------------------------------------------------------------------
qd_fit <- cooks.distance(fit, type = "QD")

# Get the extreme points:
extreme_points <- as.vector(Rfast::nth(abs(qd_fit), k = 5,
                                    num.of.nths = 5,
                                    index.return = TRUE, descending = TRUE))
idx_y <- qd_fit[extreme_points]

ylim <- range(qd_fit, na.rm = TRUE)

ylim <- extendrange(r = ylim, f = 0.15)

plot(qd_fit, xlab = "Obs. number", ylab = "Q-displacement", ylim = ylim, type = "h")
text(extreme_points, idx_y, labels = extreme_points, pos = 3, offset = 0.2)

## -----------------------------------------------------------------------------
ld_fit <- cooks.distance(fit, type = "LD")

# Get 5 most extreme points:
extreme_points <- as.vector(Rfast::nth(abs(ld_fit), k = 5,
                                    num.of.nths = 5,
                                    index.return = TRUE, descending = TRUE))
idx_y <- ld_fit[extreme_points]

ylim <- range(ld_fit, na.rm = TRUE)

ylim <- extendrange(r = ylim, f = 0.15)

plot(ld_fit, xlab = "Obs. number", ylab = "Likelihood displacement", ylim = ylim, type = "h")
text(extreme_points, idx_y, labels = extreme_points, pos = 3, offset = 0.2)

## ----message=FALSE------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(ggrepel)

qd_fit <- cooks.distance(fit, type = "QD")

qd_tbl <- tibble("Q-displacement" = qd_fit, "Obs. number" = 1:length(qd_fit))

# Get 5 most extreme points
qd.extreme <- arrange(qd_tbl, desc(`Q-displacement`))
qd.extreme <- head(qd.extreme, 5)

ggplot(qd_tbl, aes(x = `Obs. number`, y = `Q-displacement`)) + 
  geom_linerange(aes(ymin = 0, ymax = `Q-displacement`)) +
  geom_text_repel(data = qd.extreme, aes(label = `Obs. number`))


## ----message=FALSE------------------------------------------------------------
ld_fit <- cooks.distance(fit, type = "LD")

ld_tbl <- tibble("Likelihood displacement" = ld_fit, "Obs. number" = 1:length(ld_fit))

# Get 5 most extreme points
ld.extreme <- arrange(ld_tbl, desc(`Likelihood displacement`))
ld.extreme <- head(ld.extreme, 5)

ggplot(ld_tbl, aes(x = `Obs. number`, y = `Likelihood displacement`)) + 
  geom_linerange(aes(ymin = 0, ymax = `Likelihood displacement`)) +
  geom_text_repel(data = ld.extreme, aes(label = `Obs. number`))


## ----warning = FALSE----------------------------------------------------------
fit <- mixpoissonreg(daysabs ~ gender + math + prog | gender + math + prog, 
                        data = Attendance, em_controls = list(maxit=1))

loc_inf_fit <- local_influence(fit)

ls(loc_inf_fit)

head(loc_inf_fit$case_weights)

attr(loc_inf_fit$case_weights, "benchmark")

## -----------------------------------------------------------------------------
loc_inf_normal_fit <- local_influence(fit, curvature = "normal")

ls(loc_inf_normal_fit)

head(loc_inf_normal_fit$case_weights)

attr(loc_inf_normal_fit$case_weights, "benchmark")

## -----------------------------------------------------------------------------
# Conformal normal curvature

loc_inf_fit_larg_curv <- local_influence(fit, direction = "max.eigen")

ls(loc_inf_fit_larg_curv)

head(loc_inf_fit_larg_curv$case_weights)

attr(loc_inf_fit_larg_curv$case_weights, "benchmark")

# Normal curvature

loc_inf_normal_fit_larg_curv <- local_influence(fit, curvature = "normal", 
                                                direction = "max.eigen")

ls(loc_inf_normal_fit_larg_curv)

head(loc_inf_normal_fit_larg_curv$case_weights)

attr(loc_inf_normal_fit_larg_curv$case_weights, "benchmark")


## -----------------------------------------------------------------------------
loc_inf_fit <- local_influence(fit)

ls(loc_inf_fit)

## ----warning = FALSE----------------------------------------------------------
fit2 <- mixpoissonreg(daysabs ~ gender + math + prog, 
                        data = Attendance, 
                      em_controls = list(maxit=1))

loc_inf_fit2 <- local_influence(fit2)

ls(loc_inf_fit2)

head(loc_inf_fit2$case_weights)

head(loc_inf_fit2$precision_explanatory)

head(loc_inf_fit2$simultaneous_explanatory)

## -----------------------------------------------------------------------------
loc_inf_1 <- local_influence(fit, perturbation = c("case_weights", "hidden_variable"))

ls(loc_inf_1)

head(loc_inf_1$case_weights)

loc_inf_2 <- local_influence(fit, perturbation = c("case_weights", "hidden_variable"),
                             curvature = "normal",
                             direction = "max.eigen")

ls(loc_inf_2)

head(loc_inf_2$case_weights)


## -----------------------------------------------------------------------------
loc_inf_fit_mean <- local_influence(fit, parameters = "mean")

head(loc_inf_fit_mean$case_weights)

## -----------------------------------------------------------------------------
loc_inf_fit_precision <- local_influence(fit, parameters = "precision")

head(loc_inf_fit_precision$case_weights)

## ----warning = FALSE----------------------------------------------------------
    set.seed(1234)
    
    x1 <- rexp(200, rate = 2)
    x2 <- rnorm(200)
    x3 <- factor(as.integer(2*runif(200) + 1))
    x4 <- as.integer(10*runif(200))
    
    y <- stats::rnbinom(200, mu = exp(1-x1-x2-(x3==2)+0.1*x4), 
                       size = exp(1+2*x1+x2))
    
    fit_example <- mixpoissonreg(y ~ x1 + x2 + x3 + x4 | x1 + x2,
                                 em_controls = list(maxit=1))
    
    summary(fit_example)

## -----------------------------------------------------------------------------
loc_inf_x1 <- local_influence(fit_example, mean.covariates = "x1")

head(loc_inf_x1$mean_explanatory)

## -----------------------------------------------------------------------------
loc_inf_x1_x2 <- local_influence(fit_example, mean.covariates = c("x1", "x2"))

head(loc_inf_x1_x2$mean_explanatory)

## -----------------------------------------------------------------------------
attr(loc_inf_x1$mean_explanatory, "covariates")
attr(loc_inf_x1$simultaneous_explanatory, "covariates")

attr(loc_inf_x1_x2$mean_explanatory, "covariates")
attr(loc_inf_x1_x2$simultaneous_explanatory, "covariates")

## -----------------------------------------------------------------------------
loc_inf_prec_x1 <- local_influence(fit_example, precision.covariates = "x1")

head(loc_inf_prec_x1$precision_explanatory)

## -----------------------------------------------------------------------------
loc_inf_prec_x1_x2 <- local_influence(fit_example, precision.covariates = c("x1", "x2"))

head(loc_inf_prec_x1_x2$precision_explanatory)

## -----------------------------------------------------------------------------
attr(loc_inf_prec_x1$precision_explanatory, "covariates")
attr(loc_inf_prec_x1$simultaneous_explanatory, "covariates")

attr(loc_inf_prec_x1_x2$precision_explanatory, "covariates")
attr(loc_inf_prec_x1_x2$simultaneous_explanatory, "covariates")

## ----warning = FALSE----------------------------------------------------------
fit <- mixpoissonreg(daysabs ~ gender + math + prog | gender + math + prog, 
                        data = Attendance, em_controls = list(maxit=1))

# Notice that since gender and prog are factors,
# they are not considered in the computation of the 
# explanatory variables perturbation schemes

local_influence_plot(fit)

## -----------------------------------------------------------------------------
local_influence_autoplot(fit)

## -----------------------------------------------------------------------------
local_influence_plot(fit, curvature = "normal")

## -----------------------------------------------------------------------------
local_influence_autoplot(fit, curvature = "normal")

## -----------------------------------------------------------------------------
local_influence_plot(fit, direction = "max.eigen")

## -----------------------------------------------------------------------------
local_influence_autoplot(fit, direction = "max.eigen")

## -----------------------------------------------------------------------------
local_influence_plot(fit, direction = "max.eigen", curvature = "normal",
                     n.influential = 3)

## -----------------------------------------------------------------------------
local_influence_autoplot(fit, direction = "max.eigen", curvature = "normal",
                     n.influential = 3)

## -----------------------------------------------------------------------------
local_influence_plot(fit, which = c(1,2))

## -----------------------------------------------------------------------------
local_influence_autoplot(fit, which = c(1,2))

## -----------------------------------------------------------------------------
local_influence_plot(fit, draw.benchmark = TRUE)

## -----------------------------------------------------------------------------
local_influence_autoplot(fit, draw.benchmark = TRUE)

## ----warning = FALSE----------------------------------------------------------
    set.seed(1234)
    
    x1 <- rexp(200, rate = 2)
    x2 <- rnorm(200)
    x3 <- factor(as.integer(2*runif(200) + 1))
    x4 <- as.integer(10*runif(200))
    
    y <- stats::rnbinom(200, mu = exp(1-x1-x2-(x3==2)+0.1*x4), 
                       size = exp(1+2*x1+x2))
    
    fit_example <- mixpoissonreg(y ~ x1 + x2 + x3 + x4 | x1 + x2, 
                                 em_controls = list(maxit=1))

## -----------------------------------------------------------------------------
local_influence_plot(fit_example, which = c(3,4,5), 
                     mean.covariates = "x1", precision.covariates = "x1")

## -----------------------------------------------------------------------------
local_influence_autoplot(fit_example, which = c(3,4,5), 
                         mean.covariates = "x1", precision.covariates = "x1")

