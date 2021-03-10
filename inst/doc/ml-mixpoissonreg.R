## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----warning=FALSE------------------------------------------------------------
library(mixpoissonreg)

fit_ml <- mixpoissonreg(daysabs ~ gender + math + prog, method = "ML", 
                        data = Attendance, optim_controls = list(maxit=1))

summary(fit_ml)

## ----warning=FALSE------------------------------------------------------------
fit_ml2 <- mixpoissonregML(daysabs ~ gender + math + prog,
                        data = Attendance, optim_controls = list(maxit=1))

summary(fit_ml2)

## -----------------------------------------------------------------------------
identical(coef(fit_ml), coef(fit_ml2))

## ----warning = FALSE----------------------------------------------------------
fit_ml_prec <- mixpoissonregML(daysabs ~ gender + math + prog | prog,
                        model = "PIG", data = Attendance,
                        optim_controls = list(maxit=1))

autoplot(fit_ml_prec)

local_influence_autoplot(fit_ml_prec)

lmtest::lrtest(fit_ml_prec)

fit_ml_reduced <- mixpoissonregML(daysabs ~ gender + math + prog,
                        model = "PIG", data = Attendance,
                        optim_controls = list(maxit=1))

lmtest::lrtest(fit_ml_prec, fit_ml_reduced)

## ----warning = FALSE----------------------------------------------------------
fit_ml_env <- mixpoissonregML(daysabs ~ gender + math + prog | prog,
                        model = "PIG", envelope = 10, data = Attendance,
                        optim_controls = list(maxit=1))

summary(fit_ml_env)

plot(fit_ml_env, which = 2)

autoplot(fit_ml_env, which = 2)

## -----------------------------------------------------------------------------

data("Attendance", package = "mixpoissonreg")

X = cbind(1, Attendance$math)
y = Attendance$daysabs

mixpoissonregML.fit(X, y)$coefficients

W = X

mixpoissonregML.fit(X, y, W)$coefficients


