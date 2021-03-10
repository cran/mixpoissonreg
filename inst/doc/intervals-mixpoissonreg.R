## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----warning=FALSE------------------------------------------------------------
library(mixpoissonreg)

fit <- mixpoissonreg(daysabs ~ gender + math + prog, 
                        data = Attendance, em_controls = list(maxit=1))

pred_fit <- predict(fit, type = "link", se.fit = TRUE)

head(pred_fit$fit)

head(pred_fit$se.fit)

## -----------------------------------------------------------------------------
predict(fit, newdata = data.frame(gender = c("male", "female"), math = c(34, 85), 
                                  prog = factor(c("General", "Academic"), 
                                  levels = c("General", "Academic", "Vocational"))),
        type = "link", se.fit = TRUE)

## ----message=FALSE------------------------------------------------------------
library(dplyr)

augment(fit, type.predict = "link", se_fit = TRUE) %>% dplyr::select(.fitted, .se.fit)

## ----warning=FALSE------------------------------------------------------------
fit_math <- mixpoissonreg(daysabs ~ math, 
                        data = Attendance, 
                        em_controls = list(maxit=1))

head(predict(fit_math, interval = "confidence"))

head(predict(fit_math, interval = "confidence", level = 0.99))

## -----------------------------------------------------------------------------
data("Attendance", package = "mixpoissonreg")

new_math <- seq(0, 100, by=0.25)
conf_fit <- predict(fit_math, newdata = data.frame(math = new_math), interval = "confidence")


graphics::plot(Attendance$math, Attendance$daysabs, xlab = "Math score", ylab = "Days abscent")
curve(exp(fit_math$coefficients$mean[1] + fit_math$coefficients$mean[2]*x), add = TRUE)

graphics::lines(new_math, conf_fit[, 2], col = grDevices::rgb(0.7, 0.7, 0.7))
graphics::lines(new_math, conf_fit[, 3], col = grDevices::rgb(0.7, 0.7, 0.7))
graphics::polygon(c(new_math, rev(new_math)), c(conf_fit[, 2], rev(conf_fit[, 3])), 
                  col = grDevices::rgb(0.7, 0.7, 0.7, 0.7), border = NA)

## -----------------------------------------------------------------------------
augment(fit_math, level = 0.99) %>% select(math, daysabs, .fittedlwrconf, .fitteduprconf)

## ----message=FALSE------------------------------------------------------------
library(ggplot2)

fit_data <- augment(fit_math) %>% dplyr::select(math, daysabs) %>% 
  dplyr::rename("Math Score" = math, "Days Abscent" = daysabs)

new_math <- seq(0, 100, by=0.25)

fit_int <- augment(fit_math, newdata = data.frame(math = new_math), level = 0.99) %>% 
  dplyr::rename("Math Score" = math) %>% mutate("Days Abscent" = 0)

ggplot(fit_data, aes(x = `Math Score`, y = `Days Abscent`)) + geom_point() +
  geom_function(fun = function(x){exp(fit_math$coefficients$mean[1] + 
                                        fit_math$coefficients$mean[2]*x)}, colour = "blue") +
  geom_ribbon(data = fit_int, aes(ymin = .fittedlwrconf, ymax = .fitteduprconf), 
              fill = "grey70", alpha = 0.7)

## ----warning=FALSE------------------------------------------------------------
fit_math <- mixpoissonreg(daysabs ~ math, 
                        data = Attendance,
                        em_controls = list(maxit=1))

head(predict(fit_math, interval = "prediction", nsim_pred = 50, nsim_pred_y = 50))

head(predict(fit_math, interval = "prediction", level = 0.99, nsim_pred = 50, nsim_pred_y = 50))

## ----warning=FALSE------------------------------------------------------------
new_math <- seq(0, 100, by=0.25)
pred_fit <- predict(fit_math, newdata = data.frame(math = new_math), interval = "prediction",
                    nsim_pred = 50, nsim_pred_y = 50)


plot(Attendance$math, Attendance$daysabs, xlab = "Math score", ylab = "Days abscent")
curve(exp(fit_math$coefficients$mean[1] + fit_math$coefficients$mean[2]*x), add = TRUE)

lines(new_math, pred_fit[, 2], col = grDevices::rgb(0.7, 0.7, 0.7))
lines(new_math, pred_fit[, 3], col = grDevices::rgb(0.7, 0.7, 0.7))
polygon(c(new_math, rev(new_math)), c(pred_fit[, 2], rev(pred_fit[, 3])), 
                  col = grDevices::rgb(0.7, 0.7, 0.7, 0.7), border = NA)

## ----warning=FALSE------------------------------------------------------------
augment(fit_math, pred_int = TRUE, level = 0.99,
        nsim_pred = 10, nsim_pred_y = 10) %>% select(math, daysabs, .fittedlwrpred, .fitteduprpred)

## ----message=FALSE, warning=FALSE---------------------------------------------
fit_data <- augment(fit_math) %>%
  dplyr::select(math, daysabs) %>% 
  dplyr::rename("Math Score" = math, "Days Abscent" = daysabs)

new_math <- seq(0, 100, by=0.25)

fit_pred <- augment(fit_math, newdata = data.frame(math = new_math), 
                   pred_int = TRUE, nsim_pred = 10, nsim_pred_y = 10) %>% 
  dplyr::rename("Math Score" = math) %>% mutate("Days Abscent" = 0)

ggplot(fit_data, aes(x = `Math Score`, y = `Days Abscent`)) + geom_point() +
  geom_function(fun = function(x){exp(fit_math$coefficients$mean[1] + 
                                        fit_math$coefficients$mean[2]*x)}, colour = "blue") +
  geom_ribbon(data = fit_pred, aes(ymin = .fittedlwrpred, ymax = .fitteduprpred), 
              fill = "grey70", alpha = 0.7)

