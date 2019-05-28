#' may_forecast.R
#'
#' 15/25/50 %tiles of timing using

library(dplyr)
library(ggplot2)
theme_set(theme_classic())
library(readr)
library(patchwork) # https://github.com/thomasp85/patchwork

# Data loading/cleanup
######################
yuk <- read_csv("data/yukon.csv")
forecast_year <- 2019

# Plots
p_msstc <- ggplot(yuk, aes(msstc, mdj)) +
  geom_point(shape=1) +
  geom_vline(xintercept = yuk[which(yuk$year == forecast_year),"msstc"][[1]]) +
  labs(x = expression("MSSTC,"*~degree*"C"), y = "Median Run Timing (June)")

ggsave("may_forecast/figures/mdj_against_msstc.png", width = 4, height = 4)

p_pice <- ggplot(yuk, aes(pice, mdj)) +
  geom_point(shape=1) +
  geom_vline(xintercept = yuk[which(yuk$year == forecast_year),"pice"][[1]]) +
  scale_x_continuous(limits = c(0, 1.0)) +
  labs(x = "PICE", y = "Median Run Timing (June)")

ggsave("may_forecast/figures/mdj_against_pice.png", width = 4, height = 4)

p_amatc <- ggplot(yuk, aes(amatc, mdj)) +
  geom_point(shape=1) +
  geom_vline(xintercept = yuk[which(yuk$year == forecast_year),"msstc"][[1]]) +
  labs(x = expression("AMATC,"*~degree*"C"), y = "Median Run Timing (June)")

ggsave("may_forecast/figures/mdj_against_amatc.png", width = 4, height = 4)

# Three panel plot
p_amatc + p_msstc + p_pice

# Models

# 15%
model_fifdj <- lm(fifdj ~ amatc + msstc + pice, data = subset(yuk, year < forecast_year))
summary(model_fifdj)
prediction_fifdj <- floor(predict(model_fifdj, newdata = yuk[yuk$year == forecast_year,]))
prediction_fifdj

# 25%
model_qdj <- lm(qdj ~ amatc + msstc + pice, data = subset(yuk, year < forecast_year))
summary(model_qdj)
prediction_qdj <- floor(predict(model_qdj, newdata = yuk[yuk$year == forecast_year,]))
prediction_qdj

# 50%
model_mdj <- lm(mdj ~ amatc + msstc + pice, data = subset(yuk, year < forecast_year))
summary(model_mdj)
prediction_mdj <- floor(predict(model_mdj, newdata = yuk[yuk$year == forecast_year,]))
prediction_mdj

predictions <- data.frame(percentile = c("fifdj", "qdj", "mdj"),
                          prediction = as.integer(c(prediction_fifdj,
                                         prediction_qdj,
                                         prediction_mdj)))
write_csv(predictions, path = "may_forecast/predictions.csv")
