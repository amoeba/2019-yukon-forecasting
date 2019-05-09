#' april_forecast.R
#'
#' 15/25/50 %tiles of timing using just 2019 AMATC

library(dplyr)
library(ggplot2)
library(readr)

# Data loading/cleanup
######################

yuk <- read_csv("data/yukon.csv")


# Plotting
##########

ggplot(yuk, aes(amatc, mdj)) +
  geom_point(shape = 1) +
  geom_vline(xintercept = as.numeric(yuk[which(yuk$year == 2019),"amatc"])) +
  labs(x = expression("April Average Air Temp.,"*~degree*"C"), y = "Median Run Timing (Day of June)")

ggsave("april_forecast/mdj_against_amatc.png", width = 4, height = 4)

# Modeling
##########

model <- lm(mdj ~ amatc, data = subset(yuk, year < 2019))
summary(model)

predict(model, newdata =  yuk[yuk$year == 2019,])

result <- data.frame()

for (y in (max(yuk$year) - 15):(max(yuk$year) - 1)) {
  model <- lm(mdj ~ amatc, data = subset(yuk, year < y))
  model_pred <- as.numeric(floor(predict(model, newdata =  yuk[yuk$year == y,"amatc"])))
  
  result <- rbind(result,
                  data.frame(year = y,
                             obs = as.numeric(yuk[yuk$year == y,"mdj"]),
                             pred = model_pred))
}


mean(abs(result$obs - result$pred))
sd(abs(result$obs - result$pred))
max(abs(result$obs - result$pred))
mean(result$obs - result$pred)

ggplot(yuk, aes(amatc, mdj)) + 
  geom_point(shape = 1) +
  stat_smooth(method = "lm", color = "black") + 
  labs(x = "Median Run Timing (June)", y = expression("AMATC,"*~degree*"C"))

# Predictions

model_fifdj <- lm(fifdj ~ amatc, data = subset(yuk, year < 2019))
predict(model_fifdj, newdata =  yuk[yuk$year == 2019,])

model_qdj <- lm(qdj ~ amatc, data = subset(yuk, year < 2019))
predict(model_qdj, newdata =  yuk[yuk$year == 2019,])

model_mdj <- lm(mdj ~ amatc, data = subset(yuk, year < 2019))
predict(model_mdj, newdata =  yuk[yuk$year == 2019,])

