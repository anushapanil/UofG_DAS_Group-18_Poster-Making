
library(dplyr)
library(ggplot2)
library(janitor)
library(moderndive)
library(infer)
library(broom)
library(kableExtra)
library(GGally)
library(skimr)
library(knitr)
library(gridExtra)
library(readr)
library(kableExtra)
library(olsrr)
library(tidyverse)


bodyfat <- read_csv("Body Fat Prediction Dataset.csv")

bodyfat$Index <- 1:nrow(bodyfat)

bodyfat <- bodyfat %>%
  relocate(Index)

bodyfat <- bodyfat %>%
  mutate(AHR = Abdomen / Hip)




head(bodyfat)


head(bodyfat)
str(bodyfat)

colnames(bodyfat)

# It looks like there is 1 very heavy person with not too high bodyfat -
# probably some kind of weightlifter as this person is the max for chest/bicep too.
# Also 1 very short person

ggplot(bodyfat, aes(x = AHR, y = BodyFat)) +
  geom_point() +
  geom_smooth(method = "lm", se = F)

bodyfat %>%
  arrange((Height))

which.max(bodyfat$Weight)

bodyfat_clean <- bodyfat[-c(39),]

ggplot(bodyfat_clean, aes(x = Weight, y = BodyFat)) +
  geom_point() +
  geom_smooth(method = "lm", se = F)

agelm <- lm(data = bodyfat_clean, BodyFat ~ Age)

summary(agelm)

summary(lm(data = bodyfat_clean, BodyFat ~ Weight))

fullvar <- colnames(bodyfat_clean[,-c(1,2)]) %>%
  str_flatten(collapse = "+")

# Model with all explanatory variables
fullmodel <- lm(data = bodyfat_clean, BodyFat ~ Age+Weight+Height+Neck+Chest+Abdomen+Hip+Thigh+Knee+Ankle+Biceps+Forearm+Wrist+AHR)

# Adjusted R^2 is 0.7368 so pretty good but a lot of insignificant terms are used
summary(fullmodel)

# MODEL SELECTION


refinedmodel <- ols_step_both_aic(fullmodel, details = T)
refinedmodel$model

# This gives best model with 3 vars as Weight+Abdomen+Wrist
# Best with 2 as Weight+Abdomen
refmodel2 <- ols_step_all_possible(fullmodel)
refmodel2



# Backwards from full model
refmodel3 <- ols_step_backward_aic(fullmodel)
refmodel3$model
refmodel3$arsq
refmodel3$aics
refmodel3$steps


AHRmodel <- lm(data = bodyfat_clean,BodyFat~AHR)
summary(AHRmodel)

abdomenmodel <- lm(data=bodyfat_clean, BodyFat~Abdomen)
summary(abdomenmodel)

WAHRmodel <- lm(data=bodyfat_clean, BodyFat~Weight+Abdomen)
summary(WAHRmodel)


refmodel4 <- ols_step_forward_aic(fullmodel)
refmodel4$model
refmodel4$arsq
refmodel4$aics
refmodel4$steps

twomodel <- lm(data=bodyfat_clean, BodyFat~Weight+Abdomen)
threemodel <- lm(data=bodyfat_clean, BodyFat~Weight+Abdomen+Wrist)
fourmodel <- lm(data=bodyfat_clean, BodyFat~Weight+Abdomen+Wrist+Biceps)

summary(twomodel)
# Model with 3 variables gains 1% on the model with 2 variables

summary(twomodel)$adj.r.squared
summary(threemodel)$adj.r.squared

summary(fourmodel)$adj.r.squared
summary(fullmodel)$adj.r.squared

bodyfat_39 <- bodyfat[-39,]
bodyfat_39

