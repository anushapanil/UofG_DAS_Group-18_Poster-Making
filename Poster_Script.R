library(tidyverse)
library(GGally)
library(olsrr)
library(readr)
library(corrplot)
library(data.table)
library(ggfortify)
library(dplyr)

body_fat <- read_csv("Body Fat Prediction Dataset.csv", 
                     col_types = cols(Density = col_skip()))

body_fat <- as_tibble(body_fat)

str(body_fat)
skimr::skim(body_fat)

#Multicollinearity
ggpairs(body_fat[-1])

#Model
model<- lm(BodyFat ~ Age+Weight+Height+Neck+Chest+Abdomen+Hip+Thigh+Knee+Ankle+Biceps+Forearm+Wrist, data = body_fat)
summary(model)

#Model Selection
model_selection <- ols_step_both_aic(model, details=T)

#Model after performing Step-wise Model Selection using AIC Values
model_after_aic <- lm(BodyFat ~ Age+Weight+Neck+Abdomen+Thigh+Forearm+Wrist, data = body_fat)
summary(model_after_aic)

#Checking Model Assumptions
autoplot(model_after_aic)





