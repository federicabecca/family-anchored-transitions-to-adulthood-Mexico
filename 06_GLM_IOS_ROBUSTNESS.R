############
## COMPARING CONTINOUS  AND FACTOR IOS FOR MODELS

install.packages("MASS")
install.packages("caret")

library(dplyr)
library(MASS)
library(caret)

load('modeldata.RData')
m1w <- m1 |> filter(Sex ==  "Female")
m1m <- m1 |> filter(Sex ==  "Male")


m1wP <- m1 |> filter(cat_ft ==  "P")
m1wPC<- m1 |> filter(cat_ft ==  "PC")
m1wC <- m1 |> filter(cat_ft ==  "C")

# Logistic regression with ios as a continuous variable
model_continuous <- glm(Complexity ~ Cohort + AgeC + urban + Union_duration + 
                          Marital_Status + Education + ios, data = m1wP, family = binomial)
summary(model_continuous)

model_tercile <- glm(Complexity ~ Cohort + AgeC + urban + Union_duration + 
                         Marital_Status + Education + iosterciles_f, data = m1wP, family = binomial)
summary(model_tercile)

model_quartile <- glm(Complexity ~ Cohort + AgeC + urban + Union_duration + 
                         Marital_Status + Education + ioscuartiles_f, data = m1wP, family = binomial)
summary(model_quartile)


AIC_continuous <- AIC(model_continuous)
BIC_continuous <- BIC(model_continuous)

AIC_terc <- AIC(model_tercile)
BIC_terc <- BIC(model_tercile)

# AIC and BIC for factor model
AIC_quart <- AIC(model_quartile)
BIC_quart <- BIC(model_quartile)


cat("Continuous model - AIC:", AIC_continuous, "BIC:", BIC_continuous, "\n")
cat("Factor model - AIC:", AIC_terc, "BIC:", BIC_terc, "\n")
cat("Factor model - AIC:", AIC_quart, "BIC:", BIC_quart, "\n")









# Logistic regression with ios as a continuous variable
model_continuous <- glm(Complexity ~ Cohort + AgeC + urban + Union_duration + 
                          Marital_Status + Education + ios, data = m1, family = binomial)
summary(model_continuous)

model_tercile <- glm(Complexity ~ Cohort + AgeC + urban + Union_duration + 
                       Marital_Status + Education + ioscuartiles_f, data = m1, family = binomial)
summary(model_tercile)

model_quartrile <- glm(Complexity ~ Cohort + AgeC + urban + Union_duration + 
                         Marital_Status + Education + ioscuartiles_f, data = m1, family = binomial)
summary(model_quartrile)


AIC_continuous <- AIC(model_continuous)
BIC_continuous <- BIC(model_continuous)

# AIC and BIC for factor model
AIC_factor <- AIC(model_quartrile)
BIC_factor <- BIC(model_quartrile)


cat("Continuous model - AIC:", AIC_continuous, "BIC:", BIC_continuous, "\n")
cat("Factor model - AIC:", AIC_factor, "BIC:", BIC_factor, "\n")



P_cohort <- m1 |> 
  filter(Cohort == '1962-1967')
