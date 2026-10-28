
library(dplyr)
library(tidyverse)
library(haven)
library(survey)
library(openxlsx)
library(writexl)

# libraries: doBy
# functions: predict(), summaryBy()

load('modeldata.RData')


# using survey (Lummey, T.) - define an overall sample design, and then access subsets

# exploration of nas
sum(is.na(m1$Cohort))
sum(is.na(m1$AgeC))
sum(is.na(m1$urban))
sum(is.na(m1$Marital_Status))
sum(is.na(m1$Education))
sum(is.na(m1$ioscuartiles_f))

# check samples sizes and if correspond with descriptives --> THEY CORRESPOND (update 01.2025)
mP_w<-droplevels(m1[m1$cat_ft=='P' & m1$cc_lag_ft==0 & m1$Sex=="Female", ])
mP_m<-droplevels(m1[m1$cat_ft=='P' & m1$cc_lag_ft==0 & m1$Sex=="Male", ])

mPC_w<-droplevels(m1[m1$cat_ft=='PC' & m1$cc_lag_ft==0 & m1$Sex=="Female", ])
mPC_m<-droplevels(m1[m1$cat_ft=='PC' & m1$cc_lag_ft==0 & m1$Sex=="Male", ])

mC_w<-droplevels(m1[m1$cat_ft=='C' & m1$cc_lag_ft==0 & m1$Sex=="Female", ])
mC_m<-droplevels(m1[m1$cat_ft=='C' & m1$cc_lag_ft==0 & m1$Sex=="Male", ])


sum(is.na(mP_w$ioscuartiles_f))


options(scipen = 100)

##################################################################
##### WOMEN ########################


## FIRST COHABITATION ####

mP<-droplevels(m1[m1$cat_ft=='P' & m1$cc_lag_ft==0, ])


sd<-svydesign(ids=~upm+ID, weights = ~factor_per, data=mP); sd

sdw<-subset(sd, Sex=="Female")
sdm<-subset(sd, Sex=="Male")



# model 1 + controls WOMEN
mP2W<-svyglm(Complexity~Cohort + AgeC + urban + ioscuartiles_f+Education+
               Marital_Status, 
             design = sdw,
             family=quasibinomial())


# Extract coefficients, standard errors, and p-values model2
coef_summary_women <- summary(mP2W)$coefficients
coefficients_women <- coef_summary_women[, "Estimate"]
standard_errors_women <- coef_summary_women[, "Std. Error"]
p_values_women <- coef_summary_women[, "Pr(>|t|)"]

# Calculate Odds Ratios (ORs) and 95% Confidence Intervals (CIs) model2
odds_ratios_women <- exp(coefficients_women)
conf_int_lower_women  <- exp(coefficients_women - 1.96 * standard_errors_women)
conf_int_upper_women  <- exp(coefficients_women + 1.96 * standard_errors_women)

# Create a data frame with coefficients, odds ratios, CIs, and p-values model2
results_mP2W <- data.frame(
  category = rownames(coef_summary_women),
  Coefficient = coefficients_women,
  Odds_Ratio = odds_ratios_women,
  CI_Lower = conf_int_lower_women,
  CI_Upper = conf_int_upper_women,
  P_Value = p_values_women
)
results_mP2W[, -1] <- round(results_mP2W[, -1], 3)

summary(mP2W)

### MEN
# model 1 + controls MEN
mP2M<-svyglm(Complexity~Cohort + AgeC + urban + ioscuartiles_f+Education+
               Marital_Status, 
             design = sdm,
             family=quasibinomial())

# Extract coefficients, standard errors, and p-values model2
coef_summary_men <- summary(mP2M)$coefficients
coefficients_men <- coef_summary_men[, "Estimate"]
standard_errors_men <- coef_summary_men[, "Std. Error"]
p_values_men <- coef_summary_men[, "Pr(>|t|)"]

# Calculate Odds Ratios (ORs) and 95% Confidence Intervals (CIs) model2
odds_ratios_men <- exp(coefficients_men)
conf_int_lower_men <- exp(coefficients_men - 1.96 * standard_errors_men)
conf_int_upper_men <- exp(coefficients_men + 1.96 * standard_errors_men)

# Create a data frame with coefficients, odds ratios, CIs, and p-values model2
results_mP2M <- data.frame(
  category = rownames(coef_summary_men),
  Coefficient = coefficients_men,
  Odds_Ratio = odds_ratios_men,
  CI_Lower = conf_int_lower_men,
  CI_Upper = conf_int_upper_men,
  P_Value = p_values_men
)
results_mP2M[, -1] <- round(results_mP2M[, -1], 3)



# Create a combined data frame with formatted CI columns
results_combined <- data.frame(
  Variable = rownames(coef_summary_women),
  Coefficient_Women = round(coefficients_women, 2),
  Odds_Ratio_Women = round(odds_ratios_women, 2),
  CI_Women = paste0("(", round(conf_int_lower_women, 2), "-", round(conf_int_upper_women, 2), ")"),
  P_Value_Women = round(p_values_women, 2),
  Coefficient_Men = round(coefficients_men, 2),
  Odds_Ratio_Men = round(odds_ratios_men, 2),
  CI_Men = paste0("(", round(conf_int_lower_men, 2), "-", round(conf_int_upper_men, 2), ")"),
  P_Value_Men = round(p_values_men, 2)
)

# Extract the number of cases
number_of_cases_women <- nobs(mP2W)
number_of_cases_men <- nobs(mP2M)

# Add the number of cases to the combined data frame
results_combined <- rbind(
  results_combined,
  c("Number of cases", NA, NA, NA, number_of_cases_women, NA, NA, NA, number_of_cases_men)
)

# Display the combined results
print(results_combined)







## FIRST CHILD #### 
mPC<-droplevels(m1[m1$cat_ft=='PC' & m1$cc_lag_ft==0, ])


sd<-svydesign(ids=~upm+ID, weights = ~factor_per, data=mPC); sd
sdw<-subset(sd, Sex=="Female")
sdm<-subset(sd, Sex=="Male")


######## model WOMEN
mPC2W<-svyglm(Complexity~Cohort + AgeC + urban + ioscuartiles_f + Education+
                Marital_Status + Union_duration , 
              design = sdw,
              family=quasibinomial())


# Extract coefficients, standard errors, and p-values model2
coef_summary_women <- summary(mPC2W)$coefficients
coefficients_women <- coef_summary_women[, "Estimate"]
standard_errors_women <- coef_summary_women[, "Std. Error"]
p_values_women <- coef_summary_women[, "Pr(>|t|)"]

# Calculate Odds Ratios (ORs) and 95% Confidence Intervals (CIs) model2
odds_ratios_women <- exp(coefficients_women)
conf_int_lower_women <- exp(coefficients_women - 1.96 * standard_errors_women)
conf_int_upper_women <- exp(coefficients_women + 1.96 * standard_errors_women)

# Create a data frame with coefficients, odds ratios, CIs, and p-values model2
results_mPC2W <- data.frame(
  category = rownames(coef_summary_women),
  Coefficient_women = coefficients_women,
  Odds_Ratio_women = odds_ratios_women,
  CI_Lower_women = conf_int_lower_women,
  CI_Upper_women = conf_int_upper_women,
  P_Value_women = p_values_women
)
results_mPC2W[, -1] <- round(results_mPC2W[, -1], 3)


### MEN
# model MEN
mPC2M<-svyglm(Complexity~Cohort + AgeC + urban + ioscuartiles_f + Education+
                Marital_Status + Union_duration , 
              design = sdm,
              family=quasibinomial())

# Extract coefficients, standard errors, and p-values model2
coef_summary_men <- summary(mPC2M)$coefficients
coefficients_men <- coef_summary_men[, "Estimate"]
standard_errors_men <- coef_summary_men[, "Std. Error"]
p_values_men <- coef_summary_men[, "Pr(>|t|)"]

# Calculate Odds Ratios (ORs) and 95% Confidence Intervals (CIs) model2
odds_ratios_men <- exp(coefficients_men)
conf_int_lower_men <- exp(coefficients_men - 1.96 * standard_errors_men)
conf_int_upper_men <- exp(coefficients_men + 1.96 * standard_errors_men)

# Create a data frame with coefficients, odds ratios, CIs, and p-values model2
results_mPC2M <- data.frame(
  category = rownames(coef_summary_men),
  Coefficient_men = coefficients_men,
  Odds_Ratio_men = odds_ratios_men,
  CI_Lower_men = conf_int_lower_men,
  CI_Upper_men = conf_int_upper_men,
  P_Value_men = p_values_men
)
results_mPC2M[, -1] <- round(results_mPC2M[, -1], 3)



# Create a combined data frame with formatted CI columns
results_combinedPC <- data.frame(
  Variable = rownames(coef_summary_women),
  Coefficient_Women = round(coefficients_women, 2),
  Odds_Ratio_Women = round(odds_ratios_women, 2),
  CI_Women = paste0("(", round(conf_int_lower_women, 2), "-", round(conf_int_upper_women, 2), ")"),
  P_Value_Women = round(p_values_women, 2),
  Coefficient_Men = round(coefficients_men, 2),
  Odds_Ratio_Men = round(odds_ratios_men, 2),
  CI_Men = paste0("(", round(conf_int_lower_men, 2), "-", round(conf_int_upper_men, 2), ")"),
  P_Value_Men = round(p_values_men, 2)
)

# Extract the number of cases
number_of_cases_women <- nobs(mP2W)
number_of_cases_men <- nobs(mP2M)

# Add the number of cases to the combined data frame
results_combinedPC <- rbind(
  results_combinedPC,
  c("Number of cases", NA, NA, NA, number_of_cases_women, NA, NA, NA, number_of_cases_men)
)

# Display the combined results
print(results_combinedPC)






#### FIRST TIME SINGLE PARENT ####

mC<-droplevels(m1[m1$cat_ft=='C' & m1$cc_lag_ft==0, ])

mC <- mC |> 
  mutate(Marital_Status2=
           case_when(Marital_Status %in% c("Consensual union", "Marriage") ~ 'In union', 
                     .default="Not in union"))
## we add marital status 2, whihc combines people in union in the year of SP in one category,
## in order to account for partner migration
mC$Marital_Status2<-factor(mC$Marital_Status2, levels = c("Not in union", "In union"))

sd<-svydesign(ids=~upm+ID, weights = ~factor_per, data=mC); sd
sdw<-subset(sd, Sex=="Female")
sdm<-subset(sd, Sex=="Male")

## we also incude lag_union, in union the year before SP to account for SP from a separation
## or from an out of wedlock birth
# model WOMEN
mC2W<-svyglm(Complexity~Cohort + AgeC + urban+ + ioscuartiles_f+Education+
               + Marital_Status2+  Lag_union  ,
             design = sdw,
             family=quasibinomial())


# Extract coefficients, standard errors, and p-values model2
coef_summary_women <- summary(mC2W)$coefficients
coefficients_women <- coef_summary_women[, "Estimate"]
standard_errors_women <- coef_summary_women[, "Std. Error"]
p_values_women <- coef_summary_women[, "Pr(>|t|)"]


# Calculate Odds Ratios (ORs) and 95% Confidence Intervals (CIs) model2
odds_ratios_women <- exp(coefficients_women)
conf_int_lower_women <- exp(coefficients_women - 1.96 * standard_errors_women)
conf_int_upper_women <- exp(coefficients_women + 1.96 * standard_errors_women)

# Create a data frame with coefficients, odds ratios, CIs, and p-values model2
results_mC2W <- data.frame(
  category = rownames(coef_summary_women),
  Coefficient_women = coefficients_women,
  Odds_Ratio_women = odds_ratios_women,
  CI_Lower_women = conf_int_lower_women,
  CI_Upper_women = conf_int_upper_women,
  P_Value_women = p_values_women
)
results_mC2W[, -1] <- round(results_mC2W[, -1], 3)


### MEN ###
# model MEN

mC2M<-svyglm(Complexity~Cohort + AgeC + urban+ + ioscuartiles_f+Education+
               + Marital_Status2+  Lag_union, 
             design = sdm,
             family=quasibinomial())


# Extract coefficients, standard errors, and p-values model2
coef_summary_men <- summary(mC2M)$coefficients
coefficients_men <- coef_summary_men[, "Estimate"]
standard_errors_men <- coef_summary_men[, "Std. Error"]
p_values_men <- coef_summary_men[, "Pr(>|t|)"]

# Calculate Odds Ratios (ORs) and 95% Confidence Intervals (CIs) model2
odds_ratios_men <- exp(coefficients_men)
conf_int_lower_men <- exp(coefficients_men - 1.96 * standard_errors_men)
conf_int_upper_men <- exp(coefficients_men + 1.96 * standard_errors_men)

# Create a data frame with coefficients, odds ratios, CIs, and p-values model2
results_mC2M <- data.frame(
  category = rownames(coef_summary_men),
  Coefficient_men = coefficients_men,
  Odds_Ratio_men = odds_ratios_men,
  CI_Lower_men = conf_int_lower_men,
  CI_Upper_men = conf_int_upper_men,
  P_Value_men = p_values_men
)
results_mC2M[, -1] <- round(results_mC2M[, -1], 3)



# Create a combined data frame with formatted CI columns
results_combinedC <- data.frame(
  Variable = rownames(coef_summary_women),
  Coefficient_Women = round(coefficients_women, 2),
  Odds_Ratio_Women = round(odds_ratios_women, 2),
  CI_Women = paste0("(", round(conf_int_lower_women, 2), "-", round(conf_int_upper_women, 2), ")"),
  P_Value_Women = round(p_values_women, 2),
  Coefficient_Men = round(coefficients_men, 2),
  Odds_Ratio_Men = round(odds_ratios_men, 2),
  CI_Men = paste0("(", round(conf_int_lower_men, 2), "-", round(conf_int_upper_men, 2), ")"),
  P_Value_Men = round(p_values_men, 2)
)

# Extract the number of cases
number_of_cases_women <- nobs(mP2W)
number_of_cases_men <- nobs(mP2M)

# Add the number of cases to the combined data frame
results_combinedC <- rbind(
  results_combinedC,
  c("Number of cases", NA, NA, NA, number_of_cases_women, NA, NA, NA, number_of_cases_men)
)

# Display the combined results
print(results_combinedC)




## LIST OF DATA FRAME FROM ALL 3 MODELS, AND EXPORT TO EXCEL FOR EDITING

list_of_datasets <- list("model_P" = results_combined, 'model_PC' = results_combinedPC,
                         'model_C' = results_combinedC) # create a file in excel with different sheet

write.xlsx(list_of_datasets, file = 'may/results_models_MAY.xlsx')
# write.xlsx(list_of_datasets, file = 'results_models_MAY.xlsx')
# write.xlsx(list_of_datasets, file = 'results_models_NOV.xlsx')


