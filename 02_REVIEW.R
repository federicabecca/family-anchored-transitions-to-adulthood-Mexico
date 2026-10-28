
# NEW CALCULATIONS FOR REVIEW & RESUBMIT DEMRES
# 07.10.2025


library(dplyr)
library(tidyverse)
library(ggplot2)
library(viridis)
library(haven)
library(survey)
library(foreign)


# REVIEWER A #3 --> INTERGENERATIONA TRANSMISSION OF LIVING ARRANGEMENTS -------

load('modeldata.RData')
load("data7_toy.Rdata")


IDfinal <- m1$ID


# filter full life trajectories only for indiv included in our last sample

df <- data7_toy |> 
  filter(ID %in% IDfinal)

# create a variable that calculates how many years lived in nuclear / extend


# UNTILE AGE 15
df2 <- df |> 
  select(ID, edad_retro, gen2, complex_year) |> 
  group_by(ID) |> 
  filter(edad_retro <= 15) |> 
  mutate(cum_ext=cumsum(complex_year)) 

df3 <- df2 |> filter(edad_retro == 15) 

table(df3$cum_ext)

ggplot(df3, aes(cum_ext))+
  geom_bar()+ theme_minimal()

# INDIVIDUALS WHO NEVER LIVED IN EXTENDED 10907/12993
# [1] 0.839452
# # INDIVIDUALS WHO LIVED 5 OR LESS YEARS (10907 + 444 +177 +127 +108+81)/12993
# [1] 0.9115678


# 17.10.2025 -------------------------------------------------------------------
## WE TRY TO INCLUDE IT IN THE MODEL AND SEE IF IT HAS AN EFFECT

# we need to merge this info to the model data

final <- merge(m1, df3, by='ID')

# manipulate the variable - ref. never exposed (=0)
final <- final |> 
  mutate(everext = ifelse(cum_ext != 0, 1, 0))

table(final$everext, final$Complexity)

m1 <- final

# save(m1, file='modeldata-REVIEW.RData')

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
    Marital_Status + everext, 
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
    Marital_Status + everext, 
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
    Marital_Status + Union_duration + everext, 
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
    Marital_Status + Union_duration + everext, 
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
number_of_cases_women <- nobs(mPC2W)
number_of_cases_men <- nobs(mPC2M)

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
    + Marital_Status2+  Lag_union  + everext,
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
    + Marital_Status2+  Lag_union + everext, 
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
number_of_cases_women <- nobs(mC2W)
number_of_cases_men <- nobs(mC2M)

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

write.xlsx(list_of_datasets, file = 'october/results_models_OCT.xlsx')


#-------------------------------------------------------------------------------
## REVIEWER A #9 --> CONFIDENCE INTERVALS TO FIGURE 1 --------------------------

library(dplyr)
library(tidyverse)
library(ggplot2)
library(haven)
library(survey)
library(ggpubr)


load("data7_toy.Rdata")

'%notin%' <- Negate('%in%')

mypal4<-c('#003049', '#6b2c39', '#d62828', '#f77f00', '#fcbf49')



# data manipulation
data7_toy$gen2<-factor(data7_toy$gen2, levels = c('1962-1967', '1968-1972', '1973-1977', '1978-1982',
  '1983-1987','1988-1992', '1993-1997'))
data7_filter<-data7_toy |> 
  filter(gen2 %notin% c('1988-1992', '1993-1997')) |>  # find a way to drop levels
  group_by(ID) |> 
  mutate(cat_status=ifelse(cat%in%c('c01', 'c03', 'c05', 'c06', 'c08', 'c12', 'c13'), 'NPNC', #cat NCNU, U, CU, C
    ifelse(cat%in%c('c02', 'c17','c09', 'c10'), 'P',
      ifelse(cat%in%c('c07', 'c15',  'c18', 'c19'), 'PC',
        ifelse(cat%in%c('c04', 'c11', 'c14', 'c16'), 'C',NA)))),
    gen2 = fct_drop(gen2),
    # NPNC=ifelse(cat%in%c('c01', 'c03', 'c05', 'c06', 'c08', 'c12', 'c13'), 1,0),
    PAR=ifelse(cat%in%c('c02', 'c07','c09','c10','c11','c17','c18','c19',
      'c20','c21','c22','c27','c28','c29','c30', 'c32'), 1,0),
    CHI=ifelse(cat%in%c('c03', 'c07','c12','c13','c14','c17','c18','c19',
      'c23','c24','c25','c27','c28','c29','c31', 'c32'), 1,0))


datap1_2<-data7_filter |> filter(edad_retro%in% c(15:30))
datap1_2$gen2<-factor(datap1_2$gen2, levels = c('1962-1967', '1968-1972', '1973-1977', '1978-1982',
  '1983-1987'))
datap1_2$partner_l<-'With partner'
datap1_2$child_l<-'With child(ren)'
datap1_2$npnc_l<-'Without partner or child(ren)'

facet_labels <- c("Female" = "Women", "Male" = "Men")

# plot ## NO ES CUMULATIVO, ES CROSS-SECTIONAL A CADA EDAD, CUANTA GENTE COHABITA CON LA PAREJA
sd<-svydesign(id=~upm+ID, data=datap1_2, weights=~factor_per); sd

tP<-as.data.frame(svyby(~PAR, ~partner_l+sexo2+edad_retro+gen2, sd, svymean, na.rm = TRUE))

tP <- tP %>%
  mutate(
    lo = pmax(0, PAR - 1.96 * se),
    hi = pmin(1, PAR + 1.96 * se))

tP <- tP |> 
  mutate(lo2 = ifelse(gen2 %in% c('1962-1967', '1983-1987'), lo, NA),
    hi2 = ifelse(gen2 %in% c('1962-1967', '1983-1987'), hi, NA))

p1<-ggplot(tP, aes(edad_retro, y=PAR, group=gen2))+
  geom_ribbon(aes(ymin = lo2, ymax = hi2, fill = gen2), 
    alpha = .1, colour = NA, show.legend = FALSE) +
  geom_line(aes(col=gen2, group=gen2), linewidth=.7)+
  geom_point(aes(col=gen2, group=gen2), size=1.2)+
  scale_x_continuous(breaks = seq(15,30,1))+
  scale_y_continuous(breaks = seq(0,1,.1), limits = c(0,1))+
  scale_color_manual(values=mypal4, name = 'Cohort')+
  scale_fill_manual(values=mypal4, name = 'Cohort')+
  facet_grid(partner_l~sexo2, labeller = labeller(sexo2 = facet_labels))+
  theme_light(base_size = 22)+
  theme(legend.position = c(0.1,0.8),
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank())+
  labs(x='Age', y='Proportion'); p1


tC<-as.data.frame(svyby(~CHI, ~child_l+sexo2+edad_retro+gen2, sd, svymean, na.rm = TRUE))

tC <- tC %>%
  mutate(
    lo = pmax(0, CHI - 1.96 * se),
    hi = pmin(1, CHI + 1.96 * se))

# second option: leave CI only for first and last cohort

tC <- tC |> 
  mutate(lo2 = ifelse(gen2 %in% c('1962-1967', '1983-1987'), lo, NA),
    hi2 = ifelse(gen2 %in% c('1962-1967', '1983-1987'), hi, NA))


p2<-ggplot(tC, aes(edad_retro, y=CHI, group=gen2))+
  geom_ribbon(aes(ymin = lo2, ymax = hi2, fill = gen2), 
    alpha = .13, colour = NA, show.legend = FALSE) +
  geom_line(aes(col=gen2, group=gen2), linewidth=.7, show.legend = F)+
  geom_point(aes(col=gen2, group=gen2), size=1.2, show.legend = F)+
  scale_x_continuous(breaks = seq(15,30,1))+
  scale_y_continuous(breaks = seq(0,1,.1), limits = c(0,1))+
  scale_color_manual(values=mypal4, name = 'Cohort')+
  scale_fill_manual(values=mypal4, name = 'Cohort')+
  facet_grid(child_l~sexo2, labeller = labeller(sexo2 = facet_labels))+
  theme_light(base_size = 22)+
  theme(panel.grid.minor = element_blank())+
  labs(x='Age', y='Proportion'); p2


plot<- ggarrange(p1+rremove("ylab")+rremove("xlab"), 
  p2+rremove("ylab")+rremove("xlab"),
  nrow = 2);plot


plot_1<- annotate_figure(plot,
  left = text_grob("Proportion", color = "black", rot = 90,size=14),
  bottom = text_grob("Age", color = "black",
    hjust = 1, x = .55,  size = 14)); plot_1

ggsave( "plot1_stability_OCT_v2.png", plot=plot_1, scale = 1, dpi = 500,  
  height = 12, width = 13) 



# REVIEWER A #11 - test differences between cohorts ----------------------------
# REVIEWER C - CHANGE SINGLE PARENTHOOD TO MOTHERHOOD

library(dplyr)
library(tidyverse)
library(ggplot2)
library(haven)
library(survey)

# Load the dataset
load("ccmodel2_filtered.Rdata")

# Filter out rows where `sexo2` is 'Male' and `cat_ft` is 'C'
desc <- ccmodel2_filtered |> 
  filter(cc_lag_ft == 0) |> 
  filter(!(sexo2 == 'Male' & cat_ft == 'C'))

# Drop unused levels from `cat_ft`
desc$cat_ft <- droplevels(desc$cat_ft)

# Create the survey design
sd <- svydesign(ids = ~upm, weights = ~factor_per, data = desc)

# Calculate summary statistics
complex <- as.data.frame(svyby(~cc_ft, ~cat_ft + sexo2 + gen2, sd, svymean, na.rm = TRUE))
complex$cc_ft <- round(complex$cc_ft, 3)
complex$upper <- complex$cc_ft + 1.96 * complex$se
complex$lower <- complex$cc_ft - 1.96 * complex$se

# Map `cat_ft` to a new factor variable with clean levels
complex <- complex |> 
  mutate(cat_ft_c = case_when(
    cat_ft == 'P' ~ 'Partnership\n(T1)',
    cat_ft == 'PC' ~ 'Parenthood\n(T2)',
    cat_ft == 'C' ~ 'Single motherhood\n(T3)',
    TRUE ~ NA_character_
  ))

# Drop unused levels in `cat_ft_c` after filtering
complex$cat_ft_c <- factor(complex$cat_ft_c, 
  levels = c('Partnership\n(T1)', 'Parenthood\n(T2)', 'Single motherhood\n(T3)'))

# Filter out any unused facet combinations (e.g., Male and Single-parent.)
complex_filtered <- complex |> 
  filter(!(sexo2 == 'Male' & cat_ft_c == 'Single motherhood\n(T3)'))

# Define color palette
mypal2 <- c('#D00000', '#FFBA08', '#3F88C5', '#309f5f', '#032B43')

# Define custom labels
custom_labels <- c(
  "Male" = "Men",
  "Female" = "Women"
)

# Plot the graph

p5 <- ggplot(complex_filtered, aes(cat_ft_c, y = cc_ft)) +
  geom_bar(aes(fill = gen2, group = gen2), stat = 'identity', alpha = .7, width = .65,
    show.legend = TRUE, position = position_dodge(.75)) +
  geom_errorbar(aes(ymin = lower, ymax = upper, group = gen2), width = .2, 
    linewidth = .4, position = position_dodge(.75)) +
  facet_grid(~sexo2, scales = "free_x", space = "free_x", 
    labeller = labeller(sexo2 = custom_labels)) + # Allow facets to adjust to missing categories
  scale_fill_manual(values = mypal2, name = 'Cohort') +
  scale_y_continuous(breaks = seq(0, .6, .1), limits = c(0, .65)) +
  theme_light(base_size = 22, base_family = "Times New Roman") +
  labs(x = '',
    y = 'Proportion') +
  theme(
    legend.position = 'bottom',
    strip.text.x = element_text(size = 16, family = "Times New Roman"),  # Adjust facet label font
    axis.text = element_text(family = "Times New Roman"),  # Axis text
    axis.title = element_text(family = "Times New Roman"),  # Axis titles
    legend.text = element_text(family = "Times New Roman"),  # Legend text
    legend.title = element_text(family = "Times New Roman")  # Legend title
  ); p5 # Adjust facet label size if needed


ggsave( "plot3_complexity_OCT.png", plot=p5, scale = 1, dpi = 500,  
  height = 8, width = 12, bg='white')


















