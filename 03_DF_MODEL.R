#######################
#     
#     SYNTAX_p
#    DB LOGISTIC REG
#     
#######################

library(dplyr)
library(tidyverse)
library(ggplot2)
library(viridis)
library(haven)
library(survey)
library(foreign)


###################### dataset for model #########################################
load('ccmodel2_filtered.RData')
m0<-ccmodel2_filtered

m0$sexo2<-factor(m0$sexo2, levels = c('Male', 'Female'))
m0$edu_par_rel<-factor(m0$edu_par_rel, levels = c( '3','2', '1', '0'))
m0$educ_par_abs2<-factor(m0$educ_par_abs2, levels = c( '3','2', '1', '0'))
m0$iosterciles_f<-factor(m0$iosterciles, levels = c('1', '2', '3'))
m0$ioscuartiles_f<-factor(m0$ioscuartiles, levels = c('1', '2', '3', '4'))
m0<-rename(m0, AgeC= ft_age)
m0<-rename(m0, AgeF= edad5)
m0<-rename(m0, Sex= sexo2)
m0<-rename(m0, Education= edattain)
m0<-rename(m0, Marital_Status= marstat_c) # maybe change levels
m0<-rename(m0, Cohort= gen2)
m0<-rename(m0, Complexity= cc_ft)
m0<-rename(m0, Union_duration= length_uc)
m0<-rename(m0, Lag_union = lag_union)
m0$Age_squared <- m0$AgeC^2

m1<-m0 |> dplyr :: select(ID, Cohort, AgeF, AgeC,Age_squared, Sex, urban, Education, Marital_Status, 
                 ios,ioscuartiles_f, iosterciles_f,  Union_duration, Lag_union, Complexity, 
                 cat_ft, cc_lag_ft, edu_par_rel, educ_par_abs2, upm, factor_per)

save(m1,file ="modeldata.RData" )
