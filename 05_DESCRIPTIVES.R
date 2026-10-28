#######################
#     
#     SYNTAX_p
#    DESCRIPTIVE
#     
#######################

library(dplyr)
library(tidyverse)
library(ggplot2)
library(viridis)
library(haven)
library(writexl)
library(RColorBrewer)
library(survey)
library(descr)
#####################

library(openxlsx)

load("ccmodel2_filtered.Rdata")
load("ccmodel2final.RData")


'%notin%' <- Negate('%in%')

# path<-"G:\\Shared drives\\CORESIDENCE\\TEAM FOLDERS\\Federica Becca\\Paper_2\\EDER_2017\\eder_project\\descriptives.xlsx"

desc <- ccmodel2_filtered



##### WEIGHTED DESCRIPTIVE
### N
desc<-desc |> filter(cc_lag_ft==0, 
                         edad_retro %in% c(15:30)) 
desc$ioscuartiles_f<-factor(desc$ioscuartiles, levels = c('1', '2', '3', '4'))
length(unique(desc$ID))# unique N = 12933



######### TABLE OR_TYPE
# si lo miramos por cohorte vemos que las mujeres van menos a casa de los suegros
desc$or_type_f <- factor(desc$or_type, levels = c('Parents',
                                                  'Parents + Parents-in-law',
                                                  'Parents + Other Relatives',
                                                  'Parents-in-law',
                                                  'Parents-in-law + Other Relatives',
                                                  'Other Relatives',
                                                  'Parents + Parents-in-law + Other Relatives',
                                                  'Nuclear'))

or_table<-desc |> 
  filter(cc_ft==1) |> 
  group_by(sexo2, cat_ft, or_type_f) |> 
  summarise(count = n()) |> 
  group_by(sexo2, cat_ft) |>
  mutate(percentage = round(count / sum(count) * 100, 2)) |> 
  ungroup()

write_xlsx(or_table,"or_table_jan.xlsx") # OKKKKK



#### DESCRIPTIVE TABLE #####

#universe
t1<-desc |>
  group_by(sexo2, cat_ft) |>
  count()
t1

# Define survey Design
sd<-svydesign(ids=~upm, weights = ~factor_per, data=desc); sd

# prop by cohort and ft
cohort<-as.data.frame(svyby(~gen2, ~sexo2+cat_ft, sd, svymean, na.rm = TRUE, keep.var = F))
cohort<-cohort |> pivot_longer(cols=c(starts_with('stat')), names_to = 'Cohort', values_to = 'Values') 
cohort$Cohort<-substr(cohort$Cohort, 15,23)
cohort$Values<-round(cohort$Values, 3)*100


#  mean age at transition by ft
agetrans<-as.data.frame(svyby(~ft_age, ~cat_ft+sexo2, sd, svymean, na.rm = TRUE))
agetrans$ft_age<-round(agetrans$ft_age, 2)

# # for ppt
# agetrans<-as.data.frame(svyby(~ft_age, ~cat_ft, sd, svymean, na.rm = TRUE))
# agetrans$ft_age<-round(agetrans$ft_age, 2)

#  urban
urban<-as.data.frame(svyby(~urban, ~cat_ft+sexo2, sd, svymean, na.rm = TRUE))
urban$urbanurban<-round(urban$urbanurban, 3)*100

#  social strata
ses<-as.data.frame(svyby(~ioscuartiles_f, ~cat_ft+sexo2, sd, svymean, na.rm = TRUE))
ses<-ses |> pivot_longer(cols=c(starts_with('ioscuartiles')), names_to = 'ios', values_to = 'Values') |>  select(-c(starts_with('se.')))
ses$Values<-round(ses$Values, 3)*100

#  educational att
edatt<-as.data.frame(svyby(~edattain, ~cat_ft+sexo2, sd, svymean, na.rm = TRUE))
edatt<-edatt |> pivot_longer(cols=c(starts_with('edattain')), names_to = 'Level', values_to = 'Values') |>  select(-c(starts_with('se.')))
edatt$Values<-round(edatt$Values, 3)*100

#  marital status
marstat<-as.data.frame(svyby(~marstat_c, ~cat_ft+sexo2, sd, svymean, na.rm = TRUE))
marstat<-marstat |> pivot_longer(cols=c(starts_with('marstat')), names_to = 'Status', values_to = 'Values') |>
  select(-c(starts_with('se.')))
marstat$Status<-substring(marstat$Status, 10)
marstat$Values<-round(marstat$Values, 3)*100

#  union duration
uniondur<-as.data.frame(svyby(~length_uc, ~cat_ft+sexo2, sd, svymean, na.rm = TRUE))
uniondur<-uniondur |> pivot_longer(cols=c(starts_with('length')), names_to = 'Duration', values_to = 'Values') |>
  select(-c(starts_with('se.')))
uniondur$Duration<-substring(uniondur$Duration, 10)
uniondur$Values<-round(uniondur$Values, 3)*100

#  lag union
lagunion<-as.data.frame(svyby(~lag_union, ~cat_ft+sexo2, sd, svymean, na.rm = TRUE))
lagunion<-lagunion |> pivot_longer(cols=c(starts_with('lag_union')), names_to = 'lagunion', values_to = 'Values') |>
  select(-c(starts_with('se.')))
lagunion$lagunion<-substring(lagunion$lagunion, 10)
lagunion$Values<-round(lagunion$Values, 3)*100

#  complex LA
complex<-as.data.frame(svyby(~cc_ft, ~cat_ft+sexo2, sd, svymean, na.rm = TRUE))
complex$cc_ft<-round(complex$cc_ft, 3)*100

list_of_datasets <- list("universe" = t1, "Cohort" = cohort, 'Age'= agetrans, 'Urban'=urban, 
                         'Marital_S'= marstat,'Union Duration' = uniondur, 'Lag Union' = lagunion,
                         'Education' = edatt, 'Ses'= ses, 'Complex' = complex) # create a file in excel with different sheet

write.xlsx(list_of_datasets, file = 'descriptives_jan.xlsx')

# write.xlsx(list_of_datasets, file = 'descriptives_nov.xlsx')



###### paper calculation
df<-ccmodel2 |> filter(cc_ft==1) |> 
  group_by(cat_ft, cc_lag_ft) |> 
  count()
print(df)
# A tibble: 6 × 3
# Groups:   cat_ft, cc_lag_ft [6]
# cat_ft cc_lag_ft     n
# <fct>      <dbl> <int>
#   1 P              0  3399
# 2 P              1   372
# 3 PC             0   973
# 4 PC             1  2365
# 5 C              0   892
# 6 C              1   634
# > 3399/(372+3399) # EC-C from nuclear
# [1] 0.9013524
# > 973/(2365+973) # EC-P from nuclear
# [1] 0.2914919
# > 892/(982+634) # EC-SP from nuclear
# [1] 0.5519802
# > (3399+973+892)/(3399+973+892+372+2365+634) # tot from nuclear
# [1] 0.609612

library(dplyr)

# Sample data
df <- data.frame(
  cat_ft = factor(c("P", "P", "PC", "PC", "C", "C")),
  cc_lag_ft = c(0, 1, 0, 1, 0, 1),
  n = c(3398, 372, 973, 2365, 892, 633)
)

# Calculate percentages by 'cat_ft' group
df <- df %>%
  group_by(cat_ft) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  ungroup() |> 
  filter(cc_lag_ft==0)

mean(df$percentage)




# count by ft
table(desc$cat_ft, useNA = "always")



t2<-desc |>
  group_by(cat_ft) |>
  summarise(ma=mean(ft_age))
t2



########### test

test <- desc

table(desc$lag_union, desc$length_uc, desc$cat_ft)







