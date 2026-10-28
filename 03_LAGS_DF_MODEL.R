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


load("data7_toy.Rdata")
gin <- read_dta("data_edu_fede_g.dta")

######
# new datasets:

# load("ccmodel.Rdata")
# load("data_edu_rel.Rdata")
# # load("ccmodelft.Rdata")
# load("ccmodelft_new.Rdata")
#


'%notin%' <- Negate('%in%')



########## RECODE VARS FOR MODELS

gin<-gin |> select(key, edu_par_rel, educ_par_abs2)
data_edu_rel<-merge(data7_toy, gin, by='key')

# save(data_edu_rel, file = 'data_edu_rel.RData')
  

ccmodel<-data_edu_rel |>
  select(ID, folioviv.x, sexo2, edad_retro, edad5, edad5n, edad_grouped15, gen2, cat, cat_a,
         starts_with('c_'), starts_with('ad_'), d_unido, marstat, marstat_c, d_padre_vivo,d_madre_vivo,ad_suegros_cor ,
         ad_union_cor, ad_parien_cor , ad_otros_cor,  starts_with('complex_year'), geo_eder, nivel_esc, niv_aprob ,scian,
         sinco, est_socio, nivel_p, nivel_m, sinco_p, sinco_m , edu_par_rel, educ_par_abs2, starts_with('ios'), 
         migracion, geo_eder_t ,upm, factor_per ) |>
  mutate(ambito=substr(folioviv.x, 3,3),
         urban=ifelse(ambito!=6, 'urban', 'rural'), # rural 6, urban different from 6
         edattain=recode(nivel_esc,
                         '0'='Low', '1'='Low', '2'='Low','3'='Middle', '4'='Middle','5'='Middle',
                         '6'='Middle','7'='Middle','8'='Middle','9'='High','10'='High','11'='High',
                         '12'='High','13'='High', '901'='Low', '902'='Low','903'='Middle', '904'='Middle',
                         '905'='Middle','906'='Middle','907'='Middle','908'='Middle',
                         '909'='High','910'='High','911'='High','912'='High','913'='High'),
         edattain_tv=recode(nivel_esc,
                            '0'='In school - Primary', '1'='In school - Primary', '2'='In school - Primary','3'='In school - Secondary', 
                            '4'='In school - Upper secondary','5'='In school - Upper secondary',
                            '6'='In school - Upper secondary','7'='In school - Upper secondary','8'='In school - Upper secondary',
                            '9'='In school - Tertiary','10'='In school - Tertiary','11'='In school - Tertiary',
                            '12'='In school - Tertiary','13'='In school - Tertiary', '901'='Completed - Primary', 
                            '902'='Completed - Primary','903'='Completed - Secondary', '904'='Completed - Upper secondary',
                            '905'='Completed - Upper secondary','906'='Completed - Upper secondary','907'='Completed - Upper secondary',
                            '908'='Completed - Upper secondary', '909'='Completed - Tertiary','910'='Completed - Tertiary',
                            '911'='Completed - Tertiary','912'='Completed - Tertiary','913'='Completed - Tertiary'),
         edattain_complete=recode(nivel_esc,
                                  '0'='In school', '1'='In school', '2'='In school','3'='In school', 
                                  '4'='In school','5'='In school',
                                  '6'='In school','7'='In school','8'='In school',
                                  '9'='In school','10'='In school','11'='In school',
                                  '12'='In school','13'='In school', '901'='Completed', 
                                  '902'='Completed','903'='Completed', '904'='Completed',
                                  '905'='Completed','906'='Completed','907'='Completed',
                                  '908'='Completed', '909'='Completed','910'='Completed',
                                  '911'='Completed','912'='Completed','913'='Completed'),
         edattain=as.factor(edattain),
         scian2=(substr(scian, 1,2)),
         scian2=as.numeric(scian2),
         occ_attain=ifelse(scian2==0, 'Inactive',
                           ifelse(scian2<=20, 'Primary sector',
                                  ifelse(scian2<=50, 'Secondary sector', 'Tertiary sector'))),
         cat_status=ifelse(cat%in%c('c01', 'c04', 'c05', 'c06', 'c08', 'c15', 'c16', 'c26'), 'NPNC', #cat NCNU, U, CU, C
                           ifelse(cat%in%c('c02', 'c09', 'c10', 'c11', 'c20', 'c21', 'c22', 'c30'), 'P',
                                  ifelse(cat%in%c('c07','c17', 'c18',  'c19', 'c27', 'c28', 'c29', 'c32'), 'PC',
                                         ifelse(cat%in%c('c03', 'c12', 'c13', 'c14', 'c23', 'c24', 'c25', 'c31'), 'C',NA)))))
  # mutate_if(is.character, ~na_if(., '')) |>
  

ccmodel$edattain=factor(ccmodel$edattain, levels = c('Low', 'Middle','High'))
ccmodel$edattain_tv=factor(ccmodel$edattain_tv, levels = c('In school - Primary', 'In school - Secondary', 
                                                           'In school - Upper secondary','In school - Tertiary',
                                                           'Completed - Primary', 'Completed - Secondary', 
                                                           'Completed - Upper secondary','Completed - Tertiary'))
ccmodel$edattain_complete=factor(ccmodel$edattain_complete, levels = c('In school','Completed'))
ccmodel$occ_attain=factor(ccmodel$occ_attain, levels = c('Inactive', 'Primary sector', 'Secondary sector','Tertiary sector'))
ccmodel$gen2=factor(ccmodel$gen2, levels = c('1962-1967', '1968-1972', '1973-1977','1978-1982', '1983-1987'))
ccmodel$edad5=factor(ccmodel$edad5, levels = c('0-4','5-9', '10-14','15-19','20-24','25-29',
                                               '30-34','35-39','40-44','45-49','50-55'))


save(ccmodel, file="ccmodel.RData")



######## FILTER - FULL TRAJECTORIES SELECTED INDIVUALS
load('ccmodel.RData')

filter<-ccmodel |>
  filter(edad_retro%in% c(0:30),
         gen2 %notin% c('1988-1992','1993-1997')) 


####### LAGS

ccmodelft <- filter |>
  group_by(ID) |> 
  arrange(ID, edad_retro) |> 
  mutate(ft_age=ifelse(cat_status != dplyr::lag(cat_status), edad_retro, NA),
         cat_ft=ifelse(cat_status != dplyr::lag(cat_status), cat_status, NA),
         cc_ft=ifelse(ft_age==edad_retro, complex_year, NA), #ft in cc or not
         cc_lag_ft=ifelse(ft_age==edad_retro, dplyr::lag(complex_year), NA),
         lag_union_cor=ifelse(cat_ft %in% c('PC', 'C'), dplyr::lag(ad_union_cor), NA),
         lag_union=ifelse(cat_ft ==cat_status, dplyr::lag(d_unido), NA),
         lag_cat_ft=ifelse(cat_status != dplyr::lag(cat_status), dplyr::lag(cat_status), NA),
         lag_cat=ifelse(cat_status != dplyr::lag(cat_status), dplyr::lag(cat), NA)) |>
  ungroup()


ccmodelft <- ccmodelft |>
  filter(ID != 22572) |>
  group_by(ID) |>
  arrange(ID, edad_retro) |>
  mutate(group = cumsum(d_unido == 0)) %>%
  group_by(ID, group) %>%
  mutate(length_u = ifelse(d_unido == 1, cumsum(d_unido), 0)) %>%
  ungroup()


save(ccmodelft, file="ccmodelft.RData")



########## LAST DATASET ONLY WITH FAMILY TRANSITIONS ####
simple<-c("c01","c02","c03","c04","c05","c06","c07")

parents<-c('c09', 'c12', 'c17')
parents_or<-c('c15', 'c21', 'c24', 'c28')
parents_sib<-c('c20', 'c23', 'c27')
parents_or_sib<-c('c26', 'c30', 'c31', 'c32') ## 
or<-c('c08', 'c11', 'c14', 'c19')
sib<-c('c13', 'c18', 'c10')
or_sib<-c('c16', 'c22', 'c25', 'c29')

ccmodel2<-ccmodelft|> 
  group_by(ID, cat_ft) |>
  filter(edad_retro==min(edad_retro),
         edad_retro!=0) |>
  ungroup() |> 
  filter(cat_ft!="NPNC")|>  
  group_by(ID) |> 
  mutate(ft_order=row_number()) |> 
  ungroup() |> 
  mutate(marstat=ifelse(marstat==0 & ad_union_cor==1, 1, marstat),
         marstat_c=ifelse(marstat==1, 'Consensual union',
                          ifelse(marstat==2, 'Marriage', 'Not in union')),
         ad_parien2_cor=ifelse(ad_parien_cor==1, 1,
                               ifelse(ad_hnos_cor==1 & ad_padres_cor==0,1, 0)),
         or_type=ifelse(cat %in% parents,  'Parents',
                        ifelse(cat %in% parents_or & ad_suegros_cor==1 & ad_parien_cor ==1,  'Parents + Parents-in-law + Other Relatives',
                               ifelse(cat %in% parents_or & ad_suegros_cor==1 & ad_parien_cor ==0,  'Parents + Parents-in-law',
                                      ifelse(cat %in% parents_or & ad_suegros_cor==0 & ad_parien_cor ==1,  'Parents + Other Relatives',
                                             ifelse(cat %in% parents_sib ,  'Parents + Other Relatives',
                                                    ifelse(cat %in% parents_or_sib, 'Parents + Parents-in-law + Other Relatives',
                                                           ifelse(cat %in% or & ad_suegros_cor==0 & ad_parien_cor ==1,  'Other Relatives',
                                                                  ifelse(cat %in% or & ad_suegros_cor==1 & ad_parien_cor ==0,  'Parents-in-law',
                                                                         ifelse(cat %in% or & ad_suegros_cor==1 & ad_parien_cor == 1,  'Parents-in-law + Other Relatives',
                                                                                ifelse(cat %in% sib ,  'Other Relatives',
                                                                                       ifelse(cat %in% or_sib ,  'Other Relatives', 'Nuclear'))))))))))),

          length_uc=ifelse(length_u==0, '0',
                          ifelse(length_u<=2, '1-2',
                                 ifelse(length_u>=3, '>2', length_u))))


ccmodel2$cat_ft<-factor(ccmodel2$cat_ft, levels = c('P', 'PC', 'C'))
ccmodel2$ft_order<-factor(ccmodel2$ft_order, levels = c('1', '2', '3'))
ccmodel2$marstat_c<-factor(ccmodel2$marstat_c, levels = c('Consensual union', 'Marriage', 'Not in union'))
ccmodel2$or_type<-factor(ccmodel2$or_type, levels = c('Parents','Parents-in-law', 'Parents + Parents-in-law',
                                                      'Parents + Other Relatives', 'Parents-in-law + Other Relatives',
                                                      'Other Relatives', 'Parents + Parents-in-law + Other Relatives', 'Nuclear'))
ccmodel2$length_uc<-factor(ccmodel2$length_uc, levels = c('0', '1-2', '>2'))
ccmodel2$lag_union<-factor(ccmodel2$lag_union, levels = c('0', '1'))


save(ccmodel2, file="ccmodel2.RData")
load('ccmodel2.RData')

ccmodel2final <-ccmodel2 |> 
  group_by(ID) |>
  filter((cat_ft == 'P' & lag_cat_ft == 'NPNC' & ft_order == '1') | 
           (cat_ft == 'C' & (ft_order == '1' & lag_cat_ft == 'NPNC')) |
      (cat_ft == 'C' & (ft_order == '2' & lag_cat_ft %in% c('P', 'PC')) |
          (cat_ft == 'C' & (ft_order == '3' & lag_cat_ft %in% c('PC')) |
           (cat_ft == 'PC' & lag_cat_ft %in% c('P', 'NPNC') & ft_order %in% c('1','2')))),
         edad_retro %in% c(15:30))
# perdemos 96 transiciones porque por alguna razón no respetan el order

save(ccmodel2final, file="ccmodel2final.RData")

#################################################################
load('ccmodel2final.RData')

ccmodel2_filtered <- ccmodel2final |> 
  filter(cc_lag_ft == '0', 
         !is.na(ioscuartiles))

save(ccmodel2_filtered, file="ccmodel2_filtered.RData")


# DATASET WITH EVERYONE, EVEN FROM EXTENDED HOUSEHOLDS
length(unique(ccmodel2final$ID)) # -> 13923

#DATASET ONLY WITH INDIVIDUALS FROM NUCLEAR HOUSEHOLDS AND NOT NA IN IOS
length(unique(ccmodel2_filtered$ID)) #-> 12993






