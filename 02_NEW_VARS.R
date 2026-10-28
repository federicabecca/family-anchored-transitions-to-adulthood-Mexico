#######################
#     SYNTAX_2
#  newvars coresidence
#      
#      DATA2
#######################



library(dplyr)
library(ggplot2)
library(viridis)
library(survey)
library(writexl)
library(tidyverse)
library(writexl)
library(paletteer)
library(forcats)

# install.packages("devtools")
# devtools::install_github("r-lib/conflicted")
# library(conflicted)

# setwd("G:\\Unidades compartidas\\CORESIDENCE\\TEAM FOLDERS\\Federica Becca\\Paper_2\\EDER_2017\\data")
load("actual.Rdata")
load("retro.Rdata")

data2<- data13 %>% 
  mutate(gen=ifelse(anio_nac<=1967, "gen_62_67",
                    ifelse(anio_nac<=1972, "gen_68_72",
                           ifelse(anio_nac<=1977, "gen_73_77",
                                  ifelse(anio_nac<=1982, "gen_78_82",
                                         ifelse(anio_nac<=1987, "gen_83_87",
                                                ifelse(anio_nac<=1992, "gen_88_92",
                                                       ifelse(anio_nac<=1997, "gen_93_97",NA))))))),
         
         d_padre_vivo=ifelse(padre_cor %in% c(1,2,3,60),1,0),
         d_madre_vivo=ifelse(madre_cor %in% c(1,2,3,60),1,0),
         d_av_unido=ifelse(edo_civil1==0,0,1),
         d_unido=ifelse(edo_civil1 %in% c(1,2,3,4,10,12,13,14,20,30,40,17,18,26,27,28,37,46,47,48,126), 1,
                        ifelse(edo_civil2 %in% c(1,2,3,4,10,12,13,14,20,30,40,17,18,26,27,28,37,46,47,48,126), 1,
                               ifelse(edo_civil3 %in% c(1,2,3,4,10,12,13,14,20,30,40,17,18,26,27,28,37,46,47,48,126), 1,
                                      ifelse(edo_civil4 %in% c(1,2,3,4,10,12,13,14,20,30,40,17,18,26,27,28,37,46,47,48,126), 1,
                                             ifelse(edo_civil5 %in% c(1,2,3,4,10,12,13,14,20,30,40,17,18,26,27,28,37,46,47,48,126), 1,
                                                    ifelse(edo_civil6 %in% c(1,2,3,4,10,12,13,14,20,30,40,17,18,26,27,28,37,46,47,48,126), 1, 0)))))),
                                                          
         edad5=ifelse(edad_retro<=4, "0-4",
                    ifelse(edad_retro<=9, "5-9",
                           ifelse(edad_retro<=14, "10-14",
                                  ifelse(edad_retro<=19, "15-19",
                                         ifelse(edad_retro<=24, "20-24",
                                                ifelse(edad_retro<=29, "25-29",
                                                       ifelse(edad_retro<=34, "30-34",
                                                              ifelse(edad_retro<=39, "35-39",
                                                                     ifelse(edad_retro<=44, "40-44",
                                                                            ifelse(edad_retro<=49, "45-49",
                                                                                   ifelse(edad_retro<=55, "50-55",NA))))))))))),
         edad5_act=ifelse(edad_act<=24, "20-24",
                                                  ifelse(edad_act<=29, "25-29",
                                                         ifelse(edad_act<=34, "30-34",
                                                                ifelse(edad_act<=39, "35-39",
                                                                       ifelse(edad_act<=44, "40-44",
                                                                              ifelse(edad_act<=49, "45-49",
                                                                                     ifelse(edad_act<=55, "50-55",NA)))))))) %>%
 
  mutate(unitype1=recode(edo_civil1,'0'=0,'1'=1,'10'=1,'2'=2,'3'=2,'4'=2, '12'=2,'13'=2, '14'=2,
                        '20'=2,'30'=2,'40'=2,'126'=2,'6'=0,'7'=0,'17'=0,'26'=0,
                        '27'=0,'37'=0, '46'=0,'47'=0,'60'=0,'70'=0, '8'=0,'18'=0,'28'=0, 
                        '48'=0,'80'=0),
         unitype2=recode(edo_civil2,'0'=0,'1'=1,'10'=1,'2'=2,'3'=2,'4'=2, '12'=2,'13'=2, '14'=2,
                         '20'=2,'30'=2,'40'=2,'126'=2,'6'=0,'7'=0,'17'=0,'26'=0,
                         '27'=0,'37'=0, '46'=0,'47'=0,'60'=0,'70'=0, '8'=0,'18'=0,'28'=0, 
                         '48'=0,'80'=0),
         unitype3=recode(edo_civil3,'0'=0,'1'=1,'10'=1,'2'=2,'3'=2,'4'=2, '12'=2,'13'=2, '14'=2,
                         '20'=2,'30'=2,'40'=2,'126'=2,'6'=0,'7'=0,'17'=0,'26'=0,
                         '27'=0,'37'=0, '46'=0,'47'=0,'60'=0,'70'=0, '8'=0,'18'=0,'28'=0, 
                         '48'=0,'80'=0),
         unitype4=recode(edo_civil4,'0'=0,'1'=1,'10'=1,'2'=2,'3'=2,'4'=2, '12'=2,'13'=2, '14'=2,
                         '20'=2,'30'=2,'40'=2,'126'=2,'6'=0,'7'=0,'17'=0,'26'=0,
                         '27'=0,'37'=0, '46'=0,'47'=0,'60'=0,'70'=0, '8'=0,'18'=0,'28'=0, 
                         '48'=0,'80'=0),
         unitype5=recode(edo_civil5,'0'=0,'1'=1,'10'=1,'2'=2,'3'=2,'4'=2, '12'=2,'13'=2, '14'=2,
                         '20'=2,'30'=2,'40'=2,'126'=2,'6'=0,'7'=0,'17'=0,'26'=0,
                         '27'=0,'37'=0,'46'=0,'47'=0,'60'=0,'70'=0, '8'=0,'18'=0,'28'=0, 
                         '48'=0,'80'=0),
         unitype6=recode(edo_civil6,'0'=0,'1'=1,'10'=1,'2'=2,'3'=2,'4'=2, '12'=2,'13'=2, '14'=2,
                         '20'=2,'30'=2,'40'=2,'126'=2,'6'=0,'7'=0,'17'=0,'26'=0,
                         '27'=0,'37'=0, '46'=0,'47'=0,'60'=0,'70'=0, '8'=0,'18'=0,'28'=0, 
                         '48'=0,'80'=0),
         # el maximo de cada vector
         marstat=pmax(unitype1,unitype2, unitype3, unitype4, unitype5, unitype6), 
         marstat_c=ifelse(marstat==1, 'Consensual union',
                          ifelse(marstat==2, 'Marriage', 'Not in union')))
  


###############################################
# SAVE data2 #
############################################
save(data2, file='data2.RData')


#################################################################################

#     SYNTAX_3
#  newvars complex
#      
#      DATA3



# load("data2.RData")

# create general variable for complex coresidence: 
# - complex_year= dummy var, person-year
# - complex_id= dummy var, ID (equal to 1 if ID has lived at least one year in complex coresidence [CC] )
# in each plot I group by ID and just keep one obsrevation per ID so i can do the mean for each "complexity variable". 
# - complex_year_union= dummy var, it's equal to 1 in years when ego lives in CC while coresiding with spouse
# - complex_year_union1= dummy var, it's equal to 1 in years when ego lives in CC while coresiding with spouse of first union only
# - complex_id_union= dummy var, ID equals to 1 if has lived at least one year in CC while coresiding with spouse
# - complex_id_union1= dummy var, ID equals to 1 if has lived at least one year in CC while coresiding with spouse of first union only
# - disruption_year= it equals to 1 when union interrupts, it equals to 0 when ego coresdids with spouse or is single
# - complex_year_dis= it equals to 1 in years when ego lived in CC after union disruption
# - disruption_id= ID equals to 1 when has experienced a union disruption at least once (used to create the universe of a plot)
# - complex_id_dis= ID equals to 1 when ego lived at least one year in CC after union disruption
# - complex_year_child = year == 1 when ego was coresiding with children in CC
# - complex_ID_child= ID == 1 when ego has coresided at least one year in CC while coresiding with child
# - child_id= ID == 1 when ego has had a child

# example to check vars:
# ex<-data2 |> filter(ID==30) |> select(d_cor_union1)
# ex<-data3 |> filter(ID==55) |> select(ID, sexo2, edad_retro, cat, edo_civil1, edo_civil2, disruption_year, complex_id_dis, ad_union_cor)


data3<- data2 |> 
  mutate(complex_year=if_else(cat%in%c("c01","c02","c03","c04","c05","c06","c07"),0,1),
         complex_year_union=if_else(complex_year==1 & ad_union_cor==1,1,0),
         complex_year_child=ifelse(complex_year==1 & ad_hij_cor==1,1,0)) |> 
  group_by(ID) |> 
  mutate(complex_id=if_else(max(complex_year)==1, 1,0),
         complex_id_union=if_else(max(complex_year_union)==1, 1,0),
         child_id=ifelse(max(ad_hij_cor)==1, 1, 0),
         complex_id_child=ifelse(max(complex_year_child)==1,1,0)) |> 
  ungroup()


########################
# SAVE DATA3 (INCLUDES COMPLEXITY VARIABLES)
save(data3, file="data3.RData")

#################################################################################

#     SYNTAX4
#  newvars complex by age groups
#      
#      DATA4

# load("data3.RData")

## CALCULAR LA PROPORCION DE PERSONAS QUE HAN VIVIDO ALMENOS UN AÑO EN CC :
# - EN GENERAL (complex_id)
# - MIENTRAS ESTABAN EN UNION (complex_id_union)
# - EN LOS AÑOS DESPUES DEL DIVORCIO (complex_id_dis)
# - MIENTRAS VIVIA CON UN HIJO (complex_id_child)
## PROPORCION TIENE QUE SER POR SEXO, GENERACION Y EDAD RETRO (5Y-AGE GROUPS)


## PRIMERO, CREO VARIABLE DE CC POR GRUPO DE EDAD PARA PODER HACER LA TABLA LUEGO
# - complex_age= EQUALS TO 1 WHEN EGO LIVED IN CC IN EACH SPECIFIC AGE GROUP (ih lived 1 year while he was 22, all observation in age group 20-24 equals to 1)
data4<-data3 |> 
  group_by(ID, edad5) |> 
  mutate(complex_age=max(complex_year),
         complex_age_union=max(complex_year_union),
         complex_age_child=max(complex_year_child)) |>
  ungroup()

##########################################
# SAVE DATA4 (INCLUDES COMPLEXITY VARIABLES PER AGE GROUPS)
save(data4, file="data4.RData")

#################################################################################

#     SYNTAX5
#  newvars cumulative
#      
#      DATA5

# load("data4.RData")

## CALCULAR COMPEXITY BUT CUMULATIVE.
# - create support variables s1, s2, s3, s4 = to differentiate the years before which ego never experienced CC, and years after first experience
# - complex_age_cum*= 1 from first year in CC (according to conditions), 0 has never experienced it

data5<-data4 |> 
  group_by(ID) |> 
  mutate(s1=cumsum(complex_age),
         complex_age_cum=ifelse(s1==0, 0,
                                ifelse(s1!=0,1,NA)),
         s2=cumsum(complex_age_union),
         complex_age_cum_union=ifelse(s2==0, 0,
                                      ifelse(s2!=0,1,NA)),
         s4=cumsum(complex_age_child),
         complex_age_cum_child=ifelse(s4==0, 0,
                                      ifelse(s4!=0,1,NA)),
         x1=cumsum(complex_year_union),
         complex_year_cum_union=ifelse(x1==0, 0,
                                       ifelse(x1!=0,1,NA))) |> 
  ungroup() 


#####
# SAVE data5
save(data5, file="G:\\Unidades compartidas\\CORESIDENCE\\TEAM FOLDERS\\Federica Becca\\Paper_2\\EDER_2017\\data\\data5.RData")

#################################################################################
#     SYNTAX5
#  newvars duration
#      
#      DATA6

load("data5.RData")

simple<-c("c01","c02","c03","c04","c05","c06","c07")

data6<-data5 |> 
  mutate(gen2=recode(gen, "gen_62_67"="1962-1967",
                     "gen_68_72"="1968-1972",
                     "gen_73_77"='1973-1977',
                     'gen_78_82'='1978-1982',
                     'gen_83_87'='1983-1987',
                     'gen_88_92'='1988-1992',
                     'gen_93_97'='1993-1997'),
         gen_n=ifelse(gen=="gen_62_67", 7, 
                      ifelse(gen=="gen_68_72", 6, 
                             ifelse(gen=="gen_73_77", 5, 
                                    ifelse(gen=="gen_78_82", 4, 
                                           ifelse(gen=="gen_83_87", 3, 
                                                  ifelse(gen=="gen_88_92", 2, 
                                                         ifelse(gen=="gen_93_97", 1,NA ))))))),
         edad5n=ifelse(edad_retro<=4, 1,
                       ifelse(edad_retro<=9, 2,
                              ifelse(edad_retro<=14, 3,
                                     ifelse(edad_retro<=19, 4,
                                            ifelse(edad_retro<=24, 5,
                                                   ifelse(edad_retro<=29, 6,
                                                          ifelse(edad_retro<=34, 7,
                                                                 ifelse(edad_retro<=39, 8,
                                                                        ifelse(edad_retro<=44, 9,
                                                                               ifelse(edad_retro<=49, 10,
                                                                                      ifelse(edad_retro<=55, 11,NA))))))))))),
         edad_grouped15=ifelse(edad_retro<=15, 15,edad_retro))|> 
  group_by(ID) |> 
  mutate(cc_length=as.factor(sum(complex_year)),
         cc_length_union=sum(complex_year_union),
         cc_length_child=sum(complex_year_child),
         cc_length=fct_relevel(cc_length, c("1","2","3","4","5","6","7","8","9","10")),
         x1=cumsum(complex_year),
         complex_year_cum=ifelse(x1==0,0,
                              ifelse(x1!=0,1, NA))) |> 
  ungroup() |> 
  mutate(cat_a=ifelse(cat%in%c('c01','c03', 'c05', 'c06'), 'NPNC',
                      ifelse(cat%in%c('c02'), 'P',
                             ifelse(cat%in%c( 'c04'), 'C',
                                    ifelse(cat%in%c('c07'), 'PC',
                                       ifelse(cat%in%c('c08', 'c12', 'c13'), 'NPNC_cc',
                                          ifelse(cat%in%c('c17','c09', 'c10'), 'P_cc',
                                             ifelse(cat%in%c('c11', 'c14', 'c16'), 'C_cc',
                                                ifelse(cat%in%c('c15',  'c18', 'c19'), 'PC_cc',NA))))))))) |> 
  group_by(ID) |> 
  mutate(Pcc=ifelse(cat_a=='P_cc', 1, 0),
         PCcc=ifelse(cat_a=='PC_cc', 1, 0),
         Ccc=ifelse(cat_a=='C_cc', 1, 0),
         NPNCcc=ifelse(cat_a=='NPNC_cc', 1, 0),
         a1=cumsum(Pcc),
         a2=cumsum(PCcc),
         a3=cumsum(Ccc),
         a4=cumsum(NPNCcc),
         complex_year_cum_P=ifelse(a1==0, 0,
                                       ifelse(a1!=0,1,NA)),
         complex_year_cum_PC=ifelse(a2==0, 0,
                                   ifelse(a2!=0,1,NA)),
         complex_year_cum_C=ifelse(a3==0, 0,
                                   ifelse(a3!=0,1,NA)),
         complex_year_cum_NPNC=ifelse(a4==0, 0,
                                   ifelse(a4!=0,1,NA)),
         av_partner=ifelse(ad_union_cor==1 & ad_hij_cor==0, 1, 0), # todas las personas que almenos una vez han cohabitado con la pareja
         av_partner=ifelse(max(av_partner)==1, 1, 0),
         av_child=ifelse(ad_union_cor==0 & ad_hij_cor==1, 1,0),
         av_child=ifelse(max(av_child)==1, 1, 0),
         av_parchi=ifelse(ad_union_cor==1 & ad_hij_cor==1, 1, 0),
         av_parchi=ifelse(max(av_parchi)==1, 1, 0))
        

data6$edad5f<-factor(data6$edad5, levels =c('0-4', '5-9', '10-14', '15-19', 
                                          '20-24', '25-29', '30-34',
                                          '35-39', '40-44', '45-49'))
data6$cat_a<-factor(data6$cat_a, levels = c('NPNC','P','PC','C', 'NPNC_cc', 'P_cc',
                                            'PC_cc','C_cc' ))


#####
# SAVE data6
save(data6, file="G:\\Unidades compartidas\\CORESIDENCE\\TEAM FOLDERS\\Federica Becca\\Paper_2\\EDER_2017\\data\\data6.RData")
load('data6.RData')
#################################################################################

### READ SAV FILE OF ANTECEDENTES TO HAVE PARENTAL BACKGROUND AND SAVE DATA7
path = file.path("antecedentes.sav")
library(haven)
ant = read_sav("antecedentes.sav")

antec<-ant |> select(folioviv, foliohog, id_pobla,  nivel_p, nivel_m, sinco_p, sinco_m, c('television':'calle_pav'))

data6$key<-paste0(data6$folioviv, data6$foliohog, data6$id_pobla)

antec$key<-paste0(antec$folioviv, antec$foliohog, antec$id_pobla)

# id<-data6$key
# isUnique <- function(id){
#   return(!any(duplicated(data6$key)))
# }
# isUnique(id)


data7_toy<-merge(data6, antec, by='key')

save(data7_toy, file="data7_toy.RData")

###############################################################################







