library(haven)
library(dplyr)
library(ggplot2)
library(tidyverse)

data1 <- read_dta("EDER2017_HV_junio21.dta")
viv <- read_dta("vivienda.dta")

viv<-viv |> select(folioviv, est_socio, upm, est_dis)

# merge two dataset by var folioviv and add vars
data12<- merge(data1, viv, by="folioviv")

test <- data12 |>  filter(ID %in% c(30:40))

data13 <- data12 %>% 
  mutate(sexo2=recode(sexo,'1'='Male',
                      '2'='Female'),
         d_padre_cor=recode(padre_cor,'1'=1,
                            '2'=1,
                            '3'=0,
                            '7'=0,
                            '60'=0,
                            '70'=0,
                            '73'=0,
                            '90'=0,
                            '91'=0,
                            '92'=0,
                            '93'=0),
         d_madre_cor=recode(madre_cor,'1'=1,
                            '2'=1,
                            '3'=0,
                            '7'=0,
                            '60'=0,
                            '70'=0,
                            '73'=0,
                            '90'=0,
                            '91'=0,
                            '92'=0,
                            '93'=0),
         d_hij_cor_1=recode(hij_cor_1,'0'=0,
                            '1'=1,
                            '2'=1,
                            '3'=0),
         d_hij_cor_2=recode(hij_cor_2,'0'=0,
                            '1'=1,
                            '2'=1,
                            '3'=0),
         d_hij_cor_3=recode(hij_cor_3,'0'=0,
                            '1'=1,
                            '2'=1,
                            '3'=0),
         d_hij_cor_4=recode(hij_cor_4,'0'=0,
                            '1'=1,
                            '2'=1,
                            '3'=0),
         d_hij_cor_5=recode(hij_cor_5,'0'=0,
                            '1'=1,
                            '2'=1,
                            '3'=0),
         d_hij_cor_6=recode(hij_cor_6,'0'=0,
                            '1'=1,
                            '2'=1,
                            '3'=0),
         d_hij_cor_7=recode(hij_cor_7,'0'=0,
                            '1'=1,
                            '2'=1,
                            '3'=0),
         d_hij_cor_8=recode(hij_cor_8,'0'=0,
                            '1'=1,
                            '2'=1,
                            '3'=0),
         d_hij_cor_9=recode(hij_cor_9,'0'=0,
                            '1'=1,
                            '2'=1,
                            '3'=0),
         d_hij_cor_10=recode(hij_cor_10,'0'=0,
                             '1'=1,
                             '2'=1,
                             '3'=0),
         d_hij_cor_11=recode(hij_cor_11,'0'=0,
                             '1'=1,
                             '2'=1,
                             '3'=0),
         d_hij_cor_12=recode(hij_cor_12,'0'=0,
                             '1'=1,
                             '2'=1,
                             '3'=0),
         d_hij_cor_13=recode(hij_cor_13,'0'=0,
                             '1'=1,
                             '2'=1,
                             '3'=0),
         d_hij_cor_14=recode(hij_cor_14,'0'=0,
                             '1'=1,
                             '2'=1,
                             '3'=0),
         d_hij_cor_15=recode(hij_cor_15,'0'=0,
                             '1'=1,
                             '2'=1,
                             '3'=0),
         d_cor_union1=recode(cor_union1,'0'=0,
                             '1'=1,
                             '2'=1,
                             '3'=0,
                             '4'=0),
         d_cor_union2=recode(cor_union2,'0'=0,
                             '1'=1,
                             '2'=1,
                             '3'=0,
                             '4'=0),
         d_cor_union3=recode(cor_union3,'0'=0,
                             '1'=1,
                             '2'=1,
                             '3'=0,
                             '4'=0),
         d_cor_union4=recode(cor_union4,'0'=0,
                             '1'=1,
                             '2'=1,
                             '3'=0,
                             '4'=0),
         d_cor_union5=recode(cor_union5,'0'=0,
                             '1'=1,
                             '2'=1,
                             '3'=0,
                             '4'=0),
         d_cor_union6=recode(cor_union6,'0'=0,
                             '1'=1,
                             '2'=1,
                             '3'=0,
                             '4'=0),
         d_hnos_cor=recode(hnos_cor,'0'=0,
                           '1'=1,
                           '2'=1,
                           '3'=0),
         d_parien_cor=recode(parien_cor,'0'=0,
                             '1'=1,
                             '2'=1,
                             '3'=0),
         d_suegro_cor=recode(suegro_cor,'0'=0,
                             '1'=1,
                             '2'=1,
                             '3'=0),
         d_suegra_cor=recode(suegra_cor,'0'=0,
                             '1'=1,
                             '2'=1,
                             '3'=0),
         d_familia_cor=recode(familia_c,'0'=0,
                              '1'=1,
                              '2'=1,
                              '3'=0)) %>% 
  mutate(ad_hij_cor=ifelse(d_hij_cor_1==1, 1,
                           ifelse(d_hij_cor_2==1, 1,
                                  ifelse(d_hij_cor_3==1, 1,
                                         ifelse(d_hij_cor_4==1, 1,
                                                ifelse(d_hij_cor_5==1, 1,
                                                       ifelse(d_hij_cor_6==1, 1,
                                                              ifelse(d_hij_cor_7==1, 1,
                                                                     ifelse(d_hij_cor_8==1, 1,
                                                                            ifelse(d_hij_cor_9==1, 1,
                                                                                   ifelse(d_hij_cor_10==1, 1,
                                                                                          ifelse(d_hij_cor_11==1, 1,
                                                                                                 ifelse(d_hij_cor_12==1, 1,
                                                                                                        ifelse(d_hij_cor_13==1, 1,
                                                                                                               ifelse(d_hij_cor_14==1, 1,
                                                                                                                      ifelse(d_hij_cor_15==1, 1,0))))))))))))))),
         
         ad_union_cor=ifelse(d_cor_union1==1, 1,
                             ifelse(d_cor_union2==1, 1,
                                    ifelse(d_cor_union3==1, 1,
                                           ifelse(d_cor_union4==1, 1,
                                                  ifelse(d_cor_union5==1, 1,
                                                         ifelse(d_cor_union6==1, 1, 0)))))),
         
         ad_padres_cor=ifelse(d_padre_cor==1, 1,
                              ifelse(d_madre_cor==1, 1,0)),
         
         ad_hnos_cor=ifelse(d_hnos_cor==1, 1,0),
         
         ad_suegros_cor=ifelse(d_suegro_cor==1, 1,
                               ifelse(d_suegra_cor==1, 1,0)),
         
         ad_parien_cor=ifelse(d_parien_cor==1, 1,
                              ifelse(d_familia_cor==1, 1,0)),
         
         ad_otros_cor=ifelse(d_parien_cor==1, 1,
                             ifelse(d_familia_cor==1, 1,
                                    ifelse(ad_suegros_cor==1, 1,
                                           ifelse(d_familia_cor==1, 1,0)))),
         
         ad_cor=ifelse(ad_suegros_cor==1, 'parents-in-law',
                       ifelse(ad_parien_cor==1, 'other relatives',
                              ifelse(ad_hnos_cor==1, 'Siblings', 'none'))),
         
         c01=ifelse(ad_hij_cor==0 & ad_union_cor==0 & ad_padres_cor==0 & ad_otros_cor==0 & ad_hnos_cor==0, 1,0), # lives alone (S)
         c02=ifelse(ad_hij_cor==0 & ad_union_cor==1 & ad_padres_cor==0 & ad_otros_cor==0 & ad_hnos_cor==0, 1,0), #partner (S)
         c03=ifelse(ad_hij_cor==1 & ad_union_cor==0 & ad_padres_cor==0 & ad_otros_cor==0 & ad_hnos_cor==0, 1,0), #children (S)
         c04=ifelse(ad_hij_cor==0 & ad_union_cor==0 & ad_padres_cor==1 & ad_otros_cor==0 & ad_hnos_cor==0, 1,0), # parents (S)
         c05=ifelse(ad_hij_cor==0 & ad_union_cor==0 & ad_padres_cor==0 & ad_otros_cor==0 & ad_hnos_cor==1, 1,0), # siiblings (S)
         c06=ifelse(ad_hij_cor==0 & ad_union_cor==0 & ad_padres_cor==1 & ad_otros_cor==0 & ad_hnos_cor==1, 1,0), # parents + siblings (S)
         c07=ifelse(ad_hij_cor==1 & ad_union_cor==1 & ad_padres_cor==0 & ad_otros_cor==0 & ad_hnos_cor==0, 1,0),#children + partner  (S)
         c08=ifelse(ad_hij_cor==0 & ad_union_cor==0 & ad_padres_cor==0 & ad_otros_cor==1 & ad_hnos_cor==0, 1,0),# OR
         c09=ifelse(ad_hij_cor==0 & ad_union_cor==1 & ad_padres_cor==1 & ad_otros_cor==0 & ad_hnos_cor==0, 1,0), # partner + parents
         c10=ifelse(ad_hij_cor==0 & ad_union_cor==1 & ad_padres_cor==0 & ad_otros_cor==0 & ad_hnos_cor==1, 1,0), #partner + siblings
         c11=ifelse(ad_hij_cor==0 & ad_union_cor==1 & ad_padres_cor==0 & ad_otros_cor==1 & ad_hnos_cor==0, 1,0), # partner + OR 
         c12=ifelse(ad_hij_cor==1 & ad_union_cor==0 & ad_padres_cor==1 & ad_otros_cor==0 & ad_hnos_cor==0, 1,0), # children + parents
         c13=ifelse(ad_hij_cor==1 & ad_union_cor==0 & ad_padres_cor==0 & ad_otros_cor==0 & ad_hnos_cor==1, 1,0), #children + siblings
         c14=ifelse(ad_hij_cor==1 & ad_union_cor==0 & ad_padres_cor==0 & ad_otros_cor==1 & ad_hnos_cor==0, 1,0), # children + OR 
         c15=ifelse(ad_hij_cor==0 & ad_union_cor==0 & ad_padres_cor==1 & ad_otros_cor==1 & ad_hnos_cor==0, 1,0), # parents + OR  
         c16=ifelse(ad_hij_cor==0 & ad_union_cor==0 & ad_padres_cor==0 & ad_otros_cor==1 & ad_hnos_cor==1, 1,0), #or + siblings
         c17=ifelse(ad_hij_cor==1 & ad_union_cor==1 & ad_padres_cor==1 & ad_otros_cor==0 & ad_hnos_cor==0, 1,0), # children + partner + parents 
         c18=ifelse(ad_hij_cor==1 & ad_union_cor==1 & ad_padres_cor==0 & ad_otros_cor==0 & ad_hnos_cor==1, 1,0),  #children + partner + siblings
         c19=ifelse(ad_hij_cor==1 & ad_union_cor==1 & ad_padres_cor==0 & ad_otros_cor==1 & ad_hnos_cor==0, 1,0), # children, partner, or
         c20=ifelse(ad_hij_cor==0 & ad_union_cor==1 & ad_padres_cor==1 & ad_otros_cor==0 & ad_hnos_cor==1, 1,0), # partner + parents + siblings
         c21=ifelse(ad_hij_cor==0 & ad_union_cor==1 & ad_padres_cor==1 & ad_otros_cor==1 & ad_hnos_cor==0, 1,0),  #partner + parents + OR 
         c22=ifelse(ad_hij_cor==0 & ad_union_cor==1 & ad_padres_cor==0 & ad_otros_cor==1 & ad_hnos_cor==1, 1,0),  #partner + siblings + OR 
         c23=ifelse(ad_hij_cor==1 & ad_union_cor==0 & ad_padres_cor==1 & ad_otros_cor==0 & ad_hnos_cor==1, 1,0),  #child + parents + siblings 
         c24=ifelse(ad_hij_cor==1 & ad_union_cor==0 & ad_padres_cor==1 & ad_otros_cor==1 & ad_hnos_cor==0, 1,0), # children + parents + OR 
         c25=ifelse(ad_hij_cor==1 & ad_union_cor==0 & ad_padres_cor==0 & ad_otros_cor==1 & ad_hnos_cor==1, 1,0),  #child + siblings + or
         c26=ifelse(ad_hij_cor==0 & ad_union_cor==0 & ad_padres_cor==1 & ad_otros_cor==1 & ad_hnos_cor==1, 1,0),  #parents + siblings + or
         c27=ifelse(ad_hij_cor==1 & ad_union_cor==1 & ad_padres_cor==1 & ad_otros_cor==0 & ad_hnos_cor==1, 1,0), #childen + parter + parents + siblings
         c28=ifelse(ad_hij_cor==1 & ad_union_cor==1 & ad_padres_cor==1 & ad_otros_cor==1 & ad_hnos_cor==0, 1,0), #childen + parter + parents + or
         c29=ifelse(ad_hij_cor==1 & ad_union_cor==1 & ad_padres_cor==0 & ad_otros_cor==1 & ad_hnos_cor==1, 1,0), #childen + partner + siblings + or
         c30=ifelse(ad_hij_cor==0 & ad_union_cor==1 & ad_padres_cor==1 & ad_otros_cor==1 & ad_hnos_cor==1, 1,0), #parter + parents + siblings + or
         c31=ifelse(ad_hij_cor==1 & ad_union_cor==0 & ad_padres_cor==1 & ad_otros_cor==1 & ad_hnos_cor==1, 1,0), #childen + parents + siblings + or
         c32=ifelse(ad_hij_cor==1 & ad_union_cor==1 & ad_padres_cor==1 & ad_otros_cor==1 & ad_hnos_cor==1, 1,0), #childen + parter + parents + siblings + or
         
         cat=
           case_when(
           c01 == 1 ~ "c01",
           c02 == 1 ~ "c02",
           c03 == 1 ~ "c03",
           c04 == 1 ~ "c04",
           c05 == 1 ~ "c05",
           c06 == 1 ~ "c06",
           c07 == 1 ~ "c07",
           c08 == 1 ~ "c08",
           c09 == 1 ~ "c09",
           c10 == 1 ~ "c10",
           c11 == 1 ~ "c11",
           c12 == 1 ~ "c12",
           c13 == 1 ~ "c13",
           c14 == 1 ~ "c14",
           c15 == 1 ~ "c15",
           c16 == 1 ~ "c16",
           c17 == 1 ~ "c17",
           c18 == 1 ~ "c18",
           c19 == 1 ~ "c19",
           c20 == 1 ~ "c20",
           c21 == 1 ~ "c21",
           c22 == 1 ~ "c22",
           c23 == 1 ~ "c23",
           c24 == 1 ~ "c24",
           c25 == 1 ~ "c25",
           c26 == 1 ~ "c26",
           c27 == 1 ~ "c27",
           c28 == 1 ~ "c28",
           c29 == 1 ~ "c29",
           c30 == 1 ~ "c30",
           c31 == 1 ~ "c31",
           TRUE ~ "c32"
         ))
         
         

# vector coresidence children
CHI <- c('c03', 'c07','c12','c13','c14','c17','c18','c19',
         'c23','c24','c25','c27','c28','c29','c31', 'c32')
PAR <- c('c02', 'c07','c09','c10','c11','c17','c18','c19',
         'c20','c21','c22','c27','c28','c29','c30', 'c32')
         # cat=ifelse(c01==1,"c01",
         #            ifelse(c02==1,"c02",
         #                   ifelse(c03==1,"c03",
         #                          ifelse(c04==1,"c04",
         #                                 ifelse(c05==1,"c05",
         #                                        ifelse(c06==1,"c06",
         #                                               ifelse(c07==1,"c07",
         #                                                      ifelse(c08==1,"c08",
         #                                                             ifelse(c09==1,"c09",
         #                                                                    ifelse(c10==1,"c10",
         #                                                                           ifelse(c11==1,"c11",
         #                                                                                  ifelse(c12==1,"c12",
         #                                                                                         ifelse(c13==1,"c13",
         #                                                                                                ifelse(c14==1,"c14",
         #                                                                                                       ifelse(c15==1,"c15",
         #                                                                                                              ifelse(c16==1,"c16",
         #                                                                                                                     ifelse(c17==1,"c17",
         #                                                                                                                            ifelse(c18==1,"c18",
         #                                                                                                                                   ifelse(c19==1,"c19",
         #                                                                                                                                          ifelse(c20==1,"c20",
         #                                                                                                                                                 ifelse(c21==1,"c21",
         #                                                                                                                                                        ifelse(c22==1,"c22",NA)))))))))))))))))))))))

save(data13, file="retro.Rdata")

# test
# test there is no NA
test<-data13 |> 
  filter(is.na(cat)) |> 
  select(ID, cat, starts_with('ad_'))

#### all combinations of LA
# Define the set of elements
elements <- c("Partner", "Child", "Parents", "Siblings", "OR")

# Initialize an empty list to store all combinations
all_combinations <- list()

# Loop through each possible combination length
for (i in 1:length(elements)) {
  # Get combinations of length i
  comb <- combn(elements, i, simplify = FALSE)
  # Append to the list of all combinations
  all_combinations <- c(all_combinations, comb)
}

# Initialize an empty vector to store the combinations as strings
combination_strings <- c()

# Save each combination as a string in the vector
for (comb in all_combinations) {
  combination_strings <- c(combination_strings, paste(comb, collapse = " + "))
}

# Now, `combination_strings` contains all combinations
print(combination_strings)
