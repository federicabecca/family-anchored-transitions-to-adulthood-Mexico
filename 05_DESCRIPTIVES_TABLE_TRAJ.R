
## DESCRIPTIVE TABLE ON TRAJECTORIES
library(dplyr)
library(tidyverse)
library(haven)
library(writexl)
library(survey)
library(descr)


load('ccmodel2final.RData')

length(unique(ccmodel2final$ID))
unique(ccmodel2final$edad_retro)

df <- ccmodel2final

# exclude transition to single parenthood for men 
df <- df |> 
  filter(sexo2=='Female'| (sexo2=='Male' & cat_ft != 'C') )

## N : total sample size by type of transition and sex
table(df$sexo2, df$cat_ft)

table(df$sexo2, df$cat_ft, df$ft_order)



# Calculate the number of unique IDs by sex
unique_ids_by_sex <- ccmodel2final %>%
    group_by(sexo2) %>%
    summarize(unique_id_count = n_distinct(ID))

# View the result
print(unique_ids_by_sex)


## WOMEN 
## N y type of lag household and at time of transition
women <- df |> 
    filter(sexo2=="Female")

table(women$cc_lag_ft, women$cc_ft, women$cat_ft)

# Create the table
cc_table_3d <- table(women$cc_lag_ft, women$cc_ft, women$cat_ft)

# Percentages within slices of the third dimension
cc_percentages_slices <- prop.table(cc_table_3d, margin = 3) * 100

# View the result
print(cc_percentages_slices)



## within orders
dfpc <- women |> filter(cat_ft=='PC')

# Create the table
cc_table_3d <- table(dfpc$cc_lag_ft, dfpc$cc_ft, dfpc$ft_order)

# Percentages within slices of the third dimension
cc_percentages_slices <- prop.table(cc_table_3d, margin = 3) * 100

# View the result
print(cc_percentages_slices)



dfc <- women |> filter(cat_ft=='C')

# Create the table
cc_table_3d <- table(dfc$cc_lag_ft, dfc$cc_ft, dfc$ft_order)

# Percentages within slices of the third dimension
cc_percentages_slices <- prop.table(cc_table_3d, margin = 3) * 100

# View the result
print(cc_percentages_slices)

###########################################################


## MEN 
## N y type of lag household and at time of transition
men <- df |> 
    filter(sexo2=="Male")

table(men$cc_lag_ft, men$cc_ft, men$cat_ft)


# Create the table
cc_table_3d <- table(men$cc_lag_ft, men$cc_ft, men$cat_ft)

# Percentages within slices of the third dimension
cc_percentages_slices <- prop.table(cc_table_3d, margin = 3) * 100

# View the result
print(cc_percentages_slices)



## within orders
dfpc <- men |> filter(cat_ft=='PC')

# Create the table
cc_table_3d <- table(dfpc$cc_lag_ft, dfpc$cc_ft, dfpc$ft_order)

# Percentages within slices of the third dimension
cc_percentages_slices <- prop.table(cc_table_3d, margin = 3) * 100

# View the result
print(cc_percentages_slices)



dfc <- men |> filter(cat_ft=='C')

# Create the table
cc_table_3d <- table(dfc$cc_lag_ft, dfc$cc_ft, dfc$ft_order)

# Percentages within slices of the third dimension
cc_percentages_slices <- prop.table(cc_table_3d, margin = 3) * 100

# View the result
print(cc_percentages_slices)


############################################3
## ANALYSIS PARA PAPER
df <- toy2
toy <- df |> filter(cat_ft=='C' & sexo2=='Female') |> 
  select(ID, cat_ft, lag_cat_ft, ft_age, ft_order) |> 
  group_by(ft_order, lag_cat_ft) |> 
  count()

### total per column N in table

table(df$cc_lag_ft, df$cc_ft)

# Extract subsets based on combinations of cc_lag_ft and cc_ft
sub_00 <- df[df$cc_lag_ft == 0 & df$cc_ft == 0, ]  # (0,0) group
sub_01 <- df[df$cc_lag_ft == 0 & df$cc_ft == 1, ]  # (0,1) group
sub_10 <- df[df$cc_lag_ft == 1 & df$cc_ft == 0, ]  # (1,0) group
sub_11 <- df[df$cc_lag_ft == 1 & df$cc_ft == 1, ]  # (1,1) group

a<- length(unique(sub_00$ID)) # nuc nuc
b <-length(unique(sub_01$ID)) # nuc ext
c <-length(unique(sub_10$ID)) # ext nuc
d <-length(unique(sub_11$ID)) # ext ext

a+b+c+d




