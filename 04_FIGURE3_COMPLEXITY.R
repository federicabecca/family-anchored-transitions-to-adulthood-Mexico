
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
    cat_ft == 'C' ~ 'Single-parenthood\n(T3)',
    TRUE ~ NA_character_
  ))

# Drop unused levels in `cat_ft_c` after filtering
complex$cat_ft_c <- factor(complex$cat_ft_c, 
  levels = c('Partnership\n(T1)', 'Parenthood\n(T2)', 'Single-parenthood\n(T3)'))

# Filter out any unused facet combinations (e.g., Male and Single-parent.)
complex_filtered <- complex |> 
  filter(!(sexo2 == 'Male' & cat_ft_c == 'Single-parenthood\n(T3)'))

# Define color palette
mypal2 <- c('#D00000', '#FFBA08', '#3F88C5', '#309f5f', '#032B43')

# Define custom labels
custom_labels <- c(
  "Male" = "Men",
  "Female" = "Women"
)

# Plot the graph

# path <- 'G:\\Unidades compartidas\\CORESIDENCE\\TEAM FOLDERS\\Federica Becca\\102025_CORESIDENCE_OCTOBER_GUESTS\\FIGURE3_PAPER2_COMPLEXITY.RData'
# 
# save(complex_filtered, file = path)

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



# ggsave( "plot3_cc_ft_cohort_AUGUST.png", plot=p5, scale = 1, dpi = 500,  
#         height = 8, width = 12, bg='white')

ggsave( "plot3_complexity_MAR.png", plot=p5, scale = 1, dpi = 500,  
        height = 8, width = 12, bg='white')




## PAPER ANALYSIS
complex_tot<-as.data.frame(svyby(~cc_ft, ~cat_ft+sexo2, sd, svymean, na.rm = TRUE))
complex_tot$cc_ft<-round(complex_tot$cc_ft, 3); complex_tot

# > complex_tot$cc_ft<-round(complex_tot$cc_ft, 3); complex_tot
# cat_ft  sexo2 cc_ft          se
# P.Female       P Female 0.418 0.010788219
# PC.Female     PC Female 0.168 0.009038176
# C.Female       C Female 0.423 0.018215234
# P.Male         P   Male 0.321 0.011128784
# PC.Male       PC   Male 0.099 0.007514369
# C.Male         C   Male 0.302 0.030073468

