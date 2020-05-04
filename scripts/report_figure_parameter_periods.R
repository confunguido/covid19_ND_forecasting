##===============================#
## Get msa names
## Author: Guido España
## 2020
##===============================#
## Setup-------------
##===============================#
library(dplyr)
library(tidyverse)
library(RColorBrewer)

##===============================#
## Process output-------------
##===============================#
## FRED output
outdir = '../../experiments/Validation_simulations/output/'

intervals_df = read_csv(file.path(outdir, 'fred_test_intervals.csv')) %>%
    mutate(serial_interval = symp - symp_infector, generation_interval = day - day_infector )
periods_df = read_csv(file.path(outdir, 'fred_test_periods.csv'))
asymp_periods_df = read_csv(file.path(outdir, 'fred_test_asymp_periods.csv'))

##===============================#
## Serial interval --------
##===============================#
jpeg('../figures/report_figure_parameter_periods.jpeg', width=7,height=5, units = "in", res = 300)
layout(matrix(c(1,2,3,4),nrow = 2, byrow =T))
par(mar = c(4,4,2,1))
hist(periods_df$inf_period, main ="",xaxs = "i", yaxs = "i",
     xlab = "Infectious period (days)", freq = F, col = "#e0e0e0", breaks = 1:17)

hist(periods_df$symp_period, main = "",xaxs = "i", yaxs = "i", freq = F,
     xlab = "Symptomatic period (days)", col = "#e0e0e0", ylab = "")

hist(intervals_df$serial_interval, main ="",xaxs = "i", yaxs = "i",freq = F,
     xlab = "Serial interval (days)", xlim = c(0,30), col = "#e0e0e0")

hist(intervals_df$generation_interval, main ="", xaxs = "i", yaxs = "i", freq = F,
     xlab = "Generation interval (days)", xlim = c(0,30), col = "#e0e0e0", ylab = "")

dev.off()

##===============================#
## Serial interval --------
##===============================#
age_cuts = seq(from=0,by=10,to=90)
age_cuts[length(age_cuts)] = 120
age_labels = sprintf("%d",age_cuts[2:length(age_cuts)])
age_labels[length(age_labels)] = "81+"
asymp_age_df = asymp_periods_df %>%
    mutate(age_group = as.character(cut(age,breaks = age_cuts,labels = age_labels, include.lowest = T, right = F))) 
inf_counts = asymp_age_df %>% 
    group_by(age_group) %>%
    summarize(totalInf = n())

ar_prop = array(0,c(150,2))

for(ii in 1:150){
    symp_counts = asymp_age_df %>%
        filter(symp1 != -1, symp1 < ii) %>%
        group_by(age_group) %>%
        summarize(totalSymp = n()) %>%
        right_join(inf_counts, by = "age_group") %>%
        mutate(PropSymp = totalSymp / totalInf) %>%
        arrange(age_group)
    ar_prop[ii,1] = sum(symp_counts$totalSymp) / sum(symp_counts$totalInf)
    ar_prop[ii,2] = sum(symp_counts$totalSymp)
    
}
