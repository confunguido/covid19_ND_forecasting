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
deaths_df = read_csv(file.path(outdir, 'fred_test_cf_delays.csv'))
periods_df = read_csv(file.path(outdir, 'fred_test_periods.csv'))
##===============================#
## sCFR by age band--------
##===============================#
jpeg('../figures/report_figure_age_CFR.jpeg', width=7,height=4.5, units = "in", res = 300)
age_cuts = seq(from=0,by=10,to=90)
age_cuts[length(age_cuts)] = 120
age_labels = sprintf("%d-%d", age_cuts[-length(age_cuts)], age_cuts[-1] -1)
age_labels[length(age_labels)] = "80+"
cs_age_df = periods_df %>% mutate(age_group = cut(age,breaks = age_cuts,labels = age_labels, include.lowest = T, right = F)) %>% 
    group_by(age_group) %>%
    summarize(Cs = n())

cf_age_df = deaths_df %>% mutate(age_group = cut(age,breaks = age_cuts,labels = age_labels, include.lowest = T, right = F)) %>% 
    group_by(age_group) %>%
    summarize(CF = n()) %>%
    left_join(cs_age_df, by = "age_group") %>%
    mutate(CFR = CF / Cs)

barplot(cf_age_df$CFR,names.arg = as.character(cf_age_df$age_group), col = "#bdbdbd")
mtext("Age groups", side = 1, line =2)
mtext("Case fatality ratio", side = 2, line =2.5)
dev.off()
