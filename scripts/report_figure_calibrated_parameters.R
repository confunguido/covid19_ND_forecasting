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
library(lubridate)

##===============================#
## Data from NYT or JHU------------
##===============================#
interventions_df = read_csv('../../experiments/input_files/interventions_covid_timeseries.csv') %>%
    filter(State != 'NY')

fit_data = read_csv('../../experiments/Midwest_simulations/output/CALIBRATION/US_states_covid_data.csv') %>%
    filter(state %in% interventions_df$state_name) %>%
    left_join(interventions_df, by = c("state" =  "state_name")) %>%
    group_by(State) %>%
    mutate(cases_inc = c(cases[1], diff(cases)),
           deaths_inc = c(deaths[1], diff(deaths))) %>%
    ungroup()

##===============================#
## Process output-------------
##===============================#
outdir = '../../experiments/Midwest_simulations/output/SHORT_FORECAST'
fred_sweep_df = tibble()
params_sweep_df = tibble()

for(nn in 1:nrow(interventions_df)){    
    outdir_st = file.path(outdir, sprintf('FRED_%s_short_forecast_asymp_out', interventions_df$State[nn]))
    data_out = file.path(outdir_st,'fred_output.csv')

    params_out = file.path(outdir_st, 'FRED_parameters_out.csv')
    params_df = read_csv(params_out)
    tmp_sweep_df = read_csv(data_out) %>%
        right_join(params_df, by = c("job_id" = "job_id")) %>%
        mutate(Date = as.Date("2020-01-01") + Day)    
    fred_sweep_df = bind_rows(fred_sweep_df, tmp_sweep_df)
    params_sweep_df = bind_rows(params_sweep_df,params_df)
}

fred_sweep_df = fred_sweep_df %>% group_by(seed,state_name) %>% mutate(CumCF = cumsum(CF_mean)) %>% ungroup()


##===============================#
## PLOT Shelter by state --------
##===============================
col_palette = brewer.pal(n = 7, name='Dark2')
jpeg('../figures/report_figure_calibrated_parameters.jpeg', width=7,height=5, units = "in", res = 300)
layout(matrix(1:4,nrow = 2, byrow = T))
par(mar = c(1,3,3,2), oma = c(1,4,0,1))
particles_sampled_df = tibble()

state_fred = fred_sweep_df %>%    
    group_by(intervention_name, seed, state_name) %>%
    summarize(H_sheltering_mean = max(H_sheltering_mean),
              shelter_in_place_compliance = mean(shelter_in_place_compliance),
              influenza_transmissibility = mean(influenza_transmissibility),
              imports_factor = mean(imports_factor),
              influenza_asymp_infectivity = mean(influenza_asymp_infectivity)
              ) %>%
        ungroup()%>%
    filter(intervention_name %in% c("ShelterAll"))


plot(-100, -100, xlim = c(0.5, nrow(interventions_df) + 0.5), 
     ylim = c(min(state_fred$shelter_in_place_compliance, na.rm = T),
              max(state_fred$shelter_in_place_compliance, na.rm = T)),
     las = 2, xaxt = 'n', xlab = '', ylab = '')
mtext(side = 2, "Proportion of houses sheltering", line = 3, cex = 0.7)
axis(side = 1, at = 1:nrow(interventions_df), labels = interventions_df$State, cex.axis = 0.6)
for(nn in 1:nrow(interventions_df)){
    print(interventions_df$State[nn])
    shelter_data = filter(state_fred, state_name == interventions_df$State[nn]) %>% pull(shelter_in_place_compliance)
    boxplot(x = shelter_data,
            at = nn, add = T,
            col = sprintf("%s80",col_palette[nn]),
            outcol = col_palette[nn],
            outpch = 16,
            border = col_palette[nn],
            medcol = col_palette[nn],
            pars = list(xaxt = 'n', yaxt = 'n', axes = F), lwd = 1)
    print(quantile(shelter_data, probs=c(0.025,0.5,0.975)))        
}

plot(-100, -100, xlim = c(0.5, nrow(interventions_df) + 0.5), 
     ylim = c(min(state_fred$influenza_transmissibility, na.rm = T),
              max(state_fred$influenza_transmissibility, na.rm = T)),
     las = 2, xaxt = 'n', xlab = '', ylab = '')
mtext(side = 2, "Effective transmissibility", line = 3, cex = 0.7)
axis(side = 1, at = 1:nrow(interventions_df), labels = interventions_df$State, cex.axis = 0.6)
for(nn in 1:nrow(interventions_df)){
    print(sprintf("Transmissibility %s", interventions_df$State[nn]))
    shelter_data = filter(state_fred, state_name == interventions_df$State[nn]) %>% pull(influenza_transmissibility)
    boxplot(x = shelter_data,
            at = nn, add = T,
            col = sprintf("%s80",col_palette[nn]),
            outcol = col_palette[nn],
            outpch = 16,
            border = col_palette[nn],
            medcol = col_palette[nn],
            pars = list(xaxt = 'n', yaxt = 'n', axes = F), lwd = 1)
    print(quantile(shelter_data, probs=c(0.025,0.5,0.975)))        
}


plot(-100, -100, xlim = c(0.5, nrow(interventions_df) + 0.5), 
     ylim = c(min(state_fred$imports_factor, na.rm = T),
              max(state_fred$imports_factor, na.rm = T)),
     las = 2, xaxt = 'n', xlab = '', ylab = '', cex = 0.5)
mtext(side = 2, "Scaling factor of importations", line = 3, cex = 0.7)
axis(side = 1, at = 1:nrow(interventions_df), labels = interventions_df$State, cex.axis = 0.6)
for(nn in 1:nrow(interventions_df)){
    print(sprintf("Imports %s",interventions_df$State[nn]))
    shelter_data = filter(state_fred, state_name == interventions_df$State[nn]) %>% pull(imports_factor)
    boxplot(x = shelter_data,
            at = nn, add = T,
            col = sprintf("%s80",col_palette[nn]),
            outcol = col_palette[nn],
            outpch = 16,
            border = col_palette[nn],
            medcol = col_palette[nn],
            pars = list(xaxt = 'n', yaxt = 'n', axes = F), lwd = 1)
    print(quantile(shelter_data, probs=c(0.025,0.5,0.975)))        
}



plot(-100, -100, xlim = c(0.5, nrow(interventions_df) + 0.5), 
     ylim = c(min(state_fred$influenza_asymp_infectivity, na.rm = T),
              max(state_fred$influenza_asymp_infectivity, na.rm = T)),
     las = 2, xaxt = 'n', xlab = '', ylab = '', cex = 0.5)
mtext(side = 2, "Asymptomatic infectivity", line = 3, cex = 0.7)
axis(side = 1, at = 1:nrow(interventions_df), labels = interventions_df$State, cex.axis = 0.6)
for(nn in 1:nrow(interventions_df)){
    print(sprintf("Asymptomatic infectivity %s",interventions_df$State[nn]))
    shelter_data = filter(state_fred, state_name == interventions_df$State[nn]) %>% pull(influenza_asymp_infectivity)
    boxplot(x = shelter_data,
            at = nn, add = T,
            col = sprintf("%s80",col_palette[nn]),
            outcol = col_palette[nn],
            outpch = 16,
            border = col_palette[nn],
            medcol = col_palette[nn],
            pars = list(xaxt = 'n', yaxt = 'n', axes = F), lwd = 1)
    print(quantile(shelter_data, probs=c(0.025,0.5,0.975)))        
}

mtext("Calibrated parameters", side = 3, outer = T, line = -2)

dev.off()


