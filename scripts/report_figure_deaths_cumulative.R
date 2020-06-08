##===============================#
## Get msa names
## Author: Guido España & Rachel Oidtman
## 2020
##===============================#
## Setup-------------
##===============================#
library(dplyr)
library(tidyverse)
library(RColorBrewer)
library(wesanderson)
library(lubridate)
library(vioplot)

max_date = '2020-06-15'
args = (commandArgs(TRUE))
if(length(args) >= 1){
    max_date = args[1]
}
max_date = as.Date(max_date)
print(max_date)
##===============================#
## Data from NYT or JHU------------
##===============================#
interventions_df = read_csv('../../experiments/input_files/interventions_covid_timeseries.csv') %>%
    filter(State != "NY", State != "PA")

data_source = "JHU"
data_sys = system(sprintf("Rscript ./download_covid_data.R %s", data_source), intern = TRUE)

if(!is.null(attr(data_sys, "status"))){
    stop("Something went wrong with downloading incidence data\n")
}

if(!file.exists('../../data/US_states_covid_data.csv')){
    stop("Something went wrong with downloading incidence data\n")
}
us_states_temp = read_csv('../../data/US_states_covid_data.csv')

us_states = us_states_temp %>%
    filter(state %in% interventions_df$state_name) %>%
    left_join(interventions_df, by = c("state" = "state_name")) %>%
    group_by(State) %>%
    mutate(cases_inc = c(cases[1], diff(cases)),
           deaths_inc = c(deaths[1], diff(deaths))) %>%
    ungroup()

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
# fred_sweep_df = fred_sweep_df %>% drop_na

##===============================#
## Necessary functions-----------
##===============================#

# get sunday of each week for labeling
yearSunday <- function(year) {
  dates <- as.Date(paste(year, "-01-01", sep = "")) + 0:6
  days <- weekdays(dates) == "Sunday"
  seq(dates[days], as.Date(paste(year, "-12-31", sep = "")),  by = "+7 day")
}

##===============================#
## Cumulative deaths ------------
##===============================#
fit_date = max(fit_data$date)
intervention_scenarios = c('ShelterAll')
CF_fit = fred_sweep_df %>% group_by(intervention_name, state_name, seed) %>% filter(Date < fit_date) %>% summarize(CF_mean = sum(CF_mean, na.rm = T)) %>% ungroup() %>% filter(intervention_name %in% intervention_scenarios)
CF_forecast = fred_sweep_df %>% group_by(intervention_name, state_name, seed) %>% filter(Date < max_date, Date >= fit_date) %>% summarize(CF_mean = sum(CF_mean, na.rm = T)) %>% ungroup() %>% filter(intervention_name %in% intervention_scenarios)

# number of reps
num_reps = 500
states = interventions_df$State

##=========================================#
## Cumulative death plot combined----------
##=========================================#
cols = brewer.pal(n = 7, name='Dark2')
f = '../figures/report_figure_deaths_cumulative.jpeg'
jpeg(file = f, height = 3 , width = 7, res = 300, units = 'in')

inds_to_plot = c(1:length(states))
layout(matrix(1:2, nrow = 1))
par(mar = c(1,3,2,1), oma = c(1, 2, 2,0))

plot(-100, -100, xlim = c(0.5, length(states)+0.5), 
     ylim = c(min(min(CF_fit$CF_mean, na.rm = T), min(CF_forecast$CF_mean, na.rm = T), na.rm = T),
              max(max(CF_fit$CF_mean, na.rm = T), max(CF_forecast$CF_mean, na.rm = T), na.rm = T)),
     las = 2, xaxt = 'n', xlab = '', ylab = '',cex.axis=0.8)
mtext(side = 2, "Deaths", line = 3.5)
mtext(side = 3, sprintf("Cumulative deaths through %s %d",as.character(month(as.Date(fit_date),label = T)), day(as.Date(fit_date))), line = 1)
axis(side = 1, at = inds_to_plot, labels = states,cex.axis=0.8)
for(ss in 1:length(states)){
  x_to_plot = inds_to_plot[ss]
  CF_fit_state = filter(CF_fit, state_name == interventions_df$State[ss])
  
  boxplot(x = CF_fit_state$CF_mean,
          at = x_to_plot, add = T,
          col = adjustcolor(cols[ss], alpha.f = 0.5),
          outcol = cols[ss],
          outpch = 16,
          border = cols[ss],
          medcol = cols[ss], 
          pars = list(xaxt = 'n', yaxt = 'n', axes = F))
  
  tmp_data = filter(us_states, State == states[ss])
  points(x = inds_to_plot[ss],
         y = tmp_data$deaths[which(tmp_data$date == as.Date(fit_date))],
         pch = 23, cex = 1.5, col = 'navy', bg= 'navy')
}

# FORECAST
plot(-100, -100, xlim = c(0.5, length(states)+0.5), 
     ylim = c(min(CF_forecast$CF_mean, na.rm = T),
              max(CF_forecast$CF_mean, na.rm = T)),
     las = 2, xaxt = 'n', xlab = '', ylab = '', yaxt = 'n')


mtext(side = 3, sprintf("Cumulative deaths from %s %d\n through %s %d",
                        as.character(month(as.Date(fit_date),label = T)), day(as.Date(fit_date)),
                        as.character(month(as.Date(max_date),label = T)), day(as.Date(max_date))), line=1)
axis(side = 1, at = inds_to_plot, labels = states,cex.axis=0.8)
axis(2,las = 2, cex.axis = 0.8)
## axis(side = 2, at = seq(0, max(CF_forecast$CF_mean), length.out = 5), 
##      las = 1,cex.axis=0.8)
for(ss in 1:length(states)){
    x_to_plot = inds_to_plot[ss]
    CF_forecast_state = filter(CF_forecast, state_name == interventions_df$State[ss])
  
    boxplot(x = CF_forecast_state$CF_mean,
            at = x_to_plot, add = T,
            col = adjustcolor(cols[ss], alpha.f = 0.5),
            outcol = cols[ss],
            outpch = 16,
            border = cols[ss],
            medcol = cols[ss], 
            pars = list(xaxt = 'n', yaxt = 'n', axes = F))
 
}

dev.off()
