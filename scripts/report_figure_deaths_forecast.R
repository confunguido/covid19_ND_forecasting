##===============================#
## Plot deaths for forecast
## FRED-COVID19
## Author: Guido España
## 2020
##===============================#
## Setup-------------
##===============================#
library(dplyr)
library(tidyverse)
library(RColorBrewer)
library(lubridate)

forecast_date = as.Date("2020-06-15")
args = (commandArgs(TRUE))
if(length(args) >= 1){
    forecast_date = as.Date(args[1])
}
print(forecast_date)

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

##===============================#
## PLOT Deaths by state --------
##===============================
col_palette = brewer.pal(n = 7, name='Dark2')
jpeg('../figures/report_figure_deaths_forecast.jpeg', width=7,height=5, units="in", res = 300)
par(mar = c(3,2,3,1), oma = c(4,4,0,1))
layout(matrix(c(1,1,2,2,3,3,4,4,8,5,5,6,6,7,7,8),nrow = 2, byrow = T))
##layout(matrix(c(1,1,2,2,3,3),nrow = 1, byrow = T))
particles_sampled_df = tibble()
times_to_plot = seq(from=as.Date('2020-02-06'),to=forecast_date, by = 1)
x_inds = seq(from=times_to_plot[1],to=times_to_plot[length(times_to_plot)],length.out = 8)
xlab_str = gsub(" +", " ", format(x_inds, "%b %e"))

for(nn in 1:nrow(interventions_df)){
    ss = interventions_df$State[nn]
    print(ss)
    st_intervention_tmp = filter(interventions_df, State == ss)
    state_fred = filter(fred_sweep_df, state_name == ss)   
    interv_sc = "ShelterAll"
    intervention_fred = filter(state_fred, intervention_name == interv_sc )
    intervention_end = as.Date(st_intervention_tmp$end_shelter_date[1])
    
    tmp_fred = intervention_fred %>%
        group_by(Day, Date, start_date, school_closure_day, shelter_in_place_delay_mean) %>%
        summarize(CF_median = quantile(CF_mean, probs = c(0.5), na.rm = T),
                  CF_low = quantile(CF_mean, probs = c(0.025), na.rm =T),
                  CF_high = quantile(CF_mean, probs = c(0.975), na.rm = T),
                  CF_IQRlow = quantile(CF_mean, probs = c(0.25), na.rm =T),
                  CF_IQRhigh = quantile(CF_mean, probs = c(0.75), na.rm = T),                  
                  Cs_median = quantile(Cs_mean, probs = c(0.5)),
                  Cs_low = quantile(Cs_mean, probs = c(0.025)), Cs_high = quantile(CF_mean, probs = c(0.975)),
                  AR_low = quantile(AR_mean, probs = c(0.025)), AR_high = quantile(AR_mean, probs = c(0.975)),
                  AR_median = quantile(AR_mean, probs = c(0.5))) %>%
        ungroup()

    plot(tmp_fred$Date, tmp_fred$CF_median, xaxs = "i", yaxs = "i", type = "l", lwd = 2, col = col_palette[nn],
         xlab = "", ylab = "", xlim = c(times_to_plot[1],times_to_plot[length(times_to_plot)]),
         ylim = c(0,max(tmp_fred$CF_high)), xaxt = 'n', yaxt = 'n')
    mtext(side = 3, text = ss)
    axis(side = 2, las = 2, at = seq(0, max(intervention_fred$CF_mean), length.out = 6),
         labels = signif(round(seq(0, max(intervention_fred$CF_mean), length.out = 6)), 2))

    abline(v = as.Date(tmp_fred$start_date[1] + tmp_fred$school_closure_day[1] ), lty = 2, col = "navy", lwd = 1.5)
    abline(v = as.Date(tmp_fred$start_date[1] + tmp_fred$shelter_in_place_delay_mean[1]), lty = 3, col = "navy", lwd = 1.5)
    
    polygon(x = c(tmp_fred$Date, rev(tmp_fred$Date)),
            y = c(tmp_fred$CF_high,
                    rev(tmp_fred$CF_low)),
            border = adjustcolor(col_palette[nn], alpha.f = 0.7),
            col = adjustcolor(col_palette[nn], alpha.f = 0.2))    
    lines(tmp_fred$Date, tmp_fred$CF_median, lwd = 1.5, 
          col = adjustcolor(col_palette[nn], alpha.f = 0.2)) 

    polygon(x = c(tmp_fred$Date, rev(tmp_fred$Date)),
            y = c(tmp_fred$CF_IQRhigh,
                    rev(tmp_fred$CF_IQRlow)),
            border = adjustcolor(col_palette[nn], alpha.f = 0.3),
            col = adjustcolor(col_palette[nn], alpha.f = 0.2))    

    tmp_data_fit = filter(fit_data, State == ss)
    tmp_fred_data = filter(tmp_fred, Date <= max(tmp_data_fit$date))
    
    axis(side = 1, at = x_inds, labels = xlab_str, las = 2)        
    abline(v = intervention_end, col = "navy", lwd = 1.5)
    
    tmp_data = filter(us_states, State == ss,date > tmp_data_fit$date[nrow(tmp_data_fit)], date < as.Date(Sys.Date()))
    points(tmp_data$date, tmp_data$deaths_inc, col = "navy", lwd = 0.3, pch = 9, cex = 0.8)
    
    points(tmp_data_fit$date, tmp_data_fit$deaths_inc, col = "navy", lwd = 0.3,pch = 18)
}

mtext("Deaths", side = 2, line = 2, outer = T)
mtext("Date", side = 1, line = 2, outer = T)

dev.off()

