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

##===============================#
## Process output-------------
##===============================#
interventions_df = read_csv('../../experiments/input_files/interventions_covid_timeseries.csv') %>%
    filter(State != 'NY')

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
max_date = '2020-06-15'
col_palette = brewer.pal(n = 7, name='Dark2')
jpeg('../figures/report_figure_reproduction_number.jpeg', width=7,height=5, units="in", res = 300)
par(mar = c(3,2,3,1), oma = c(4,4,0,1))
layout(matrix(c(1,1,2,2,3,3,4,4,8,5,5,6,6,7,7,8),nrow = 2, byrow = T))
particles_sampled_df = tibble()
times_to_plot = seq(from=as.Date('2020-02-06'),to=as.Date(max_date), by = 1)
x_inds = seq(from=times_to_plot[1],to=times_to_plot[length(times_to_plot)],length.out = 8)
xlab_str = gsub(" +", " ", format(x_inds, "%b %e"))

for(nn in 1:nrow(interventions_df)){
    ss = interventions_df$State[nn]
    print(ss)
    st_intervention_tmp = filter(interventions_df, State == ss)
    state_fred = filter(fred_sweep_df, state_name == ss)   
    interv_sc = "ShelterAll"
    intervention_fred = filter(state_fred, intervention_name == interv_sc )
    intervention_end = intervention_fred$shelter_in_place_duration_mean[1] + intervention_fred$shelter_in_place_delay_mean[1]
    
    tmp_fred = intervention_fred %>%
        group_by(Day, Date, start_date, school_closure_day, shelter_in_place_delay_mean) %>%
        summarize(RR_median = quantile(RR_mean, probs = c(0.5)),
                  RR_low = quantile(RR_mean, probs = c(0.025)), RR_high = quantile(RR_mean, probs = c(0.975))) %>%
        ungroup()

    plot(tmp_fred$Date, tmp_fred$RR_median, xaxs = "i", yaxs = "i", type = "l", lwd = 2, col = "#00000090",
         xlab = "", ylab = "", xlim = c(times_to_plot[1],times_to_plot[length(times_to_plot)]),
         ylim = c(0,6), xaxt = 'n', yaxt = 'n')
    mtext(side = 3, text = ss)
    axis(side = 2, las = 2, at = seq(0, max(intervention_fred$RR_mean), length.out = 6),
         labels = signif(round(seq(0, max(intervention_fred$RR_mean), length.out = 6)), 2))

    abline(v = as.Date(tmp_fred$start_date[1] + tmp_fred$school_closure_day[1] ), lty = 2, col = "navy", lwd = 1.5)
    abline(v = as.Date(tmp_fred$start_date[1] + tmp_fred$shelter_in_place_delay_mean[1]), lty = 3, col = "navy", lwd = 1.5)
    
    polygon(x = c(tmp_fred$Date, rev(tmp_fred$Date)),
            y = c(tmp_fred$RR_high,
                    rev(tmp_fred$RR_low)),
            border = adjustcolor('black', alpha.f = 0.7),
            col = adjustcolor('black', alpha.f = 0.2))    
    lines(tmp_fred$Date, tmp_fred$RR_median, lwd = 1.5, 
          col = adjustcolor('black', alpha.f = 0.2)) 
    
    interv_sc = "Shelter_0"
    intervention_fred = filter(state_fred, intervention_name == interv_sc) %>%
        filter(Date >= times_to_plot[1], Date <= times_to_plot[length(times_to_plot)])

    intervention_end = intervention_fred$shelter_in_place_duration_mean[1] + intervention_fred$shelter_in_place_delay_mean[1]
    
    
    tmp_fred = intervention_fred %>%
        group_by(Day, Date, start_date, school_closure_day, shelter_in_place_delay_mean) %>%
        summarize(RR_median = quantile(RR_mean, probs = c(0.5)), RR_low = quantile(RR_mean, probs = c(0.025)), RR_high = quantile(RR_mean, probs = c(0.975))) %>%
        ungroup()
    
    polygon(x = c(tmp_fred$Date, rev(tmp_fred$Date)),
              y = c(tmp_fred$RR_high,
                    rev(tmp_fred$RR_low)),
            border = adjustcolor(col_palette[nn],alpha.f = 0.9),
            col = adjustcolor(col_palette[nn],alpha.f = 0.5))
    lines(tmp_fred$Date, tmp_fred$RR_median, lwd = 1.5, col = col_palette[nn]) 
    
    axis(side = 1, at = x_inds, labels = xlab_str, las = 2)        
    abline(v = as.Date(intervention_fred$start_date[1] + intervention_end), col = "navy", lwd = 1.5)
    abline(h = 1, lwd = 1.5, col = "navy")
}

mtext("Reproduction number", side = 2, line = 2, outer = T)
mtext("Date", side = 1, line = 2, outer = T)

dev.off()

