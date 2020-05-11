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
library(mgcv)

shelter_ts <- function(data_in, dates_in, lm_in){
    shelter_out = rep(0, nrow(data_in))
    lm_pred = predict(lm_in)
    shelter_out[data_in$date %in% dates_in] = lm_pred
    shelter_out[data_in$date > max(dates_in)] = max(lm_pred)
    shelter_out[shelter_out < 0] = 0
    return(shelter_out)
}
max_date = '2020-06-20'
args = (commandArgs(TRUE))
if(length(args) >= 1){
    max_date = args[1]
}
##===============================#
## Read data-------------
##===============================#
interventions_df = read_csv('../../experiments/input_files/interventions_covid_timeseries.csv') 

## Update file
download.file('https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv?cachebust=911a386b6c9c230f',
              '../../data/USA_google_movement_data.csv')

if(!file.exists('../../data/USA_google_movement_data.csv')){
    stop("Incidence data not found")
}

google_data = read_csv('../../data/USA_google_movement_data.csv') %>%
    filter(sub_region_1 %in% interventions_df$state_name) %>%
    mutate(day = as.numeric(date - min(date))) %>%
    dplyr::select(-c(country_region_code, country_region, sub_region_2, day))%>%
    tidyr::gather(key = category, value = trend_mobility, -c('sub_region_1', 'date'))

ct = "residential_percent_change_from_baseline"
col_palette = brewer.pal(n = nrow(interventions_df), name='Dark2')


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

## TEMPORARY!! Fix ASAP
fred_sweep_df = fred_sweep_df %>%
    group_by(job_id,state_name) %>%
    mutate(CumCF = cumsum(CF_mean)) %>%
    ungroup()


##===============================#
## Figure output-------------
##===============================#

jpeg("../figures/report_figure_shelter_patterns.jpeg", width = 7, height = 5, units = "in", res = 300)
par(mar = c(3,2,3,1), oma = c(4,4,0,4))
layout(matrix(c(1,1,2,2,3,3,4,4,8,5,5,6,6,7,7,8),nrow = 2, byrow = T))
times_to_plot = seq(from=as.Date('2020-02-06'),to=as.Date(max_date), by = 1)
x_inds = seq(from=times_to_plot[1],to=times_to_plot[length(times_to_plot)],length.out = 6)
y_ticks = c(0,10,20,30,40,50)
y_tick_labs = y_ticks

xlab_str = gsub(" +", " ", format(x_inds, "%b %e"))
start_date = as.Date('2020-03-08')
for(ss in 1:nrow(interventions_df)){
    st = interventions_df$State[ss]
    ## FRED OUTPUT
    state_fred = filter(fred_sweep_df, state_name == st)    
    interv_sc = "ShelterAll"
    intervention_fred = filter(state_fred, intervention_name == interv_sc )
    
    tmp_fred = intervention_fred %>%
        group_by(Day, Date, start_date, school_closure_day, shelter_in_place_delay_mean) %>%
        summarize(N_sheltering_median = quantile(N_sheltering_mean, probs = c(0.5)),
                  N_sheltering_low = quantile(N_sheltering_mean, probs = c(0.025)),
                  N_sheltering_high = quantile(N_sheltering_mean, probs = c(0.975)),
                  N_median = quantile(N_mean, probs = c(0.5)),
                  shelter_in_place_compliance_median = quantile(shelter_in_place_compliance, probs = c(0.5))) %>%
        mutate(N_sheltering_prop = N_sheltering_median / N_median,
               N_sheltering_prop_low = N_sheltering_low / N_median,
               N_sheltering_prop_high = N_sheltering_high / N_median) %>%
        ungroup()


    ## Google data    
    tmp_data_base = filter(google_data, category == ct, sub_region_1 == interventions_df$state_name[ss]) %>%
        group_by(date) %>% summarize(trend_mobility = mean(trend_mobility, na.rm = T)) %>%
        ungroup() %>%
        mutate(trend_mobility_prop = trend_mobility / max(trend_mobility),
               day = row_number())

    mod_gam1 = gam(trend_mobility_prop ~ s(day), data=tmp_data_base, family=gaussian(link='identity'))
    
    tmp_data_base$predicted = as.numeric(predict(mod_gam1))

    max_gam = max(tmp_data_base$trend_mobility) * max(tmp_data_base$predicted)
    ylimits =  c(-5,50)
    plot(tmp_data_base$date, tmp_data_base$trend_mobility, type = "l", col = "gray", main = "", ylab = "",xlab = "", xaxs = "i", yaxs ="i", lwd = 3, xlim = c(times_to_plot[1],times_to_plot[length(times_to_plot)]), ylim =ylimits,xaxt = 'n', las = 2, yaxt = "n")
    if(ss %in% c(1,5)){
        axis(side = 2, at = y_ticks, labels = y_tick_labs, las = 2)
    }
    axis(side = 1, at = x_inds, labels = xlab_str, las = 2)
    

    abline(h = 0, col = "gray")
    
    lines(tmp_fred$Date, tmp_fred$N_sheltering_prop * max_gam / max(tmp_fred$N_sheltering_prop), lwd = 2, lty = 1, col = col_palette[ss])
    polygon(x = c(tmp_fred$Date, rev(tmp_fred$Date)),
            y = c(tmp_fred$N_sheltering_prop_high * max_gam / max(tmp_fred$N_sheltering_prop),
                  rev(tmp_fred$N_sheltering_prop_low * max_gam / max(tmp_fred$N_sheltering_prop))),
            border = adjustcolor(col_palette[ss], alpha.f = 0.7),
            col = adjustcolor(col_palette[ss], alpha.f = 0.2))    
    
    yticks_shelter = c(0,25,50,75,100,125,150)
    yticks2 =  yticks_shelter / (max(tmp_fred$N_sheltering_prop) * 100 / max_gam)
    
    axis(4, at = yticks2, labels = yticks_shelter, las = 2, col = "navy", col.axis = "navy")
    
    mtext(sprintf("%s", interventions_df$State[ss]))    
    abline(v = interventions_df$Shelter_in_place[ss], lwd = 1.5, col = "navy", lty = 3)
    mtext(side = 3, text = st)
}

mtext("Percentage change on mobility trends\n for places of residence", side = 2, line = 1.2, outer = T)
    mtext("Percentage of people staying at home(calibrated)",side = 4, line = 2, outer = T, col = "navy")
mtext("Date", side = 1, outer = T, line = 1.5)
dev.off()

