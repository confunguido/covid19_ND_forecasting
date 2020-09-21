##=====================================================#
## Format FRED output for cdc
## forecasting challenge
## Author: Guido España, Anneliese Wieler, Sean Cavany
## 2020
##=====================================================#
## Setup-------------
##=====================================================#
library(dplyr)
library(tidyverse)
library(RColorBrewer)
library(lubridate)

get_quantiles <- function(df_in, dd, p_in, var_in){
    df_in = filter(df_in, Date == dd) %>% pull(var_in)
    tmp_quantile = quantile(df_in, probs = p_in)
    names(tmp_quantile) = p_in
    return(tmp_quantile)
}

submission_date = "2020-06-08"
args = (commandArgs(TRUE))
if(length(args) >= 1){
    submission_date = args[1]
}
submission_date = as.Date(submission_date)
last_week_date = submission_date - wday(submission_date)
max_forecast_date = last_week_date + 7*6 + 1
print(max_forecast_date)

##===============================#
## Interventions data------------
##===============================#
interventions_df = read_csv('../../experiments/input_files/interventions_covid_timeseries.csv') %>% filter(State != "NY", State != "PA")

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


fit_data = read_csv('../../experiments/Midwest_simulations/output/CALIBRATION/US_states_covid_data.csv') %>%
    filter(state %in% interventions_df$state_name) %>%
    left_join(interventions_df, by = c("state" =  "state_name")) %>%
    group_by(State) %>%
    mutate(cases_inc = c(cases[1], diff(cases)),
           deaths_inc = c(deaths[1], diff(deaths))) %>%
    ungroup() %>%
    arrange(date)


## Only do cumulative deaths after the maximum fitting date
fred_sweep_df = fred_sweep_df %>%
    left_join(dplyr::select(fit_data, c("State", "date", "deaths_inc")), by = c("state_name" = "State", "Date" = "date")) %>%
    mutate(CF_postfit = ifelse(Date <= max(fit_data$date), deaths_inc, CF_mean))%>%
    replace_na(list(CF_postfit = 0)) %>%
    group_by(seed,state_name) %>% arrange(Date) %>%
    mutate(CumCF = cumsum(CF_postfit)) %>% ungroup() 
    

##===============================#
## INC  Deaths by state --------
##===============================#
# make state into fips
fips_codes = read_csv('fips_codes.csv')
fips = fips_codes %>%
  dplyr::select(state, state_code, state_name) %>%
  unique()

p_quantiles = c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)
target_str = c("inc death", "cum death")
var_list = c("CF_mean", "CumCF")

cdc_formatted_df = data.frame(stringsAsFactors = F)

for(tt in 1:length(target_str)){
    for(nn in 1:nrow(interventions_df)){
        ss = interventions_df$State[nn]
        print(ss)
        st_intervention_tmp = filter(interventions_df, State == ss)
        
        for(interv_sc in c("ShelterAll")){
            state_fred = filter(fred_sweep_df, intervention_name == interv_sc, state_name == ss, Date > last_week_date) %>%
                mutate(var_out = !!sym(var_list[tt]),
                       Week = as.numeric(floor((Date - min(Date))/7)))

            if(var_list[tt] == "CumCF"){
                weekly_state_fred = state_fred %>%
                    group_by(state_name, intervention_name, Week, job_id) %>%
                    summarize(Date = Date[7], var_out = var_out[7]) %>%
                    drop_na()
            }else{
                weekly_state_fred = state_fred %>%
                    group_by(state_name, intervention_name, Week, job_id) %>%
                    summarize(Date = Date[7], var_out = sum(var_out)) %>%
                    drop_na()
            }
            dates_tmp = unique(state_fred$Date)
            tmp_df = as.data.frame(t(sapply(dates_tmp, function(x){get_quantiles(state_fred, x, p_quantiles,'var_out')}))) %>%
                mutate(Date = dates_tmp, state_name = ss, intervention_name = interv_sc) %>%
                gather(key = "quantile", value = "death", -c(Date, intervention_name, state_name)) %>% 
                mutate(target = sprintf("%d day ahead %s", as.integer(Date - submission_date), target_str[tt]))

            dates_tmp = unique(weekly_state_fred$Date)
            week_tmp_df = as.data.frame(t(sapply(dates_tmp, function(x){get_quantiles(weekly_state_fred, x, p_quantiles,'var_out')}))) %>%
                mutate(Date = dates_tmp, state_name = ss, intervention_name = interv_sc) %>%
                gather(key = "quantile", value = "death", -c(Date, intervention_name, state_name)) %>% 
                mutate(target = sprintf(
                           "%d wk ahead %s",
                           floor(as.integer(Date - min(Date))/7)+1, target_str[tt]))
            
            cdc_formatted_df = bind_rows(cdc_formatted_df, week_tmp_df)            
        }
    }      
}

## remove columns not needed for analysis
model_predictions_out = cdc_formatted_df %>%
    filter(intervention_name == "ShelterAll") %>%
    dplyr::select(state_name, Date, quantile, death, target) %>%
    ## only take forecasts after the forecast date
    filter(Date > submission_date, Date <= max_forecast_date) %>%
    ## rename columns to match format
    rename(location_name_abbr = state_name, target_end_date = Date, value = death) %>%
    mutate(forecast_date = submission_date, type = "quantile") %>%
    left_join(fips, by = c("location_name_abbr" = "state")) %>%    
    rename(location = state_code, location_name = state_name) %>%
    dplyr::select(forecast_date, target, location, target_end_date, type, quantile, value)

point_estimate_out = filter(model_predictions_out, quantile == 0.5) %>%
    mutate(type = "point", quantile = NA)

model_predictions_out = bind_rows(model_predictions_out, point_estimate_out) %>%
    arrange(target, location, target_end_date)

##===============================#
## Group by epi week --------
##===============================#
write_csv(model_predictions_out, sprintf('../output/%s-NotreDame-FRED.csv',submission_date))


## ### For alex-----------------
## test_df = read_csv('../output/2020-06-01-NotreDame-FRED.csv')
## load('./forecast_states_20200601.RData')

## df.forecast = dplyr::select(df.forecast, -location) %>%
##     mutate(location_name = as.character(location_name))
## fips_codes = read_csv('fips_codes.csv')
## fips = fips_codes %>%
##     dplyr::select( state_code, state_name) %>%
##     rename(location = state_code, location_name = state_name) %>%
##     unique()

## formatted_df = left_join(df.forecast, fips, by = "location_name") %>%
##     dplyr::select(forecast_date, target, location, location_name, target_end_date, type, quantile, value)

