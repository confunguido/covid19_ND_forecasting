#!/usr/bin/env Rscript
##=========================================#
## Author: Guido Espana
## Collect data from param sweep
## Year: 2019
## 
## requires:
##          densimtools library
##=========================================#
## Setup----------------
##=========================================#
library(tidyverse)
library(lubridate)

tidy_jhu_deaths <- function(df) {
    df_out = df %>% mutate(Population = 0) %>%
        dplyr::select(-UID, -iso2, -iso3, -code3, -Admin2, -FIPS,
                      -Lat, -Long_, -Country_Region, -Combined_Key, -Population) %>%
        gather(key = "date_str", value = deaths, - Province_State) %>%
        group_by(Province_State, date_str) %>%
        summarize(deaths = sum(deaths, na.rm = T))  %>%
        ungroup() %>%
        rename(state = Province_State) %>%
        mutate(date = mdy(date_str)) %>%
        select(state, date, deaths) %>%
        arrange(state, date)
    return(df_out)
}
tidy_jhu_cases <- function(df) {
    df_out = df %>% mutate(Population = 0) %>%
        dplyr::select(-UID, -iso2, -iso3, -code3, -Admin2, -FIPS,
                      -Lat, -Long_, -Country_Region, -Combined_Key, -Population) %>%
        gather(key = "date_str", value = cases, - Province_State) %>%
        group_by(Province_State, date_str) %>%
        summarize(cases = sum(cases, na.rm = T))  %>%
        ungroup() %>%
        rename(state = Province_State) %>%
        mutate(date = mdy(date_str)) %>%
        select(state, date, cases) %>%
        arrange(state, date)
    return(df_out)
}
##=========================================#
## User's input----------------
##=========================================#
args = (commandArgs(TRUE))
data_source = "JHU"
if(length(args) >= 1){
    data_source = args[1]
}
if(!data_source %in% c("JHU", "NYT")) {
    stop("Please enter either JHU or NYT for data_source")
}

if (data_source == "NYT"){
    download.file('https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv',
                  '../../data/US_states_covid_nytimes.csv')
} else if (data_source == "JHU") {
    download.file('https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv',
                  '../../data/US_states_covid_nytimes.csv')
    download.file('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv',
                  '../../data/US_covid_cases_JHU.csv')
    download.file('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv',
                  '../../data/US_covid_deaths_JHU.csv')
}

if(data_source == "NYT" & !file.exists('../../data/US_states_covid_nytimes.csv')){
    stop("Incidence data not found")
}
if(data_source == "JHU" & !(file.exists('../../data/US_covid_cases_JHU.csv') &
                            file.exists('../../data/US_covid_deaths_JHU.csv') &
                            file.exists('../../data/US_states_covid_nytimes.csv'))){
    stop("Incidence data not found")
}


if (data_source == "NYT") {
    us_states_temp = read_csv('../../data/US_states_covid_nytimes.csv')
} else if (data_source == "JHU") {
    fips_codes = read_csv('../../data/US_states_covid_nytimes.csv') %>%
        select(state, fips) %>%
        distinct()
    us_cases_raw = read_csv('../../data/US_covid_cases_JHU.csv')
    us_deaths_raw = read_csv('../../data/US_covid_deaths_JHU.csv')
    us_states_temp <- tidy_jhu_cases(us_cases_raw) %>%
        full_join(tidy_jhu_deaths(us_deaths_raw)) %>%
        full_join(fips_codes) %>%
        filter(cases != 0 | deaths != 0) # I'm not sure we should do this!!
}

write_csv(us_states_temp,'../../data/US_states_covid_data.csv')

