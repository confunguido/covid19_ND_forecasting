rm(list = ls())
data = read_csv("../../output/COVID19_FRED_forecasting_perkinslab_20200430.csv")
library(tigris)

# date that the forecast is being submitted
forecast_date = "2020-04-26"

# remove columns not needed for analysis
data = data %>%
  filter(InterventionScenario == "ShelterAll") %>%
  dplyr::select(state_name, Date, quantile, death) %>%
  # only take forecasts after the forecast date
  filter(Date > forecast_date) %>%
  # rename columns to match format
  rename(location_name = state_name, target_end_date = Date, value = death) %>%
  mutate(forecast_date = forecast_date, type = "quantile")

# make state into fips
data(fips_codes)
fips = fips_codes %>%
  dplyr::select(state, state_code) %>%
  unique()

data = data %>%
  mutate(target = 1)

# join the fips data and data
data = data %>%
  left_join(fips, by = c("location_name" = "state")) %>%
  rename(location = state_code) %>%
  select(forecast_date, target, target_end_date, location, location_name, type, quantile, value) %>%
  mutate(quantile = as.numeric(gsub("[\\%,]", "", quantile)))

filename = paste0(forecast_date, "-NotreDame-FRED-", "FRED.csv")
write.csv(data, paste0(forecast_date, "-NotreDame-FRED-", "FRED.csv"))