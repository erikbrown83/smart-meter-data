
library(tidyverse)
library(lubridate)
library(tsibble)
library(feasts)
library(fable)

#set ggplot2 theme:
theme_set(theme_light())

#load functions for accessing smart meter data via octopus api:
source(here::here("scripts", "api-functions.R"))

#load current tariff data:
current_tariff_data <- get_tariff() %>% filter(is.na(valid_to))

#create a reference table for night/day rates - we assume that the night rate applies between 8pm and 8am:
tariff_ref <- tibble(time = seq.POSIXt(floor_date(Sys.time(), unit = "day"),
                                       ceiling_date(Sys.time(), unit = "day"),
                                       "30 mins")) %>%
                mutate(tariff = ifelse(hour(time) < 8 | hour(time) >= 20, 
                                       current_tariff_data$value_exc_vat[current_tariff_data$id == "Night rate"], 
                                       current_tariff_data$value_exc_vat[current_tariff_data$id == "Day rate"]),
                       time = format(time, "%H:%M:%S"))

#update and then load consumption data:
consumption_data <- get_consumption_data()

#combine with tariff data:
consumption_data <- consumption_data %>% left_join(tariff_ref, by = c("time")) %>% 
                                         mutate(cost = ((tariff * consumption) / 100) * 1.05) #convert to £ and apply VAT

#calculate daily consumption and cost
daily_consumption <- consumption_data %>% group_by(date,
                                                   day = wday(date, week_start = 1, label = T)) %>% 
                                          summarise(consumption = sum(consumption),
                                                    cost = sum(cost),
                                                    timepoints = n()) %>%
                                          ungroup()


#convert to a tsibble:
daily_ts <- daily_consumption %>% filter(timepoints >= 48) %>% tsibble(index = date)

daily_ts %>% autoplot(consumption)
