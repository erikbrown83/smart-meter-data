####define helper functions for taking account info and smart meter data via the Octopus API

#create a function for connecting to octopus api, accessing the required content and returning a data frame:
get_octopus_api <- function(url, key){
  
  require(httr)
  require(jsonlite)
  
  html <- GET(url, authenticate(key, ""))
  
  as.data.frame(fromJSON(content(html,
                                 "text", encoding = "UTF-8"))$results)
  
}

#function for updating and saving a file containing smart meter consumption data:

get_consumption_data <- function(consumption_url = "https://api.octopus.energy/v1/electricity-meter-points/1800051384103/meters/20M0071263/consumption/",
                                 api_key = keyring::key_get("smart-meter", "octopus"),
                                 output_path = here::here("data", "consumption-data.csv"),
                                 meter_install_date = "2022-05-24"){
  
  require(tidyverse)
  require(lubridate)
  require(httr)
  require(jsonlite)
  
  if(file.exists(output_path)){
    
    consumption_to_date <- read_csv(output_path)
    
    start_date <- max(consumption_to_date$interval_start)
    
  } else{
    
    start_date <- as.POSIXct(meter_install_date)
    
  }
  
  #query the octopus api to take the consumption data - note that we combine the base url with a few additional headers
  #so that we take data from the current date:
  consumption_data <- get_octopus_api(url = paste0(consumption_url, 
                                                   "?page_size=10000&period_from=",
                                                   format(start_date, "%Y-%m-%dT%H:%M:%SZ"),
                                                   "&period_to=", 
                                                   format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ")), 
                                      key = api_key)
  
  #convert timestamps
  consumption_data <- consumption_data %>% mutate(across(interval_start:interval_end, ~ymd_hms(.x))) %>% 
                                           arrange(interval_start)
  
  consumption_data <- consumption_data %>% mutate(date = as.Date(interval_start),
                                                  time = format(interval_start, "%H:%M"))
  
  #combine with previous data and or save output:
  
  if(exists("consumption_to_date")){
    
    output <- consumption_to_date %>% mutate(time = as.character(time)) %>% 
                                      bind_rows(consumption_data) %>% 
                                      distinct()
    
  } else{
    
    output <- consumption_data
    
  }
  
  #write output to file:
  write_csv(output, output_path)
  
}

#function for getting tariff details:

get_tariff <- function(url_standing_charge = "https://api.octopus.energy/v1/products/VAR-19-04-12/electricity-tariffs/E-2R-VAR-19-04-12-N/standing-charges/",
                       url_night_unit = "https://api.octopus.energy/v1/products/VAR-19-04-12/electricity-tariffs/E-2R-VAR-19-04-12-N/night-unit-rates/",
                       url_day_unit = "https://api.octopus.energy/v1/products/VAR-19-04-12/electricity-tariffs/E-2R-VAR-19-04-12-N/day-unit-rates/",
                       api_key = keyring::key_get("smart-meter", "octopus")){
  
  require(tidyverse)
  require(lubridate)
  
  #combine urls into one vector:
  tariff_links <- c(url_standing_charge, url_day_unit, url_night_unit)
  
  #use to take our unit price data:
  tariff_data <- tariff_links %>% map_df(~get_octopus_api(.x, key = api_key) %>% mutate(id = .x))

  #convert urls to nice labels:
  tariff_data <- tariff_data %>% mutate(id = case_when(id == url_standing_charge ~ "Standing Charge",
                                                       id == url_night_unit ~ "Night rate",
                                                       id == url_day_unit ~ "Day rate",
                                                       TRUE ~ "???"))
  
  tariff_data
                
  }
