# get_market_names.R ------------------------------------------------------
# Goal: to identify the index numbers and names of previous PredictIt markets
#       and to save results to file for use in other scraping activities

# Load libraries
library(tidyverse)
library(config)

# Pull the configuration file that includes all file paths and other key vars
cfig_fn <- 'C:/Users/jondo/OneDrive/Documents/Coding/config/pi_config.yml'
config <- config::get(file = cfig_fn)

market_id <- '7905'

hm <- jsonlite::fromJSON(paste0("https://www.predictit.org/api/marketdata/markets/", market_id))
# Pulls market names from PredictIt API
get_names_fun <- function(x){
  try <- tryCatch(jsonlite::fromJSON(paste0(config$pi_market_url, x))$shortName,
                  error = function(e) NULL)
  Sys.sleep(sleep_time)
  if(identical(try, NULL)){
    return(cbind(x, NA_character_))
  } else{
    return(cbind(x, try))
  }
}

# Get as many names as we can, save to file
# It will eventually time out and stop returning results
names_all <- lapply(search_nums, function(x) tryCatch(get_names_fun(x), error = function(e) NULL))
names_new <- data.frame(do.call(rbind, names_all)) %>%
  rename(id = 1, market_name = 2) %>%
  mutate(id = as.integer(id)) %>%
  inner_join(new_marks %>% select(-market_name), by = 'id') %>%
  mutate(ntry = ntry + 1) %>%
  filter(id <= 7894)

# Get some stats for curiosity's sake
table(is.na(names_new$market_name))
prop.table(table(is.na(names_new$market_name)))

# Make final output dataframe
out <- new_marks %>%
  anti_join(names_new, by = c('id')) %>%
  bind_rows(names_new)

# Write to file
saveRDS(out, config$markets_fn)

# Overall stats
table(is.na(out$market_name))
prop.table(table(is.na(out$market_name)))
