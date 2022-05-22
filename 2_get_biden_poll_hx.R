# get_biden_poll_hx.R ------------------------------------------------------
# Goal: to get the market prices for each day for the Biden polls

# Load libraries, read in previous market data
library(tidyverse)
library(data.table)
library(config)

# Pull the configuration file that includes all file paths and other key vars
cfig_fn <- 'C:/Users/jondo/OneDrive/Documents/Coding/config/pi_config.yml'
config <- config::get(file = cfig_fn)

# Read in previous historical data prices
prices_start <- readRDS(config$prices_fn)

# Get a list of markets
new_markets <- readRDS(config$markets_fn) %>%
  filter(grepl("Biden's 538", market_name) & !id %in% prices_start$marketId)

# Function to get daily price index
get_daily_prices <- function(x){
  lnk <- paste0(config$pi_prices_url, x, config$pi_prices_week_opts)
  return(tryCatch(jsonlite::fromJSON(lnk), error = function(e){NULL}))
}

# Run it on all data
prices <- lapply(new_markets$id, get_daily_prices)
prices2 <- do.call(rbind, prices)

# Combine new price data with existing data, save to file
prices_out <- rbind(prices_start, prices2) %>%
  arrange(marketId, contractId, date)
saveRDS(prices_out, config$prices_fn)


# Get daily market prices data ----------------------------------------
# Read in daily prices for each bracket, and a list of the market names
# Manipulate the data and pull what we need
prices <- readRDS('./data/daily_approval_prices.Rds') %>%
  left_join(readRDS('./data/markets.Rds'), by = c('marketId' = 'id')) %>%
  mutate(val_low = extract_num_range(contractName)[[1]],
         val_high = extract_num_range(contractName)[[2]],
         date = as.Date(date)) %>%
  arrange(marketId, contractId, date, val_low) %>%
  group_by(contractId) %>%
  mutate(eval_date = max(date)-1) %>%
  select(-dateString, -lineColor) %>%
  select(marketId, contractId, contractName, eval_date, date, everything()) %>%
  filter(eval_date > date) %>%
  ungroup() %>%
  arrange(date, eval_date, val_low) %>%
  group_by(date, eval_date) %>%
  mutate(bracket_number = row_number(),
         target_value = round(ave[ave$polldate == eval_date,]$approval, 3),
         is_target = target_value >= val_low & target_value <= val_high) %>%
  ungroup() %>%
  filter(eval_date >= as.Date('2021-04-01')) %>%
  group_by(date) %>%
  mutate(total_price = sum(closeSharePrice)) %>%
  ungroup()

