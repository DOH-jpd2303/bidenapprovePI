# get_biden_poll_hx.R ------------------------------------------------------
# Goal: to get the market prices for each day for the Biden polls

# Load libraries, read in previous market data
library(tidyverse)
library(data.table)
library(config)

# Pull the configuration file that includes all file paths and other key vars
cfig_fn <- 'C:/Users/jondo/OneDrive/Documents/Coding/config/pi_config.yml'
config <- config::get(file = cfig_fn)
devtools::load_all()

# Get 538 daily polling averages
ave <- get_538_biden_ave()

# Get daily market prices data ----------------------------------------
# Read in daily prices for each bracket, and a list of the market names
# Manipulate the data and pull what we need
pi_analysis <- readRDS(config$prices_fn) %>%
  left_join(readRDS(config$markets_fn), by = c('marketId' = 'id')) %>%
  mutate(poll_low = extract_num_range(contractName)[[1]],
         poll_high = extract_num_range(contractName)[[2]],
         date = as.Date(date)) %>%
  arrange(marketId, contractId, date, poll_low) %>%
  group_by(contractId) %>%
  mutate(eval_date = max(date) - 1) %>%
  filter(eval_date >= date) %>%
  ungroup() %>%
  arrange(date, eval_date, poll_low) %>%
  group_by(date, eval_date) %>%
  mutate(bracket_number = row_number(),
         target_value = round(ave[ave$polldate == eval_date,]$approval, 3),
         is_target =  round(target_value, 3) >= round(poll_low, 3) & round(target_value, 3) <= round(poll_high, 3)) %>%
  ungroup() %>%
  group_by(date) %>%
  mutate(total_price = sum(closeSharePrice)) %>%
  ungroup() %>%
  select('marketId', 'contractId', 'eval_date', 'date', 'poll_low', 'poll_high',
         'openSharePrice', 'highSharePrice', 'lowSharePrice', 'closeSharePrice',
         'tradeVolume', 'is_target', 'target_value')

# Save to file
saveRDS(pi_analysis, config$pi_analysis_fn)
