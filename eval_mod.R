###############################################################################
# SETUP AND DATA READIN
###############################################################################
# Set up ------------------------------------------------------------------
# Load libraries
library(tidyverse)
library(forecast)
library(jsonlite)
library(mgcv)
options(scipen = 999)

# Pull the config file, load custom fnx
cfig_fn <- 'C:/Users/jondo/OneDrive/Documents/Coding/config/pi_config.yml'
config <- config::get(file = cfig_fn)
devtools::load_all()

################################################################################
# MODEL EVAL / IDENTIFYING STRATEGIES
################################################################################
pi_preds <- readRDS(config$pi_preds_fn)

# Morph data into long format, do some more calcs
pivot_ids <- c('marketId', 'contractId', 'eval_date', 'date', 'tradeVolume',
                'poll_low', 'poll_high', 'is_target', 'yhat', 'yhat2', 'yhat3')
longpred <- pi_preds %>%
  select(-target_value) %>%
  pivot_longer(cols = -all_of(pivot_ids)) %>%
  ungroup() %>%
  group_by(marketId, eval_date, date, name) %>%
  mutate(total_price = sum(value),
         pred_price_dif = yhat - value,
         net_value = ifelse(is_target, 0.9*(1 - value), - value),
         days_out = as.numeric(eval_date - date),
         buy_signal = pred_price_dif >= 0.1) %>%
  ungroup()

# Let's look for winners in opening prices
dollars <- 5
opens <- longpred %>%
  filter(name == 'openSharePrice')

# See how our keepers did based on opening share prices
keeps <- filter(opens, buy_signal == 1) %>%
  mutate(shares = floor(dollars/value),
         result = shares*net_value)
sum(keeps$result)
sum(keeps$result)/nrow(keeps)

