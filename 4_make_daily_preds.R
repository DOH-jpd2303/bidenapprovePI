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

# Pull the configuration file that includes all file paths and other key vars
cfig_fn <- 'C:/Users/jondo/OneDrive/Documents/Coding/config/pi_config.yml'
config <- config::get(file = cfig_fn)

# Load custom fxns
devtools::load_all()

# Read in the dataset
ave <- get_538_biden_ave()
pi_analysis <- readRDS(config$pi_analysis_fn)

# Get prediction for each row ---------------------------------------------
# Split dataframe by target date and market date
prices_split <- pi_analysis %>%
  filter(date < eval_date) %>%
  group_split(date, eval_date)

# Get bracket probabilities
preds <- data.frame()
for(i in(prices_split)){
  train_end_date <- unique(i$date)
  target_date <- unique(i$eval_date)
  tmp <- predict_polling_bracket(poll_dates = ave$polldate,
                                 poll_values = ave$approval,
                                 train_end_date = train_end_date,
                                 target_date = target_date,
                                 range_mins = i$poll_low,
                                 range_maxes = i$poll_high)
  preds <- rbind(preds, tmp)
}

# Get bracket probabilities
preds2 <- data.frame()
for(i in(prices_split)){
  train_end_date <- unique(i$date)
  target_date <- unique(i$eval_date)
  tmp <- predict_polling_bracket_arima(poll_dates = ave$polldate,
                                 poll_values = ave$approval,
                                 train_end_date = train_end_date,
                                 target_date = target_date,
                                 range_mins = i$poll_low,
                                 range_maxes = i$poll_high)
  preds2 <- rbind(preds2, tmp)
}

# Get in final format, save
prices2 <- pi_analysis %>%
  filter(date < eval_date) %>%
  bind_cols(preds) %>%
  mutate(yhat2 = preds2$probs,
         yhat3 = (probs + yhat2)/2) %>%
  select(-range_mins, -range_maxes, -s, -m, yhat = probs)
saveRDS(prices2, config$pi_preds_fn)

