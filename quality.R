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


uhoh <- pi_preds %>%
  group_by(eval_date, date) %>%
  summarize(n = sum(is_target)) %>%
  filter(n != 1) %>%
  left_join(pi_preds) %>%
  mutate(is_target2 =  round(target_value, 3) >= round(poll_low, 3) & round(target_value, 3) <= round(poll_high, 3))
