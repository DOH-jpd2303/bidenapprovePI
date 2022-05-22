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

# Load custom functions (including secrets)
devtools::load_all()
source(config$model_funs_fn)

# Training end date and model target date
train_end_date <- Sys.Date()
target_date <- as.Date('2022-05-25')

# Read in price data for the market
market_no <- 7920
market_lnk <- 'https://www.predictit.org/api/marketdata/markets/'
prices <- jsonlite::fromJSON(paste0(market_lnk, market_no))$contracts %>%
  mutate(poll_low = extract_num_range(name)[[1]],
         poll_high = extract_num_range(name)[[2]],
         dateEnd = as.POSIXct(dateEnd, format = '%Y-%m-%dT%H:%M:%S')-lubridate::hours(3)) %>%
  select(id, dateEnd, poll_low, poll_high,
         bestBuyYesCost, bestBuyNoCost, bestSellYesCost, bestSellNoCost) %>%
  arrange(poll_low)

# Read in 538 data, make predictions
ave <- get_538_biden_ave()

# Get the main model result
preds <- predict_polling_bracket(poll_dates = ave$polldate,
                                 poll_values = ave$approval,
                                 train_end_date = train_end_date,
                                 target_date = target_date,
                                 range_mins = prices$poll_low,
                                 range_maxes = prices$poll_high)

# Add predictions to prices dataframe
prices$yhat <- preds$probs

# Convert to long, filter out buy signals
prices_piv_cols <- c('id', 'dateEnd', 'poll_low', 'poll_high', 'yhat')
prices_long <- prices %>%
  pivot_longer(-all_of(prices_piv_cols)) %>%
  filter(grepl('Buy', name)) %>%
  mutate(pred = ifelse(grepl('Yes', name), yhat, 1 - yhat),
         pred_price_dif = pred - value)

# Get buys, calculate number of shares
spend <- 5
buys <- filter(prices_long, pred_price_dif >= 0.1) %>%
  mutate(shares = spend/value,
         dateEnd = as.POSIXct(dateEnd, format = '%Y-%m-%dT%H:%M:%S')-lubridate::hours(3))

# And here's the plot
fc <- forecast::splinef(ave$approval, h = 5, level = c(80))
plot(fc)

# Update trade hx ---------------------------------------------------------
# MANUAL: Download HX
# Read in trade history, get in final format
pi_tradehx <- data.table::fread(paste0(config$data_dir, 'TradeHistory.csv')) %>%
  mutate(DateExecuted = as.POSIXct(DateExecuted, format = '%m/%d/%Y %H:%M') - lubridate::hours(3),
         market = basename(URL)) %>%
  mutate_at(c('Price', 'ProfitLoss', 'Fees', 'Risk', 'CreditDebit'), .funs = list(money_to_number)) %>%
  filter(market == market_no)

# Join to predictions, combine with previous trade histories
trade_hxout <- buys %>%
  inner_join(pi_tradehx) %>%
  select(id, DateExecuted, Type, dateEnd, poll_low, poll_high, pred, Shares, Price, ProfitLoss, Fees, Risk, CreditDebit) %>%
  bind_rows(readRDS(paste0(config$data_dir, 'myBuys.Rds')))

# Save new trade histories to file
saveRDS(trade_hxout, paste0(config$data_dir, 'myBuys.Rds'))
