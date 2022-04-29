# Load libraries
library(tidyverse)
library(forecast)
library(jsonlite)
library(tidyverse)
devtools::load_all()

# Location to download temp file
temp <- paste0(tempdir(), '/tmpfile.zip')

# The market ID for this weeks' contest
market_id <- '7797'

# Links for polls and overall averages
polls_lnk <- "https://projects.fivethirtyeight.com/biden-approval-data/approval_polllist.csv"
ave_lnk <- "https://projects.fivethirtyeight.com/biden-approval-data/approval_topline.csv"
prices_lnk <- paste0('https://www.predictit.org/api/marketdata/markets/', market_id)

# Date we are trying to predict
model_start <- as.Date('2021-07-01')
pred_date <- as.Date('2022-03-19')

# Date formatting pattern
dt_fmt = "%m/%d/%Y"
dt_time_fmt = "%H:%M:%S %d %b %Y"

# Get polling average by day
ave <- data.table::fread(ave_lnk) %>% filter(subgroup == 'All polls') %>%
  mutate(modeldate = as.Date(modeldate, format = dt_fmt),
         timestamp = as.POSIXct(timestamp, format = dt_time_fmt)) %>%
  group_by(modeldate) %>%
  summarize_all(last)

# Get current prices
prices <- jsonlite::fromJSON(prices_lnk)$contracts %>%
  select(id, dateEnd, name, price_yes = bestBuyYesCost, price_no = bestBuyNoCost) %>%
  mutate(val_low = case_when(grepl('lower', name) ~ 0,
                             TRUE ~ as.numeric(stringr::str_extract(name, ".*(?=\\% (to|or))"))),
         val_high = case_when(grepl('lower', name) ~ as.numeric(stringr::str_extract(name, ".*(?=\\%)")),
                              grepl('higher', name) ~ 1,
                              TRUE ~ as.numeric(stringr::str_extract(name, "(?<= to ).*(?=\\%)")))) %>%
  arrange(val_low)

# Get data, do manipulations ----------------------------------------------
# Get bootstrapped predictions for the target date
preds <- get_bootstrap_preds(input_dates = ave$modeldate, input_values = ave$approve_estimate,
                             predict_date = pred_date,  date_start = model_start,
                             num_samps = 10000, include_drift = TRUE)

# Get all probabilities by price
probs <- data.frame(t(mapply(range_fun, min_range = prices$val_low, max_range = prices$val_high, MoreArgs = list(preds = preds$preds)))) %>%
  mutate(price_no = as.numeric(prices$price_no),
         price_yes = as.numeric(prices$price_yes),
         prob_no = unlist(prob_no),
         prob_yes = unlist(prob_yes)) %>%
  mutate(rat_no = round(prob_no/price_no, 2),
         rat_yes = round(prob_yes/price_yes, 2)) %>%
  select(min_range, max_range, price_yes, prob_yes, rat_yes, price_no, prob_no, rat_no) %>%
  mutate_all(as.numeric)

# Plot it
ggplot(preds$resids, aes(x = date, y = err)) + geom_line() + geom_smooth()
# ggplot(preds$preds, aes(x = err_cast)) + geom_density()
