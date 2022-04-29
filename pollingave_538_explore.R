###############################################################################
# SETUP AND DATA READIN
###############################################################################
# Set up ------------------------------------------------------------------
# Load libraries
library(tidyverse)
library(forecast)
library(jsonlite)

# This calls all custom functions I wrote in the 'R' subdirectory
devtools::load_all()

# Links for the 538 poll list and approval topline estimates
ave_lnk <- "https://projects.fivethirtyeight.com/biden-approval-data/approval_topline.csv"

# Get polling averages by day ---------------------------------------------
# Date formatting pattern from 538 files
dt_fmt = "%m/%d/%Y"
dt_time_fmt = "%H:%M:%S %d %b %Y"

# Read in/manipulate polling average data from 538 website
ave <- data.table::fread(ave_lnk) %>% filter(subgroup == 'All polls') %>%
  mutate(modeldate = as.Date(modeldate, format = dt_fmt),
         timestamp = as.POSIXct(timestamp, format = dt_time_fmt),
         approval = approve_estimate/100) %>%
  group_by(modeldate) %>%
  summarize_all(last) %>%
  select(modeldate, approval, timestamp) %>%
  filter(modeldate >= as.Date('2021-05-01'))

# Get daily market prices data ----------------------------------------
# Read in daily prices for each bracket, and a list of the market names
# Manipulate the data and pull what we need
prices <- readRDS('./data/daily_approval_prices.Rds') %>%
  left_join(data.table::fread('./data/markets.csv'), by = c('marketId' = 'id')) %>%
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
         target_value = round(ave[ave$modeldate == eval_date,]$approval, 3),
         is_target = target_value >= val_low & target_value <= val_high) %>%
  ungroup() %>%
  filter(eval_date >= as.Date('2021-09-01')) %>%
  group_by(date) %>%
  mutate(total_price = sum(closeSharePrice)) %>%
  ungroup()

# Get prediction for each row ---------------------------------------------
# Split dataframe by target date and market date
prices_split <- group_split(prices, date, eval_date)

# Start the vector of predictions
preds <- c()

for(price in(prices_split)){
  train_end_date <- unique(price$date)
  target_date <- unique(price$eval_date)
  tmp <- predict_polling_brackets(poll_dates = ave$modeldate,
                                  poll_values = ave$approval,
                                  train_end_date = train_end_date,
                                  target_date = target_date,
                                  range_mins = price$val_low,
                                  range_maxes = price$val_high)
  preds <- c(preds, tmp)
}

prices$pred <- round(preds, 4)


hi <- prices %>%
  group_by(date) %>%
  summarize(ptot = sum(closeSharePrice))

prices2 <- prices %>%
  filter(pred >= 0.01) %>%
  select(marketId, bracket_number, eval_date, date, openSharePrice, closeSharePrice, market_name,
         val_low, val_high, is_target, pred, total_price) %>%
  mutate(pred_price_rat = pred/closeSharePrice,
         days_out = eval_date - date) %>%
  arrange(eval_date, bracket_number, date)

targets <- filter(prices2, is_target)
prop.table(table(targets$pred_price_rat > 1, cut(targets$total_price, 5)), 2)
ggplot(targets, aes(x = total_price, y = pred_price_rat)) +
  geom_point() +
  geom_smooth()

prices_sums <- prices2 %>%
  mutate(pred = round(pred, 1)) %>%
  group_by(pred, round(total_price, 2)) %>%
    summarize(mean(pred_price_rat)) %>%
  mutate(p_targ = n_targ / n_tot)
ggplot(prices_sums, aes(x = cutz, y = p_targ)) +
  geom_line()

prices2 %>%
  group_by(days_out, is_target) %>%
  summarize(val = mean(pred_price_rat))
ggplot(targets, aes(x = closeSharePrice, y = pred)) +
  geom_point() +
  geom_smooth()
summary(lm(closeSharePrice ~ pred + total_price, data = targets))
prices2 %>%
  filter(pred >= 0.2) %>%
  group_by(days_out, is_target) %>%
  summarize(mean(pred_price_rat))
