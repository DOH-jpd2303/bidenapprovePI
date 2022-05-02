###############################################################################
# SETUP AND DATA READIN
###############################################################################
# Set up ------------------------------------------------------------------
# Load libraries
library(tidyverse)
library(forecast)
library(jsonlite)
library(foreach)
library(doParallel)
library(doSNOW)
library(mgcv)

# This calls all custom functions I wrote in the 'R' subdirectory
devtools::load_all()

# Links for the 538 poll list and approval topline estimates
ave_lnk <- "https://projects.fivethirtyeight.com/biden-approval-data/approval_topline.csv"

# Get polling averages by day ---------------------------------------------
# Date formatting pattern from 538 files
dt_fmt = "%m/%d/%Y"
dt_time_fmt = "%H:%M:%S %d %b %Y"

# Read in/manipulate polling average data from 538 website
ave <- data.table::fread(ave_lnk) %>%
  filter(subgroup == 'All polls') %>%
  mutate(polldate = as.Date(modeldate, format = dt_fmt),
         timestamp = as.POSIXct(timestamp, format = dt_time_fmt),
         approval = approve_estimate/100) %>%
  group_by(polldate) %>%
  summarize_all(last) %>%
  select(polldate, approval, timestamp)

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

##############################################################################################
poll_dates <- ave$polldate
poll_values <- ave$approval
train_end_date <- as.Date('2021-06-17')
target_date <- as.Date('2021-06-22')
df <- prices %>% filter(date == train_end_date & eval_date == target_date)
range_mins <- df$val_low
range_maxes <- df$val_high
predict_polling_bracket(poll_dates, poll_values, train_end_date, target_date, range_mins, range_maxes)
predict_polling_bracket <- function(poll_dates, poll_values, train_end_date, target_date, range_mins, range_maxes){
  h <- as.numeric(target_date - train_end_date)

  # Subset to dates before the training end date
  poll_dates2 <- as.numeric(poll_dates[poll_dates < train_end_date])
  poll_values2 <- as.numeric(poll_values[poll_dates < train_end_date])
  target_date2 <- as.numeric(target_date)

  # Fit a splined forecast, get the implied sigma
  fit <- forecast::splinef(poll_values2, h, level = 95)
  m <- fit$mean[h]
  s <- (fit$upper[h] - m)/1.96

  # Get the probability that the predicted value lies within each range of values
  # Uses the implied sigma and forecasted value
  new_vals <- c(0, lag(range_maxes)[-1])
  pnorms_low <- as.numeric(lapply(new_vals, pnorm, mean = m, sd = s))
  pnorms_high <- as.numeric(lapply(range_maxes, pnorm, mean = m, sd = s))
  prob <- pnorms_high - pnorms_low

  # Return the point forecast, sd, and probabilities
  out <- data.frame(m, s, probs = prob)
  return(out)
}

# Get prediction for each row ---------------------------------------------
# Split dataframe by target date and market date
prices_split <- group_split(prices, date, eval_date)

preds <- data.frame()
for(i in(prices_split)){
  train_end_date <- unique(i$date)
  target_date <- unique(i$eval_date)
  tmp <- predict_polling_bracket(poll_dates = ave$polldate,
                                 poll_values = ave$approval,
                                 train_end_date = train_end_date,
                                 target_date = target_date,
                                 range_mins = i$val_low,
                                 range_maxes = i$val_high)
  preds <- bind_rows(preds, tmp)
  rm(tmp)
}

prices2 <- bind_cols(prices, preds) %>%
  rename(yhat = probs) %>%
  mutate(pred_price_ratio = yhat/closeSharePrice,
         net_value = ifelse(is_target, 0.9*(1-closeSharePrice), -closeSharePrice),
         days_out = as.numeric(eval_date - date),
         buy_signal = pred_price_ratio >= 1.05 & pred_price_ratio < 5,
         pred_tile = ntile(yhat, 10),
         price_tile = ntile(total_price, 10))

calib <- prices2 %>%
  group_by(pred_tile) %>%
  summarize(n = n(),
            nhit = sum(is_target),
            phit = nhit/n,
            mpred = mean(yhat),
            dif = phit - mpred)
moneys <- filter(prices2, buy_signal == 1) %>%
  group_by(price_tile) %>%
  summarize(n = n(), net_value = sum(net_value),
            netper = net_value/n)
moneys
buys <- filter(prices2, buy_signal == 1 & total_price <= 1.05) %>%
  select(-all_of(c('ntry', 'marketId', 'contractId', 'buy_signal', 'contractName', 'highSharePrice', 'lowSharePrice', 'tradeVolume',
                   'market_name')))
sum(buys$net_value)

buys %>%
  group_by(price_tile) %>%
  summarize(n = n(),
            min_price = min(total_price),
            max_price = max(total_price),
            nhit = sum(is_target),
            phit = nhit/n,
            mpred = mean(yhat),
            net_value = sum(net_value),
            )

ggplot(buys[buys$price_tile <= 2,], aes(x = date, y = yhat)) +
  geom_point()

buys %>%
  group_by(price_tile) %>%
  summarize(val = sum(net_value))

