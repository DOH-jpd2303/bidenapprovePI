# Load libraries, read in previous market data
library(tidyverse)
library(data.table)

# Filenames
prices_fn <- './data/daily_approval_prices.Rds'
markets_fn <- './data/markets.csv'

# Read in previous historical data prices
prices_start <- readRDS(prices_fn)

# Get a list of markets
new_markets <- data.table::fread(markets_fn) %>%
  filter(grepl("Biden's 538", market_name) & !id %in% prices_start$marketId)

# Function to get daily price index
get_daily_prices <- function(x){
  lnk <- paste0("https://www.predictit.org/api/Public/GetMarketChartData/", x,
                "?timespan=7&maxContracts=12&isTimespanInHours=false")
  return(tryCatch(jsonlite::fromJSON(lnk), error = function(e){NULL}))
}

# Run it on all data
prices <- lapply(new_markets$id, get_daily_prices)
prices2 <- do.call(rbind, prices)
prices_out <- rbind(prices_start, prices2) %>%
  arrange(marketId, contractId, date)

# Save to file
saveRDS(prices_out, './data/daily_approval_prices.Rds')
