# Load libraries, read in previous market data
library(tidyverse)
out_fn <- 'markets.csv'
prev_markets <- data.table::fread(out_fn)
biden_marks <- prev_markets[grepl('Biden.*538', prev_markets$name),]

# Get a list of numbers to search
nsamp <- 50
nums <- 7700:7900
new_nums <- nums[!(nums %in% prev_markets$x)]
nums <- sample(new_nums, min(length(new_nums), nsamp), replace = FALSE)

# Function to get market names
get_names_fun <- function(x){
  try <- tryCatch(jsonlite::fromJSON(paste0('https://www.predictit.org/api/marketdata/markets/', x))$shortName, error = function(e) NULL)
  if(identical(try, NULL)){
    Sys.sleep(30)
    return(cbind(x, NA_character_))
  } else{
    return(cbind(x, try))
  }

}

# Get as many names as we can, save to file
# It will eventually time out and stop returning results
# I've lazily handled this by babysitting the function and running the script many times
names_all <- lapply(nums, function(x) tryCatch(get_names_fun(x), error = function(e) NULL))
names_keep <- data.frame(do.call(rbind, names_all)) %>%
  rename(name = 2) %>%
  filter(!is.na(name))

# Write to file
data.table::fwrite(names_keep, out_fn, append = TRUE)

