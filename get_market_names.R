# get_market_names.R ------------------------------------------------------
# Goal: to identify the index numbers and names of previous PredictIt markets
#       and to save results to file for use in other scraping activities

# Load libraries
library(tidyverse)

# Set search parameters
nsamp <- 30
max_try <- 5
ids <- 7700:7900

# Read in list of previous markets
mark_fn <- './data/markets.Rds'
marks <- readRDS(mark_fn)

# Initialize a dataframe of numbers not currently in the market search data frame
new_ids <- ids[!(ids %in% marks$id)]
if(length(new_ids) > 0){

  # Add new IDs to the main dataframe.
  new_ids <- data.frame(id = new_ids, market_name = NA_character_, ntry = 0)

  # Combine current market and new ids
  new_marks <- marks %>%
    bind_rows(new_ids) %>%
    arrange(id)
} else{
  new_marks <- marks %>% arrange(id)
}


# These are new markets to search
to_search <- filter(new_marks, is.na(market_name) & ntry < max_try)
search_nums <- sample(to_search$id, min(length(to_search$id), nsamp), replace = FALSE)

# Function to get market names
get_names_fun <- function(x){
  try <- tryCatch(jsonlite::fromJSON(paste0('https://www.predictit.org/api/marketdata/markets/', x))$shortName, error = function(e) NULL)
  if(identical(try, NULL)){
    Sys.sleep(5)
    return(cbind(x, NA_character_))
  } else{
    return(cbind(x, try))
  }

}

# Get as many names as we can, save to file
# It will eventually time out and stop returning results
names_all <- lapply(search_nums, function(x) tryCatch(get_names_fun(x), error = function(e) NULL))
names_new <- data.frame(do.call(rbind, names_all)) %>%
  rename(id = 1, market_name = 2) %>%
  mutate(ntry = 1,
         id = as.integer(id))

out <- new_marks %>%
  anti_join(names_new, by = c('id')) %>%
  bind_rows(names_new)

# Write to file
saveRDS(out, mark_fn)

