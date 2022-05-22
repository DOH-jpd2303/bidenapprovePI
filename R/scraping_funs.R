get_538_biden_ave <- function(){
  # Links for the 538 poll list and approval topline estimates
  ave_lnk <- "https://projects.fivethirtyeight.com/biden-approval-data/approval_topline.csv"

  # Date formatting pattern from 538 files
  dt_fmt = "%m/%d/%Y"
  dt_time_fmt = "%H:%M:%S %d %b %Y"

  # Read in/manipulate polling average data from 538 website
  ave <- data.table::fread(ave_lnk)
  ave <- ave[ave$subgroup == 'All polls', ]
  ave$polldate <- as.Date(ave$modeldate, format = dt_fmt)
  ave$timestamp <- as.POSIXct(ave$timestamp, format = dt_time_fmt) - lubridate::hours(3)
  ave$approval <- ave$approve_estimate/100

  # Get last poll of each day
  out <- ave %>%
    dplyr::group_by(polldate) %>%
    dplyr::summarize_all(last) %>%
    dplyr::select(polldate, approval, timestamp)
  return(out)
}

money_to_number <- function(x){
  mult <- ifelse(grepl("\\(", x), -1, 1)
  out <- mult*as.numeric(gsub("[^\\d\\.]+", "", x, perl=TRUE))
  return(out)
}

#' Convert the contract name to a range of percent values
#'
#' @param contractName - The string containing the contract name
#'
#' @return The range of approval ratings implied by the string
#' @export
#'
extract_num_range <- function(x){
  val_low <- dplyr::case_when(grepl('lower', x) ~ 0,
                              TRUE ~ as.numeric(stringr::str_extract(x, ".*(?=\\% (to|or))")))
  val_high <- dplyr::case_when(grepl('lower', x) ~
                                 as.numeric(stringr::str_extract(x, ".*(?=\\%)")),
                               grepl('higher', x) ~ 100,
                               TRUE ~ as.numeric(stringr::str_extract(x, "(?<= (to|or) ).*(?=\\%)")))
  return(list(as.numeric(val_low)/100, as.numeric(val_high)/100))
}
