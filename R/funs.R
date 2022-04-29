#' Determine the probability that the polling average will lie between a range of possible values
#'
#' @param poll_dates- Vector of dates that will be used to create the training set
#' @param poll_values- Vector of polling averages (must be same size as poll_dates)
#' @param train_end_date- The last date that should be used to fit the model
#' @param target_date - The date the model is trying to predict a polling average for
#' @param range_mins - Vector of lower-end of each bracket range
#' @param range_maxes - Vector of upper-end of each bracket range
#'
#' @return- A list of two objects:
#' The pred object contains the point estimate (pred) and standard error (se).
#' The probs object is a data frame including rows for each prediction range, and
#' the estimated probability of a value in that range.
#'
#' @export
predict_polling_brackets <- function(poll_dates, poll_values, train_end_date, target_date, range_mins, range_maxes){
  # Subset to dates before the training end date
  poll_dates2 <- poll_dates[poll_dates < train_end_date]
  poll_values2 <- poll_values[poll_dates < train_end_date]

  # Get first/last date in range
  min_date <- min(poll_dates2)
  max_date <- max(poll_dates2)

  # Create a sequence of dates from the end of training to target
  pred_dates <- seq.Date(from = (max_date + 1), to = target_date, by = 'day')
  h <- length(pred_dates)

  # Create a time series from the testing set, fit an ARIMA model
  ts <- as.ts(poll_values2, start = min(poll_dates2), end = max(poll_dates2), frequency = 1)
  fit <- forecast::Arima(ts, order = c(2,1,1), include.drift = FALSE, include.mean = TRUE)

  # Forecast to the target date, extract mean and err
  fc <- forecast::forecast(fit, h = h, level = 95, bootstrap = TRUE)
  m <- fc$mean
  s <- ((fc$upper - fc$lower)/1.96/2)

  # Get the probability that the predicted value lies within each range of values
  new_vals <- c(0, lag(range_maxes)[-1])
  pnorms_low <- as.numeric(lapply(new_vals, pnorm, mean = m[h], sd = s[h]))
  pnorms_high <- as.numeric(lapply(range_maxes, pnorm, mean = m[h], sd = s[h]))
  prob <- pnorms_high - pnorms_low
  return(prob)
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
                               TRUE ~ as.numeric(stringr::str_extract(x, "(?<= to ).*(?=\\%)")))
  return(list(as.numeric(val_low)/100, as.numeric(val_high)/100))
}
