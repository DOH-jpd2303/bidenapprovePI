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
  out <- data.frame(date = train_end_date, target_date, range_mins, range_maxes, m, s, probs = prob)
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
