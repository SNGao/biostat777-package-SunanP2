#' Calculate confidence
#'
#' Calculate the confidence intervals from simulated data
#'
#' @details This function calculates the confidence intervals of a sample mean and returns a named vector of length 2, where the first value is the lower_bound, the second value is the upper_bound.
#' @details If the input data contains NaN value, the function will print warning to remind.
#' @details If the input is empty, the function will print warning to remind
#' @details If the input doesn't belong to numeric or ci_class class, the function will print warning to remind.
#' @details If the 1-α lower than 0 or larger than 1, the function will print warning to remind.
#'
#' @param x the input data vector with the length of N
#' @param conf the \code{1-α} confidence interval
#'
#' @return The lower and upper bound in the confidence interval
#'
#' @export calculate_CI
#'
#' @examples
#' calculate_CI(c(1:10), 0.95)
#' calculate_CI(c(1:10, 'String'), 0.95)
#'
#' library(SunanP2)
#' set.seed(1234)
#' x <- rnorm(100)
#' obj <- make_ci_class(x)
#' output = calculate_CI(obj, 0.95)


calculate_CI <- function(obj, ...) {
  if (!is.numeric(obj) && !inherits(obj, "ci_class")) {
    stop("Input must be numeric or of class 'ci_class'")
  }
  UseMethod ("calculate_CI")}


#' @export
#' @method calculate_CI numeric
calculate_CI.numeric <- function(x, conf=0.95) {
  # Calculate confidence interval using your desired method
  if (conf <= 0 | conf >= 1) {
    stop("Confidence level must be between 0 and 1.")
  }
  if (sum(is.na(x))>=1){
    stop('Warning: null value in the input data, please check')
  }

  mean_val <- sample_mean(x)
  sd_val <- sample_sd(x)
  n_obs <- length(x)
  margin_error <- qnorm(1 - (1 - conf) / 2) * (sd_val / sqrt(n_obs))
  lower_bound <- mean_val - margin_error
  upper_bound <- mean_val + margin_error

  result <- c('lower_bound' = lower_bound, 'upper_bound'= upper_bound)
  result
}

#' @export
#' @method calculate_CI ci_class
calculate_CI.ci_class <- function(obj, conf=0.95) {
  if (conf <= 0 | conf >= 1) {
    stop("Confidence level must be between 0 and 1.")
  }
  if (sum(is.na(obj))>=1){
    stop('Warning: null value in the input data, please check')
  }

  mean_val <- sample_mean(obj$data)
  se <- sample_sd(obj$data) / sqrt(length(obj$data))
  margin <- qnorm((1 + conf) / 2) * se

  lower_bound <- mean_val - margin
  upper_bound <- mean_val + margin

  result <- c('lower_bound' = lower_bound, 'upper_bound'= upper_bound)
  result
}


sample_mean <- function(x) {
  N <- length(x)
  if (N == 0) {
    stop("The vector input is empty. Cannot calculate mean.")
  }
  mean_x <- sum(x) / N
  return(mean_x)
}

sample_sd <- function(x) {
  N <- length(x)
  if (N == 0) {
    stop("The vector input is empty. Cannot calculate standard deviation.")
  }
  mean_x <- sample_mean(x)
  difference_sd <- (x - mean_x)^2
  sd_x <- sqrt(sum(difference_sd) / (N - 1))
  return(sd_x)
}


