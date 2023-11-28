#' Create a constructor function for the ci_class
#'
#' Create a constructor function for the ci_class called make_ci_class()
#'
#' @details This function creates a constructor function for the ci_class called make_ci_class().
#' @details If the input data contains NaN value, the function will print warning to remind.
#' @details If the input data contains Non-numeric value, the function will print warning to remind.
#'
#' @param x the input data vector with the length of N
#' @param conf the \code{1-α} confidence interval
#'
#' @return the ci_class for confidence interval
#'
#' @export make_ci_class
#'
#' @examples
#' library(SunanP2)
#' set.seed(1234)
#' x <- rnorm(100)
#' obj <- make_ci_class(x)
#' print(obj)


make_ci_class<- function(x, conf = 0.95) {
  # Check if 'x' is numeric
  if (!is.numeric(x)) {
    stop("Input 'data' must be a numeric vector.")
  }

  # Check if 'level' is between 0 and 1
  if (conf <= 0 || conf >= 1) {
    stop("Confidence level must be between 0 and 1.")
  }

  # Create and return the ci_class object
  structure(list(data = x, conf = conf), class = "ci_class")
}

#' @export
#' @method print ci_class
print.ci_class <- function(x) {
  cat("ci_class object with", length(x$data), "observations.\n")
  invisible(x)
}

