#' Sine function
#'
#' Compute the approximation to the sin() function.
#'
#' @details This function computes the approximation to the Sine function.
#' @details If the input data contains NaN value, the function will print warning to remind.
#' @details If the input data contains Non-numeric value, the function will print warning to remind.
#'
#' @param x the number to be transformed
#' @param k the number of terms to be used in the series expansion beyond the constant 1. The value of k is always over than 1.
#'
#' @return The approximation to the sine function
#'
#' @export
#'
#' @examples
#' fn_sin(c(1:10), 5)
#'

fn_sin <- function(x, k=1) {
  if (sum(is.na(x))>=1){
    stop('Warning: null value in the input data, please check')
  }

  if (sum(!is.numeric(x))>=1){
    stop('Warning: non-numeric value in the input data, please check')
  }

  sum = 0
  for (i in c(1:k)){
    steps = i-1
    sum = sum + (-1)^steps*x^(2*steps+1)/factorial(2*steps+1)
  }
  return(sum)
}
