#' Cosine function
#'
#' Compute the approximation to the cosine function.
#'
#' @details This function computes the approximation to the cosine function.
#' @details If the input data contains NaN value, the function will print warning to remind.
#' @details If the input data contains Non-numeric value, the function will print warning to remind.

#' @param x the number to be transformed
#' @param k the number of terms to be used in the series expansion beyond the constant 1. The value of k is always over than 1.
#'
#' @return The approximation to the cosine function
#'
#' @export
#'
#' @examples
#' fn_cos(c(1:10), 5)
#'

fn_cos <- function(x, k=1) {
  if (sum(is.na(x))>=1){
    stop('Warning: null value in the input data, please check')
  }

  if (sum(!is.numeric(x))>=1){
    stop('Warning: non-numeric value in the input data, please check')
  }

  sum = 0
  for (i in c(1:k)){
    steps = i-1
    sum = sum + (-1)^steps*x^(2*steps)/factorial(2*steps)
  }
  return(sum)
}




