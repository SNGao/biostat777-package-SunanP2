
## SunanP2

<!-- badges: start -->
<!-- badges: end -->

### Author
Sunan Gao

### Description
The goal of SunanP2 is to realize the basic statistical calculation, including trigonometric approximation and estimating confidence interval.

### Installation
You can install the package using the following R code:
``` r
# Install the devtools package if not already installed
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}

# Install MyPackage from GitHub
devtools::install_github("https://github.com/SNGao/biostat777-package-SunanP2")
```

### Exported Functions

#### `fn_sin()`
- Description: Compute the approximation to the sin() function.
- Usage: `fn_sin(x, k)`
- Example:
``` r
library(SunanP2)
fn_sin(45, 20)

## compare the difference ratio between sin() function.
diff = (fn_sin(45, 20) - sin(45))/sin(45)
print(diff)
```

#### `fn_cos()`
- Description: Compute the approximation to the cos() function.
- Usage: `fn_cos(x, k)`
- Example:
``` r
library(SunanP2)
fn_cos(45, 20)

## compare the difference ratio between cos() function.
diff = (fn_cos(45, 20) - cos(45))/cos(45)
print(diff)
```

#### `calculate_CI()`
- Description: Calculate the confidence intervals from input data.
- Usage: `calculate_CI(x, conf)`
- Example:
``` r
library(SunanP2)
set.seed(1234)
x <- rnorm(100)
obj <- make_ci_class(x)
output = calculate_CI(obj, 0.95)
```
