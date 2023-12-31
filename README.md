
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

# Install SunanP2 from GitHub (contain vignettes)
devtools::install_github("https://github.com/SNGao/biostat777-package-SunanP2", build_vignettes = TRUE)
```

### View vignettes in SunanP2
``` r
library(SunanP2)
browseVignettes('SunanP2')
```

### Exported Functions

#### `fn_sin()`
- Description: Compute the approximation to the sin() function.
- Usage: `fn_sin(x, k)`
- Example:
``` r
library(SunanP2)
fn_sin(1, 10)

## compare the difference ratio between sin() function.
diff = (fn_sin(1, 10) - sin(1))/sin(1)
print(diff)
```

#### `fn_cos()`
- Description: Compute the approximation to the cos() function.
- Usage: `fn_cos(x, k)`
- Example:
``` r
library(SunanP2)
fn_cos(1, 10)

## compare the difference ratio between cos() function.
diff = (fn_cos(1, 10) - cos(1))/cos(1)
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
calculate_CI(obj, 0.95)
calculate_CI(x, 0.95)
```

#### `make_ci_class()`
- Description: change the class of object to ci_class.
- Usage: `make_ci_class(x)`
- Example:
``` r
library(SunanP2)
set.seed(1234)
x <- rnorm(100)
obj <- make_ci_class(x)
print(obj)
```
