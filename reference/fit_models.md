# Fit models in parallel across a list-column of datasets

Applies a user-specified model-fitting function to each element of a
list-column of datasets in `.data`, fitting models in parallel with a
progress bar, and returns the original data frame with a new `model`
column containing the fitted models.

## Usage

``` r
fit_models(
  .data,
  .x,
  .f,
  packages = NULL,
  n_cores = parallel::detectCores() - 1
)
```

## Arguments

- .data:

  A data frame containing a list-column of datasets to which the model
  function will be applied.

- .x:

  Unquoted column name of the list-column containing the datasets.

- .f:

  A function or formula to apply to each dataset to fit the desired
  model (e.g., `~ lm(y ~ x, data = .)` or
  `~ lme4::lmer(y ~ x + (x | group), data = .)`).

- packages:

  A character vector of package names to load on each parallel worker,
  if your model-fitting function requires additional packages. Defaults
  to `NULL`.

- n_cores:

  Number of cores to use for parallel processing. Defaults to
  `parallel::detectCores() - 1`.

## Value

The original `.data` data frame with an additional `model` column
containing the fitted model objects returned by `.f`.

## Details

This function is intended for use in simulation pipelines where multiple
datasets are generated (e.g., via `simulate_datasets()`), and models
need to be fitted to each dataset efficiently in parallel.

It uses
[`pbapply::pblapply()`](https://peter.solymos.org/pbapply/reference/pbapply.html)
to provide a progress bar during model fitting, and
[`parallel::makeCluster()`](https://rdrr.io/r/parallel/makeCluster.html)
for multi-core processing.

Packages specified in `packages` will be loaded on each worker to ensure
model-fitting functions that depend on those packages work correctly in
parallel.

## Examples

``` r
library(dplyr)
library(purrr)
library(lme4)
#> Loading required package: Matrix

# Create example grouped datasets for mixed models
datasets <- tibble(
  id = 1:5,
  data = map(1:5, ~ {
    df <- sleepstudy[sample(nrow(sleepstudy), 50, replace = TRUE), ]
    df$Subject <- factor(df$Subject)
    df
  })
)

# Fit linear mixed models in parallel (lmerTest adds the p-values that
# evaluate_model_results() uses to compute power)
fitted_models <- fit_models(
  datasets,
  .x = data,
  .f = ~ lmerTest::lmer(Reaction ~ Days + (Days | Subject), data = .),
  packages = c("lmerTest"),
  n_cores = 1
)
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')

# Inspect the first fitted mixed model
summary(fitted_models$model[[1]])
#> Linear mixed model fit by REML. t-tests use Satterthwaite's method [
#> lmerModLmerTest]
#> Formula: Reaction ~ Days + (Days | Subject)
#>    Data: .
#> 
#> REML criterion at convergence: 469.9
#> 
#> Scaled residuals: 
#>      Min       1Q   Median       3Q      Max 
#> -1.81615 -0.32564  0.01431  0.34205  2.65244 
#> 
#> Random effects:
#>  Groups   Name        Variance Std.Dev. Corr  
#>  Subject  (Intercept) 722.34   26.876         
#>           Days         68.83    8.296   -0.38 
#>  Residual             291.73   17.080         
#> Number of obs: 50, groups:  Subject, 17
#> 
#> Fixed effects:
#>             Estimate Std. Error      df t value Pr(>|t|)    
#> (Intercept)  255.240      8.548  15.061  29.860 8.08e-15 ***
#> Days           8.004      2.571  10.163   3.113   0.0108 *  
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Correlation of Fixed Effects:
#>      (Intr)
#> Days -0.520

# Tidy the fitted models using extract_model_results() for further evaluation
extracted <- extract_model_results(fitted_models)
head(extracted)
#> # A tibble: 6 × 11
#>      id data   model      effect  group term  estimate std.error statistic    df
#>   <int> <list> <list>     <chr>   <chr> <chr>    <dbl>     <dbl>     <dbl> <dbl>
#> 1     1 <df>   <lmrMdLmT> fixed   NA    (Int…  255.         8.55     29.9   15.1
#> 2     1 <df>   <lmrMdLmT> fixed   NA    Days     8.00       2.57      3.11  10.2
#> 3     1 <df>   <lmrMdLmT> ran_pa… Subj… sd__…   26.9       NA        NA     NA  
#> 4     1 <df>   <lmrMdLmT> ran_pa… Subj… sd__…    8.30      NA        NA     NA  
#> 5     1 <df>   <lmrMdLmT> ran_pa… Subj… cor_…   -0.377     NA        NA     NA  
#> 6     1 <df>   <lmrMdLmT> ran_pa… Resi… sd__…   17.1       NA        NA     NA  
#> # ℹ 1 more variable: p.value <dbl>

# Summarise estimates for 'Days' across simulated fits
extracted |>
  filter(term == "Days") |>
  evaluate_model_results(
    mean_estimate = mean(estimate, na.rm = TRUE),
    sd_estimate = sd(estimate, na.rm = TRUE)
  )
#> # A tibble: 1 × 5
#>   n_models mean_estimate mean_std.error power sd_estimate
#>      <int>         <dbl>          <dbl> <dbl>       <dbl>
#> 1        5          9.21           2.13     1        1.48
```
