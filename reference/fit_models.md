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

# Fit linear mixed models in parallel
fitted_models <- fit_models(
  datasets,
  .x = data,
  .f = ~ lme4::lmer(Reaction ~ Days + (Days | Subject), data = .),
  packages = c("lme4")
)

# Inspect the first fitted mixed model
summary(fitted_models$model[[1]])
#> Linear mixed model fit by REML ['lmerMod']
#> Formula: Reaction ~ Days + (Days | Subject)
#>    Data: .
#> 
#> REML criterion at convergence: 462.4
#> 
#> Scaled residuals: 
#>      Min       1Q   Median       3Q      Max 
#> -2.06752 -0.30054 -0.02271  0.36166  1.90815 
#> 
#> Random effects:
#>  Groups   Name        Variance Std.Dev. Corr 
#>  Subject  (Intercept) 714.49   26.730        
#>           Days         55.67    7.461   -0.10
#>  Residual             211.52   14.544        
#> Number of obs: 50, groups:  Subject, 18
#> 
#> Fixed effects:
#>             Estimate Std. Error t value
#> (Intercept)  247.973      8.057  30.776
#> Days           9.881      2.268   4.356
#> 
#> Correlation of Fixed Effects:
#>      (Intr)
#> Days -0.377

# Tidy the fitted models using extract_model_results() for further evaluation
extracted <- extract_model_results(fitted_models)
head(extracted)
#> # A tibble: 6 × 9
#>      id data          model     effect  group term  estimate std.error statistic
#>   <int> <list>        <list>    <chr>   <chr> <chr>    <dbl>     <dbl>     <dbl>
#> 1     1 <df [50 × 3]> <lmerMod> fixed   NA    (Int…  248.         8.06     30.8 
#> 2     1 <df [50 × 3]> <lmerMod> fixed   NA    Days     9.88       2.27      4.36
#> 3     1 <df [50 × 3]> <lmerMod> ran_pa… Subj… sd__…   26.7       NA        NA   
#> 4     1 <df [50 × 3]> <lmerMod> ran_pa… Subj… cor_…   -0.101     NA        NA   
#> 5     1 <df [50 × 3]> <lmerMod> ran_pa… Subj… sd__…    7.46      NA        NA   
#> 6     1 <df [50 × 3]> <lmerMod> ran_pa… Resi… sd__…   14.5       NA        NA   

# Summarise estimates for 'Days' across simulated fits
extracted |>
  filter(term == "Days") |>
  evaluate_model_results(
    mean_estimate = mean(estimate, na.rm = TRUE),
    sd_estimate = sd(estimate, na.rm = TRUE)
  )
#> Error in dplyr::summarise(results, n_models = dplyr::n(), mean_estimate = dplyr::if_else(condition = all(is.na(p.value)),     true = NA_real_, false = mean(estimate, na.rm = TRUE)), mean_std.error = dplyr::if_else(condition = all(is.na(p.value)),     true = NA_real_, false = mean(std.error, na.rm = TRUE)),     power = dplyr::if_else(condition = all(is.na(p.value)), true = NA_real_,         false = mean(p.value < alpha, na.rm = TRUE)), !!!summary_exprs,     !!!{        if (.summarise_standard_broom) {            rlang::exprs(dplyr::across(dplyr::all_of(intersect(broom_cols,                 names(results))), list(mean = mean, sd = sd),                 .names = "{fn}_{col}"))        }        else {            rlang::exprs()        }    }): ℹ In argument: `mean_estimate = dplyr::if_else(...)`.
#> Caused by error:
#> ! object 'p.value' not found
```
