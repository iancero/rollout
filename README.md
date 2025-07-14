
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rollout

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/rollout)](https://CRAN.R-project.org/package=rollout)
[![R-CMD-check](https://github.com/iancero/rollout/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/iancero/rollout/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/iancero/rollout/graph/badge.svg)](https://app.codecov.io/gh/iancero/rollout)
<!-- badges: end -->

## Introduction: Simulating a Rollout Trial

Rollout trials, including stepped wedge and sequential rollout designs,
allow staggered implementation of interventions across sites while
collecting outcome data over time. These designs are increasingly used
in implementation science because they balance practical, ethical, and
statistical considerations when rolling out interventions in real-world
settings.

The `rollout` R package supports the design and analysis of rollout
trials using simulation-based methods. This tutorial demonstrates how to
simulate, analyze, and estimate power for a stepped wedge trial with
multiple sites per cohort. The workflow aligns with the following
structure:

1.  **Define the rollout schedule** across sites and cohorts.
2.  **Join unit-level information** such as the number of participants
    per site.
3.  **Specify simulation parameters** for intervention effects and
    variance components.
4.  **Simulate outcomes** under the specified design.
5.  **Fit models** across multiple simulated replicates.
6.  **Evaluate bias and power** of the planned design.

By following this pipeline, you can efficiently evaluate and optimize
your rollout trial design before conducting your study.

## Installing the `rollout` Package

You can install the development version of `rollout` directly from
GitHub using the `remotes` package:

``` r
# Install remotes package if needed
install.packages("remotes")

# Install rollout from GitHub
remotes::install_github("iancero/rollout")
```

## Setup and Package Loading

We begin by loading the required packages and setting a seed for
reproducibility. Throughout this tutorial, we will use the `tidyverse`
for data manipulation and plotting, and the `rollout` package for
design, simulation, and evaluation of rollout trials.

``` r
library(tidyverse)
#> Warning: package 'tidyverse' was built under R version 4.4.3
library(rollout)

set.seed(1234)  # for reproducibility
```

## Defining the Rollout Schedule

We will simulate a **stepped wedge rollout trial** with:

- **4 cohorts**
- **8 sites total** (2 per cohort)
- **8 timepoints**
- Each cohort transitions to the intervention (`"intv"`) at a different
  time while data are collected continuously.

We use a `tribble()` for clarity, then convert to long format using
`pivot_schedule_longer()`.

``` r
# Create a stepped wedge schedule with 8 sites, 4 cohorts, 8 timepoints
rollout_schedule <- tribble(
  ~cohort, ~site, ~t1, ~t2, ~t3, ~t4, ~t5, ~t6, ~t7, ~t8,
       1,    "A",  "ctrl", "ctrl", "ctrl",  "intv", "intv", "intv", "intv", "intv",
       1,    "B",  "ctrl", "ctrl", "ctrl",  "intv", "intv", "intv", "intv", "intv",
       2,    "C",  "ctrl", "ctrl",  "intv", "intv", "intv", "intv", "intv", "intv",
       2,    "D",  "ctrl", "ctrl",  "intv", "intv", "intv", "intv", "intv", "intv",
       3,    "E",  "ctrl", "ctrl", "ctrl", "ctrl",  "intv", "intv", "intv", "intv",
       3,    "F",  "ctrl", "ctrl", "ctrl", "ctrl",  "intv", "intv", "intv", "intv",
       4,    "G",  "ctrl", "ctrl", "ctrl", "ctrl", "ctrl", "ctrl",  "intv", "intv",
       4,    "H",  "ctrl", "ctrl", "ctrl", "ctrl", "ctrl", "ctrl",  "intv", "intv"
)

# Transform to long format for simulation
long_schedule <- rollout_schedule |>
  pivot_schedule_longer(time_cols = starts_with("t"))

# Preview structure
long_schedule |> 
  head()
#> # A tibble: 6 × 5
#>   cohort site  chron_time condition local_time
#>    <dbl> <chr>      <dbl> <fct>          <dbl>
#> 1      1 A              1 ctrl               0
#> 2      1 A              2 ctrl               1
#> 3      1 A              3 ctrl               2
#> 4      1 A              4 intv               0
#> 5      1 A              5 intv               1
#> 6      1 A              6 intv               2
```

## Adding Unit-Level Information

Next, we specify the **unit-level information** for each site, which is
sometimes available in advance of a trial. For this example, each site
will have a **slightly different number of units**, reflecting
real-world differences in site sizes.

We will then join this information to our long-format rollout schedule
using `join_info()`. This will expand the dataset to include a row for
each unit at each timepoint.

``` r
# Create unit-level information with variable site sizes
unit_info <- tribble(
  ~site, ~n_units,
   "A",   18,
   "B",   22,
   "C",   20,
   "D",   19,
   "E",   21,
   "F",   20,
   "G",   23,
   "H",   17
)

# Join unit-level info to the long-format schedule
design_df <- join_info(
  long_schedule,
  unit_info = unit_info,
  by = "site",
  uncount_vars = "n_units",
  .ids = "unit_id"
)

# Preview the expanded design
design_df |> 
  head()
#> # A tibble: 6 × 6
#>   cohort site  chron_time condition local_time unit_id
#>    <dbl> <chr>      <dbl> <fct>          <dbl>   <int>
#> 1      1 A              1 ctrl               0       1
#> 2      1 A              1 ctrl               0       2
#> 3      1 A              1 ctrl               0       3
#> 4      1 A              1 ctrl               0       4
#> 5      1 A              1 ctrl               0       5
#> 6      1 A              1 ctrl               0       6
```

## Adding Simulation Parameters

We will now add **simulation parameters** to our design. A key advantage
of `rollout` is the ability to expand a parameter grid, enabling the
evaluation of **multiple scenarios** within the same simulation
pipeline.

In this example: - We will evaluate **two intervention effect sizes**
(`b_intv = 0.2` for a small effect, `b_intv = 0.5` for a moderate
effect). - We will specify a constant **site-level standard deviation
(`sigma_site = 0.5`)** and **unit-level residual standard deviation
(`sigma_unit = 1`)**.

``` r
design_df <- design_df |>
  add_parameter(
    b_intv = c(0.2, 0.5),
    sigma_site = 0.5,
    sigma_unit = 1
  )

# Preview to confirm parameter expansion
design_df |> 
  head()
#> # A tibble: 6 × 9
#>   cohort site  chron_time condition local_time unit_id b_intv sigma_site
#>    <dbl> <chr>      <dbl> <fct>          <dbl>   <int>  <dbl>      <dbl>
#> 1      1 A              1 ctrl               0       1    0.2        0.5
#> 2      1 A              1 ctrl               0       1    0.5        0.5
#> 3      1 A              1 ctrl               0       2    0.2        0.5
#> 4      1 A              1 ctrl               0       2    0.5        0.5
#> 5      1 A              1 ctrl               0       3    0.2        0.5
#> 6      1 A              1 ctrl               0       3    0.5        0.5
#> # ℹ 1 more variable: sigma_unit <dbl>
```

## Initializing Replicates

After adding simulation parameters, we expand our design to include
**replicates**. This enables us to simulate multiple trials under the
same design to estimate power and evaluate bias efficiently.

Here, we initialize **10 replicates** to keep memory usage manageable
for demonstration. (In practice, you may use 100+ replicates for stable
power estimation.)

``` r
design_df <- design_df |>
  initialize_replicates(n = 10)

# Preview structure with replicate IDs
design_df |> 
  head()
#> # A tibble: 6 × 10
#>   sample_id cohort site  chron_time condition local_time unit_id b_intv
#>       <int>  <dbl> <chr>      <dbl> <fct>          <dbl>   <int>  <dbl>
#> 1         1      1 A              1 ctrl               0       1    0.2
#> 2         2      1 A              1 ctrl               0       1    0.2
#> 3         3      1 A              1 ctrl               0       1    0.2
#> 4         4      1 A              1 ctrl               0       1    0.2
#> 5         5      1 A              1 ctrl               0       1    0.2
#> 6         6      1 A              1 ctrl               0       1    0.2
#> # ℹ 2 more variables: sigma_site <dbl>, sigma_unit <dbl>
```

## Simulating Outcomes

We now simulate the outcome data according to our specified design. This
prepares the dataset for model fitting, power estimation, and bias
evaluation.

------------------------------------------------------------------------

### Adding a Fixed Time Trend

First, we add a **small negative time trend** (`b_time = -0.05`) to
represent gradual improvement over time, independent of intervention.

``` r
design_df <- design_df |>
  add_parameter(b_time = -0.05)
```

### Adding Fixed Effects

Next, we add fixed effects for: - The intervention effect, applied when
`condition == "intv"`. - The time trend, scaled by `chron_time`.

``` r
design_df <- design_df |>
  add_fixed_effect(
    intv_effect = b_intv * as.numeric(condition == "intv")) |>
  add_fixed_effect(
    time_trend = b_time * chron_time)
```

### Adding Random Intercepts for Sites

We add a random intercept at the site level to simulate between-site
variability.

``` r
design_df <- design_df |>
  add_random_effect(
    site_intercept = rnorm(1, mean = 0, sd = sigma_site),
    .nesting = "site"
  )
```

### Adding Unit-Level Residual Error

We add unit-level residual error with variance `sigma_unit^2`.

``` r
design_df <- design_df |>
  add_error(variance = sigma_unit^2)
```

### Generating the Linear Outcome

Finally, we generate a linear outcome (y_linear) by summing all fixed,
random, and error components.

``` r
design_df <- design_df |>
  add_linear_outcome(output_col = "y_linear")

# Preview simulated data
design_df |> 
  head()
#> # A tibble: 6 × 16
#>   sample_id cohort site  chron_time condition local_time unit_id b_intv
#>       <int>  <dbl> <chr>      <dbl> <fct>          <dbl>   <int>  <dbl>
#> 1         1      1 A              1 ctrl               0       1    0.2
#> 2         2      1 A              1 ctrl               0       1    0.2
#> 3         3      1 A              1 ctrl               0       1    0.2
#> 4         4      1 A              1 ctrl               0       1    0.2
#> 5         5      1 A              1 ctrl               0       1    0.2
#> 6         6      1 A              1 ctrl               0       1    0.2
#> # ℹ 8 more variables: sigma_site <dbl>, sigma_unit <dbl>, b_time <dbl>,
#> #   .intv_effect <dbl>, .time_trend <dbl>, .site_intercept <dbl>, .error <dbl>,
#> #   y_linear <dbl>
```

## Fitting Models Across Replicates

With our simulated outcomes generated, we now fit our **planned analysis
model** across all replicates and parameter scenarios.

This process is automatically conducted in parallel, but you’ll need to
load any packages for that parallel process explicitly (see line
`packages = "lmerTest"`).

In this example: - We use **linear mixed-effects models** to account for
site-level clustering. - Our model includes: - `condition` (intervention
vs control), - `chron_time` (continuous time trend), - A random
intercept for `site`. - We group by `sample_id` (replicates) and
`b_intv` (effect size scenario).

This approach allows us to evaluate **bias and power** across both
**replicates** and **effect size conditions**.

``` r
fitted_models <- design_df |>
  group_by(sample_id, b_intv) |>
  nest() |>
  ungroup() |> 
  fit_models(
    .x = data,
    .f = ~ lmer(
      formula = y_linear ~ condition + chron_time + (1 | site),
      data = .x),
    packages = "lmerTest") # load lmerTest in each parallel process

fitted_models |> 
  head()
#> Loading required package: lmerTest
#> Loading required package: lme4
#> Warning: package 'lme4' was built under R version 4.4.2
#> Loading required package: Matrix
#> 
#> Attaching package: 'Matrix'
#> The following objects are masked from 'package:tidyr':
#> 
#>     expand, pack, unpack
#> 
#> Attaching package: 'lmerTest'
#> The following object is masked from 'package:lme4':
#> 
#>     lmer
#> The following object is masked from 'package:stats':
#> 
#>     step
#> # A tibble: 6 × 4
#>   sample_id b_intv data                  model     
#>       <int>  <dbl> <list>                <list>    
#> 1         1    0.2 <tibble [1,280 × 14]> <lmrMdLmT>
#> 2         2    0.2 <tibble [1,280 × 14]> <lmrMdLmT>
#> 3         3    0.2 <tibble [1,280 × 14]> <lmrMdLmT>
#> 4         4    0.2 <tibble [1,280 × 14]> <lmrMdLmT>
#> 5         5    0.2 <tibble [1,280 × 14]> <lmrMdLmT>
#> 6         6    0.2 <tibble [1,280 × 14]> <lmrMdLmT>
```

## Extracting and Evaluating Results

After fitting the models, we now **extract key results** and **evaluate
bias and power** for our intervention effect.

We will: - **Extract model estimates** using
`extract_model_results()`. - **Summarize power** by calculating the
proportion of p-values below .05 (done by default in
`evaluate_model_results). - **Summarize bias** of the intervention effect using`eval_bias()`. - **Estimate a critical value** by calculating the observed value of b-estimates above the 97.5th percentile, using`eval_quantile()\`.

``` r
# Extract tidy model results
model_results <- fitted_models |>
  extract_model_results()

# Summarize bias and power
summary_results <- model_results |>
  group_by(b_intv, term) |>
  evaluate_model_results(
    bias = eval_bias(estimate, term = c("(Intercept)" = 0, conditionintv = b_intv)),
    critical_val = eval_quantile(estimate, term = c("conditionintv" = 0.975))
  )
#> Warning: There were 16 warnings in `dplyr::summarise()`.
#> The first warning was:
#> ℹ In argument: `bias = eval_bias(estimate, term = c(`(Intercept)` = 0,
#>   conditionintv = b_intv))`.
#> ℹ In group 2: `b_intv = 0.2` and `term = "chron_time"`.
#> Caused by warning:
#> ! Term 'chron_time' not found in `term` mapping. Returning NA.
#> ℹ Run `dplyr::last_dplyr_warnings()` to see the 15 remaining warnings.

# View summary of bias and power
summary_results
#> # A tibble: 10 × 6
#> # Groups:   b_intv [2]
#>    b_intv term            n_models power   bias critical_val
#>     <dbl> <chr>              <int> <dbl>  <dbl>        <dbl>
#>  1    0.2 (Intercept)           10   0   -0.154       NA    
#>  2    0.2 chron_time            10   0.7 NA           NA    
#>  3    0.2 conditionintv         10   0.5 NA            0.334
#>  4    0.2 sd__(Intercept)       10  NA   NA           NA    
#>  5    0.2 sd__Observation       10  NA   NA           NA    
#>  6    0.5 (Intercept)           10   0   -0.193       NA    
#>  7    0.5 chron_time            10   0.4 NA           NA    
#>  8    0.5 conditionintv         10   1   NA            0.663
#>  9    0.5 sd__(Intercept)       10  NA   NA           NA    
#> 10    0.5 sd__Observation       10  NA   NA           NA
```
