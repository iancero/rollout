
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
7.  **Visualize results** to inform design decisions.

By following this pipeline, you can efficiently evaluate and optimize
your rollout trial design before conducting your study.

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
#> # A tibble: 64 × 4
#>    cohort site  chron_time condition
#>     <dbl> <chr>      <dbl> <fct>    
#>  1      1 A              1 ctrl     
#>  2      1 A              2 ctrl     
#>  3      1 A              3 ctrl     
#>  4      1 A              4 intv     
#>  5      1 A              5 intv     
#>  6      1 A              6 intv     
#>  7      1 A              7 intv     
#>  8      1 A              8 intv     
#>  9      1 B              1 ctrl     
#> 10      1 B              2 ctrl     
#> # ℹ 54 more rows

# Preview structure
long_schedule |> head()
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
