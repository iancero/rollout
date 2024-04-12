
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rollout

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/rollout)](https://CRAN.R-project.org/package=rollout)
[![R-CMD-check](https://github.com/iancero/rollout/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/iancero/rollout/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/iancero/rollout/branch/main/graph/badge.svg)](https://app.codecov.io/gh/iancero/rollout?branch=main)
<!-- badges: end -->

A **rollout trial** involves “rolling out” an intervention across
different sites or groups in incremental phases. The **rollout package**
simulates and evaluates statistical properties (e.g., power) of
different rollout study designs.

## Installation

Rollout is under development and therefore not yet available on CRAN.
However, you can install the development version via GitHub.

``` r
devtools::install_github("iancero/rollout")
```

## Basic workflow

The rollout package follows a four-step workflow for simulating and
evaluating rollout designs.

1.  Specifying the rollout schedule and expected intervention effects.
2.  Simulating several datasets.
3.  Fitting a statistical model to each dataset.
4.  Evaluating the statistical properties (e.g., power, bias) of those
    fitted models.

Rollout’s main functions guide you through each of these steps and are
described in more detail below.

## Specifying rollout schedules and effects

In this step, we specify the rollout schedule and expected effects of
the conditions (e.g., interventions) in that schedule. For example, we
might specify that the intervention will be rolled out in three phases,
each lasting a different amount of time.[^1]

- A three month **baseline phase**, in which no intervention is applied,
  but data from each participating cohort are collected.
- An two month **intervention phase**, in which each cohort receives the
  intervention.
- A four month **sustainment phase**, in which the intervention is no
  longer applied, but data are still collected to evaluate whether the
  effects of the intervention persist beyond its initial application.

### Phase schedules

Specifying the **rollout schedule** in the rollout package is done by
creating a named list of rollout **phase durations**. The names of the
list elements correspond to the names of the phases, and the values
correspond to the duration of each phase in time steps. For example, the
rollout schedule described above would be specified as follows:

``` r
library(rollout)

phase_durations <- list(baseline = 3, intervention = 2, sustainment = 4)
```

### Phase effects

**Phase effects** are specified in a similar way. For example, we might
specify that the intervention has no effect during the baseline phase, a
positive effect during the intervention phase, and a smaller positive
effect during the sustainment phase.

``` r
phase_effects <- list(baseline = 0, intervention = 2, sustainment = 1)
```

## Simulating sample data

To generate sample data, we need both our phase durations and phase
effects, as well as a few more general simulation parameters (e.g.,
sample size). For example, we might specify that our design is rolled
out across 12 cohorts, each with 100 subjects.

``` r
cohorts <- 12
subjects_per_cohort <- 100
```

To keep track of the growing collection of parameters, we’ll combine all
of our these specifications into a named list.

``` r
sim_params <- list(
  phase_durations = phase_durations,
  phase_effects = phase_effects,
  cohorts = cohorts,
  subjects_per_cohort = subjects_per_cohort
)
```

Lastly, we simulate several datasets consistent with the rollout
schedule and expected effects. The `simulate_rollout_samples()` function
generates a dataset with the specified number of cohorts and subjects
per cohort, and adds columns for the phase, time, and outcome variables.

``` r
simulated_samples_df <- simulate_rollout_samples(sim_params, n = 10)

head(simulated_samples_df)
#> # A tibble: 6 × 10
#>   .cohort .time phase    .sample .subject phase_score cohort_score subject_score
#>     <int> <int> <fct>      <int>    <int>       <dbl>        <dbl>         <dbl>
#> 1       1     1 baseline       1        1           0       -0.560         0.401
#> 2       1     1 baseline       1        2           0       -0.560         0.111
#> 3       1     1 baseline       1        3           0       -0.560        -0.556
#> 4       1     1 baseline       1        4           0       -0.560         1.79 
#> 5       1     1 baseline       1        5           0       -0.560         0.498
#> 6       1     1 baseline       1        6           0       -0.560        -1.97 
#> # ℹ 2 more variables: error <dbl>, y <dbl>
```

## Fitting a statistical model

Once a collection of simulated samples have been generated, the
`fit_model()` function can be used to fit a linear mixed-effects model
to each dataset. The model includes fixed effects for the phase, time,
and cohort, and random effects for the cohort and subject.

``` r
model <- "y ~ phase + (1 | .cohort/.subject)"

fitted_models <- fit_model(simulated_samples_df, model)
#> Warning: There was 1 warning in `dplyr::mutate()`.
#> ℹ In argument: `.fit = purrr::map(data, ~lmerTest::lmer(model, data = .x))`.
#> Caused by warning in `checkConv()`:
#> ! Model failed to converge with max|grad| = 0.00205116 (tol = 0.002, component 1)

head(fitted_models)
#> # A tibble: 6 × 3
#>   .sample data                  .fit      
#>     <int> <list>                <list>    
#> 1       1 <tibble [10,800 × 9]> <lmrMdLmT>
#> 2       2 <tibble [10,800 × 9]> <lmrMdLmT>
#> 3       3 <tibble [10,800 × 9]> <lmrMdLmT>
#> 4       4 <tibble [10,800 × 9]> <lmrMdLmT>
#> 5       5 <tibble [10,800 × 9]> <lmrMdLmT>
#> 6       6 <tibble [10,800 × 9]> <lmrMdLmT>
```

## Evaluating design properties

Finally, we can evaluate the statistical properties of the fitted models
to get a sense of the quality of our rollout design. The `eval_design()`
function calculates a range of useful statistics, including the power
and bias of the estimated intervention effect for each phase.

``` r
eval_design(fitted_models, feature = "power")
#> # A tibble: 3 × 2
#>   term              power
#>   <chr>             <dbl>
#> 1 (Intercept)           0
#> 2 phaseintervention     1
#> 3 phasesustainment      1
```

# Advanced features

The rollout package includes a number of advanced features to help you
simulate and evaluate rollout designs.

## Visualization

``` r
library(ggplot2)

plot_schedule(rollout_params)
```

## Complex phase schedules

In addition to straightforward phase schedules, the rollout package
supports more complex phase schedules. For example, you can specify
multiple interventions in a single phase to produce a head-to-head
comparison trial.

``` r
phase_durations <- list(
  baseline = 3,
  interventions = list( # head-to-head interventions
    intervetion_a = 3,
    intervention_b = 1
  ),
  sustainment = 4
)
```

## Complex phase effects

The rollout package allows you to specify a custom function for each
phase effect. This can be useful when the effect of the intervention is
not constant over time, like an intervention that has a delayed effect
or a time-varying effect.

``` r
phase_effects <- list(
  baseline = 0,
  intervention = function(phase_time) case_when(
    phase_time < 3 ~ phase_time**2, # quadratic effect for rapid uptake
    phase_time >= 3 ~ 2 # constant effect after 3 months
  )
)
```

## Pipe-friendly framework

The rollout package is designed to be pipe-friendly, so you can easily
chain together multiple functions to simulate and evaluate rollout
designs. The entire workflow can be written in a single pipe, like this:

``` r
results <- sim_params |>
  simulate_rollout_samples(n = 10) |>
  fit_model(model) |>
  eval_design(feature = "power")
```

# Performance

Rollout trials typically involve multi-level effects and large sample
size. This means simulations of rollout trials can be both slow and
memory intensive. The rollout package addresses these issues proactively
and comes equipped with a range of features to speed up computation.

## Fast data.table backend

The rollout package uses the `data.table` package (via `dtplyr`) as a
backend for data manipulation. This allows for faster data processing
and more efficient memory usage, especially for simulations with large
samples.

## Parallel processing

The rollout package supports parallel processing, which can
significantly speed up simulations of large rollout trials. By default,
the package uses the `future` package to run simulations in parallel.
You can control the number of cores used for parallel processing by
setting the `future::plan()` options.

``` r
future::plan(future::multisession, workers = 4)

results <- sim_params |>
  sim_samples(n = 100) |>
  fit_models(model) |>
  eval_design(feature = c("power", "bias"))
```

## Threshold inferences

Although final inferences should be based on the full set of
simulations, the study design process is typically iterative and
exploratory. The rollout package supports “threshold inferences,” which
can speed up simulations by stopping the simulation process early, if a
given design statistic (e.g., power) has crossed a particilar threshold.

``` r
results <- sim_params |>
  sim_samples(n = 100) |>
  fit_models(model) |>
  eval_design(
    feature = c("power", "bias"),
    threshold = list(power = 0.8, p = 0.05)
  )
```

## Progress bars

The rollout package includes a progress bar to help you track the
progress of your simulations. The progress bar is displayed by default,
but can be turned off by setting the `show_progress` argument to
`FALSE`.

[^1]: Note, the rollout package measures time in discrete steps, like
    days, months, or years. However, it is agnostic to units, so phases
    all need to be specified on the same time scale (e.g., if
    `baseline = 3` signifies “3 months” in your design, rollout will
    then assume `intervention = 2` is also in months). Lastly, the
    package does not (yet) support continuous time.
