
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

## Basic Example

We begin with a basic example of the most widely known rollout trial
format: a stepped wedge trial. In such a trial, sites are randomly
assigned to cohorts. Then, these cohorts are assigned to receive the
intervention beginning at different times. Data are recorded for the
duration of the study, both before (baseline condition) and after the
initiation of an intervention condition.

In `rollout`, simulating such a trial begins with a matrix specification
of the cohorts and their transition points, like so.

``` r
library(tidyverse)
#> ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
#> ✔ dplyr     1.1.4     ✔ readr     2.1.5
#> ✔ forcats   1.0.0     ✔ stringr   1.5.1
#> ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
#> ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
#> ✔ purrr     1.0.2     
#> ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
#> ✖ dplyr::filter() masks stats::filter()
#> ✖ dplyr::lag()    masks stats::lag()
#> ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

rollout_schedule <- tribble(
  ~cohort, ~site, ~t1,    ~t2,    ~t3,    ~t4,    ~t5,
  "c1",    "001", "bsln", "intv", "intv", "intv", "intv",
  "c1",    "002", "bsln", "intv", "intv", "intv", "intv",
  "c2",    "003", "bsln", "bsln", "intv", "intv", "intv",
  "c2",    "004", "bsln", "bsln", "intv", "intv", "intv",
  "c3",    "005", "bsln", "bsln", "bsln", "intv", "intv",
  "c3",    "006", "bsln", "bsln", "bsln", "intv", "intv",
  "c4",    "007", "bsln", "bsln", "bsln", "bsln", "intv",
  "c4",    "008", "bsln", "bsln", "bsln", "bsln", "intv"
)
```

This allows the user maximum control over what can become a complex
process of scheduling, responding to real world pragmatic constraints.
But in this format, it is not yet convenient for simulation, so
`rollout` offers the utility function `pivot_schedule_longer()`.

``` r
library(rollout)

rollout_design <- rollout_schedule |> 
  pivot_schedule_longer(t1:t5)
#> # A tibble: 40 × 4
#>    cohort site  chron_time condition
#>    <chr>  <chr>      <dbl> <fct>    
#>  1 c1     001            1 bsln     
#>  2 c1     001            2 intv     
#>  3 c1     001            3 intv     
#>  4 c1     001            4 intv     
#>  5 c1     001            5 intv     
#>  6 c1     002            1 bsln     
#>  7 c1     002            2 intv     
#>  8 c1     002            3 intv     
#>  9 c1     002            4 intv     
#> 10 c1     002            5 intv     
#> # ℹ 30 more rows

head(rollout_design)
#> # A tibble: 6 × 5
#>   cohort site  chron_time condition local_time
#>   <chr>  <chr>      <dbl> <fct>          <dbl>
#> 1 c1     001            1 bsln               0
#> 2 c1     001            2 intv               0
#> 3 c1     001            3 intv               1
#> 4 c1     001            4 intv               2
#> 5 c1     001            5 intv               3
#> 6 c1     002            1 bsln               1
```

In addition to performing a wide-to-long pivot on our schedule, this
function has also computed two time columns: chron_time and local_time
(0-indexed, to make cohort-specific effects easier to program). It has
also constructed a condition column of all the conditions.

Now that the data are in this format, the user is prepared to incoporate
any site-level background information they might have. For example, as
part of pre-trial assessment and coordination with the study sites, a
user might have access to site-level information, like the self-reported
readiness to receive the intervention at the site, as well as the
relative population of observational units at the site (e.g., classes
per timepoint, servicemembers per class). For simplicity, we imagine
some of these data below.

``` r
site_info <- tribble(
  ~site, ~admin_readiness, ~classes_per_time, ~members_per_class,
  "001",    6,                11,                23,
  "002",    7,                15,                25,
  "003",    6,                11,                22,
  "004",    9,                12,                16,
  "005",    7,                11,                24,
  "006",    2,                9,                 22,
  "007",    2,                9,                 16,
  "008",    7,                14,                25,
)
```

And then link them to our design dataframe. We can do this with a simple
left_join(), but we are stuck with the fact that two of our columns are
actually counts of observational units (in fact, nested units), which
will need to be expanded too. Anticipating this is a common situation,
`rollout` has its own helper function for joining and expanding such
site-related information:

``` r
rollout_design <- rollout_design |> 
  join_info(
    site_info,
    by = "site",
    uncount_vars = c("classes_per_time", "members_per_class"),
    .ids = c("class", "member")
  )
```

With our design fully expanded, we now need to initialize several
replicates of this same dataset for later simulation of data.

``` r
simulated_trials <- rollout_design |> 
  initialize_replicates(n = 100)

head(simulated_trials)
#> # A tibble: 6 × 9
#>   sample_id cohort site  chron_time condition local_time admin_readiness class
#>       <int> <chr>  <chr>      <dbl> <fct>          <dbl>           <dbl> <int>
#> 1         1 c1     001            1 bsln               0               6     1
#> 2         2 c1     001            1 bsln               0               6     1
#> 3         3 c1     001            1 bsln               0               6     1
#> 4         4 c1     001            1 bsln               0               6     1
#> 5         5 c1     001            1 bsln               0               6     1
#> 6         6 c1     001            1 bsln               0               6     1
#> # ℹ 1 more variable: member <int>
```

### Simulating effects

#### Fixed effects

To start, let’s add an intervention effect that gives an immediate boost
in individual-level outcomes, followed by slow further improvement.
We’ll also add an effect representing a potential confound - a global
improvement in the overall population (what an economist might call a
“secular trend”).

``` r
simulated_trials <- simulated_trials |>
  add_fixed_effect(
    condition = case_when(
      condition == "bsln" ~ 0,
      condition == "intv" ~ .50 + .10*local_time)) |> 
  add_fixed_effect(
    popwide_decline = .10*chron_time)

head(simulated_trials)
#> # A tibble: 6 × 11
#>   sample_id cohort site  chron_time condition local_time admin_readiness class
#>       <int> <chr>  <chr>      <dbl> <fct>          <dbl>           <dbl> <int>
#> 1         1 c1     001            1 bsln               0               6     1
#> 2         2 c1     001            1 bsln               0               6     1
#> 3         3 c1     001            1 bsln               0               6     1
#> 4         4 c1     001            1 bsln               0               6     1
#> 5         5 c1     001            1 bsln               0               6     1
#> 6         6 c1     001            1 bsln               0               6     1
#> # ℹ 3 more variables: member <int>, .condition <dbl>, .popwide_decline <dbl>
```

Underneath the hood, `add_fixed_effect()` is a thin wrapper around
`mutate()` that does a few things: first, it limits the number of
variables that can be created in a single call to exactly one named
argument. This admittedly creates some redundant work for the user, who
now has to call `add_fixed_effect()` multiple times, but comes with the
advantage of forcing more readable code for these designs. All effect
generation functions (more on these below) in `rollout` follow this
convention. Second, they pre-pend all effect variables with a “`.`” to
make it obvious which variables in the `simulated_trials` dataframe are
data and which are effects that will eventually be used in the
generation of an outcome variable. In this way, the user can focus on
data that they want the effect to be associated with, rather than
keeping variable names straight. It also facilitates the creation of an
outcome creation function, which can automatically search for effects in
`simulated_trials` and aggregate them into an outcome.

#### Random effects

Random effects are simulated similarly to fixed effects, except that it
is sometimes important to take nesting structure into account. For
example, to create a class-level random intercept, we need to specify
that classes are nested within sites and cohorts, like so:

``` r
simulated_trials <- simulated_trials |>
  add_random_effect(
    instructor_experience = rnorm(n = 1, mean = 0, sd = .30),
    .nesting = c("cohort", "site", "class"))

head(simulated_trials)
#> # A tibble: 6 × 12
#>   sample_id cohort site  chron_time condition local_time admin_readiness class
#>       <int> <chr>  <chr>      <dbl> <fct>          <dbl>           <dbl> <int>
#> 1         1 c1     001            1 bsln               0               6     1
#> 2         2 c1     001            1 bsln               0               6     1
#> 3         3 c1     001            1 bsln               0               6     1
#> 4         4 c1     001            1 bsln               0               6     1
#> 5         5 c1     001            1 bsln               0               6     1
#> 6         6 c1     001            1 bsln               0               6     1
#> # ℹ 4 more variables: member <int>, .condition <dbl>, .popwide_decline <dbl>,
#> #   .instructor_experience <dbl>
```

As another example in this trial, it might also be important for even
instances of a given class to share variance because they have
overlapping students.

``` r
simulated_trials <- simulated_trials |>
  add_random_effect(
    student_cohesion = rnorm(n = 1, mean = 0, sd = .20),
    .nesting = c("cohort", "site", "class", "local_time"))

head(simulated_trials)
#> # A tibble: 6 × 13
#>   sample_id cohort site  chron_time condition local_time admin_readiness class
#>       <int> <chr>  <chr>      <dbl> <fct>          <dbl>           <dbl> <int>
#> 1         1 c1     001            1 bsln               0               6     1
#> 2         2 c1     001            1 bsln               0               6     1
#> 3         3 c1     001            1 bsln               0               6     1
#> 4         4 c1     001            1 bsln               0               6     1
#> 5         5 c1     001            1 bsln               0               6     1
#> 6         6 c1     001            1 bsln               0               6     1
#> # ℹ 5 more variables: member <int>, .condition <dbl>, .popwide_decline <dbl>,
#> #   .instructor_experience <dbl>, .student_cohesion <dbl>
```

Again, underneath the hood, this is a wrapper to some dplyr functions
applying a groupby-mutate-ungroup pattern. However, it is slightly more
complicated because it will intercept the existing group structure of
the incoming dataframe (e.g., if group_by has been called before), store
it, calculate the random effect according to the nesting structure (if
specified), then regroup the dataframe exactly as it was before the
function. If no nesting structure is specified, this function will
simply rely on the grouping structure given to it in the incoming
dataframe.

#### Error variance

Finally, the simplest effect to add is random error, which is simple
enough to specify with just a variance.

``` r
simulated_trials <- simulated_trials |>
  add_error(variance = .25)
```

### Outcome variables

We can add three different types of outcome variables: linear, binary
(logistic), and Poisson. Unlike the effects above, these at NOT
automatically prepended with “.” because they are observed values -
rather than effects, they are data. Additionally, both the binary and
Poisson functions return multiple columns, including the final observed
output column (i.e., the simulated binary outcome) and the associated
input columns that were involved in producing it (i.e., y_linear,
y_prob).

``` r
simulated_trials <- simulated_trials |>
  add_binary_outcome()

head(simulated_trials)
#> # A tibble: 6 × 17
#>   sample_id cohort site  chron_time condition local_time admin_readiness class
#>       <int> <chr>  <chr>      <dbl> <fct>          <dbl>           <dbl> <int>
#> 1         1 c1     001            1 bsln               0               6     1
#> 2         2 c1     001            1 bsln               0               6     1
#> 3         3 c1     001            1 bsln               0               6     1
#> 4         4 c1     001            1 bsln               0               6     1
#> 5         5 c1     001            1 bsln               0               6     1
#> 6         6 c1     001            1 bsln               0               6     1
#> # ℹ 9 more variables: member <int>, .condition <dbl>, .popwide_decline <dbl>,
#> #   .instructor_experience <dbl>, .student_cohesion <dbl>, .error <dbl>,
#> #   y_linear <dbl>, y_prob <dbl>, y_bin <int>
```
