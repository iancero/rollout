# Pivot a rollout schedule from wide to long format with local time calculation

Transforms a wide-format rollout schedule into a long-format schedule,
extracting chronological time from column names, converting condition
columns to factors, and adding local time within each cohort if desired.

## Usage

``` r
pivot_schedule_longer(
  schedule,
  time_cols,
  names_to = "chron_time",
  names_pattern = ".*(\\d+)",
  names_transform = as.numeric,
  values_to = "condition",
  values_transform = as.factor,
  cohort_name = cohort,
  local_time = TRUE
)
```

## Arguments

- schedule:

  A data frame containing the rollout schedule in wide format.

- time_cols:

  Columns containing time-specific condition assignments (tidyselect
  syntax).

- names_to:

  Name of the new column to store extracted chronological time (default
  `"chron_time"`).

- names_pattern:

  Regular expression to extract the numeric time from column names
  (default `".*(\\d+)"`).

- names_transform:

  Function to transform extracted time values (default `as.numeric`).

- values_to:

  Name of the new column to store condition values (default
  `"condition"`).

- values_transform:

  Function to transform condition values (default `as.factor`).

- cohort_name:

  The column indicating cohort membership for local time calculation
  (default `cohort`).

- local_time:

  Logical; if `TRUE`, adds a `local_time` column indicating time since
  rollout start for each cohort and condition (default `TRUE`).

## Value

A long-format `tibble` with columns for cohort, condition, chronological
time, and optionally local time.

## Examples

``` r
library(dplyr)
library(tidyr)
#> 
#> Attaching package: ‘tidyr’
#> The following objects are masked from ‘package:Matrix’:
#> 
#>     expand, pack, unpack
schedule <- tibble::tibble(
  site = c("A", "B"),
  cohort = c(1, 2),
  t1 = c("control", "intervention"),
  t2 = c("intervention", "intervention")
)
pivot_schedule_longer(schedule, time_cols = starts_with("t"))
#> Error in dplyr::group_by(schedule, rlang::.data[[cohort_name_char]], rlang::.data[[values_to]]): ℹ In argument: `rlang::.data[[cohort_name_char]]`.
#> Caused by error in `pivot_schedule_longer()`:
#> ! Can't subset `.data` outside of a data mask context.
```
