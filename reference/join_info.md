# Join unit-level information to a long-format rollout schedule

Merges unit-level characteristics or parameters into a long-format
rollout schedule and optionally expands rows based on count variables to
create multiple units per site.

## Usage

``` r
join_info(
  long_schedule,
  unit_info,
  by = NULL,
  uncount_vars = NULL,
  .ids = NULL
)
```

## Arguments

- long_schedule:

  A long-format schedule (output from `pivot_schedule_longer`).

- unit_info:

  A data frame with unit-level information to join.

- by:

  Columns used to join `long_schedule` and `unit_info` (default `NULL`
  uses shared columns).

- uncount_vars:

  Optional character vector or list of quosures indicating count
  variables to expand rows.

- .ids:

  Optional character vector specifying names of id columns when
  uncounting, one per `uncount_var`.

## Value

A `tibble` with joined and optionally expanded rows to reflect unit
counts.

## Examples

``` r
schedule <- tibble::tibble(site = "A", cohort = 1, chron_time = 0, condition = "control")
unit_info <- tibble::tibble(site = "A", n_units = 3)
join_info(schedule, unit_info, by = "site", uncount_vars = "n_units")
#> # A tibble: 3 × 5
#>   site  cohort chron_time condition n_units_id
#>   <chr>  <dbl>      <dbl> <chr>          <int>
#> 1 A          1          0 control            1
#> 2 A          1          0 control            2
#> 3 A          1          0 control            3
```
