# Add replicate identifiers for simulation replicates

Expands a long-format schedule to include a replicate identifier for
running multiple simulation replicates efficiently.

## Usage

``` r
initialize_replicates(long_schedule, n)
```

## Arguments

- long_schedule:

  A long-format rollout schedule.

- n:

  Integer specifying the number of replicates to generate.

## Value

A `tibble` with an added `sample_id` column for replicate indexing.

## Examples

``` r
schedule <- tibble::tibble(site = "A", cohort = 1, chron_time = 0, condition = "control")
initialize_replicates(schedule, n = 3)
#> # A tibble: 3 × 5
#>   sample_id site  cohort chron_time condition
#>       <int> <chr>  <dbl>      <dbl> <chr>    
#> 1         1 A          1          0 control  
#> 2         2 A          1          0 control  
#> 3         3 A          1          0 control  
```
