test_that("pivot_schedule_longer correctly pivots wide schedule to long format", {
  schedule <- tibble::tibble(
    site = c("A", "B"),
    cohort = c(1, 2),
    t1 = c("ctrl", "ctrl"),
    t2 = c("intv", "intv")
  )

  result <- pivot_schedule_longer(schedule, time_cols = starts_with("t"))

  # Check structure
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 4)  # 2 sites × 2 timepoints
  expect_true("chron_time" %in% names(result))
  expect_true("condition" %in% names(result))
  expect_true("local_time" %in% names(result))

  # Check chronological time
  expect_equal(result$chron_time, c(1, 2, 1, 2))
  expect_type(result$chron_time, "double")

  # Check condition is a factor
  expect_s3_class(result$condition, "factor")
})

test_that("local_time resets correctly for each site within cohort", {
  # This is the bug fix test - multiple sites in same cohort
  schedule <- tibble::tibble(
    cohort = c(3, 3, 3),
    site = c("C", "D", "E"),
    t1 = c("ctrl", "ctrl", "ctrl"),
    t2 = c("ctrl", "ctrl", "ctrl"),
    t3 = c("ctrl", "ctrl", "ctrl"),
    t4 = c("ctrl", "ctrl", "ctrl"),
    t5 = c("intv", "intv", "intv"),
    t6 = c("intv", "intv", "intv"),
    t7 = c("intv", "intv", "intv"),
    t8 = c("intv", "intv", "intv")
  )

  result <- pivot_schedule_longer(schedule, time_cols = starts_with("t"))

  # Check that local_time resets for each site
  site_c <- result[result$site == "C", ]
  site_d <- result[result$site == "D", ]
  site_e <- result[result$site == "E", ]

  # Each site should have local_time: 0,1,2,3 for ctrl and 0,1,2,3 for intv
  expect_equal(site_c$local_time[site_c$condition == "ctrl"], 0:3)
  expect_equal(site_c$local_time[site_c$condition == "intv"], 0:3)

  expect_equal(site_d$local_time[site_d$condition == "ctrl"], 0:3)
  expect_equal(site_d$local_time[site_d$condition == "intv"], 0:3)

  expect_equal(site_e$local_time[site_e$condition == "ctrl"], 0:3)
  expect_equal(site_e$local_time[site_e$condition == "intv"], 0:3)
})

test_that("local_time increments correctly within site-cohort-condition", {
  schedule <- tibble::tibble(
    site = c("A"),
    cohort = c(1),
    t1 = c("ctrl"),
    t2 = c("ctrl"),
    t3 = c("intv"),
    t4 = c("intv"),
    t5 = c("intv")
  )

  result <- pivot_schedule_longer(schedule, time_cols = starts_with("t"))

  # Check ctrl local_time: should be 0, 1
  ctrl_rows <- result[result$condition == "ctrl", ]
  expect_equal(ctrl_rows$local_time, c(0, 1))

  # Check intv local_time: should be 0, 1, 2
  intv_rows <- result[result$condition == "intv", ]
  expect_equal(intv_rows$local_time, c(0, 1, 2))
})

test_that("local_time can be disabled", {
  schedule <- tibble::tibble(
    site = c("A", "B"),
    cohort = c(1, 2),
    t1 = c("ctrl", "intv"),
    t2 = c("intv", "intv")
  )

  result <- pivot_schedule_longer(schedule, time_cols = starts_with("t"), local_time = FALSE)

  # local_time column should not exist
  expect_false("local_time" %in% names(result))
})

test_that("custom column names work correctly", {
  schedule <- tibble::tibble(
    location = c("A", "B"),
    group = c(1, 2),
    time1 = c("ctrl", "intv"),
    time2 = c("intv", "intv")
  )

  result <- pivot_schedule_longer(
    schedule,
    time_cols = starts_with("time"),
    names_to = "timepoint",
    values_to = "treatment",
    cohort_name = "group",
    site_name = "location"
  )

  # Check custom names are present
  expect_true("timepoint" %in% names(result))
  expect_true("treatment" %in% names(result))
  expect_true("local_time" %in% names(result))

  # Check local_time still works with custom names
  expect_equal(nrow(result), 4)
  expect_type(result$local_time, "integer")
})

test_that("names_pattern extracts time correctly", {
  schedule <- tibble::tibble(
    site = c("A"),
    cohort = c(1),
    wave_1 = c("ctrl"),
    wave_2 = c("intv")
  )

  result <- pivot_schedule_longer(
    schedule,
    time_cols = starts_with("wave"),
    names_pattern = "wave_(\\d+)"
  )

  expect_equal(result$chron_time, c(1, 2))
})

test_that("values_transform converts to factor", {
  schedule <- tibble::tibble(
    site = c("A", "B"),
    cohort = c(1, 2),
    t1 = c("control", "intervention"),
    t2 = c("intervention", "intervention")
  )

  result <- pivot_schedule_longer(schedule, time_cols = starts_with("t"))

  expect_s3_class(result$condition, "factor")
  expect_equal(levels(result$condition), c("control", "intervention"))
})

test_that("handles single site correctly", {
  schedule <- tibble::tibble(
    site = c("A"),
    cohort = c(1),
    t1 = c("ctrl"),
    t2 = c("ctrl"),
    t3 = c("intv")
  )

  result <- pivot_schedule_longer(schedule, time_cols = starts_with("t"))

  expect_equal(nrow(result), 3)
  expect_equal(result$site, rep("A", 3))
  expect_equal(result$local_time[result$condition == "ctrl"], c(0, 1))
  expect_equal(result$local_time[result$condition == "intv"], 0)
})

test_that("handles stepped wedge design correctly", {
  # Realistic stepped wedge with 4 cohorts, 8 sites, 8 timepoints
  schedule <- tibble::tribble(
    ~cohort, ~site, ~t1, ~t2, ~t3, ~t4, ~t5, ~t6, ~t7, ~t8,
    1,    "A",  "intv", "intv", "intv",  "intv", "intv", "intv", "intv", "intv",
    2,    "B",  "ctrl", "ctrl", "intv",  "intv", "intv", "intv", "intv", "intv",
    3,    "C",  "ctrl", "ctrl",  "ctrl", "ctrl", "intv", "intv", "intv", "intv",
    3,    "D",  "ctrl", "ctrl",  "ctrl", "ctrl", "intv", "intv", "intv", "intv",
    3,    "E",  "ctrl", "ctrl", "ctrl", "ctrl",  "intv", "intv", "intv", "intv",
    4,    "F",  "ctrl", "ctrl", "ctrl", "ctrl",  "ctrl", "ctrl", "intv", "intv",
    4,    "G",  "ctrl", "ctrl", "ctrl", "ctrl", "ctrl", "ctrl",  "intv", "intv",
    4,    "H",  "ctrl", "ctrl", "ctrl", "ctrl", "ctrl", "ctrl",  "intv", "intv"
  )

  result <- pivot_schedule_longer(schedule, time_cols = starts_with("t"))

  expect_equal(nrow(result), 64)  # 8 sites × 8 timepoints

  # Check cohort 3 sites all have proper local_time
  cohort3_c <- result[result$cohort == 3 & result$site == "C", ]
  cohort3_d <- result[result$cohort == 3 & result$site == "D", ]
  cohort3_e <- result[result$cohort == 3 & result$site == "E", ]

  # All three should have same pattern: 4 ctrl (0-3), 4 intv (0-3)
  expect_equal(cohort3_c$local_time[cohort3_c$condition == "ctrl"], 0:3)
  expect_equal(cohort3_c$local_time[cohort3_c$condition == "intv"], 0:3)

  expect_equal(cohort3_d$local_time[cohort3_d$condition == "ctrl"], 0:3)
  expect_equal(cohort3_d$local_time[cohort3_d$condition == "intv"], 0:3)

  expect_equal(cohort3_e$local_time[cohort3_e$condition == "ctrl"], 0:3)
  expect_equal(cohort3_e$local_time[cohort3_e$condition == "intv"], 0:3)
})

test_that("local_time is always non-negative integer", {
  schedule <- tibble::tibble(
    site = rep(c("A", "B", "C"), each = 1),
    cohort = c(1, 2, 3),
    t1 = c("ctrl", "ctrl", "ctrl"),
    t2 = c("intv", "ctrl", "ctrl"),
    t3 = c("intv", "intv", "ctrl"),
    t4 = c("intv", "intv", "intv")
  )

  result <- pivot_schedule_longer(schedule, time_cols = starts_with("t"))

  # All local_time values should be >= 0
  expect_true(all(result$local_time >= 0))

  # All local_time values should be integers
  expect_type(result$local_time, "integer")
})

test_that("preserves additional columns in schedule", {
  schedule <- tibble::tibble(
    site = c("A", "B"),
    cohort = c(1, 2),
    region = c("North", "South"),
    population = c(1000, 1500),
    t1 = c("ctrl", "intv"),
    t2 = c("intv", "intv")
  )

  result <- pivot_schedule_longer(schedule, time_cols = starts_with("t"))

  # Additional columns should be preserved
  expect_true("region" %in% names(result))
  expect_true("population" %in% names(result))
  expect_equal(unique(result$region), c("North", "South"))
  expect_equal(unique(result$population), c(1000, 1500))
})

test_that("handles complex cohort structures", {
  # Different number of sites per cohort
  schedule <- tibble::tribble(
    ~cohort, ~site, ~t1, ~t2, ~t3,
    1,    "A", "intv", "intv", "intv",
    2,    "B", "ctrl", "intv", "intv",
    2,    "C", "ctrl", "intv", "intv",
    2,    "D", "ctrl", "intv", "intv",
    3,    "E", "ctrl", "ctrl", "intv"
  )

  result <- pivot_schedule_longer(schedule, time_cols = starts_with("t"))

  # Cohort 2 has 3 sites - each should have independent local_time
  cohort2_b <- result[result$site == "B", ]
  cohort2_c <- result[result$site == "C", ]
  cohort2_d <- result[result$site == "D", ]

  expect_equal(cohort2_b$local_time[cohort2_b$condition == "ctrl"], 0)
  expect_equal(cohort2_b$local_time[cohort2_b$condition == "intv"], 0:1)

  expect_equal(cohort2_c$local_time[cohort2_c$condition == "ctrl"], 0)
  expect_equal(cohort2_c$local_time[cohort2_c$condition == "intv"], 0:1)

  expect_equal(cohort2_d$local_time[cohort2_d$condition == "ctrl"], 0)
  expect_equal(cohort2_d$local_time[cohort2_d$condition == "intv"], 0:1)
})
