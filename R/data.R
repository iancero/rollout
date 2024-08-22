#' Example Site Data for Stepped Wedge Trial
#'
#' A dataset containing information about sites (clusters) for a stepped wedge trial.
#'
#' @format A data frame with rows and columns:
#' \describe{
#'   \item{cohort}{Factor or character, unique identifier for each site/cluster}
#'   \item{num_pcps}{Numeric, number of primary care providers at each site}
#'   \item{...}{Add other relevant variables that are in the site_data_ex dataset}
#' }
#' @source Simulated data for demonstration purposes
"site_data_ex"

#' Example Stepped Wedge Schedule
#'
#' A dataset representing a typical stepped wedge trial schedule.
#'
#' @format A data frame with rows and columns:
#' \describe{
#'   \item{cohort}{Factor or character, unique identifier for each site/cluster}
#'   \item{t1}{Factor, condition (control/intervention) at time point 1}
#'   \item{t2}{Factor, condition (control/intervention) at time point 2}
#'   \item{...}{Additional time points up to t9}
#' }
#' @source Simulated data for demonstration purposes
"stepped_wedge_ex"

#' Generate Example Site Data
#'
#' This function generates example site data for use in stepped wedge trial simulations.
#'
#' @param n Number of sites to generate
#' @param min_pcps Minimum number of primary care providers per site
#' @param max_pcps Maximum number of primary care providers per site
#'
#' @return A data frame with site information
#' @export
#'
#' @examples
#' generate_site_data(10, 1, 5)
generate_site_data <- function(n, min_pcps, max_pcps) {
  data.frame(
    cohort = paste0("Site", 1:n),
    num_pcps = sample(min_pcps:max_pcps, n, replace = TRUE)
  )
}

#' Generate Stepped Wedge Schedule
#'
#' This function generates a stepped wedge schedule for a given number of sites and time points.
#'
#' @param n_sites Number of sites
#' @param n_timepoints Number of time points
#' @param n_baseline_timepoints Number of time points where all sites are in control condition
#'
#' @return A data frame with the stepped wedge schedule
#' @export
#'
#' @examples
#' generate_stepped_wedge_schedule(10, 9, 2)
generate_stepped_wedge_schedule <- function(n_sites, n_timepoints, n_baseline_timepoints) {
  schedule <- matrix("control", nrow = n_sites, ncol = n_timepoints)
  sites_per_step <- ceiling(n_sites / (n_timepoints - n_baseline_timepoints))

  for (i in (n_baseline_timepoints + 1):n_timepoints) {
    start_site <- (i - n_baseline_timepoints - 1) * sites_per_step + 1
    end_site <- min(start_site + sites_per_step - 1, n_sites)
    schedule[start_site:end_site, i:n_timepoints] <- "intervention"
  }

  schedule_df <- as.data.frame(schedule)
  names(schedule_df) <- paste0("t", 1:n_timepoints)
  schedule_df$cohort <- paste0("Site", 1:n_sites)

  schedule_df
}
