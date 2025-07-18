% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model_fitting.R
\name{fit_models}
\alias{fit_models}
\title{Fit models in parallel across a list-column of datasets}
\usage{
fit_models(
  .data,
  .x,
  .f,
  packages = NULL,
  n_cores = parallel::detectCores() - 1
)
}
\arguments{
\item{.data}{A data frame containing a list-column of datasets to which the model
function will be applied.}

\item{.x}{Unquoted column name of the list-column containing the datasets.}

\item{.f}{A function or formula to apply to each dataset to fit the desired model
(e.g., \code{~ lm(y ~ x, data = .)} or \code{~ lme4::lmer(y ~ x + (x | group), data = .)}).}

\item{packages}{A character vector of package names to load on each parallel worker,
if your model-fitting function requires additional packages. Defaults to \code{NULL}.}

\item{n_cores}{Number of cores to use for parallel processing.
Defaults to \code{parallel::detectCores() - 1}.}
}
\value{
The original \code{.data} data frame with an additional \code{model} column containing
the fitted model objects returned by \code{.f}.
}
\description{
Applies a user-specified model-fitting function to each element of a list-column
of datasets in \code{.data}, fitting models in parallel with a progress bar, and returns
the original data frame with a new \code{model} column containing the fitted models.
}
\details{
This function is intended for use in simulation pipelines where multiple datasets
are generated (e.g., via \code{simulate_datasets()}), and models need to be fitted to
each dataset efficiently in parallel.

It uses \code{pbapply::pblapply()} to provide a progress bar during model fitting,
and \code{parallel::makeCluster()} for multi-core processing.

Packages specified in \code{packages} will be loaded on each worker to ensure model-fitting
functions that depend on those packages work correctly in parallel.
}
\examples{
library(dplyr)
library(purrr)
library(lme4)

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

# Tidy the fitted models using extract_model_results() for further evaluation
extracted <- extract_model_results(fitted_models)
head(extracted)

# Summarise estimates for 'Days' across simulated fits
extracted |>
  filter(term == "Days") |>
  evaluate_model_results(
    mean_estimate = mean(estimate, na.rm = TRUE),
    sd_estimate = sd(estimate, na.rm = TRUE)
  )

}
