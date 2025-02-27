% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/grbLMM.R
\name{cv.grbLMM}
\alias{cv.grbLMM}
\title{Cross-validated Generalized Randomized Boosted Linear Mixed Models}
\usage{
cv.grbLMM(
  y,
  X,
  Z,
  id,
  prop,
  beta.fit = NULL,
  beta.predict = NULL,
  beta.init = NULL,
  beta.keep.all = TRUE,
  m.stop = 500,
  ny = 0.1,
  cores = 1,
  cl = NULL,
  refit = FALSE,
  ...
)
}
\arguments{
\item{y}{The dependent variable.}

\item{X}{Numeric design matrix for the fixed effects (no intercept column).}

\item{Z}{Numeric design matrix for the random effects, same shape as X with intercepts in the first column.}

\item{id}{Grouping factor.}

\item{prop}{Proportion of data to use for validation.}

\item{beta.fit}{Fit function for fixed effects.}

\item{beta.predict}{Predict function for fixed effects.}

\item{beta.init}{Initial value for fixed effects.}

\item{beta.keep.all}{Whether to return all betas in all iterations.}

\item{m.stop}{Given number of total iterations.}

\item{ny}{Learning rate, i.e., step length, of the algorithm.}

\item{cores}{Number of CPU cores to use for parallel processing.}

\item{cl}{Cluster object for parallel processing.}

\item{refit}{Whether to refit the model with the optimal number of iterations.}

\item{...}{Additional arguments passed to the fit and predict functions.}
}
\value{
A list containing the model parameters, evaluation metrics, and cross-validation results.
}
\description{
Fits a boosting algorithm for linear mixed models with k-fold cross-validation.
}
