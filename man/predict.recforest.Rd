% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/0_s3_methods.R
\name{predict.recforest}
\alias{predict.recforest}
\title{Predict using a recforest model}
\usage{
\method{predict}{recforest}(
  object,
  newdata,
  id_var,
  covariates,
  time_vars = c("t.start", "t.stop"),
  death_var = NULL,
  ...
)
}
\arguments{
\item{object}{A recforest model object.}

\item{newdata}{A data frame containing the input features.}

\item{id_var}{The name of the column containing the unique identifier for each subject.}

\item{covariates}{A character vector containing the names of the columns to be used as predictors in the model.}

\item{time_vars}{A length-2 character vector containing the names of the columns representing the start and stop times (default "t.start" and "t.stop").}

\item{death_var}{The name of the column containing the death indicator or other any terminal event (optional).}

\item{...}{Optional parameters to be passed to the low level function}
}
\value{
A vector of expected mean cumulative number of recurrent events per individual at the end of follow-up.
}
\description{
This function generates predictions from a recforest model given a set of input features.
}
\details{
The \code{predict_recforest} function utilizes the ensemble of trees in the recforest model to generate predictions for new data. For each observation in \code{newdata}, the function aggregates the predictions from all trees in the recforest to provide a robust estimate.

Depending on the \code{method} specified during the initial training of the recforest model, the algorithm employs different prediction strategies:
\itemize{
\item For standard recurrent event data, the function outputs the Nelson-Aalen estimates of the mean cumulative function.
\item In the presence of terminal events, the function outputs the Ghosh-Lin estimates of the mean cumulative function.
}

The predictions represent the expected mean number of recurrent events for each individual at the end of the follow-up period.
}
\examples{
data("bladder1_recforest")
trained_forest <- train_forest(
  data = bladder1_recforest,
  id_var = "id",
  covariates = c("treatment", "number", "size"),
  time_vars = c("t.start", "t.stop"),
  death_var = "death",
  event = "event",
  n_trees = 2,
  n_bootstrap = 70,
  mtry = 2,
  minsplit = 3,
  nodesize = 15,
  method = "NAa",
  min_score = 5,
  max_nodes = 20,
  seed = 111,
  parallel = FALSE,
  verbose = FALSE
)
predictions <- predict(
 trained_forest,
 newdata = bladder1_recforest,
 id_var = "id",
 covariates = c("treatment", "number", "size"),
 time_vars = c("t.start", "t.stop"),
 death_var = "death"
)
}
\references{
Cook, R. J., & Lawless, J. F. (1997). Marginal analysis of recurrent events and a terminating event. Statistics in medicine, 16(8), 911-924.

Ghosh, D., & Lin, D. Y. (2002). Marginal regression models for recurrent and terminal events. Statistica Sinica, 663-688.

Ishwaran, H., Kogalur, U. B., Blackstone, E. H., & Lauer, M. S. (2008). Random survival forests.
}
