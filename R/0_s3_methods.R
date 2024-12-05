#' Check if a Variable is Supported
#'
#' This function is a generic method that checks if a given variable is supported.
#' The actual implementation of the check is provided by specific methods for different classes of variables.
#'
#' @param x The variable to be checked.
#' @return A logical value indicating whether the variable is supported.
#' @export
is_supported_variable <- function(x) {
  UseMethod("is_supported_variable")
}

#' @export
is_supported_variable.default <- function(x) {
  FALSE
}

#' @export
is_supported_variable.factor <- function(x) {
  nlevels(x) > 1
}

#' @export
is_supported_variable.numeric <- function(x) {
  TRUE
}

#' Make a Decision Based on Input
#'
#' This function serves as a generic method for making decisions based on the input `x` and `value`.
#' It dispatches to the appropriate method depending on the class of `x`.
#'
#' @param x An object for which a decision needs to be made.
#' @param value A value that influences the decision-making process.
#' @return The result of the decision-making process, which depends on the specific method implementation.
#' @export
make_decision <- function(x, value) {
  UseMethod("make_decision")
}

#' @export
make_decision.factor <- function(x, value) {
  x == value
}

#' @export
make_decision.numeric <- function(x, value) {
  x <= value
}


#' Predict using a recforest model
#'
#' This function generates predictions from a recforest model given a set of input features.
#'
#' @param object A recforest model object.
#' @param newdata A data frame containing the input features.
#' @param id_var The name of the column containing the unique identifier for each subject.
#' @param covariates A character vector containing the names of the columns to be used as predictors in the model.
#' @param time_vars A length-2 character vector containing the names of the columns representing the start and stop times (default "t.start" and "t.stop").
#' @param death_var The name of the column containing the death indicator or other any terminal event (optional).
#' @param ... Optional parameters to be passed to the low level function
#' @return A vector of expected mean cumulative number of recurrent events per individual at the end of follow-up.
#' @details
#' The `predict_recforest` function utilizes the ensemble of trees in the recforest model to generate predictions for new data. For each observation in `newdata`, the function aggregates the predictions from all trees in the recforest to provide a robust estimate.
#'
#' Depending on the `method` specified during the initial training of the recforest model, the algorithm employs different prediction strategies:
#' - For standard recurrent event data, the function outputs the Nelson-Aalen estimates of the mean cumulative function.
#' - In the presence of terminal events, the function outputs the Ghosh-Lin estimates of the mean cumulative function.
#'
#' The predictions represent the expected mean number of recurrent events for each individual at the end of the follow-up period.
#' @references
#' Cook, R. J., & Lawless, J. F. (1997). Marginal analysis of recurrent events and a terminating event. Statistics in medicine, 16(8), 911-924.
#'
#' Ghosh, D., & Lin, D. Y. (2002). Marginal regression models for recurrent and terminal events. Statistica Sinica, 663-688.
#'
#' Ishwaran, H., Kogalur, U. B., Blackstone, E. H., & Lauer, M. S. (2008). Random survival forests.
#' @export
#' @examples
#' data("bladder1_recforest")
#' trained_forest <- train_forest(
#'   data = bladder1_recforest,
#'   id_var = "id",
#'   covariates = c("treatment", "number", "size"),
#'   time_vars = c("t.start", "t.stop"),
#'   death_var = "death",
#'   event = "event",
#'   n_trees = 2,
#'   n_bootstrap = 70,
#'   mtry = 2,
#'   minsplit = 3,
#'   nodesize = 15,
#'   method = "NAa",
#'   min_score = 5,
#'   max_nodes = 20,
#'   seed = 111,
#'   parallel = FALSE,
#'   verbose = FALSE
#' )
#' predictions <- predict(
#'  trained_forest,
#'  newdata = bladder1_recforest,
#'  id_var = "id",
#'  covariates = c("treatment", "number", "size"),
#'  time_vars = c("t.start", "t.stop"),
#'  death_var = "death"
#')
predict.recforest <- function(
  object,
  newdata,
  id_var,
  covariates,
  time_vars = c("t.start", "t.stop"),
  death_var = NULL,
  ...
) {
  X <- prepare_dataset_for_predict(
    data = newdata,
    covariates = covariates,
    time_variables = time_vars,
    id_var = id_var,
    death_variable = death_var
  )

  predict_forest(object, X)
}


#' @title Summary Method for recforest Objects
#' @description This function provides a summary of a recforest object by printing its metrics.
#' @param object An object of class \code{recforest}.
#' @param ... Additional arguments to be passed to the summary function.
#' @return The function prints the metrics of the \code{recforest} object.
#' @importFrom cli cli_alert_info cli_h1
#' @export
#' @method summary recforest
summary.recforest <- function(object, ...) {
  cli::cli_h1("Data summary")
  cli::cli_alert_info(sprintf("Number of individuals : %s", object$n_indiv))
  cli::cli_alert_info(sprintf("Number of predictors : %s", object$n_predictors))
  cli::cli_h1("Model parameters")
  cli::cli_alert_info(sprintf("mtry : %s", object$params$mtry))
  cli::cli_alert_info(sprintf("minsplit : %s", object$params$minsplit))
  cli::cli_alert_info(sprintf("nodesize : %s", object$params$nodesize))
  cli::cli_alert_info(sprintf("method : %s", object$params$method))
  cli::cli_alert_info(sprintf("min_score : %s", object$params$min_score))
  cli::cli_alert_info(sprintf("max_nodes : %s", object$params$max_nodes))
  cli::cli_h1("Metrics")
  cli::cli_alert_info(sprintf("c_index : %s", object$metrics$c_index))
  cli::cli_alert_info(sprintf("mse_imse : %s", object$metrics$mse_imse))
  cli::cli_alert_info(sprintf("mse_iscore : %s", object$metrics$mse_iscore))
  cli::cli_alert_info(sprintf("computation time (seconds) : %s", format(object$time, digits = 2)))
}


#' Plot method for recforest objects
#'
#' @param x An object of class \code{recforest}.
#' @param ... Additional arguments to be passed to the plot function.
#'
#' @export
#' @method plot recforest
#'
#' @return No return value, used for side effect.
plot.recforest <- function(x, ...) {
  stop("No plot method implemented for recforest objects for the moment")
  # plot(x, ...)
}

#' Print method for recforest objects
#'
#' @param x An object of class \code{recforest}.
#' @param ... Additional arguments to be passed to the plot print
#' @importFrom purrr pmap
#' @importFrom cli cli_h2 cli_alert_info
#' @export
#' @return Used for side effect.
#' Will print to the console a description of each tree, with, for each:
#' - The number of nodes in the tree
#' - The metrics of the tree (c_index, mse_imse and mse_iscore)
#'
print.recforest <- function(x, ...) {
  n_nodes <- get_n_nodes(x)
  n_trees <- 1:length(n_nodes)
  invisible(purrr::pmap(
    list(x$tree_metrics, n_nodes, n_trees),
    function(metrics, n_nodes, n_tree) {
      cli::cli_h2(sprintf("Tree %s", n_tree))
      cli::cli_alert_info(sprintf("Number of nodes : %s", n_nodes))
      cli::cli_alert_info(sprintf("c_index : %s", metrics$c_index))
      cli::cli_alert_info(sprintf("mse_imse : %s", metrics$mse_imse))
      cli::cli_alert_info(sprintf("mse_iscore : %s", metrics$mse_iscore))
      return()
    }
  ))
}
