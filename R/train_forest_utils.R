#' @noRd
get_n_indiv <- function(X, id_col = "_original_id") {
  length(unique(X[[id_col]]))
}


#' @noRd
get_n_predictors <- function(
  X,
  not_predictors = c(
    "_original_id",
    "id",
    "t.start",
    "t.stop",
    "death",
    "_t.start",
    "_t.stop",
    "_death"
  )
) {
  length(
    colnames(X)[!colnames(X) %in% not_predictors]
  )
}

#' @noRd
#' @importFrom purrr map
get_n_nodes <- function(
  trained_forest) {
  map(trained_forest$trees, "nodes") %>%
    map(~ length(.x))
}

#' @noRd
prepare_dataset_for_recforest <- function(
  data,
  covariates,
  event,
  time_variables,
  id_var,
  death_variable = NULL
) {
  if (is.null(death_variable)) {
    death_variable <- "death"
    data[, death_variable] <- 0
  }
  X <- data[, c(id_var, time_variables, death_variable, covariates)]
  Y <- data[, event, drop = TRUE]

  list(X = X, Y = Y)
}

#' @noRd
prepare_dataset_for_predict <- function(
  data,
  covariates,
  time_variables,
  id_var,
  death_variable = NULL
) {
  if (is.null(death_variable)) {
    death_variable <- "death"
    data[, death_variable] <- 0
  }
  data[, c(id_var, time_variables, death_variable, covariates)]
}
