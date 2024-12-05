#' @noRd
#' @importFrom tibble as_tibble
summarize_tree_metrics <- function(metrics, summary_fun = mean) {
  as_tibble(t(colMeans(do.call(rbind, lapply(metrics, as.data.frame)))))
}

#' @noRd
#' @import dplyr
train_tree_and_calculate_metrics <- function(
  i,
  X,
  Y,
  X_per_id,
  Y_per_id,
  params,
  n_bootstrap,
  verbose = TRUE
) {

  trained_tree <- train_tree(
    X = X,
    Y = Y,
    X_per_id = X_per_id,
    Y_per_id = Y_per_id,
    params = params,
    n_bootstrap = n_bootstrap,
    tree_id = i,
    verbose = verbose
  )

  oob_idxs <- X[["_original_id"]] %in% trained_tree$oob_ids

  test_X <- X[oob_idxs, ]

  true_vals <- tibble(
    id = test_X[["_original_id"]],
    t.start = test_X[["_t.start"]],
    t.stop = test_X[["_t.stop"]],
    event = Y[oob_idxs],
    event_count_pred = predict_tree(
      tree = trained_tree,
      X = test_X
    )
  ) %>%
    group_by(id) %>%
    mutate(
      event_count_true = cumsum(event)
    ) %>%
    ungroup()

  mse_res <- mse_2(true_vals)

  list(
    tree = trained_tree,
    metrics = list(
      c_index = c_index_2(true_vals),
      mse_imse = mse_res$imse,
      mse_iscore = mse_res$iscore
    )
  )
}


#' Train a Recforest Model
#'
#' This function trains a recforest model using the provided data and parameters.
#'
#' @param data A data frame containing the dataset to be used for training the model.
#' @param id_var The name of the column containing the unique identifier for each subject.
#' @param covariates A character vector containing the names of the columns to be used as predictors in the model.
#' @param event The name of the column containing the recurrent event indicator.
#' @param time_vars A length-2 character vector containing the names of the columns representing the start and stop times (default "t.start" and "t.stop").
#' @param death_var The name of the column containing the death indicator or other any terminal event (optional).
#' @param n_trees The number of trees to be trained in the recforest model.
#' @param n_bootstrap The number of bootstrap samples to be used for training each tree (in-bag sample).
#'  If not provided, it is set to 2/3 of the sample size (in term of number of unique `id_var`).
#' @param seed An optional seed value to be used for reproducibility purpose (NULL by default).
#' @param mtry The number of candidate variables randomly drawn at each node of the trees.
#' This parameter should be tuned by minimizing the OOB error.
#' @param minsplit The minimal number of events required to split the node. Cannot be smaller than 2.
#' @param nodesize The minimal number of subjects required in both child nodes to split. Cannot be smaller than 1.
#' @param method The method to be used for training the model. Currently, the following methods are supported : either "NAa" for Nelson-Aalen method, with no terminal event and no longitudinal time-dependent features; either "GL" for Ghosh-Lin modelization step with a terminal event and/or at least one longitudinal time-dependent feature.
#' @param min_score The minimum score required to split a node. This parameter is used only when the method is set to "NAa".
#' @param max_nodes The maximum number of nodes per tree.
#' @param parallel A logical value indicating whether to use parallel processing for training the trees.
#' @param verbose A logical value indicating whether to print progress messages.
#' @importFrom future availableCores plan multisession
#' @importFrom future.apply future_lapply
#' @importFrom purrr transpose
#' @details
#' The recforest model aggregates predictions over an ensemble of trees, each constructed using a set of decision nodes based on specific splitting rules.
#' At each node, a subset of predictors is randomly selected, and an optimal split is determined using an appropriate statistical test.
#' Depending on the specified `method`, the algorithm employs different statistical tests to find the best split:
#' - For standard recurrent event data, the pseudo-score test statistic is used to compare two Nelson-Aalen estimates of the mean cumulative function.
#' - In the presence of terminal events and/or longitudinal variables, the Ghosh-Lin model is utilized to obtain the Wald test statistic, which provides a more accurate assessment of the split.
#' The trees grow until they meet the stopping criteria, which include a minimum number of events (`minsplit`) and a minimum number of individuals in terminal nodes (`nodesize`).
#' The final model is an ensemble of these trees, which helps to reduce overfitting and improve predictive performance by averaging the results on the out-of-bag sample.
#' @references
#' Cook, R. J., & Lawless, J. F. (1997). Marginal analysis of recurrent events and a terminating event. Statistics in medicine, 16(8), 911-924.
#'
#' Ghosh, D., & Lin, D. Y. (2002). Marginal regression models for recurrent and terminal events. Statistica Sinica, 663-688.
#'
#' Ishwaran, H., Kogalur, U. B., Blackstone, E. H., & Lauer, M. S. (2008). Random survival forests.
#' @return A list containing the following elements:
#' \item{trees}{A list of trained trees.}
#' \item{tree_metrics}{A list of metrics for each tree.}
#' \item{metrics}{A summary of the metrics for all trees.}
#' \item{columns}{A list of column names used in the training.}
#' \item{params}{A list of parameters used to set the model.}
#' \item{n_indiv}{Number of individuals in the dataset.}
#' \item{n_predictors}{Number of predictors used in the model.}
#' \item{n_trees}{Number of trees trained.}
#' \item{n_bootstrap}{Number of bootstrap samples used to grow each tree.}
#' \item{time}{Computation time used to train the model.}
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
#' print(trained_forest)
#' summary(trained_forest)
#' @export
train_forest <- function(
  data,
  id_var,
  covariates,
  event,
  time_vars = c("t.start", "t.stop"),
  death_var = NULL,
  n_trees,
  n_bootstrap = NULL,
  seed = NULL,
  mtry,
  minsplit,
  nodesize,
  method,
  min_score,
  max_nodes,
  parallel = FALSE,
  verbose = TRUE
) {
  if (missing(data)) {
    stop("data is missing")
  }

  if (missing(covariates)) {
    stop("covariates is missing")
  }

  if (missing(event)) {
    stop("event is missing")
  }

  if (missing(time_vars)) {
    stop("time_vars is missing")
  }

  if (length(time_vars) != 2) {
    stop("time_vars should have exactly 2 elements : start and stop time")
  }

  if (missing(n_trees)) {
    stop("n_trees is missing")
  }
  if (missing(mtry)) {
    stop("mtry is missing")
  }
  if (missing(minsplit)) {
    stop("minsplit is missing")
  }
  if (missing(nodesize)) {
    stop("nodesize is missing")
  }
  if (missing(min_score)) {
    stop("min_score is missing")
  }
  if (missing(max_nodes)) {
    stop("max_nodes is missing")
  }

  match.arg(method, choices = c("NAa", "GL"))

  if (missing(id_var)) {
    stop("id_var is missing")
  }
  prepared_data <- prepare_dataset_for_recforest(
    data = data,
    covariates = covariates,
    event = event,
    time_variables = time_vars,
    id_var = id_var,
    death_variable = death_var
  )

  X <- prepared_data$X
  Y <- prepared_data$Y


  start_forest <- Sys.time()
  if (!is.null(seed)) {
    set.seed(seed)
  }

  params <- list(
    mtry = mtry,
    minsplit = minsplit,
    nodesize = nodesize,
    method = method,
    min_score = min_score,
    max_nodes = max_nodes
  )

  if (is.null(n_bootstrap)) {
    n_bootstrap <- round(2 * (length(unique(X[, id_var])) / 3))
    message(sprintf("since n_bootstrap is not provided, it is set to 2/3 of the sample size: %s", n_bootstrap))
  }

  if (identical(method, "GL")) {
    X <- preprocess_data_GL(
      X = X,
      Y = Y,
      death_col = death_var
    )
  }

  X <- rename_required_columns(
    X = X,
    id_col = id_var,
    t_start_col = time_vars[1],
    t_stop_col = time_vars[2],
    death_col = death_var
  )
  # We split now, as there is no randomness
  # so no need to compute this several times
  X_per_id <- split(
    x = X,
    f = X[["_original_id"]]
  )

  Y_per_id <- split(
    x = Y,
    f = X[["_original_id"]]
  )

  args <- list(
    X = seq_len(n_trees),
    FUN = function(i) {
      if (verbose) {
        message("training tree ", i, "/", n_trees)
      }

      train_tree_and_calculate_metrics(
        i = i,
        X = X,
        Y = Y,
        X_per_id = X_per_id,
        Y_per_id = Y_per_id,
        params = params,
        n_bootstrap = n_bootstrap,
        verbose = verbose
      )
    }
  )

  if (parallel) {
    if (verbose) {
      message("Using parallel processing for training")
    }
    args[["future.seed"]] <- TRUE

    loop_fun <- future.apply::future_lapply
  } else {
    if (verbose) {
      message("Using sequential processing for training")
    }
    loop_fun <- lapply
  }
  res <- do.call(
    what = loop_fun,
    args = args
  )
  res <- purrr::transpose(res)
  metrics <- summarize_tree_metrics(res$metrics)

  end_forest <- Sys.time()

  result <- list(
    trees = res$tree,
    tree_metrics = res$metrics,
    metrics = metrics,
    columns = list(
      id_var = id_var,
      covariates = covariates,
      event = event,
      time_vars = time_vars,
      death_var = death_var
    ),
    params = params,
    n_indiv = get_n_indiv(X),
    n_predictors = get_n_predictors(
      X,
      not_predictors = c(
        id_var,
        time_vars[1],
        time_vars[2],
        death_var,
        "_t.start",
        "_t.stop",
        "_death",
        "_original_id"
      )
    ),
    n_trees = n_trees,
    n_bootstrap = n_bootstrap,
    time = as.numeric(end_forest - start_forest)
  )
  class(result) <- "recforest"
  result
}

#' @noRd
predict_forest <- function(forest, X) {
  X <- rename_required_columns(
    X = X,
    id_col = forest$columns$id_var,
    t_start_col = forest$columns$time_vars[1],
    t_stop_col = forest$columns$time_vars[2],
    death_col = forest$columns$death_var
  )

  results <- vapply(
    X = forest$trees,
    FUN = function(tree) {
      predict_tree(
        tree = tree,
        X = X
      )
    },
    FUN.VALUE = numeric(nrow(X))
  )

  rowMeans(results, na.rm = TRUE)
}
