#' @noRd
train_tree <- function(
  X,
  Y,
  X_per_id,
  Y_per_id,
  params = list(),
  n_bootstrap = 1000,
  tree_id = 1,
  verbose = TRUE
) {
  bs_sample <- create_boostrap_sample(
    X = X,
    Y = Y,
    X_per_id = X_per_id,
    Y_per_id = Y_per_id,
    n = n_bootstrap
  )
  list(
    id = tree_id,
    bootstrap_ids = bs_sample$bootstrap_ids,
    oob_ids = setdiff(X[["_original_id"]], bs_sample$boostrap_ids),
    nodes = create_new_node(
      X = bs_sample$X,
      Y = bs_sample$Y,
      node_id = 1,
      params = params,
      verbose = verbose
    )
  )
}

#' @noRd
predict_tree <- function(tree, X) {
  predict_node(
    node = tree$nodes[[1]],
    X = X,
    tree = tree
  )
}
