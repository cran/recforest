#' @noRd
create_decision_node <- function(X, Y, node_id, params, verbose = TRUE) {
  valid_variables <- X[, setdiff(names(X), reserved_variables())]
  valid_variables <- valid_variables[, sapply(valid_variables, is_supported_variable), drop = FALSE]


  fallback_node <- function() {
    list(create_terminal_node(
      X = X,
      Y = Y,
      node_id = node_id,
      method = params[["method"]],
      verbose = verbose
    ))
  }

  if (ncol(valid_variables) == 0) {
    if (verbose) {
      warning("no valid variables to continue")
    }

    return(fallback_node())
  }

  variables <- sample(
    x = colnames(valid_variables),
    size = min(ncol(valid_variables), params[["mtry"]])
  )

  res <- calculate_variable_scores(
    variables = variables,
    X = X,
    Y = Y,
    method = params[["method"]]
  )

  res <- purrr::compact(res) # remove potential NULLs

  if (length(res) == 0) {
    # only numeric variables are available and probably variance is 0 (all values are the same)
    # not possible to validate this earlier, because numeric score is dependent on the selected method
    if (verbose) {
      warning("no possible split available")
    }

    return(fallback_node())
  }

  score_values <- sapply(res, function(x) x$score)
  best_idx <- which.max(score_values)
  best_score <- res[[best_idx]]

  if (best_score$score < params[["min_score"]]) {
    if (verbose) {
      warning("score lower than minimal")
    }

    return(fallback_node())
  }

  best_decision <- make_decision(
    x = X[[best_score$variable]],
    value = best_score$value
  )

  yes_node_id <- node_id + 1

  nodes_yes <- create_new_node(
    X = X[best_decision, ],
    Y = Y[best_decision],
    node_id = yes_node_id,
    params = params,
    verbose = verbose
  )

  no_node_id <- node_id + length(nodes_yes) + 1

  nodes_no <- create_new_node(
    X = X[!best_decision, ],
    Y = Y[!best_decision],
    node_id = no_node_id,
    params = params,
    verbose = verbose
  )

  if (verbose) {
    print(paste0("creating decision node #", node_id))
  }

  node <- list(
    id = node_id,
    type = "decision",
    variable = best_score$variable,
    variable_type = best_score$type,
    yes_value = best_score$value,
    yes_node_id = yes_node_id,
    no_node_id = no_node_id
  )

  c(
    list(node),
    nodes_yes,
    nodes_no
  )
}


#' @noRd
#' @importFrom methods is
predict_node_decision <- function(node, X, tree) {
  values <- X[[node$variable]]
  if (!is(values, node$variable_type)) {
    stop(
      "incorrect variable type ",
      node$variable,
      " in node #",
      node$id,
      " - expected ",
      node$variable_type,
      ", got ",
      class(values)[1]
    )
  }

  yes_idx <- make_decision(
    x = X[[node$variable]],
    value = node$yes_value
  )

  n_yes <- sum(yes_idx)
  result <- rep(NA, nrow(X))

  if (n_yes > 0) {
    result[yes_idx] <- predict_node(
      node = tree$nodes[[node$yes_node_id]],
      X = X[yes_idx, ],
      tree = tree
    )
  }

  if (n_yes < nrow(X)) {
    result[!yes_idx] <- predict_node(
      node = tree$nodes[[node$no_node_id]],
      X = X[!yes_idx, ],
      tree = tree
    )
  }

  result
}
