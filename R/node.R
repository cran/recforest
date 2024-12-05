#' @noRd
create_new_node <- function(X, Y, node_id, params, verbose = TRUE) {
  max_n_nodes_reached <- node_id >= params[["max_nodes"]]

  is_terminal <- is_terminal_node(
    X = X,
    Y = Y,
    minsplit = params[["minsplit"]],
    nodesize = params[["nodesize"]]
  )

  if (is_terminal || max_n_nodes_reached) {
    if (max_n_nodes_reached && verbose) {
      warning("too many nodes...")
    }
    list(create_terminal_node(
      X = X,
      Y = Y,
      node_id = node_id,
      method = params[["method"]],
      verbose = verbose
    ))
  } else {
    create_decision_node(
      X = droplevels(X),
      Y = Y,
      node_id = node_id,
      params = params,
      verbose = verbose
    )
  }
}

#' @noRd
predict_node <- function(node, X, tree) {
  predict_fun <- if (node$type == "decision") {
    predict_node_decision
  } else if (node$type == "terminal") {
    predict_node_terminal
  } else {
    stop("incorrect node type")
  }

  predict_fun(
    node = node,
    X = X,
    tree = tree
  )
}
