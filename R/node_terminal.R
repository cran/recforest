#' @noRd
is_terminal_node <- function(X, Y, minsplit = 2, nodesize = 1) {
  n_event_current <- sum(Y) # events
  n_current <- length(X[["_id"]])
  return(!(n_event_current >= minsplit & n_current >= nodesize * 2)) # returns TRUE if terminal
}

#' @noRd
#' @import reda
create_terminal_node_NAa <- function(X, Y, node_id, estim_df, verbose = TRUE) {
  estim <- reda::mcf(
    object = reda::Recur(
      time = `_t.stop`,
      id = `_id`,
      event = `_event`
    ) ~ 1,
    data = estim_df
  )

  if (verbose) {
    print(paste0("creating terminal node #", node_id))
  }

  estim <- list(
    xest = estim@MCF$time,
    yest = estim@MCF$MCF
  )

  list(
    id = node_id,
    type = "terminal",
    estim = estim
  )
}

#' @noRd
#' @importFrom mets phreg recurrentMarginal
#' @importFrom survival Surv cluster
#' @importFrom dplyr group_by summarise arrange desc first pull filter n
#' @importFrom stats stepfun
create_terminal_node_GL <- function(X, Y, node_id, estim_df, verbose = TRUE) {
  # Based on Ghosh-Lin and used in following cases:
  # * With or without terminal event
  # * With or without longitudinal covariates

  final_res <- function(x, y) {
    if (verbose) {
      print(paste0("creating terminal node #", node_id))
    }

    list(
      id = node_id,
      type = "terminal",
      estim = list(
        xest = x,
        yest = y
      )
    )
  }

  tryCatch(
    expr = {
      xr <- mets::phreg(
        formula = survival::Surv(
          time = `_t.start`,
          time2 = `_t.stop`,
          event = `_event`
        ) ~ survival::cluster(`_id`),
        data = estim_df
      )
      dr <- mets::phreg(
        formula = survival::Surv(
          time = `_t.start`,
          time2 = `_t.stop`,
          event = `_death`
        ) ~ survival::cluster(`_id`),
        data = estim_df
      )

      out <- mets::recurrentMarginal(
        recurrent = xr,
        death = dr
      )

      GL_fun <- function() {
        stepfun(
          x = out$cumhaz[, 1],
          y = c(0, out$cumhaz[, 2])
        )
      }

      tseq <- seq(
        from = 0,
        to = max(estim_df[["_t.stop"]]),
        length.out = 500
      )

      muGhosh <- GL_fun()
      yest <- muGhosh(tseq)

      final_res(
        x = tseq,
        y = yest
      )
    },
    error = function(e) {
      warning(e)

      # select only 1 patient if something went wrong (the one with the most records)
      final_patient <- estim_df %>%
        group_by(`_id`) %>%
        summarise(count = n()) %>%
        arrange(desc(count)) %>%
        first() %>%
        pull(`_id`)

      final_df <- estim_df %>%
        filter(`_id` == final_patient)

      final_res(
        x = final_df[["_t.stop"]],
        y = cumsum(final_df[["_event"]])
      )
    }
  )
}

#' @noRd
#' @import dplyr
create_terminal_node <- function(X, Y, node_id, method = c("NAa", "GL"), verbose = TRUE) {
  method <- match.arg(method)

  estim_df <- X
  estim_df <- estim_df[, which(names(estim_df) %in% reserved_variables())]
  estim_df$`_event` <- Y


  node_fun <- switch(
    EXPR = method,
    "NAa" = create_terminal_node_NAa, # non-parametric Nelson-Aalen
    "GL" = create_terminal_node_GL # Ghosh-Lin estimation
  )

  node_fun(
    X = X,
    Y = Y,
    node_id = node_id,
    estim_df = estim_df,
    verbose = verbose
  )
}

#' @noRd
#' @importFrom dplyr mutate pull
#' @importFrom stats approx
predict_node_terminal <- function(node, X, tree) {
  estim <- node$estim

  if (length(estim$yest) > 1) {
    tryCatch(
      X %>%
        mutate(event_count = stats::approx(
          x = estim$xest,
          y = estim$yest,
          xout = `_t.stop`
        )$y) %>%
        pull(event_count),
      error = function(e) e
    )
  } else {
    rep(estim$yest, times = nrow(X))
  }
}
