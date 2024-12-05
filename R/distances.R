#' @noRd
#' @importFrom reda mcfDiff.test mcf
#' @importFrom dplyr bind_cols n_distinct select
#' @importFrom tibble tibble
calculate_distance_NAa <- function(X, Y, decision) {
  # Nelson-Aalen MCF - Pseudo score test
  helper <- function(value) {
    lgl <- decision == value
    Xi <- X[lgl, c("_id", "_t.stop")]
    Yi <- tibble(`_event` = Y[lgl])

    XYi <- bind_cols(Xi, Yi)

    n_uq_rows <- nrow(unique(XYi[, !names(XYi) %in% "_id"]))


    if (n_uq_rows == 1) {
      # there's a bug in reda package:
      # when all rows are the same (except of the ID) reda::mcfDiff.test function
      # throws an error related to the data format (tries to fit 2*N_rows into 2 row matrix column)
      # such a situation happens when the same 1-row individual is present mutliple times
      # in a bootstrap sample
      XYi <- XYi[1, ]
    }

    mcf(
      object = Recur(
        time = `_t.stop`,
        id = `_id`,
        event = `_event`
      ) ~ 1,
      data = XYi
    )
  }

  abs(
    mcfDiff.test(
      helper(TRUE),
      helper(FALSE)
    )[
      2,
      1
    ]
  )
}

#' @noRd
#' @importFrom mets recreg
#' @importFrom timereg Event
calculate_distance_GL <- function(X, Y, decision) {
  # Based on Ghosh-Lin model and used in following cases:
  # * With or without terminal event
  # * With or without longitudinal covariates
  helper_GL <- function(value) {
    # single observations make troubles.
    mets::recreg(
      formula = timereg::Event(
        time = `_t.start`,
        time2 = `_t.stop`,
        cause = `_statusG`,
        cens.code = `_cens`
      ) ~ as.numeric(decision == value),
      cause = 1,
      death.code = 2,
      data = X
    )
  }


  tryCatch(
    expr = {
      value <- abs(summary(helper_GL(TRUE))$coef[, "dU^-1/2"]) # as per Nelson-Aalen approach, we want to maximise the test statistic
      if (is.nan(value)) {
        stop("nan detected")
      }
      value
    },
    error = function(e) {
      # NOTE: need to fix this error:
      # Error in apply(Gts, 2, function(x) exp(cumsum(log(1 - x)))) :
      #   dim(X) must have a positive length
      Inf
    }
  )
}



#' @noRd
#' @importFrom purrr compact
calculate_factor_scores <- function(
  varname,
  X,
  values,
  Y,
  method
) {
  value_lvls <- levels(values)

  if (length(value_lvls) == 2) {
    # calculated distance will be the same in 1v2 and 2vs1, so let's keep only the 1st one
    value_lvls <- value_lvls[1]
  }

  # This is here do that we don't repeat the switch statement in the lapply
  distance_fun <- switch(
    EXPR = method,
    "NAa" = calculate_distance_NAa, # non-parametric Nelson-Aalen
    "GL" = calculate_distance_GL # Ghosh-Lin model
  )

  res <- lapply(
    X = value_lvls,
    FUN = function(lvl_value) {
      score <- distance_fun(
        X = X,
        Y = Y,
        decision = make_decision(
          x = values,
          value = lvl_value
        )
      )

      if (is.infinite(score)) {
        return()
      }

      list(
        variable = varname,
        type = "factor",
        value = lvl_value,
        score = score
      )
    }
  )

  compact(res)
}


#' @noRd
#' @importFrom purrr compact
#' @importFrom dplyr distinct pull
calculate_numeric_scores <- function(
  varname,
  values_from_x,
  X,
  Y,
  method
) {
  if (method == "NAa") {
    values <- getElement(
      distinct(
        X[, c("_id", varname)],
        `_id`,
        .keep_all = TRUE
      ),
      varname
    )
  } else {
    values <- values_from_x
  }

  thresholds <- quantile(
    x = values,
    probs = c(0.25, 0.5, 0.75)
  )
  # This is here do that we don't repeat the switch statement in the lapply
  distance_fun <- switch(
    EXPR = method,
    "NAa" = calculate_distance_NAa, # non-parametric Nelson-Aalen
    "GL" = calculate_distance_GL # Ghosh-Lin model
  )

  compact(lapply(
    X = unname(thresholds),
    FUN = function(threshold_value) {
      decision <- make_decision(
        x = values_from_x,
        value = threshold_value
      )

      if (length(unique(decision)) == 1) {
        return() # decision has to split on 2 sets, it's not possible here
      }

      score <- distance_fun(
        X = X,
        Y = Y,
        decision = decision
      )

      if (is.infinite(score)) {
        return()
      }

      list(
        variable = varname,
        type = "numeric",
        value = threshold_value,
        score = score
      )
    }
  ))
}

#' @noRd
calculate_variable_scores <- function(variables, X, Y, method) {
  unlist(
    x = lapply(
      X = variables,
      FUN = function(v) {
        values <- X[[v]]

        if (is.factor(values)) {
          calculate_factor_scores(
            varname = v,
            values = values,
            X = X,
            Y = Y,
            method = method
          )
        } else if (is.numeric(values)) {
          calculate_numeric_scores(
            varname = v,
            values_from_x = values,
            X = X,
            Y = Y,
            method = method
          )
        } else {
          stop("only factors and numerics for now")
        }
      }
    ),
    recursive = FALSE
  )
}
