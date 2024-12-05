#' @noRd
#' @importFrom dplyr group_by group_split mutate
#' @importFrom utils combn
#' @importFrom purrr map_dfr
#' @importFrom tibble tibble
c_index_2 <- function(Y_obs) {
  tmp <- Y_obs %>%
    dplyr::group_by(id) %>%
    dplyr::group_split() %>%
    purrr::map_dfr(function(id_df) {
      Nevent <- sum(id_df$event)
      t.length <- id_df$t.stop[Nevent + 1]
      rate_obs <- Nevent / t.length

      mcf.pred <- data.frame(
        time = id_df$t.stop,
        mcf = id_df$event_count_pred
      )

      condition <- which(t.length - mcf.pred$time > 0)

      rate.pred <- if (length(condition) > 0) {
        case <- max(condition)
        mcf.pred$mcf[case] / mcf.pred$time[case]
      } else {
        0 # to avoid above warning, we can replace case by its defined value
      }

      list(
        rate.pred = rate.pred,
        rate_obs = rate_obs
      )
    })

  pairs <- t(combn(
    x = seq_along(unique(Y_obs$id)),
    m = 2
  ))

  C.tmp <- tibble::tibble(
    # reference to the old code:
    a1 = tmp[[2]][pairs[, 2]], # temp[2,2]
    a2b1 = tmp[[1]][pairs[, 2]], # temp[2,1] NOTE: in the original code temp[1,2] is missing!
    b2 = tmp[[1]][pairs[, 1]] # temp[1,1]
  ) %>%
    dplyr::mutate(
      judge = (a1 - a2b1) * (a2b1 - b2),
      Concordance = as.numeric(judge >= 0)
    )

  mean(C.tmp$Concordance, na.rm = TRUE)
}

#' @noRd
#' @importFrom survival coxph survfit Surv
#' @importFrom stats stepfun qnorm quantile
np_estimate <- function(
  tseq = NULL,
  length.tseq = NULL,
  stop,
  status,
  terminal = NULL,
  id = NULL,
  CI = TRUE,
  CIlevel = 0.95,
  logtransform = TRUE
) {
  if (is.null(tseq) & is.null(length.tseq)) {
    NPfit <- survival::coxph(survival::Surv(stop, status) ~ 1)
    expecNP <- survival::survfit(NPfit, type = "aalen")
    N <- length(expecNP$time)
    tseq <- expecNP$time[-N]
    m <- (N - 1)
    # stop("Either tseq or nb.tseq must supplied.")
  } else {
    if (is.null(tseq)) {
      NPfit <- survival::coxph(survival::Surv(stop, status) ~ 1)
      expecNP <- survival::survfit(NPfit, type = "aalen")
      N <- length(expecNP$time)
      if (length.tseq >= (N - 1)) {
        tseq <- expecNP$time[-N]
        m <- (N - 1)
      } else {
        probs <- seq(0, 1, length.out = (length.tseq + 1)) #+1 when max time is removed!
        index <- c(quantile(1:N, probs[-c(1, (length.tseq + 1))], type = 1, names = FALSE)) # type=1 to force the quantile function to take values only in 1:N
        tseq <- c(min(expecNP$time), expecNP$time[index]) # ,max(expecNP$time)#we remove the max time because of numerical instabilities with CIs!!
        m <- length.tseq
      }
    } else {
      m <- length(tseq)
      # For the recurrent events
      NPfit <- survival::coxph(survival::Surv(stop, status) ~ 1)
      expecNP <- survival::survfit(NPfit, type = "aalen")
    }
  }
  CIleft <- CIright <- ResMean <- NULL
  if (is.null(terminal)) {
    mufun <- stepfun(expecNP$time, c(0, cumsum(expecNP$n.event / expecNP$n.risk))) # \hat \mu
    Shat <- ResMeanD <- NULL
  } else {
    NPfitT <- survival::coxph(survival::Surv(stop, terminal) ~ 1)
    survT <- survival::survfit(NPfitT)
    mufun <- stepfun(expecNP$time, c(0, cumsum(survT$surv * expecNP$n.event / expecNP$n.risk))) # \hat \mu
    survfun <- stepfun(survT$time, c(1, survT$surv))
    Shat <- survfun(tseq)
    ResMeanD <- NULL
  }
  return(list(tseq = tseq, muhat = mufun(tseq), mufun = mufun, Shat = Shat, CIleft = CIleft, CIright = CIright, Residuals = ResMean, Residuals_Death = ResMeanD))
}

#' @noRd
#' @import dplyr
#' @import survival
#' @import purrr
#' @importFrom stats integrate
mse_2 <- function(Y_obs) { # /!\ for one tree, to be averaged over n number of trees
  # Estimating the censoring distribution using Kaplan-Meier estimator###
  TimeC_df <- Y_obs %>%
    dplyr::group_by(id) %>%
    dplyr::summarize(TimeC = dplyr::last(t.stop))

  survC <- survival::survfit(
    survival::Surv(
      time = TimeC_df$TimeC,
      time2 = rep(1, nrow(TimeC_df))
    ) ~ 1
  )

  Cens_cdf <- stepfun(
    x = survC$time,
    y = c(1, survC$surv)
  )

  # Time for evaluation of predictions
  tseq <- sort(c(0, unique(Y_obs$t.stop[Y_obs$event == 1])))

  # predicted
  ## non parametric model without terminal event ##
  np_model <- np_estimate(
    tseq = tseq,
    stop = Y_obs$t.stop,
    status = Y_obs$event,
    terminal = NULL,
    CI = FALSE
  )

  mu0fun <- np_model$mufun

  tmp <- Y_obs %>%
    dplyr::filter(
      event == 1,
      !is.na(event_count_pred)
    ) %>%
    dplyr::select(t.stop, event_count_pred) %>%
    dplyr::arrange(t.stop, event_count_pred) %>%
    unique()

  mu1fun <- function(tseq) {
    purrr::map_dbl(
      .x = tseq,
      .f = function(x) {
        if (x == 0) {
          0
        } else {
          idx <- max(which(tmp$t.stop <= x))
          tmp$event_count_pred[idx]
        }
      }
    )
  }

  scores <- Y_obs %>%
    dplyr::group_by(id) %>%
    dplyr::group_split() %>%
    purrr::map(function(indiv) {
      condition <- indiv$event == 1
      obs_fun <- if (sum(condition) > 0) {
        x_vals <- indiv$t.stop[condition]

        stepfun(
          x = x_vals,
          y = c(0, cumsum(1 / Cens_cdf(x_vals)))
        )
      } else {
        function(x) {
          rep(0, length(x))
        }
      }

      obs_vals <- obs_fun(tseq)

      list(
        score0 = (obs_vals - mu0fun(tseq))^2,
        score1 = (obs_vals - mu1fun(tseq))^2
      )
    }) %>%
    purrr::transpose()

  score0 <- Reduce(rbind, scores$score0)
  rownames(score0) <- NULL
  score1 <- Reduce(rbind, scores$score1)
  rownames(score1) <- NULL

  mse_np <- colMeans(score0) # mse for non-parametric
  mse_rsf <- colMeans(score1) # mse of the ongoing tree

  score <- mse_np - mse_rsf # the score is the difference across mse
  integrand_mse <- stepfun(
    # preparing for imse
    x = tseq,
    y = c(0, mse_rsf)
  )
  integrand_score <- stepfun(
    # preparing for iscore
    x = tseq,
    y = c(0, score)
  )

  integrate_fun <- function(f, subdivisions = 10000) {
    integrate(
      f = f,
      lower = min(tseq),
      upper = max(tseq),
      subdivisions = subdivisions * 10
    )
  }

  res <- list(
    imse = integrate_fun(integrand_mse), # the integrated mse summarizes the mse over time
    iscore = integrate_fun(integrand_score) # the integrated score summarizes the score over time
  )

  list(
    # tseq = tseq,
    # mse_np = mse_np,
    mse_rsf = mse_rsf,
    score = score,
    imse = res$imse$value,
    iscore = res$iscore$value
  )
}
