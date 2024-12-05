data(readmission, package = "frailtypack")

generate_data_for_test_forest <- function(df_data = readmission) {
  df <- z <- df_data
  z$cens <- rep(0, dim(z)[1])
  z$cens[df$event == 0 & z$death == 0] <- rep(1, sum(z$event == 0 & z$death == 0))
  z$statusG <- rep(0, dim(z)[1])
  z$statusG[z$death == 1] <- rep(2, sum(z$death == 1))
  z$statusG[z$event == 1] <- rep(1, sum(z$event == 1))

  X <- df %>%
    dplyr::select(id, t.start, t.stop, death, chemo, sex) %>% # charlson excluded because it's not consistent through the time
    dplyr::group_by(id) %>%
    dplyr::mutate(dummy = rnorm(1)) %>%
    as.data.frame()

  Y <- df %>%
    dplyr::pull(event)

  list(
    "X" = X,
    "Y" = Y
  )
}
