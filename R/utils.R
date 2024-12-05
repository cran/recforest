#' @noRd
rename_required_columns <- function(X, id_col, t_start_col, t_stop_col, death_col) {
  colnames(X)[colnames(X) == id_col] <- "_original_id"
  colnames(X)[colnames(X) == t_start_col] <- "_t.start"
  colnames(X)[colnames(X) == t_stop_col] <- "_t.stop"
  colnames(X)[colnames(X) == death_col] <- "_death"
  X
}


#' @noRd
#' @importFrom dplyr mutate case_when
preprocess_data_GL <- function(X, Y, death_col) {
  X[["_cens"]] <- ifelse(Y == 0 & X[[death_col]] == 0, 1, 0)

  # Création de la colonne `_statusG` avec les conditions de `case_when`
  X[["_statusG"]] <- ifelse(
    Y == 1,
    1,
    ifelse(X[[death_col]] == 1, 2, 0)
  )
  X
}

#' @noRd
#' @importFrom dplyr bind_rows mutate
#' @importFrom stats setNames
create_boostrap_sample <- function(
  X,
  Y,
  X_per_id,
  Y_per_id,
  n = 1000
) {
  bootstrap_ids <- sample(
    x = names(X_per_id),
    size = n,
    replace = TRUE
  )

  list(
    bootstrap_ids = bootstrap_ids,
    X = mutate(
      bind_rows(
        setNames(
          X_per_id[bootstrap_ids],
          seq_along(bootstrap_ids)
        ),
        .id = "_id"
      ),
      `_id` = as.integer(`_id`)
    ),
    Y = unlist(
      x = Y_per_id[bootstrap_ids],
      use.names = FALSE
    )
  )
}

reserved_variables <- function() {
  c("_original_id", "_id", "_t.start", "_t.stop", "_death", "_statusG", "_cens")
}
