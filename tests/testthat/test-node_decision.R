test_that("predict_node_decision works", {
  X_to_test <- structure(
    list(
      `_original_id` = c(1L, 1L),
      `_t.start` = c(
        0L,
        24L
      ),
      `_t.stop` = c(24L, 457L),
      `_death` = c(0L, 0L),
      chemo = structure(
        c(
          2L,
          2L
        ),
        levels = c("NonTreated", "Treated"),
        class = "factor"
      ),
      sex = structure(c(2L, 2L), levels = c("Male", "Female"), class = "factor"),
      dummy = c(0.807579776908013, 0.807579776908013)
    ),
    row.names = 1:2,
    class = "data.frame"
  )
  tree <- list()
  node_to_test <- list(variable_type = "character", variable = "dummy")
  expect_error(
    predict_node_decision(node = node_to_test, X = X_to_test, tree = tree),
    "incorrect variable type dummy in node # - expected character, got numeric"
  )
})
