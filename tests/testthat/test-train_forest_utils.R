test_that("get_n_indiv works", {
  X <- tibble::tibble(`_original_id` = c(1, 1, 2, 3, 3, 4, 4, 4, 5))
  expect_equal(get_n_indiv(X), 5)
})

test_that("get_n_indiv works on readmission", {
  dat <- generate_data_for_test_forest()
  expect_equal(get_n_indiv(dat$X, id_col = "id"), 403)
})


test_that("get_n_predictors works", {
  X <- tibble::tibble(
    `_original_id` = 1:5,
    id = 1:5,
    t.start = 1:5,
    t.stop = 1:5,
    death = 1:5,
    dummy1 = 1:5,
    dummy2 = 1:5
  )
  expect_equal(get_n_predictors(X), 2)
})

test_that("get_n_predictors works on readmission", {
  dat <- generate_data_for_test_forest()
  expect_equal(get_n_predictors(dat$X), 3)

  X <- rename_required_columns(
    X = dat$X,
    id_col = "id",
    t_start_col = "t.start",
    t_stop_col = "t.stop",
    death_col = "death"
  )
  expect_equal(get_n_predictors(X), 3)
})



test_that("get_n_nodes works", {
  withr::with_seed(1234567, {
    df_to_use <- generate_data_for_test_forest()
    data_to_use <- cbind(
      df_to_use$X,
      df_to_use$Y
    )
    colnames(data_to_use)[length(colnames(data_to_use))] <- "event"

    trained_forest <- suppressWarnings(train_forest(
      data = data_to_use,
      covariates = c("chemo", "sex", "dummy"),
      event = "event",
      time_vars = c("t.start", "t.stop"),
      id_var = "id",
      death_var = "death",
      seed = 1234567,
      mtry = 1,
      minsplit = 1,
      nodesize = 1,
      method = "NAa",
      min_score = 1,
      max_nodes = 10,
      n_trees = 2,
      parallel = FALSE,
      verbose = FALSE
    ))
    expect_equal(get_n_nodes(trained_forest), list(13, 15))
  })
})

test_that("prepare_dataset_for_recforest works", {
  expected_data <- generate_data_for_test_forest()

  data_to_use_for_test <- cbind(
    expected_data$X,
    expected_data$Y
  )
  colnames(data_to_use_for_test)[length(colnames(data_to_use_for_test))] <- "event"


  res <- prepare_dataset_for_recforest(
    data = data_to_use_for_test,
    covariates = c("chemo", "sex", "dummy"),
    event = "event",
    time_variables = c("t.start", "t.stop"),
    id_var = "id",
    death_variable = "death"
  )

  expect_equal(res$X, expected_data$X)
  expect_equal(res$Y, expected_data$Y)
})

test_that("prepare_dataset_for_recforest works if not death_variable is provided", {
  expected_data <- generate_data_for_test_forest()
  expected_data$X$death <- 0


  data_to_use_for_test <- cbind(
    expected_data$X,
    expected_data$Y
  )
  colnames(data_to_use_for_test)[length(colnames(data_to_use_for_test))] <- "event"


  res <- prepare_dataset_for_recforest(
    data = data_to_use_for_test,
    covariates = c("chemo", "sex", "dummy"),
    event = "event",
    time_variables = c("t.start", "t.stop"),
    id_var = "id"
  )

  expect_equal(res$X, expected_data$X)
  expect_equal(res$Y, expected_data$Y)
})


test_that("prepare_dataset_for_predict works", {
  expected_data <- generate_data_for_test_forest()

  data_to_use_for_test <- cbind(
    expected_data$X,
    expected_data$Y
  )
  colnames(data_to_use_for_test)[length(colnames(data_to_use_for_test))] <- "event"


  res <- prepare_dataset_for_predict(
    data = data_to_use_for_test,
    covariates = c("chemo", "sex", "dummy"),
    time_variables = c("t.start", "t.stop"),
    id_var = "id",
    death_variable = "death"
  )

  expect_equal(res, expected_data$X)
})


test_that("prepare_dataset_for_predict works if not death_variable is provided", {
  expected_data <- generate_data_for_test_forest()
  expected_data$X$death <- 0


  data_to_use_for_test <- cbind(
    expected_data$X,
    expected_data$Y
  )
  colnames(data_to_use_for_test)[length(colnames(data_to_use_for_test))] <- "event"


  res <- prepare_dataset_for_predict(
    data = data_to_use_for_test,
    covariates = c("chemo", "sex", "dummy"),
    time_variables = c("t.start", "t.stop"),
    id_var = "id",
    death_variable = "death"
  )

  expect_equal(res, expected_data$X)
})
