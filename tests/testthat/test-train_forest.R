test_that("detecting missing default values works for train_forest", {
  expect_error(
    train_forest(),
    "data is missing"
  )

  expect_error(
    train_forest(
      data = "iris"
    ),
    "covariates is missing"
  )


  expect_error(
    train_forest(
      data = "iris",
      covariates = "aaa"
    ),
    "event is missing"
  )

  expect_error(
    train_forest(
      data = "iris",
      covariates = "aaa",
      event = "bbb"
    ),
    "time_vars is missing"
  )

  expect_error(
    train_forest(
      data = "iris",
      covariates = "aaa",
      event = "bbb",
      time_vars = "ccc"
    ),
    "time_vars should have exactly 2 elements : start and stop time"
  )
  expect_error(
    train_forest(
      data = "iris",
      covariates = "aaa",
      event = "bbb",
      time_vars = c("start", "stop")
    ),
    "n_trees is missing"
  )

  expect_error(
    train_forest(
      data = "iris",
      covariates = "aaa",
      event = "bbb",
      time_vars = c("start", "stop"),
      n_trees = 2
    ),
    "mtry is missing"
  )

  expect_error(
    train_forest(
      data = "iris",
      covariates = "aaa",
      event = "bbb",
      time_vars = c("start", "stop"),
      n_trees = 2,
      mtry = 2
    ),
    "minsplit is missing"
  )

  expect_error(
    train_forest(
      data = "iris",
      covariates = "aaa",
      event = "bbb",
      time_vars = c("start", "stop"),
      n_trees = 2,
      mtry = 2,
      minsplit = 2
    ),
    "nodesize is missing"
  )

  expect_error(
    train_forest(
      data = "iris",
      covariates = "aaa",
      event = "bbb",
      time_vars = c("start", "stop"),
      n_trees = 2,
      mtry = 2,
      minsplit = 2,
      nodesize = 1
    ),
    "min_score is missing"
  )

  expect_error(
    train_forest(
      data = "iris",
      covariates = "aaa",
      event = "bbb",
      time_vars = c("start", "stop"),
      n_trees = 2,
      mtry = 2,
      minsplit = 2,
      nodesize = 1,
      min_score = 5
    ),
    "max_nodes is missing"
  )

  expect_error(
    train_forest(
      data = "iris",
      covariates = "aaa",
      event = "bbb",
      time_vars = c("start", "stop"),
      n_trees = 2,
      mtry = 2,
      minsplit = 2,
      nodesize = 1,
      min_score = 5,
      max_nodes = 15,
      method = "SOMEMETHOD"
    ),
    "'arg' should be one of \"NAa\", \"GL\""
  )

  expect_error(
    train_forest(
      data = "iris",
      covariates = "aaa",
      event = "bbb",
      time_vars = c("start", "stop"),
      n_trees = 2,
      mtry = 2,
      minsplit = 2,
      nodesize = 1,
      min_score = 5,
      max_nodes = 15,
      method = "NAa"
    ),
    "id_var is missing"
  )
})



test_that("train_forest and predict_forest works with method NAa", {
  withr::with_seed(1234567, {
    df_to_use <- generate_data_for_test_forest()
    data_to_use <- cbind(
      df_to_use$X,
      df_to_use$Y
    )
    colnames(data_to_use)[length(colnames(data_to_use))] <- "event"



    expect_message(
      trained_forest <- suppressWarnings(train_forest(
        data = data_to_use,
        covariates = c("chemo", "sex", "dummy"),
        event = "event",
        time_vars = c("t.start", "t.stop"),
        id_var = "id",
        death_var = "death",
        seed = 1234567,
        mtry = 2,
        minsplit = 2,
        nodesize = 1,
        method = "NAa",
        min_score = 5,
        max_nodes = 15,
        n_trees = 2,
        parallel = FALSE,
        verbose = FALSE
      )),
      "since n_bootstrap is not provided, it is set to 2/3 of the sample size: 269"
    )

    expect_type(trained_forest, "list")
    expect_equal(
      names(trained_forest),
      c("trees", "tree_metrics", "metrics", "columns", "params", "n_indiv", "n_predictors", "n_trees", "n_bootstrap", "time")
    )
    expect_equal(length(trained_forest$trees), 2)
    expect_equal(
      trained_forest$metrics,
      structure(
        list(
          c_index = 0.75673178191024,
          mse_imse = 39844.4904825378,
          mse_iscore = -20950.886673471
        ),
        class = c(
          "tbl_df",
          "tbl",
          "data.frame"
        ),
        row.names = c(NA, -1L)
      )
    )
    expect_equal(
      trained_forest$tree_metrics,
      list(
        list(
          c_index = 0.759010025062657,
          mse_imse = 34703.3147322328,
          mse_iscore = -15809.8048226738
        ),
        list(
          c_index = 0.754453538757824,
          mse_imse = 44985.6662328428,
          mse_iscore = -26091.9685242683
        )
      )
    )

    predictions <- predict_forest(
      forest = trained_forest,
      X = df_to_use$X
    )

    expect_equal(length(predictions), nrow(df_to_use$X))
    expect_equal(
      predictions[1:10],
      c(
        0.0584102769971899,
        0.573867317836384,
        0.937820351326362,
        1.0477655026989,
        1.5013511445022,
        0.0858516483516484,
        0.897641742315655,
        0.316814972716824,
        0.481545477682397,
        0.786352679697396
      )
    )
  })
})



test_that("train_forest and predict_forest works with method GL", {
  withr::with_seed(1234, {
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
      seed = 1234,
      mtry = 2,
      minsplit = 2,
      nodesize = 1,
      method = "GL",
      min_score = 5,
      max_nodes = 15,
      n_trees = 2,
      parallel = FALSE,
      verbose = TRUE,
      n_bootstrap = 500
    ))

    expect_type(trained_forest, "list")
    expect_equal(
      names(trained_forest),
      c("trees", "tree_metrics", "metrics", "columns", "params", "n_indiv", "n_predictors", "n_trees", "n_bootstrap", "time")
    )
    expect_equal(length(trained_forest$trees), 2)
    expect_equal(
      trained_forest$metrics,
      structure(
        list(
          c_index = 0.785224373482303,
          mse_imse = 18519.4034796525,
          mse_iscore = 374.623534616411
        ),
        class = c(
          "tbl_df",
          "tbl",
          "data.frame"
        ),
        row.names = c(NA, -1L)
      )
    )
    expect_equal(
      trained_forest$tree_metrics,
      list(
        list(
          c_index = 0.794180462452008,
          mse_imse = 18618.1505978492,
          mse_iscore = 275.905888575265
        ),
        list(
          c_index = 0.776268284512599,
          mse_imse = 18420.6563614558,
          mse_iscore = 473.341180657557
        )
      )
    )

    predictions <- predict_forest(
      forest = trained_forest,
      X = df_to_use$X
    )

    expect_equal(length(predictions), nrow(df_to_use$X))
    expect_equal(
      predictions[1:10],
      c(
        0.076394850443334,
        0.670711026592015,
        1.09435297593394,
        0.697103213833562,
        1.13827929989577,
        0.0471805389140517,
        0.934632879012653,
        0.39014224414879,
        0.557213915334505,
        0.849408106472211
      )
    )
  })
})
