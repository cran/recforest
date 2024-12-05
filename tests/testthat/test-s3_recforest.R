sink_tmp <- tempfile()
file.create(sink_tmp)

test_that("s3 methods for recforest works", {
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
      parallel = FALSE,
      verbose = FALSE,
      seed = 1234567,
      mtry = 1,
      minsplit = 1,
      nodesize = 1,
      method = "NAa",
      min_score = 1,
      max_nodes = 10,
      n_trees = 2
    ))

    capture.output(print(trained_forest), file = sink_tmp, type = "message")
    output_sink <- readLines(sink_tmp)
    expect_true(any(grepl("Tree 1", output_sink)))
    expect_true(any(grepl("Number of nodes", output_sink)))
    expect_true(any(grepl("Tree 2", output_sink)))
    expect_true(any(grepl("c_index : 0", output_sink)))
    expect_true(any(grepl("mse_imse", output_sink)))
    expect_true(any(grepl("mse_iscore", output_sink)))
    expect_false(any(grepl("Tree 3", output_sink)))



    capture.output(summary(trained_forest), file = sink_tmp, type = "message")
    output_sink <- readLines(sink_tmp)

    expect_true(any(grepl("Number of individuals : 403", output_sink)))
    expect_true(any(grepl("Number of predictors : 3", output_sink)))
    expect_true(any(grepl("mtry : 1", output_sink)))
    expect_true(any(grepl("minsplit : 1", output_sink)))
    expect_true(any(grepl("nodesize : 1", output_sink)))
    expect_true(any(grepl("method : NAa", output_sink)))
    expect_true(any(grepl("min_score : 1", output_sink)))
    expect_true(any(grepl("max_nodes : 1", output_sink)))
    expect_true(any(grepl("c_index : 0", output_sink)))
    expect_true(any(grepl("mse_imse", output_sink)))
    expect_true(any(grepl("mse_iscore", output_sink)))
    expect_true(any(grepl("computation time", output_sink)))


    predictions <- predict_forest(
      forest = trained_forest,
      X = df_to_use$X
    )
    predictionsS3 <- predict(
      trained_forest,
      data_to_use,
      covariates = c("chemo", "sex", "dummy"),
      time_variables = c("t.start", "t.stop"),
      id_var = "id",
      death_variable = "death"
    )

    expect_equal(predictions, predictionsS3)

    expect_error(plot(trained_forest))
  })
})

unlink(sink_tmp, recursive = TRUE)
