test_that("preprocess_data_GL works", {
  withr::with_seed(1234, {
    df_to_use <- generate_data_for_test_forest()

    res <- preprocess_data_GL(
      X = df_to_use$X,
      Y = df_to_use$Y,
      death_col = "death"
    )

    expect_equal(
      res[1:10, ],
      structure(
        list(
          id = c(1L, 1L, 1L, 2L, 2L, 3L, 3L, 4L, 4L, 4L),
          t.start = c(
            0L,
            24L,
            457L,
            0L,
            489L,
            0L,
            15L,
            0L,
            163L,
            288L
          ),
          t.stop = c(
            24L,
            457L,
            1037L,
            489L,
            1182L,
            15L,
            783L,
            163L,
            288L,
            638L
          ),
          death = c(
            0L,
            0L,
            0L,
            0L,
            0L,
            0L,
            1L,
            0L,
            0L,
            0L
          ),
          chemo = structure(
            c(
              2L,
              2L,
              2L,
              1L,
              1L,
              1L,
              1L,
              2L,
              2L,
              2L
            ),
            levels = c("NonTreated", "Treated"),
            class = "factor"
          ),
          sex = structure(
            c(2L, 2L, 2L, 1L, 1L, 1L, 1L, 2L, 2L, 2L),
            levels = c(
              "Male",
              "Female"
            ),
            class = "factor"
          ),
          dummy = c(
            -1.20706574938542,
            -1.20706574938542,
            -1.20706574938542,
            0.27742924211066,
            0.27742924211066,
            1.08444117668306,
            1.08444117668306,
            -2.34569770262935,
            -2.34569770262935,
            -2.34569770262935
          ),
          `_cens` = c(
            0,
            0,
            1,
            0,
            1,
            0,
            0,
            0,
            0,
            0
          ),
          `_statusG` = c(1, 1, 0, 1, 0, 1, 2, 1, 1, 1)
        ),
        row.names = c(
          NA,
          10L
        ),
        class = "data.frame"
      )
    )
  })
})
