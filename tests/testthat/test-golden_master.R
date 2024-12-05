test_that("golden_master works", {
  withr::with_seed(12345, {
    expect_snapshot(x = {
      source("golden_master.R")
    })
  })
})
