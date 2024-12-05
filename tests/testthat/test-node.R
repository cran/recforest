test_that("predict_node works", {
  node <- list("type" = "bad")
  expect_error(
    predict_node(node = node, X = 1, tree = 1),
    "incorrect node type"
  )
})
