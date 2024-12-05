## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  warning = FALSE,
  message = FALSE
)

## ----setup--------------------------------------------------------------------
library(recforest)
library(dplyr)

## -----------------------------------------------------------------------------
data("bladder1_recforest")

id_individuals_bladder1_recforest <- unique(bladder1_recforest$id)

train_ids <- sample(id_individuals_bladder1_recforest, size = 100, replace = FALSE)
test_ids <- setdiff(id_individuals_bladder1_recforest, train_ids)

train_bladder1_recforest <- bladder1_recforest %>%
  filter(id %in% train_ids)

test_bladder1_recforest <- bladder1_recforest %>%
  filter(id %in% test_ids)

## -----------------------------------------------------------------------------
set.seed(1234)
trained_forest <- train_forest(
  data = train_bladder1_recforest,
  id_var = "id",
  covariates = c("treatment", "number", "size"),
  time_vars = c("t.start", "t.stop"),
  death_var = "death",
  event = "event",
  n_trees = 3,
  n_bootstrap = round(2 * length(train_ids) / 3),
  mtry = 2,
  minsplit = 3,
  nodesize = 15,
  method = "NAa",
  min_score = 5,
  max_nodes = 20,
  seed = 111,
  parallel = FALSE,
  verbose = FALSE
)

## -----------------------------------------------------------------------------
predictions <- predict(
  trained_forest,
  newdata = test_bladder1_recforest,
  id_var = "id",
  covariates = c("treatment", "number", "size"),
  time_vars = c("t.start", "t.stop"),
  death_var = "death"
)

