library(survival)
## script.R content ##
data(readmission, package = "frailtypack")
df <- readmission

z <- df
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

# Example ---------------------------------------------------------------------
seed <- 1234
parallel <- FALSE
verbose <- TRUE
n_trees <- 3
mtry <- 2 # Number of candidate variables randomly drawn at each node of the trees. This parameter should be tuned by minimizing the OOB error. Default is `NULL`.
minsplit <- 2 # Minimal number of events required to split the node. Cannot be smaller than 2.
nodesize <- 1 # Minimal number of subjects required in both child nodes to split. Cannot be smaller than 1.

method <- "NAa"
min_score <- 5

# method <- "GL"
# min_score <- 0.05

if (parallel) {
  n_cores <- min(future::availableCores(), n_trees)
  future::plan(future::multisession)
}

data_to_use <- cbind(X, Y)
colnames(data_to_use)[length(colnames(data_to_use))] <- "event"

trained_forest <- train_forest(
  data = data_to_use,
  covariates = c("chemo", "sex", "dummy"),
  event = "event",
  time_vars = c("t.start", "t.stop"),
  id_var = "id",
  death_var = "death",
  n_trees = n_trees,
  n_bootstrap = 1000,
  parallel = parallel,
  verbose = verbose,
  seed = seed,
  mtry = mtry,
  minsplit = minsplit,
  nodesize = nodesize,
  method = method,
  min_score = min_score,
  max_nodes = 200
)

# predictions <- predict_forest(
#   forest = trained_forest,
#   X = X
# )
predictions <- predict(
  trained_forest,
  data_to_use,
  covariates = c("chemo", "sex", "dummy"),
  time_variables = c("t.start", "t.stop"),
  id_var = "id",
  death_variable = "death"
)


print(predictions)

true_vals <- X %>%
  dplyr::mutate(
    event = Y,
    event_count_pred = predictions
  ) %>%
  dplyr::group_by(id) %>%
  dplyr::mutate(
    event_count_true = cumsum(event)
  ) %>%
  dplyr::ungroup(id)

print(true_vals)
