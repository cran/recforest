# recforest 1.0.0

* Filling documentation for `train_forest()` and `predict()`
* Vignettes filled


# recforest 0.2.0

* Adding 3 vignettes
* Setting pkgdown parameters
* Filling README
* `train_forest()` does not used `X` and `Y` anymore but `data`,  `id_var`, `covariates`, `event`, `time_vars`, `death_var` to define data-related parameters

# recforest 0.1.0

* Refactoring `train_forest()`
  * `n_boostrap` is set to NULL by default. If NULL, the number of bootstrap samples is set to 2/3 the number of individuals in the dataset.
  * `params` is not a function parameter anymore, each parameter must be explicitly provided in the function call

# recforest 0.0.2

* Implementing s3 methods for "summary", "print" and "predict" ("plot" waiting to be implemented)
* Adding unit tests (code coverage: 94.21%)
* Adding documentation (README.md)
* Setting continuous integration on Gitlab CI
* Adding a demo dataset "bladder1_recforest"
* Setting pkgdown

# recforest 0.0.1

* Refactoring + golden master
