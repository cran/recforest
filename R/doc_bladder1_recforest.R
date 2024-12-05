#' bladder1_recforest : Bladder Cancer Recurrences
#'
#' Preparation of the survival::bladder1 dataset for the recforest package. Please run `?survival::bladder1` for more information.
#'
#' @format A data frame with 294 rows (118 individuals) and 8 variables:
#' \describe{
#'   \item{ id }{  Patient id }
#'   \item{ t.start }{  Start time }
#'   \item{ t.stop }{  Stop time }
#'   \item{ treatment }{  Placebo, pyridoxine (vitamin B6), or thiotepa }
#'   \item{ number }{  Initial number of tumors (8=8 or more) }
#'   \item{ size }{  Size (cm) of largest initial tumor }
#'   \item{ death }{  Death event }
#'   \item{ event }{  Recurrence event }
#' }
#' @source Script to generate the data can be explored using `browseURL(system.file("generate_bladder1_recforest.R", package = "recforest"))`
"bladder1_recforest"
