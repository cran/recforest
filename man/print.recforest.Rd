% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/0_s3_methods.R
\name{print.recforest}
\alias{print.recforest}
\title{Print method for recforest objects}
\usage{
\method{print}{recforest}(x, ...)
}
\arguments{
\item{x}{An object of class \code{recforest}.}

\item{...}{Additional arguments to be passed to the plot print}
}
\value{
Used for side effect.
Will print to the console a description of each tree, with, for each:
\itemize{
\item The number of nodes in the tree
\item The metrics of the tree (c_index, mse_imse and mse_iscore)
}
}
\description{
Print method for recforest objects
}
