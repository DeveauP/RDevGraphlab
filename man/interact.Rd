% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/GraphLab.R
\name{interact}
\alias{interact}
\title{interact}
\usage{
interact(allFunc, functions, i = 1)
}
\arguments{
\item{allFunc}{data generated inside of GraphLab}

\item{functions}{character vector with the name of all the functions from the pseudo-package}

\item{i}{position of the function to be tested inside functions}
}
\description{
Creates an interaction matrix for function
A 1 is present in column i, row j if i calls j
}

