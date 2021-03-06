% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/multi_component_RGE.R
\name{soft_assignment}
\alias{soft_assignment}
\title{Function to calculate the third term in the objective function}
\usage{
soft_assignment(X, C, sigma)
}
\arguments{
\item{X}{input data}

\item{C}{center of graph (D * K)}

\item{sigma}{bandwidth parameter}
}
\value{
a matrix with diagonal element as 1 while other elements as zero
  (eye matrix)
}
\description{
Function to calculate the third term in the objective function
}
