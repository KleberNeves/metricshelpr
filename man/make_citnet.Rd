% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/network.R
\name{make_citnet}
\alias{make_citnet}
\title{Builds a historical citation igraph net}
\usage{
make_citnet(M)
}
\arguments{
\item{M}{The bibliometrix data frame.}
}
\value{
A igraph network made of citations between papers.
}
\description{
Uses a M bibliometrix data frame to build the citation network
for the papers in the dataset, in the igraph format.
}
