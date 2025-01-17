% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/network.R
\name{make_net_for_pajek}
\alias{make_net_for_pajek}
\title{Removes loops from a network so that Pajek can read it}
\usage{
make_net_for_pajek(NET)
}
\arguments{
\item{NET}{An igraph network.}
}
\value{
The simplified network.
}
\description{
Removes loops of order up to 4 to simplify the network
and make it a direct acyclic graph (DAG). This is necessary
so that Pajek can run the main path analysis algorithm.
}
\details{
Called for the side effects: it will save GML and Pajek
files of the network (Historical Citation Net). It will
warn you if the resulting network is still not a DAG.
}
