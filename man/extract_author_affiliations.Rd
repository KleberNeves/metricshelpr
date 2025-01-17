% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/biblio.R
\name{extract_author_affiliations}
\alias{extract_author_affiliations}
\title{Extracts affiliation of individual authors}
\usage{
extract_author_affiliations(M)
}
\arguments{
\item{M}{A bibliometrix dataset.}
}
\value{
A data frame with the author order and country data.
}
\description{
From a M bibliometrix data frame, extracts the author affiliation and country
from the AU and C1 fields. Lots of ad hoc work arounds to handle abbreviated names and such.
Author order comes in two columns, as counted from first to last (positive) and from last to first (negative, i.e. -1 is the last author).
}
