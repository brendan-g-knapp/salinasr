% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{is_geo}
\alias{is_geo}
\alias{is_geo.metadata}
\alias{is_geo.tbl_df}
\alias{is_geo.character}
\title{Does a dataset contain geospatial features?}
\usage{
is_geo(x)

\method{is_geo}{metadata}(x)

\method{is_geo}{tbl_df}(x)

\method{is_geo}{character}(x)
}
\arguments{
\item{x}{A \code{metadata} object obtained by \code{sal_get_metadata()}, a \code{tibble} obtained via
\code{sal_get_dataset()}, or a \code{character} representing a \code{dataset_id}.}
}
\value{
\code{logical}, whether a dataset is annotated as containing geospatial features.
}
\description{
Does a dataset contain geospatial features?
}
\section{Methods (by class)}{
\itemize{
\item \code{metadata}: Method to check if a \code{metadata} object obtained via \code{sal_get_metadata()} references a
dataset that \code{is_geo()}.

\item \code{tbl_df}: Method to check if a \code{tibble} obtained via \code{sal_get_dataset()} references a dataset
that \code{is_geo()}.

\item \code{character}: Method to check if a \code{character} representing a \code{dataset_id} references a dataset that
\code{is_geo()}.
}}

\examples{
library(salinasr)

is_geo("bikeways")

sal_get_metadata("bikeways") \%>\% 
  is_geo()
  
sal_get_metadata("bikeways") \%>\% 
  sal_get_dataset() \%>\% 
  is_geo()

}
\author{
Brendan Knapp \email{brendan.g.knapp@gmail.com}
}
