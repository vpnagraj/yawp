% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/misc.R
\name{more}
\alias{more}
\title{Add filler rows to a data frame}
\usage{
more(.data, fill = ".", extra_rows = 4)
}
\arguments{
\item{.data}{Original \code{data.frame} or \link[dplyr]{tibble} to which the additional rows will be added; note that if a \code{data.frame} is passed in it will be coerced to a \code{tibble} prior to row binding}

\item{fill}{Character vector of length 1 specifying the content to fill the extra rows; default is \code{.}}

\item{extra_rows}{Numeric vector of length 1 indicating how many rows of filler content should be added to the original \code{.data} object; default is \code{4}}
}
\value{
A \code{tibble} with as many rows as the original input (\code{.data}) plus the number of extra rows (\code{extra_rows}), all of which are populated with the "filler" (\code{fill}) content in every column
}
\description{
This function is a utility to add "filler" rows to the bottom of a \code{data.frame}. Basically an ellipsis ("...") for tabular data.
}
\examples{

more(head(mtcars))
rbind(more(head(mtcars, 1), fill = ".", extra_rows = 1), tail(mtcars, 1))

}
