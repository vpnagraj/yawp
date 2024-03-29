% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read.R
\name{read_repo}
\alias{read_repo}
\title{Read files directly from a GitHub repository}
\usage{
read_repo(
  repo,
  branch = "master",
  pattern = NULL,
  to_tibble = FALSE,
  .f = readr::read_csv,
  n_files = NULL,
  ...
)
}
\arguments{
\item{repo}{Name of the GitHub repository containing the files}

\item{branch}{Name of the branch of the GitHub repository containing the files; default is \code{"master"}}

\item{pattern}{Pattern to match file names to be returned; accepts \link[base]{regex}}

\item{to_tibble}{Boolean indicating whether or not the function should attempt to return a \code{tibble}; default is \code{FALSE}}

\item{.f}{Function to read each file whose name complies to \code{pattern} argument in the repository; default is \link[readr]{read_csv}}

\item{n_files}{Maximum number of files to read in}

\item{...}{Additional arguments passed to the \code{.f} read function}
}
\value{
Contents of the files in the repository that have read using ".f" function supplied. If \code{to_tibble = TRUE} then the function will try to stack results on top of each other and return a \code{tibble}. If \code{to_tibble = FALSE} then the returned object will be a list with as many elements as there are files in the repository that match the "pattern" argument.
}
\description{
\Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}

This utility allows files from a GitHub repository to be read in bulk. The pattern for file names to read (and sub-directories to traverse) can be arbitrarily defined using a regular expression.
}
\details{
Starting with a call to the GitHub API, this function returns a list of files, which are then filtered to include only those that match the regular expression in the "pattern" argument. The function then internally constructs the raw content URLs and then applies the read function (".f") to each path.

\strong{NOTE}: The API call is unauthenticated. GitHub API request rate limits apply. For specifics, review the GitHub API documentation.
}
\examples{
\dontrun{
read_repo(repo = "cdcepi/Flusight-forecast-data",
          branch = "master",
          pattern = "data-forecasts/.*/.*\\\\.csv",
          to_tibble = TRUE,
          .f = readr::read_csv,
          n_files=10,
          col_types="DcDccdd",
          progress=FALSE)
}
}
