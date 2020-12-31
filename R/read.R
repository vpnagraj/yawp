#' Read files directly from a GitHub repository
#'
#' @description
#'
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#' This utility allows files from a GitHub repository to be read in bulk. The pattern for file names to read (and sub-directories to traverse) can be arbitrarily defined using a regular expression.
#'
#' @details
#'
#' Starting with a call to the GitHub API, this function returns a list of files, which are then filtered to include only those that match the regular expression in the "pattern" argument. The function then internally constructs the raw content URLs and then applies the read function (".f") to each path.
#'
#' **NOTE**: The API call is unauthenticated. GitHub API request rate limits apply. For specifics, review the GitHub API documentation.
#'
#' @param repo Name of the GitHub repository containing the files
#' @param branch Name of the branch of the GitHub repository containing the files; default is `"master"`
#' @param pattern Pattern to match file names to be returned; accepts \link[base]{regex}
#' @param to_tibble Boolean indicating whether or not the function should attempt to return a `tibble`; default is `FALSE`
#' @param .f Function to read each file whose name complies to `pattern` argument in the repository; default is \link[readr]{read_csv}
#' @param ... Additional arguments passed to the `.f` read function
#'
#' @return Contents of the files in the repository that have read using ".f" function supplied. If `to_tibble = TRUE` then the function will try to stack results on top of each other and return a `tibble`. If `to_tibble = FALSE` then the returned object will be a list with as many elements as there are files in the repository that match the "pattern" argument.
#'
#' @export
#'
#' @md
#'
read_repo <- function(repo, branch = "master", pattern = NULL, to_tibble = FALSE, .f = readr::read_csv, ...) {

  ## construct GET request from the repo and branch
  api_request <- httr::GET(paste0("https://api.github.com/repos/",
                                  repo,
                                  "/git/trees/",
                                  branch,
                                  "?recursive=1"))

  ## extract path element from the API response
  repo_files <- purrr::map_chr(httr::content(api_request)$tree, "path")

  ## if a pattern is passed use it to parse files of interest
  if(!is.null(pattern)) {
    repo_files <- repo_files[grepl(pattern, repo_files)]
  }

  ## construct raw content URLs to files of interest
  repo_files <- file.path("https://raw.githubusercontent.com",
                          repo,
                          branch,
                          repo_files)

  ## check that the value passed to .f is a function available in the environment
  .f <- match.fun(.f)

  ## if to_tibble then use map_df() to compile results as a tibble
  ## otherwise return a list via map()
  if(to_tibble) {
    purrr::map_df(repo_files, .f = .f, ...)
  } else {
    purrr::map(repo_files, .f = .f, ...)
  }

}
