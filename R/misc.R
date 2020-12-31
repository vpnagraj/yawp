#' Add filler rows to a data frame
#'
#' @description
#'
#' This function is a utility to add "filler" rows to the bottom of a `data.frame`. Basically an ellipsis ("...") for tabular data.
#'
#' @param .data Original `data.frame` or \link[dplyr]{tibble} to which the additional rows will be added; note that if a `data.frame` is passed in it will be coerced to a `tibble` prior to row binding
#' @param fill Character vector of length 1 specifying the content to fill the extra rows; default is `.`
#' @param extra_rows Numeric vector of length 1 indicating how many rows of filler content should be added to the original `.data` object; default is `4`
#'
#' @return A `tibble` with as many rows as the original input (`.data`) plus the number of extra rows (`extra_rows`), all of which are populated with the "filler" (`fill`) content in every column
#'
#' @export
#'
#' @md
#'
#' @examples
#'
#' more(head(mtcars))
#' rbind(more(head(mtcars, 1), fill = ".", extra_rows = 1), tail(mtcars, 1))
#'
more <- function(.data, fill = ".", extra_rows = 4) {

  ## force .data to be a tibble
  .data <- tibble::as_tibble(.data)

  ## create a matrix with as many columns as original data ...
  ## ... and as many rows as extra_rows arg
  filler <- matrix(fill, ncol = ncol(.data), nrow = extra_rows)
  ## coerce the matrix to a tibble object
  filler <- tibble::as_tibble(filler, .name_repair = "minimal")
  ## set the column names based on the original data.frame
  names(filler) <- names(.data)

  ## bind together
  rbind(.data, filler)

}

#' Find values between lower and upper bounds of a range
#'
#' @description
#'
#' This is utility function to find values of a vector tha fall between lower and upper bounds specified. The implementation includes a method to optionally ignore certain values and return either a  vector of logical indices or a vector of matching values. Inspired by \link[dplyr]{between}.
#'
#'
#' @param x Vector of numeric or date values
#' @param lower The lower bound (inclusive) for the range of values
#' @param upper The upper bound (inclusive) for the range of values
#' @param ignore Vector containing value(s) to exclude; default is `NULL`
#' @param by Value for increment between lower and upper bounds of the range; default is `1`
#' @param value Boolean as to whether the function should return values of `x` between bounds (`TRUE`) or a vector of logical indices corresponding to whether or not the value falls between bounds (`FALSE`); default is `FALSE`
#'
#' @return
#'
#' Depending on the "value" argument, this will either return a vector of values that fall between the lower and upper bounds *or* a vector of logical vector with `TRUE` for each index corresponding to a value that falls in the range.
#'
#' @export
#'
#' @examples
#'
#' betwixt(1:12, 7, 9)
#' betwixt(1:12, 7, 9, value = TRUE)
#'
#' dates <- seq(as.Date("2000/1/1"), by = "day", length.out = 365)
#' betwixt(dates,
#'         lower = as.Date("2000/1/10"),
#'         upper = as.Date("2000/3/10"),
#'         by = "day",
#'         ignore = as.Date("2000/1/25"),
#'         value = TRUE)
#' @md
betwixt <- function(x, lower, upper, ignore = NULL, by = 1, value = FALSE) {
  ## create sequence from left to right
  ## the by flag allows the user to input increments
  ## NOTE: seq will work on date and numeric objects
  tmp <- seq(from = lower,to = upper,by = by)

  ## if the ignore argument is used then find values of vector
  if(!is.null(ignore)) {
    tmp <- tmp[!tmp %in% ignore]
  }

  ## create vector of boolean indices
  ind <- x %in% tmp
  ## trigger whether to return the boolean TRUE/FALSE ...
  ## ... or the value of the vector at the TRUE indices
  if(value) {
    x[ind]
  } else {
    ind
  }
}


#' Extract the first character from a string
#'
#' @description
#'
#' This helper function will extract the first character from a string. The element may be a letter, number, or special character but will be coerced to a character vector in the output.
#'
#' @param string Character vector from which the first character will be extracted
#'
#' @return Character vector with the first character from each element in the vector passed to the input "string" argument. This value will be the same length as the original vector.
#'
#' @export
#'
#' @md
#'
#' @examples
#'
#' first_char("purple")
#' first_char(c("purple","rain"))
#' first_char(c("nothing","compares","2u"))
#'
first_char <- function(string) {
  string %>%
    stringr::str_split(., pattern = "") %>%
    purrr::map_chr(., .f = function(x) utils::head(x, 1))
}


#' Bound a vector
#'
#' @description
#'
#' This helper function bounds a numeric vector on a minimum and maximum value.
#'
#' @param x Numeric vector to be bounded
#' @param min Minimum allowed value for vector "x"; default is `0.01`
#' @param max Maximum allowed value for vector "x"; default is `0.99`
#'
#' @return Numeric vector of the same length as `x` with no values less than `minimum` nor greater than `maximum`.
#'
#' @export
#'
#' @md
#' @examples
#'
#' x <- rnorm(1000, 0.5, 0.5)
#' bound(x)
#' bound(x, min = 0.2, max = 0.8)
#' bound(x, min = -0.1, max = 1.1)
#'
bound <- function(x, min = 0.01, max = 0.99) {
  x <- ifelse(x < min, min, x)
  x <- ifelse(x > max, max, x)
  return(x)
}

