#' Calculate mean and standard error
#'
#' @description
#'
#' This function will take an input `data.frame` and summarize a specified numeric column. The summary will include mean, standard error, and a confidence interval corresponding to the width specified (see ".ci" argument). The function leverages tidy eval internally and can accommodate bare column names.
#'
#' @details The inspiration for this function was Winston Chang's `summarySE()` function from "The Cookbook for R". As mentioned in that text, this method of computing standard error is only appropriate for *between* group variation. If there is *within* subject variation (repeated measures) use another method (see references below).
#'
#' @param .data A `data.frame` or `data.frame` extension (e.g. `tibble`)
#' @param measure_var Bare column name for variable to summarize
#' @param ... Optional bare column names for grouping variables
#' @param .ci Width of confidence interval; default is `0.95`
#' @param na.rm Boolean indicating whether or not to use the `na.rm` argument in `mean()` and `sd()` functions; default is `TRUE`
#'
#' @md
#' @return Tibble with columns for grouping variables (if any) and summarized values for mean ("mean"), standard error ("se"), and confidence interval ("ci").
#' @export
#'
#' @references http://www.cookbook-r.com/Manipulating_data/Summarizing_data/
#' @references http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/#error-bars-for-within-subjects-variables
#' @examples
#' summary_se(.data = ToothGrowth, measure_var = len, supp, dose)
summary_se <- function(.data, measure_var, ..., .ci = 0.95, na.rm = TRUE) {

  ## quote measure variable and grouping variabbles
  measure_var <- dplyr::enquo(measure_var)
  group_var <- dplyr::enquos(...)

  .data %>%
    ## unquote grouping variables (if any)
    dplyr::group_by(!!! group_var) %>%
    ## calculate mean, sd, se, ci
    dplyr::summarise(mean = mean(!! measure_var, na.rm = na.rm),
                     sd = stats::sd(!! measure_var, na.rm = na.rm),
                     n = dplyr::n(),
                     se = sd/sqrt(n),
                     ci = se * stats::qt(.ci/2 + 0.5, n-1),
                     .groups = "drop")

}


#' Format median
#'
#' @description
#'
#' The purpose of this function is to calculate the median, 25th and 75th percentiles of a numeric vector and format output as a string.
#'
#' @details Note that the values for median, 25th, and 75th percentiles are calculated using \link[stats]{fivenum}.
#'
#' @param x Numeric vector to summarize
#' @param digits The number of decimal places to use; default is `2`
#' @param na.rm Boolean indicating whether or not `NA` and `NaN` values should be dropped; inherited by \link[stats]{fivenum}; default is `TRUE`
#' @param ... Additional arguments passed to \link[base]{formatC}
#'
#' @return Character vector of length 1 with formatted median followed by 25th,75th percentiles in parentheses:`"median (25th,75th)"`. The formatting can be further customized using arguments to `base::formatC`.
#'
#' @export
#'
#' @md
#'
#' @examples
#'
#' x <- sample(1:9, size = 100, replace = TRUE, prob = seq(0.9,0.1,-0.1))
#' medf(x)
#'
medf <- function(x, digits = 2, na.rm = TRUE, ...) {

  ## get 25th, median, 75th percentile
  nums <- stats::fivenum(x, na.rm = na.rm)[2:4]

  paste0(formatC(nums[2], format = "f", digits = digits, ...),
         " (",
         formatC(nums[1], format = "f", digits = digits, ...),
         ",",
         formatC(nums[3], format = "f", digits = digits, ...),
         ")")


}


#' Calculate and format proportion
#'
#' @description
#'
#' This is a simple utility that takes an input vector (character or factor) and calculates proportion of elements in the vector that equal a user-specified value.
#'
#' @details
#'
#' The function works by first converting the input vector "x" to a factor, then the counting the occurrence of the "level" value, calculating the proportion, and finally formatting the output as count and proportion or percentage in parentheses. The default value for "level" is `1`, which assumes the user is interested in calculating the proportion of non-reference values in a binary variable. This utility works just as well with non-binary variables, and the user can issue the function call on the same vector with different values of "level" if there are > 1 non-reference proportions of interest.
#'
#' @param x Vector for which the proportion should of 'level' values should be calculated
#' @param level The non-reference level of interest; default is `1`
#' @param digits The number of decimal places to use; default is `2`
#' @param percent Boolean indicating whether or not the formatted proportion should be converted to percentage; default is `TRUE`
#' @param ... Additional arguments passed to \link[base]{formatC}
#'
#' @return A character vector of length 1 formatted as either `"count (proportion)"` or `"count (percentage%)"` depending on the input to "percent" argument. In either case the formatting can be further customized using arguments to `base::formatC`.
#'
#' @export
#'
#' @md
#'
#' @examples
#'
#'
#'dogs <- c("dog","dog","dog")
#'cats <- c("cat","cat")
#'rats <- c("rat","rat","rat","rat","rat")
#'animals <- c(dogs,cats,rats)
#'
#'propf(animals, level = "rat")
#'propf(animals, level = "rat", decimal.mark = ",")
#'propf(animals, level = "dog", percent = FALSE)
#'propf(animals, level = "dog", percent = FALSE, drop0trailing = TRUE)
#'
propf <- function(x, level = "1", digits = 2, percent = TRUE, ...) {

  ## convert input to factor
  x <- as.factor(x)
  ## get the denominator for proportion
  n_total <- length(x)

  ## handle NA values if any
  if (is.na(level) | level == "NA") {
    level <- "NA"
    n_level1 <- length(x[x==level])
  } else {
    n_level1 <- length(x[x==level & !is.na(x)])
  }

  ## calculate proportion
  prop <- n_level1/n_total

  ## format
  if(percent) {
    paste0(n_level1, " (", formatC(100 * prop, format = "f", digits = digits, ...), "%)")
  } else {
    paste0(n_level1, " (", formatC(prop, format = "f", digits = digits, ...), ")")
  }

}

#' Calculate F1 score
#'
#' @description
#'
#' Starting from a `tibble`, this function will calculate binary classification accuracy measures, including the precision and recall, as well as the F1 score, which is the harmonic mean of the two. The input data should include one column for the truth value, and another for the observed. The observed data can either be in the format of predicted outcome or a vector of probabilities. If probabilities are passed to the function, then the user may specify the threshold(s) to classify predicted outcome.
#'
#' @details
#'
#' The following formulas are used to calculate the precision, recall, and F1 score:
#'
#' \deqn{Precision =  TP/(TP+FP)}
#' \deqn{Recall =  TP/(TP+FN)}
#' \deqn{F1 =  2 x ((Precision x Recall) / (Precision + Recall))}
#'
#' Note that F1 is the harmonic mean of precision and recall. The more general F beta score allows precision to be weighted greater than recall or vice versa.
#'
#' @param .data `data.frame` or \link[dplyr]{tibble} with columns for predicted and true values
#' @param observed Bare column name for the predicted value
#' @param truth Bare column name for the true value
#' @param positive Vector of length 1 specifying the value of the 'positive' class in the observed and truth columns; default is `1`
#' @param use_thresh Boolean indicating whether or not the accuracy measures calculated should be based on a threshold(s); this argument should only be set to `TRUE` if the 'observed' column is a vector of probabilities; if `TRUE` the 'thresh' argument will be used to capture thresholds to test; default is `FALSE` and the argument to 'thresh' is ignored
#' @param thresh Vector of thresholds to test; ignored if `use_thresh = FALSE`; default is `0.5`
#'
#' @return A `tibble` with at least one row and four columns: "threshold" (`NA` if `use_thresh = FALSE`), "precision", "recall", "f1". If `use_thresh = TRUE` the `tibble` returned will have as many rows as the length of the vector passed to "thresh".
#'
#' @references Sasaki, Yutaka. (2007). The truth of the F-measure. Teach Tutor Mater.
#'
#' @export
#'
#' @md
#'
#' @examples
#'
#' fit <- glm(am ~ mpg + wt, data = mtcars, family = "binomial")
#' resp <- predict(fit, newdata = dplyr::select(mtcars, wt, mpg), type = "response")
#' resp <- ifelse(resp > 0.5, 1, 0)
#' dat <- data.frame(am = mtcars$am, prediction = resp)
#'
#' f1(dat, observed = prediction, truth = am, use_thresh = FALSE)
#'
#'
#' x <- rnorm(100, mean = 0.2, sd = 0.4)
#' y <- rnorm(100, mean = 0.6, sd = 0.4)
#' x <- bound(x)
#' y <- bound(y)
#' dat <- data.frame(probs = c(x,y), class = c(rep("x",100), rep("y", 100)))
#'
#' f1(dat, observed = probs, truth = class, use_thresh = TRUE, thresh = c(0.4,0.5,0.6), positive = "x")
#'
f1 <- function(.data, observed, truth, positive = "1", use_thresh = FALSE, thresh = 0.5) {

  observed <- dplyr::enquo(observed)
  truth <- dplyr::enquo(truth)

  ## given how the precision/recall measures  are calculated ...
  ## positive class cannot be '0'
  if(positive == "0") {
    stop("The value passed to the 'positive' argument cannot be '0' ...")
  }

  if(use_thresh) {

    ## vectors to store f1, precision, recall results from loop
    f1_scores <- vector()
    precision_scores <- vector()
    recall_scores <- vector()

    ## loop over each threshold
    ## calculate precision, recall, f1
    for(i in 1:length(thresh)) {

      tmp <-
        .data %>%
        dplyr::filter(!is.na(!!observed)) %>%
        dplyr::mutate(observed = ifelse(!!observed > thresh[i] , positive, "0")) %>%
        # dplyr::count(observed, !!truth) %>%
        dplyr::rename(truth = !!truth)

      ## get n true positives, false positives, false negatives
      n_tp <- nrow(dplyr::filter(tmp, observed == positive & truth == positive))
      n_fp <- nrow(dplyr::filter(tmp, observed == positive & truth != positive))
      n_fn <- nrow(dplyr::filter(tmp, observed != positive & truth == positive))

      ## precision = n true positives divided by (n false positives PLUS  n true positives)
      precision <- n_tp / (n_tp + n_fp)
      ## recall = n true positives divided by (n true positives PLUS n false negatives)
      recall <- n_tp / (n_tp + n_fn)
      ## F1 score 2 times ((precision times recall ) divided by (precision PLUS recall))
      f1 <- 2 * ((precision*recall)/(precision + recall))

      if(length(f1) == 0) f1 <- NA
      if(length(precision) == 0) precision <- NA
      if(length(recall) == 0) recall <- NA

      f1_scores[i] <- f1
      precision_scores[i] <- precision
      recall_scores[i] <- recall

    }
  } else {
    tmp <-
      .data %>%
      dplyr::rename(truth = !!truth,
                    observed = !!observed)

    if(!any(tmp$truth == tmp$observed)) {
      stop(sprintf("None of the values for %s (truth) matched %s (observed) ... ",
                   dplyr::quo_name(truth),
                   dplyr::quo_name(observed)))
    }

    ## get n true positives, false positives, false negatives
    n_tp <- nrow(dplyr::filter(tmp, observed == positive & truth == positive))
    n_fp <- nrow(dplyr::filter(tmp, observed == positive & truth != positive))
    n_fn <- nrow(dplyr::filter(tmp, observed != positive & truth == positive))

    ## precision = n true positives divided by (n false positives PLUS  n true positives)
    precision <- n_tp / (n_tp + n_fp)
    ## recall = n true positives divided by (n true positives PLUS n false negatives)
    recall <- n_tp / (n_tp + n_fn)
    ## F1 score 2 times ((precision times recall ) divided by (precision PLUS recall))
    f1 <- 2 * ((precision*recall)/(precision + recall))

    if(length(f1) == 0) f1 <- NA
    if(length(precision) == 0) precision <- NA
    if(length(recall) == 0) recall <- NA

    ## set thresh to NA because use_thresh = FALSE
    thresh <- NA
    f1_scores <- f1
    precision_scores <- precision
    recall_scores <- recall

  }

  dplyr::tibble(threshold = thresh,
                precision = precision_scores,
                recall = recall_scores,
                f1 = f1_scores)


}


#' Calculate moving average
#'
#' @description
#'
#' This function calculates the moving average of a time series or numeric vector. The rolling calculation can be parameterized by the size of the window with the argument `window` and alignment of the window (centered or left-aligned) with the `align` parameter.
#'
#' @details
#'
#' The functionality here is a wrapper for \link[stats]{filter}. The `align` argument corresponds to `sides` in `stats::filter`, with `align = "left"` triggering `sides = 1` and `align = "center"` triggering `sides = 2`. The function uses the default convolution filter from `stats::filter`.
#'
#' @param x Numeric vector or time series
#' @param window Numeric vector of length 1 indicating the size of window for the moving average
#' @param align Character vector specifying the alignment of the window; allowed values include `"left"` or `"center"`; default is `"center"`
#'
#' @return Vector of the same length as `x` with the moving average at each index. Note `window` - 1 of the values will be `NA`, and depending on the `align` parameter these missing values will appear either at the beginning, end, or both sides of the vector.
#'
#' @export
#'
#' @md
#'
#' @examples
#'
#' x <- rpois(100, lambda = 3)
#' mav(x, window = 7, align = "center")
#' mav(x, window = 7, align = "left")
mav <- function(x, window, align = "center") {

  if(align == "center") {
    res <- stats::filter(x,rep(1/window,window), sides=2)
  } else if (align == "left") {
    res <- stats::filter(x,rep(1/window,window), sides=1)
  }

  ## if the input was a numeric vector ...
  ## then make sure the output returned is numeric (otherwise a ts)
  if(is.numeric(x)) {
    as.numeric(res)
  } else {
    res
  }
}


#' Find the mode
#'
#' @description
#'
#' This utility calculates the mode (i.e. the most frequently observed value) for a vector of values. The function  function includes an option (see "ties" argument) to return more than one value if there are ties when calculating the mode.
#'
#' @details
#'
#' The mode returned is calculated by tabulating the frequency of each element in the input vector. Note that this method allows for non-numeric vectors to be passed to the function. For example, the function would return the "mode" for a character vector as the most frequently observed string.
#'
#' @param x Vector for which the mode should be calculated
#' @param ties Boolean indicating whether or not ties for mode should be returned; if `FALSE` the function will use \link[base]{which.max} to compute the mode and given ties *only* the first index will be returned; default is `TRUE`
#'
#' @return A vector containing the most frequently observed values in the `x` vector. If there are ties in the mode and the "ties" argument is set to `TRUE`, then the length of the returned vector will be greater than 1. If not, then the length of the returned vector will be 1.
#'
#' @export
#'
#' @md
#'
#' @examples
#'
#' get_mode(c(1,1,4,3,2,2,2,2))
#' get_mode(c("dog","dog","dog","monkey", "rat"))
#' get_mode(c(1,1,1,1,4,3,2,2,2,2), ties = TRUE)
#' get_mode(c(1,1,1,1,4,3,2,2,2,2), ties = FALSE)
#'
get_mode <- function(x, ties = TRUE) {
  tmp <- unique(x)
  tabulated <- tabulate(match(x,tmp))

  if(ties) {
    tmp[which(tabulated == max(tabulated))]
  } else {
    tmp[which.max(tabulated)]
  }
}
