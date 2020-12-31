test_that("correct mode returned", {
  tst <- get_mode(c(1,1,1,1,4,3,3,3,3,3,3,3,3))
  expect_equal(tst, 3)
})

test_that("mode handles ties as expected", {
  ## if ties = TRUE should return both values
  tst <- get_mode(c(1,1,1,1,4,3,2,2,2,2), ties = TRUE)
  expect_equal(tst, c(1,2))

  ## if ties = FALSE should just pick the first of the n tied values
  tst <- get_mode(c(1,1,1,1,4,3,2,2,2,2), ties = FALSE)
  expect_equal(tst, 1)
})

test_that("bound works as expected", {
  ## if ties = TRUE should return both values
  tst <- bound(rnorm(1000, mean = 0.5, sd = 0.5), min = 0.01, max = 0.99)
  expect_true(!any(tst < 0.01))
  expect_true(!any(tst > 0.99))
})

test_that("summary_se returns correct values", {

  ## test expectations based on rounded values from Cookbook for R
  ## "Plotting means and error bars (ggplot2)"
  tst <- summary_se(ToothGrowth, measure_var = len, supp, dose)
  expect_equal(round(tst$mean, 2), c(13.23,22.70,26.06,7.98,16.77,26.14))
  expect_equal(round(tst$se, 2), c(1.41,1.24,0.84,0.87,0.80,1.52))

})

test_that("f1 score is correct for use_thresh = FALSE", {

  ## create same two class data frame used in the yardstick "two_class_example"
  two_class <-
    dplyr::tribble(~ truth, ~ predicted, ~ n,
                   "Class1", "Class1", 227,
                   "Class1", "Class2", 31,
                   "Class2", "Class1", 50,
                   "Class2", "Class2", 192)

  ## calculate f1 score
  tst <-
    data.frame(truth= rep(two_class$truth, two_class$n),
             predicted = rep(two_class$predicted, two_class$n)) %>%
    f1(., observed = predicted, truth = truth, positive = "Class1") %>%
    dplyr::pull(f1)

  ## should equal the documented value from f_meas (with beta = 1, f_meas == f1)
  expect_equal(round(tst,3), 0.849)

})
