context("Additional utilities")

test_that("more appends filler rows correctly", {
  input <- tibble::tibble(a = c("x","y"), b = c("z","w"))
  res <- more(input, fill = "-", extra_rows = 2)
  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 4)
  expect_true(all(res[3:4,] == "-"))
})

test_that("mav computes moving averages", {
  x <- 1:10
  expect_equal(mav(x, window = 3, align = "center"),
               c(NA,2:9,NA))
  expect_equal(mav(x, window = 3, align = "left"),
               c(NA,NA,2:9))
})

test_that("mav returns numeric vector for ts input", {
  x <- ts(1:4)
  res <- mav(x, window = 2, align = "left")
  expect_type(res, "double")
})

test_that("theme_mathbook customizations", {
  th <- theme_mathbook()
  expect_equal(th$plot.background$fill, "black")
  expect_equal(th$text$colour, "white")
  expect_equal(th$legend.position, "bottom")
})

test_that("gg_zoom returns patchwork object", {
  dat <- tibble::tibble(x = 1:5, y = 1:5, lbl = letters[1:5])
  p <- ggplot2::ggplot(dat, ggplot2::aes(x,y)) + ggplot2::geom_point()
  res <- gg_zoom(p, x > 3, to_label = TRUE, label = lbl, draw_box = FALSE)
  expect_true(yawp:::is_patchwork(res))
})

