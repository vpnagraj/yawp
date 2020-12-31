
test_that("proportion format works", {
  dogs <- c("dog","dog","dog")
  cats <- c("cat","cat")
  rats <- c("rat","rat","rat","rat","rat")
  animals <- c(dogs,cats,rats)

  tst <- propf(animals, level = "rat")
  expect_equal(tst, "5 (50.00%)")
})

test_that("median format works", {
  tst <- medf(mtcars$mpg, digits = 2, drop0trailing = TRUE)
  expect_equal(tst, "19.2 (15.35,22.8)")
})
