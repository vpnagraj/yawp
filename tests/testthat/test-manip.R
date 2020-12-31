test_that("first_char can extract first letter or number", {

  tst <- first_char("NOTHING")
  expect_equal(tst, "N")

  tst <- first_char("compares")
  expect_equal(tst, "c")

  tst <- first_char("2U")
  expect_equal(tst, "2")

})

test_that("logic for finding values between is working", {

  ## by default should return logical indices
  tst <- betwixt(200:300, 250, 255)
  expect_type(tst, "logical")

  ## if value is true should return same type as input
  tst <- betwixt(200:300, 250, 255, value = TRUE)
  expect_type(tst, "integer")
  ## check that returned values are correct (inclusive on both sides)
  expect_equal(tst, c(250,251,252,253,254,255))

  ## check that ignore is working
  tst <- betwixt(200:300, 250, 255, value = TRUE, ignore = 254)
  expect_equal(tst, c(250,251,252,253,255))

  ## check that by is working
  tst <- betwixt(200:300, 250, 255, value = TRUE, by = 2)
  expect_equal(tst, c(250,252,254))

})
