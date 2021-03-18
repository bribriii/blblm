test_that("blblm works", {
  fitlm <- blblm(mpg ~ wt*hp, data = mtcars, m = 3, B = 100, Parallel = FALSE)
  expect_s3_class(fitlm, "blblm")
  colm <- coef(fitlm)
  expect_equal(length(colm), 4)
})
