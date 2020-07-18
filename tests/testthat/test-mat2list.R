xdata <- matrix(ncol = sample(4:10, 1), nrow = 10, 0)
colnames(xdata) <- letters[1:ncol(xdata)]
xdata[sample(1:length(xdata), length(xdata)/2)] <- 1

x <- mat2list(xdata)
test_that("multiplication works", {
  expect_true(is.list(x))
  expect_equal(length(unlist(x)), length(xdata)/2)
})
