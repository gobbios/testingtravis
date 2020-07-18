# library(netfacswintest); library(testthat); library(microbenchmark)
xdata <- sapply(1:100, function(x)sort(sample(letters[1:12], size = sample(1:4, 1))))


# microbenchmark(create.rule.set(elements = xdata, maxlen = 3), create_rule_set2(elements = xdata, maxlen = 3), create_rule_set3(elements = xdata, maxlen = 3))

r1 <- create.rule.set(elements = xdata, maxlen = 3)[, -4]
r2 <- create_rule_set2(elements = xdata, maxlen = 3)[, -4]
r3 <- create_rule_set3(elements = xdata, maxlen = 3)

r1 <- r1[order(r1$combination), ]
r2 <- r2[order(r2$combination), ]
r3 <- r3[order(r3$combination), ]
rownames(r1) <- NULL
rownames(r2) <- NULL
rownames(r3) <- NULL

# some reformatting...
r1$observed.probability <- round(r1$observed.probability, 2)
r2$observed.probability <- round(r2$observed.probability, 2)
r3$observed.probability <- round(r3$observed.probability, 2)
r3$count <- as.integer(r3$count)
r3$combination <- as.character(r3$combination)

# identical(r1, r2)
# identical(r1, r3)
# identical(r2, r3)
#
test_that("rule functions work identically", {
  expect_identical(r1, r2)
  expect_identical(r1, r3)
})


# matrix functions:
mat <- matrix(ncol = 10, nrow = 20, 0)
colnames(mat) <- letters[1:10]
rownames(mat) <- 1:nrow(mat)
mat[sample(1:length(mat), 30)] <- 1



m1 <- testingtravis:::shuffle_checkerboard(mat, 10, TRUE)
m2 <- testingtravis:::shuffle_checkerboard(mat, 10, FALSE)
m3 <- testingtravis:::shuffle_rowwise(mat)

test_that("randomizations work", {
  expect_true(sum(mat) == sum(m1))
  expect_true(sum(mat) == sum(m2))
  expect_true(sum(mat) == sum(m3))
})


testingtravis:::sri_mat(mat)

testingtravis:::mat2list(mat)

testingtravis:::choosecpp(10, 3)
testingtravis:::comb_index(6, 2)
testingtravis:::comb_names(letters[1:5], 2)

