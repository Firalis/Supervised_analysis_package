library(testthat)


test_that("function wilcox", {
  data(iris)
  s <- iris[which(iris$Species == "setosa"), ]
  v <- iris[which(iris$Species != "setosa"), ]
  expect_equal(nrow(wilcox(1, 4, s, v, FALSE, 2, "test")), 4)
  expect_equal(ncol(wilcox(1, 4, s, v, FALSE, 2, "test")), 7)
  expect_error(wilcox(1, 4, s, v, FALSE, 2, "test"), NA)
})

test_that("function student", {
  data(iris)
  s <- iris[which(iris$Species == "setosa"), ]
  v <- iris[which(iris$Species != "setosa"), ]
  expect_equal(nrow(student(1, 4, s, v, FALSE, 2, "test")), 4)
  expect_equal(ncol(student(1, 4, s, v, FALSE, 2, "test")), 7)
  expect_error(student(1, 4, s, v, FALSE, 2, "test"), NA)
})

test_that("function normality", {
  data(iris)
  s <- iris[which(iris$Species == "setosa"), ]
  expect_equal(nrow(normality(1, 4, s, "test")), 4)
  expect_equal(ncol(normality(1, 4, s, "test")), 2)
  expect_error(normality(1, 4, s, "test"), NA)
})


test_that("function ANOVA_Non_param", {
  data(iris)
  expect_equal(nrow(ANOVA_Non_param(1, 4, iris, iris$Species, "test")), 4)
  expect_error(ANOVA_Non_param(1, 4, iris, iris$Species, "test"), NA)
})
