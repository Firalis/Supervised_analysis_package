
testthat::test_that("function wilcox", {
  data(iris)
  s <- iris[which(iris$Species == "setosa"), ]
  v <- iris[which(iris$Species != "setosa"), ]
  testthat::expect_equal(nrow(wilcox(1, 4, s, v, FALSE, 2, "test")), 4)
  testthat::expect_equal(ncol(wilcox(1, 4, s, v, FALSE, 2, "test")), 7)
  testthat::expect_error(wilcox(1, 4, s, v, FALSE, 2, "test"), NA)
})

testthat::test_that("function student", {
  data(iris)
  s <- iris[which(iris$Species == "setosa"), ]
  v <- iris[which(iris$Species != "setosa"), ]
  testthat::expect_equal(nrow(student(1, 4, s, v, FALSE, 2, "test")), 4)
  testthat::expect_equal(ncol(student(1, 4, s, v, FALSE, 2, "test")), 7)
  testthat::expect_error(student(1, 4, s, v, FALSE, 2, "test"), NA)
})

testthat::test_that("function normality", {
  data(iris)
  s <- iris[which(iris$Species == "setosa"), ]
  testthat::expect_equal(nrow(normality(1, 4, s, "test")), 4)
  testthat::expect_equal(ncol(normality(1, 4, s, "test")), 2)
  testthat::expect_error(normality(1, 4, s, "test"), NA)
})


testthat::test_that("function ANOVA_Non_param", {
  data(iris)
  testthat::expect_equal(nrow(ANOVA_Non_param(1, 4, iris, iris$Species, "test")), 4)
  testthat::expect_error(ANOVA_Non_param(1, 4, iris, iris$Species, "test"), NA)
})
