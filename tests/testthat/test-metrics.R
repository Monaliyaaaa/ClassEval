library(ClassEval)

test_that("accuracy_score works correctly", {

  y_true <- c(1, 0, 1, 1)
  y_pred <- c(1, 0, 0, 1)

  expect_equal(accuracy_score(y_true, y_pred), 0.75)
})

test_that("precision_score works correctly", {

  y_true <- c(1, 0, 1, 1)
  y_pred <- c(1, 1, 0, 1)

  expect_equal(precision_score(y_true, y_pred), 2/3)
})

test_that("recall_score works correctly", {

  y_true <- c(1, 0, 1, 1)
  y_pred <- c(1, 1, 0, 1)

  expect_equal(recall_score(y_true, y_pred), 2/3)
})

test_that("k_fold_cv returns valid value", {

  data <- data.frame(
    x = rnorm(50),
    y = rnorm(50),
    label = sample(0:1, 50, replace = TRUE)
  )

  result <- k_fold_cv(label ~ x + y, data, k = 5)

  expect_true(result >= 0 && result <= 1)
})
