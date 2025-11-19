test_that("numeric <-> sparse_numeric coercion works", {
  v <- c(0, 4, 3.2, 0, 6.1)
  sv <- as(v, "sparse_numeric")
  expect_s4_class(sv, "sparse_numeric")
  expect_identical(as(sv, "numeric"), v)

  v0 <- numeric(0)
  sv0 <- as(v0, "sparse_numeric")
  expect_identical(as(sv0, "numeric"), v0)
})

test_that("validity catches bad objects", {
  good <- new("sparse_numeric", value = c(1, 2), pos = c(2L, 5L), length = 10L)
  expect_true(validObject(good))

  expect_error(
    new("sparse_numeric", value = c(0, 2), pos = c(1L, 3L), length = 5L),
    "must not contain zeros"
  )
})

test_that("sparse arithmetic matches dense arithmetic", {
  v <- c(0, 4, 3.2, 0, 6.1)
  w <- c(1, 0, -3.2, 5, 0)
  x <- as(v, "sparse_numeric")
  y <- as(w, "sparse_numeric")

  s_add <- as(sparse_add(x, y), "numeric")
  s_sub <- as(sparse_sub(x, y), "numeric")
  s_mul <- as(sparse_mult(x, y), "numeric")

  expect_equal(s_add, v + w)
  expect_equal(s_sub, v - w)
  expect_equal(s_mul, v * w)

  expect_equal(sparse_crossprod(x, y), sum(v * w))
})

test_that("operators + - * dispatch to sparse_*", {
  v <- c(0, 4, 3.2, 0, 6.1)
  w <- c(1, 0, -3.2, 5, 0)
  x <- as(v, "sparse_numeric")
  y <- as(w, "sparse_numeric")

  expect_equal(as(x + y, "numeric"), v + w)
  expect_equal(as(x - y, "numeric"), v - w)
  expect_equal(as(x * y, "numeric"), v * w)
})

test_that("length mismatch errors", {
  v <- c(0, 4, 3.2, 0, 6.1)
  w <- c(1, 2, 3)
  x <- as(v, "sparse_numeric")
  y <- as(w, "sparse_numeric")

  expect_error(sparse_add(x, y))
  expect_error(sparse_sub(x, y))
  expect_error(sparse_mult(x, y))
  expect_error(sparse_crossprod(x, y))
})

test_that("norm and sparse_norm2 agree", {
  v <- c(0, 4, 3.2, 0, 6.1)
  x <- as(v, "sparse_numeric")

  expect_equal(sparse_norm2(x), sqrt(sum(v^2)))
  expect_equal(norm(x), sqrt(sum(v^2)))
})

test_that("mean works on sparse_numeric", {
  v <- c(0, 4, 3.2, 0, 6.1)
  x <- as(v, "sparse_numeric")
  expect_equal(mean(x), mean(v))

  x0 <- as(numeric(0), "sparse_numeric")
  expect_true(is.na(mean(x0)))
})

test_that("standardize produces mean 0 and sd 1", {
  v <- c(0, 4, 3.2, 0, 6.1)
  x <- as(v, "sparse_numeric")
  xs <- standardize(x)
  d <- as(xs, "numeric")
  expect_equal(mean(d), 0, tolerance = 1e-8)
  expect_equal(sd(d), 1, tolerance = 1e-8)
})

test_that("plot method runs without error", {
  v <- c(0, 4, 3.2, 0, 6.1)
  w <- c(1, 0, -3.2, 5, 0)
  x <- as(v, "sparse_numeric")
  y <- as(w, "sparse_numeric")

  pdf(NULL)
  expect_silent(plot(x, y))
  dev.off()
})
