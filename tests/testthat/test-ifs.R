test_that("ifv constructor validates", {
  expect_s3_class(ifv(0.6, 0.3), "ifv")
  expect_error(ifv(0.7, 0.5))
  expect_error(ifv(-0.1, 0.2))
  expect_equal(if_pi(ifv(0.6, 0.3)), 0.1)
})

test_that("Xu arithmetic identities hold", {
  a <- ifv(0.6, 0.3); b <- ifv(0.5, 0.2)
  s <- if_add(a, b)
  expect_equal(s$mu, 0.6 + 0.5 - 0.6 * 0.5)
  expect_equal(s$nu, 0.3 * 0.2)
  p <- if_mul(a, b)
  expect_equal(p$mu, 0.6 * 0.5)
  expect_equal(p$nu, 0.3 + 0.2 - 0.3 * 0.2)
  expect_equal(if_scalar_mul(1, a)$mu, a$mu)
  z <- if_scalar_mul(0, a)
  expect_equal(z$mu, 0); expect_equal(z$nu, 1)
  expect_equal(if_pow(a, 1)$mu, a$mu)
})

test_that("aggregation operators agree on equal weights sanity", {
  x <- ifv(c(0.6, 0.6), c(0.3, 0.3))
  r <- ifwa(x, c(0.5, 0.5))
  expect_equal(r$mu, 0.6, tolerance = 1e-8)
  expect_equal(r$nu, 0.3, tolerance = 1e-8)
  g <- ifwg(x, c(0.5, 0.5))
  expect_equal(g$mu, 0.6, tolerance = 1e-8)
  r2 <- ifwa(x, c(2, 2))
  expect_equal(r2$mu, r$mu)
})

test_that("score/rank ordering works", {
  x <- ifv(c(0.6, 0.5, 0.7), c(0.3, 0.2, 0.1))
  expect_equal(if_order(x), c(3L, 1L, 2L))
  expect_equal(if_rank(x), c(2L, 3L, 1L))
})

test_that("entropy ranges", {
  x <- ifv(c(0.5, 0.6, 0.4), c(0.3, 0.2, 0.4))
  for (m in c("burillo", "szmidt", "vlachos", "zeng_li"))
    expect_true(if_entropy(x, m) >= 0 && if_entropy(x, m) <= 1)
  crisp <- ifv(c(1, 0), c(0, 1))
  expect_equal(if_entropy(crisp, "burillo"), 0)
  expect_equal(if_entropy(crisp, "zeng_li"), 0)
})

test_that("distances and similarities consistent", {
  a <- ifv(c(0.6, 0.5), c(0.3, 0.2)); b <- ifv(c(0.5, 0.4), c(0.3, 0.3))
  expect_equal(if_dist(a, a, "hamming"), 0)
  expect_equal(if_dist(a, a, "euclidean"), 0)
  expect_equal(if_sim(a, a, "ye_cosine"), 1)
  expect_equal(if_sim(a, a, "dice"), 1)
  expect_equal(if_sim(a, b, "chen"), 1 - if_dist(a, b, "hamming"))
})

test_that("composition and inference run", {
  R <- ifv(c(0.6, 0.3, 0.8, 0.2), c(0.2, 0.5, 0.1, 0.6))
  S <- ifv(c(0.5, 0.7, 0.4, 0.9), c(0.3, 0.1, 0.4, 0.0))
  C <- if_compose(R, S, nx = 2, ny = 2, nz = 2)
  expect_equal(nrow(C), 4L)
  fact <- ifv(c(0.8, 0.2), c(0.1, 0.7))
  inf <- if_infer(fact, R, nx = 2, ny = 2)
  expect_equal(nrow(inf), 2L)
})

test_that("clustering runs", {
  set.seed(1)
  mu <- runif(20, 0.3, 0.7); nu <- runif(20, 0.05, 0.25)
  x <- ifv(mu, nu)
  r <- ifcmeans(x, k = 2, maxit = 10, nstart = 1)
  expect_equal(nrow(r$centers), 2L)
  expect_equal(length(r$cluster), 20L)
  k2 <- ifkmeans(x, k = 2, nstart = 1)
  expect_equal(length(k2$cluster), 20L)
})

test_that("topsis ranks", {
  m <- ifv(c(0.6, 0.5, 0.7, 0.4, 0.5, 0.6), c(0.2, 0.3, 0.1, 0.4, 0.3, 0.2))
  t <- if_topsis(m, nalt = 3, ncrit = 2, w = c(0.6, 0.4))
  expect_equal(nrow(t), 3L)
  expect_true(all(sort(t$rank) == 1:3))
})

test_that("fuzzify/defuzzify round-trip", {
  f <- if_fuzzify(1:5)
  expect_equal(nrow(f), 5L)
  expect_equal(if_defuzzify(f, "mu"), (0:4) / 4)
})
