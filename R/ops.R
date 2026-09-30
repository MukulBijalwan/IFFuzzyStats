#' Intuitionistic fuzzy arithmetic (Xu operations)
#'
#' Addition, multiplication, scalar multiplication and power for IFVs,
#' following Xu (2007): for \eqn{a=(\mu_a,\nu_a)}, \eqn{b=(\mu_b,\nu_b)},
#' \eqn{\lambda>0}:
#' \deqn{a\oplus b=(\mu_a+\mu_b-\mu_a\mu_b,\ \nu_a\nu_b)}
#' \deqn{a\otimes b=(\mu_a\mu_b,\ \nu_a+\nu_b-\nu_a\nu_b)}
#' \deqn{\lambda a=(1-(1-\mu_a)^\lambda,\ \nu_a^\lambda)}
#' \deqn{a^\lambda=(\mu_a^\lambda,\ 1-(1-\nu_a)^\lambda)}
#'
#' Einstein variants use Einstein t-norm / t-conorm:
#' \deqn{a\oplus_\epsilon b=
#'   \left(\frac{\mu_a+\mu_b}{1+\mu_a\mu_b},
#'         \frac{\nu_a\nu_b}{1+(1-\nu_a)(1-\nu_b)}\right)}
#' \deqn{a\otimes_\epsilon b=
#'   \left(\frac{\mu_a\mu_b}{1+(1-\mu_a)(1-\mu_b)},
#'         \frac{\nu_a+\nu_b}{1+\nu_a\nu_b}\right)}
#'
#' @param a,b objects coercible to \code{ifv} (recycled to common length).
#' @param lambda positive numeric scalar or vector.
#' @return An \code{ifv} object.
#' @references Xu, Z. (2007). Intuitionistic fuzzy aggregation operators.
#' \emph{IEEE Trans. Fuzzy Systems}, 15(6), 1179-1187.
#' @examples
#' a <- ifv(0.6, 0.3); b <- ifv(0.5, 0.3)
#' if_add(a, b); if_mul(a, b); if_scalar_mul(2, a); if_pow(a, 2)
#' @export
if_add <- function(a, b) {
  a <- as_ifv(a); b <- as_ifv(b)
  n <- max(nrow(a), nrow(b))
  a <- a[rep(seq_len(nrow(a)), length.out = n), , drop = FALSE]
  b <- b[rep(seq_len(nrow(b)), length.out = n), , drop = FALSE]
  ifv(a$mu + b$mu - a$mu * b$mu, a$nu * b$nu)
}

#' @rdname if_add
#' @export
if_mul <- function(a, b) {
  a <- as_ifv(a); b <- as_ifv(b)
  n <- max(nrow(a), nrow(b))
  a <- a[rep(seq_len(nrow(a)), length.out = n), , drop = FALSE]
  b <- b[rep(seq_len(nrow(b)), length.out = n), , drop = FALSE]
  ifv(a$mu * b$mu, a$nu + b$nu - a$nu * b$nu)
}

#' @rdname if_add
#' @export
if_scalar_mul <- function(lambda, a) {
  a <- as_ifv(a)
  lambda <- as.numeric(lambda)
  n <- max(nrow(a), length(lambda))
  a <- a[rep(seq_len(nrow(a)), length.out = n), , drop = FALSE]
  lambda <- rep(lambda, length.out = n)
  if (any(lambda < 0)) stop("`lambda` must be non-negative.")
  ifv(1 - (1 - a$mu)^lambda, a$nu^lambda)
}

#' @rdname if_add
#' @export
if_pow <- function(a, lambda) {
  a <- as_ifv(a)
  lambda <- as.numeric(lambda)
  n <- max(nrow(a), length(lambda))
  a <- a[rep(seq_len(nrow(a)), length.out = n), , drop = FALSE]
  lambda <- rep(lambda, length.out = n)
  if (any(lambda < 0)) stop("`lambda` must be non-negative.")
  ifv(a$mu^lambda, 1 - (1 - a$nu)^lambda)
}

#' @rdname if_add
#' @export
if_einstein_add <- function(a, b) {
  a <- as_ifv(a); b <- as_ifv(b)
  n <- max(nrow(a), nrow(b))
  a <- a[rep(seq_len(nrow(a)), length.out = n), , drop = FALSE]
  b <- b[rep(seq_len(nrow(b)), length.out = n), , drop = FALSE]
  mu <- (a$mu + b$mu) / (1 + a$mu * b$mu)
  nu <- (a$nu * b$nu) / (1 + (1 - a$nu) * (1 - b$nu))
  # fix 0/0: when nu_a=nu_b=1, denominator=1, fine; guard NaN
  nu[!is.finite(nu)] <- 1
  ifv(pmin(mu, 1), pmin(nu, 1))
}

#' @rdname if_add
#' @export
if_einstein_mul <- function(a, b) {
  a <- as_ifv(a); b <- as_ifv(b)
  n <- max(nrow(a), nrow(b))
  a <- a[rep(seq_len(nrow(a)), length.out = n), , drop = FALSE]
  b <- b[rep(seq_len(nrow(b)), length.out = n), , drop = FALSE]
  mu <- (a$mu * b$mu) / (1 + (1 - a$mu) * (1 - b$mu))
  nu <- (a$nu + b$nu) / (1 + a$nu * b$nu)
  mu[!is.finite(mu)] <- 0
  ifv(pmin(mu, 1), pmin(nu, 1))
}

#' @rdname if_add
#' @export
if_einstein_scalar_mul <- function(lambda, a) {
  a <- as_ifv(a)
  lambda <- as.numeric(lambda)
  n <- max(nrow(a), length(lambda))
  a <- a[rep(seq_len(nrow(a)), length.out = n), , drop = FALSE]
  lambda <- rep(lambda, length.out = n)
  if (any(lambda < 0)) stop("`lambda` must be non-negative.")
  mu <- ((1 + a$mu)^lambda - (1 - a$mu)^lambda) /
        ((1 + a$mu)^lambda + (1 - a$mu)^lambda)
  nu <- (2 * a$nu^lambda) / ((2 - a$nu)^lambda + a$nu^lambda)
  mu[!is.finite(mu)] <- 0; nu[!is.finite(nu)] <- 0
  ifv(mu, nu)
}

#' @rdname if_add
#' @export
if_einstein_pow <- function(a, lambda) {
  a <- as_ifv(a)
  lambda <- as.numeric(lambda)
  n <- max(nrow(a), length(lambda))
  a <- a[rep(seq_len(nrow(a)), length.out = n), , drop = FALSE]
  lambda <- rep(lambda, length.out = n)
  if (any(lambda < 0)) stop("`lambda` must be non-negative.")
  mu <- (2 * a$mu^lambda) / ((2 - a$mu)^lambda + a$mu^lambda)
  nu <- ((1 + a$nu)^lambda - (1 - a$nu)^lambda) /
        ((1 + a$nu)^lambda + (1 - a$nu)^lambda)
  mu[!is.finite(mu)] <- 0; nu[!is.finite(nu)] <- 0
  ifv(mu, nu)
}
