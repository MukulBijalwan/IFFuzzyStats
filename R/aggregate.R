#' Intuitionistic fuzzy aggregation operators
#'
#' Weighted / ordered / hybrid aggregation of IFVs after Xu (2007) and
#' Xu & Yager (2006).
#'
#' For IFVs \eqn{a_j=(\mu_j,\nu_j)} with weights \eqn{w_j}:
#' \deqn{IFWA = \left(1-\prod(1-\mu_j)^{w_j},\ \prod \nu_j^{w_j}\right)}
#' \deqn{IFWG = \left(\prod \mu_j^{w_j},\ 1-\prod(1-\nu_j)^{w_j}\right)}
#' Ordered variants (IFOWA/IFOWG) first reorder \eqn{a_j} by score
#' \eqn{S=\mu-\nu} (descending) and then apply positional weights.
#' Hybrid variants (IFHA/IFHG) weight each \eqn{a_j} by \eqn{n w_j}
#' (via scalar-mul / power) and then apply ordered weighting.
#'
#' Terms \eqn{0^0} arising from zero weights are treated as 1, and
#' \eqn{0^0}-type products skip zero-weight factors, so unused inputs do
#' not affect the result.
#'
#' @param x object coercible to \code{ifv} (vector of values to fuse).
#' @param w numeric weight vector (non-negative, sums to 1; normalised if not).
#' @param order_w positional weights for ordered/hybrid operators.
#' @return A single-row \code{ifv}.
#' @references
#' Xu, Z. (2007). Intuitionistic fuzzy aggregation operators.
#' \emph{IEEE Trans. Fuzzy Systems}, 15(6), 1179-1187.
#'
#' Xu, Z. & Yager, R. R. (2006). Some geometric aggregation operators based
#' on intuitionistic fuzzy sets. \emph{Int. J. General Systems}, 35(4), 417-433.
#' @examples
#' x <- ifv(c(0.6, 0.5, 0.7), c(0.3, 0.2, 0.1))
#' ifwa(x, c(0.5, 0.3, 0.2))
#' ifwg(x, c(0.5, 0.3, 0.2))
#' ifowa(x, c(0.5, 0.3, 0.2))
#' @export
ifwa <- function(x, w) {
  a <- as_ifv(x); w <- .norm_w(w, nrow(a))
  mu <- 1 - .wprod(1 - a$mu, w)
  nu <- .wprod(a$nu, w)
  ifv(mu, nu)
}

#' @rdname ifwa
#' @export
ifwg <- function(x, w) {
  a <- as_ifv(x); w <- .norm_w(w, nrow(a))
  ifv(.wprod(a$mu, w), 1 - .wprod(1 - a$nu, w))
}

#' @rdname ifwa
#' @export
ifowa <- function(x, order_w) {
  a <- as_ifv(x); ow <- .norm_w(order_w, nrow(a))
  idx <- order(if_score(a), decreasing = TRUE)
  b <- a[idx, , drop = FALSE]
  ifv(1 - .wprod(1 - b$mu, ow), .wprod(b$nu, ow))
}

#' @rdname ifwa
#' @export
ifowg <- function(x, order_w) {
  a <- as_ifv(x); ow <- .norm_w(order_w, nrow(a))
  idx <- order(if_score(a), decreasing = TRUE)
  b <- a[idx, , drop = FALSE]
  ifv(.wprod(b$mu, ow), 1 - .wprod(1 - b$nu, ow))
}

#' @rdname ifwa
#' @export
ifha <- function(x, w, order_w) {
  a <- as_ifv(x)
  n <- nrow(a)
  w <- .norm_w(w, n); ow <- .norm_w(order_w, n)
  adot <- if_scalar_mul(n * w, a)
  idx <- order(if_score(adot), decreasing = TRUE)
  b <- adot[idx, , drop = FALSE]
  ifv(1 - .wprod(1 - b$mu, ow), .wprod(b$nu, ow))
}

#' @rdname ifwa
#' @export
ifhg <- function(x, w, order_w) {
  a <- as_ifv(x)
  n <- nrow(a)
  w <- .norm_w(w, n); ow <- .norm_w(order_w, n)
  adot <- if_pow(a, n * w)
  idx <- order(if_score(adot), decreasing = TRUE)
  b <- adot[idx, , drop = FALSE]
  ifv(.wprod(b$mu, ow), 1 - .wprod(1 - b$nu, ow))
}

#' @rdname ifwa
#' @export
if_einstein_wa <- function(x, w) {
  a <- as_ifv(x); w <- .norm_w(w, nrow(a))
  mu_num <- .wprod(1 + a$mu, w) - .wprod(1 - a$mu, w)
  mu_den <- .wprod(1 + a$mu, w) + .wprod(1 - a$mu, w)
  nu_num <- 2 * .wprod(a$nu, w)
  nu_den <- .wprod(2 - a$nu, w) + .wprod(a$nu, w)
  ifv(mu_num / mu_den, nu_num / nu_den)
}

#' @rdname ifwa
#' @export
if_einstein_wg <- function(x, w) {
  a <- as_ifv(x); w <- .norm_w(w, nrow(a))
  mu_num <- 2 * .wprod(a$mu, w)
  mu_den <- .wprod(2 - a$mu, w) + .wprod(a$mu, w)
  nu_num <- .wprod(1 + a$nu, w) - .wprod(1 - a$nu, w)
  nu_den <- .wprod(1 + a$nu, w) + .wprod(1 - a$nu, w)
  ifv(mu_num / mu_den, nu_num / nu_den)
}

#' Aggregate rows of an IF decision matrix
#'
#' @param mat \code{ifv} with one row per alternative-criterion cell, or a
#'   list of per-alternative \code{ifv} vectors; see examples.
#' @param w criterion weights.
#' @param ncrit number of criteria (columns); rows of \code{mat} are
#'   \code{nalt} blocks of \code{ncrit} in row-major order.
#' @param fun aggregation function, e.g. \code{ifwa}, \code{ifwg}.
#' @return \code{ifv} vector (one fused value per alternative).
#' @examples
#' m <- ifv(c(0.6, 0.5, 0.7, 0.4), c(0.2, 0.3, 0.1, 0.4))
#' # 2 alternatives x 2 criteria stored row-wise:
#' if_aggregate_matrix(m, w = c(0.6, 0.4), ncrit = 2, fun = ifwa)
#' @export
if_aggregate_matrix <- function(mat, w, ncrit, fun = ifwa) {
  a <- as_ifv(mat)
  if (nrow(a) %% ncrit != 0L) stop("nrow(mat) must be a multiple of `ncrit`.")
  nalt <- nrow(a) / ncrit
  out_mu <- numeric(nalt); out_nu <- numeric(nalt)
  for (i in seq_len(nalt)) {
    idx <- ((i - 1L) * ncrit + 1L):(i * ncrit)
    r <- fun(a[idx, , drop = FALSE], w)
    out_mu[i] <- r$mu; out_nu[i] <- r$nu
  }
  ifv(out_mu, out_nu)
}

