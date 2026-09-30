#' Normalise a weight vector
#' @param w numeric weights.
#' @param n expected length.
#' @return normalised weights summing to 1.
#' @keywords internal
.norm_w <- function(w, n) {
  w <- as.numeric(w)
  if (length(w) != n) stop(sprintf("weight vector must have length %d.", n))
  if (any(!is.finite(w)) || any(w < 0)) stop("weights must be finite, >= 0.")
  s <- sum(w)
  if (s <= 0) stop("weights must sum to something positive.")
  w / s
}

#' Clip probabilities away from 0/1 for logs
#' @keywords internal
.clip01 <- function(p, eps = 1e-12) pmin(pmax(p, eps), 1 - eps)

#' Coerce two IF objects to a common matrix length
#' @keywords internal
.pair_ifv <- function(a, b) {
  a <- as_ifv(a); b <- as_ifv(b)
  n <- max(nrow(a), nrow(b))
  list(
    a = a[rep(seq_len(nrow(a)), length.out = n), , drop = FALSE],
    b = b[rep(seq_len(nrow(b)), length.out = n), , drop = FALSE]
  )
}

#' Weighted product prod(x^w) with 0^0 := 1 semantics
#'
#' Avoids NaN from `0^0` when a zero base meets a zero weight; such
#' factors contribute 1 (i.e. are skipped).
#' @keywords internal
.wprod <- function(x, w) {
  x <- as.numeric(x); w <- as.numeric(w)
  p <- x^w
  # R gives 0^0 = 1 already, but explicit guard for safety / negatives
  p[w == 0] <- 1
  p[is.nan(p) & w == 0] <- 1
  prod(p)
}
