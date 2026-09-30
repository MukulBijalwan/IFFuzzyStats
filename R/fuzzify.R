#' Fuzzify numeric data into intuitionistic fuzzy values
#'
#' Converts a numeric vector into IFVs by normalising into \eqn{[0,1]} to
#' get membership, then deriving non-membership as
#' \eqn{\nu = (1-\mu)\times \nu\_ratio} so hesitation absorbs the remainder.
#' Useful for feeding measurements into the IF toolkit.
#'
#' @param x numeric vector.
#' @param mu_range range used to scale \code{x} into membership
#'   (default \code{range(x)}).
#' @param nu_ratio share of the non-membership part in \eqn{(1-\mu)}
#'   (scalar or vector in \eqn{[0,1]}, default 0.6).
#' @return \code{ifv} vector.
#' @examples
#' if_fuzzify(1:5)
#' if_fuzzify(c(10, 20, 30), nu_ratio = 0.5)
#' @export
if_fuzzify <- function(x, mu_range = range(x, na.rm = TRUE), nu_ratio = 0.6) {
  x <- as.numeric(x)
  if (any(!is.finite(x))) stop("`x` must be finite.")
  lo <- mu_range[1L]; hi <- mu_range[2L]
  if (hi <= lo) stop("`mu_range` must have hi > lo.")
  mu <- (x - lo) / (hi - lo)
  nu_ratio <- rep(as.numeric(nu_ratio), length.out = length(x))
  if (any(nu_ratio < 0 | nu_ratio > 1)) stop("`nu_ratio` must be in [0,1].")
  nu <- (1 - mu) * nu_ratio
  ifv(mu, nu)
}

#' Defuzzify IFVs to crisp scores
#'
#' @param x object coercible to \code{ifv}.
#' @param method \code{"score"} (\eqn{\mu-\nu}), \code{"accuracy"}
#'   (\eqn{\mu+\nu}), \code{"expected"} (\eqn{\mu + \pi/2}), or
#'   \code{"mu"} (membership only).
#' @return numeric vector.
#' @examples
#' if_defuzzify(ifv(c(0.6, 0.5), c(0.3, 0.2)))
#' @export
if_defuzzify <- function(x, method = c("score", "expected", "accuracy", "mu")) {
  method <- match.arg(method)
  a <- as_ifv(x)
  switch(method,
    score = a$mu - a$nu,
    expected = a$mu + (1 - a$mu - a$nu) / 2,
    accuracy = a$mu + a$nu,
    mu = a$mu)
}
