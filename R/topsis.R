#' Intuitionistic fuzzy TOPSIS and decision helpers
#'
#' IF-TOPSIS after Boran et al. (2009): aggregate each alternative's
#' criterion IFVs with weights \code{w} (via \code{ifwa} by default), then
#' measure separation from the positive ideal \eqn{A^+=(1,0)} and negative
#' ideal \eqn{A^-=(0,1)} using normalised Euclidean IF distance. Closeness
#' \eqn{C_i = D^-_i/(D^+_i+D^-_i)}; larger is better.
#'
#' @param mat \code{ifv} of length \code{nalt*ncrit} in row-major order
#'   (alternative 1's criteria first), or an \code{nalt x ncrit} list-matrix.
#' @param nalt,ncrit dimensions (guessed from square if missing is impossible,
#'   so both are required).
#' @param w criterion weights (length \code{ncrit}).
#' @param dist distance method passed to \code{\link{if_dist}}
#'   (default \code{"euclidean"}).
#' @param agg aggregation function (default \code{\link{ifwa}}).
#' @return \code{data.frame} with columns \code{alt}, \code{mu}, \code{nu},
#'   \code{score}, \code{d_plus}, \code{d_minus}, \code{closeness},
#'   \code{rank}, ordered by rank.
#' @references
#' Boran, F. E., Genc, S., Kurt, M. & Akay, D. (2009). A multi-criteria
#' intuitionistic fuzzy group decision making for supplier selection with
#' TOPSIS method. \emph{Expert Systems with Applications}, 36(8),
#' 11363-11368.
#' @examples
#' m <- ifv(c(0.6, 0.5, 0.7, 0.4, 0.5, 0.6), c(0.2, 0.3, 0.1, 0.4, 0.3, 0.2))
#' if_topsis(m, nalt = 3, ncrit = 2, w = c(0.6, 0.4))
#' @export
if_topsis <- function(mat, nalt, ncrit, w, dist = "euclidean", agg = ifwa) {
  a <- as_ifv(mat)
  if (missing(nalt) || missing(ncrit)) stop("`nalt` and `ncrit` are required.")
  if (nrow(a) != nalt * ncrit) stop("length(mat) must equal nalt*ncrit.")
  w <- .norm_w(w, ncrit)
  agg_vals <- if_aggregate_matrix(a, w = w, ncrit = ncrit, fun = agg)
  aplus <- ifv(1, 0); aminus <- ifv(0, 1)
  d_plus <- vapply(seq_len(nalt), function(i)
    if_dist(agg_vals[i, , drop = FALSE], aplus, method = dist), numeric(1))
  d_minus <- vapply(seq_len(nalt), function(i)
    if_dist(agg_vals[i, , drop = FALSE], aminus, method = dist), numeric(1))
  closeness <- d_minus / (d_plus + d_minus)
  out <- data.frame(
    alt = seq_len(nalt), mu = agg_vals$mu, nu = agg_vals$nu,
    score = agg_vals$mu - agg_vals$nu,
    d_plus = d_plus, d_minus = d_minus, closeness = closeness
  )
  out$rank <- rank(-out$closeness, ties.method = "min")
  out[order(out$rank), , drop = FALSE]
}

#' Rank alternatives from aggregated IFVs
#'
#' @param x object coercible to \code{ifv} (one value per alternative).
#' @param labels optional alternative labels.
#' @return ranked \code{data.frame}.
#' @examples
#' if_rank_df(ifv(c(0.6, 0.5, 0.7), c(0.3, 0.2, 0.1)))
#' @export
if_rank_df <- function(x, labels = NULL) {
  a <- as_ifv(x)
  n <- nrow(a)
  lab <- if (is.null(labels)) paste0("A", seq_len(n)) else rep(labels, length.out = n)
  out <- data.frame(
    alt = lab, mu = a$mu, nu = a$nu, pi = 1 - a$mu - a$nu,
    score = a$mu - a$nu, accuracy = a$mu + a$nu
  )
  out$rank <- if_rank(a)
  out[order(out$rank), , drop = FALSE]
}
