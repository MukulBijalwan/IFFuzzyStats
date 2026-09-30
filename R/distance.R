#' Distances between intuitionistic fuzzy sets
#'
#' For sets \eqn{A,B} of equal cardinality \eqn{n} with elements
#' \eqn{(\mu^A_i,\nu^A_i,\pi^A_i)}, \eqn{(\mu^B_i,\nu^B_i,\pi^B_i)}:
#'
#' \itemize{
#'   \item \code{"hamming"} (normalised Szmidt-Kacprzyk):
#'     \eqn{\frac{1}{2n}\sum(|\Delta\mu|+|\Delta\nu|+|\Delta\pi|)}.
#'   \item \code{"euclidean"} (normalised):
#'     \eqn{\sqrt{\frac{1}{2n}\sum((\Delta\mu)^2+(\Delta\nu)^2+(\Delta\pi)^2)}}.
#'   \item \code{"hausdorff"} (normalised Hamming-Hausdorff):
#'     \eqn{\frac{1}{n}\sum\max(|\Delta\mu|,|\Delta\nu|,|\Delta\pi|)}.
#'   \item \code{"ye"} (Ye's modified Hamming-type distance):
#'     \eqn{\sum w_i
#'       \frac{|\Delta\mu_i|+|\Delta\nu_i|}
#'            {|\Delta\mu_i|+|\Delta\nu_i|
#'             +\min(\mu^A_i,\mu^B_i)+\min(\nu^A_i,\nu^B_i)
#'             +\min(\pi^A_i,\pi^B_i)+1}}.
#'   \item \code{"ye_cosine"}: \eqn{1 - S_C} where \eqn{S_C} is Ye's cosine
#'     similarity (see \code{\link{if_sim}}).
#'   \item \code{"park"}: normalised Hamming including hesitation
#'     (same numeric form as \code{"hamming"} here).
#' }
#'
#' Weighted versions: pass \code{w} (sums to 1) to weight elements:
#' \eqn{D_w=\sum w_i d_i} with per-element \eqn{d_i}.
#'
#' @param a,b objects coercible to \code{ifv} of common length.
#' @param method distance name (partial matching allowed).
#' @param w optional element weights.
#' @return numeric scalar distance.
#' @references
#' Szmidt, E. & Kacprzyk, J. (2000). Distances between intuitionistic
#' fuzzy sets. \emph{Fuzzy Sets and Systems}, 114(3), 505-518.
#'
#' Ye, J. (2012). Multicriteria decision-making method using the
#' Dice similarity measure based on the reduct intuitionistic fuzzy sets
#' of interval-valued intuitionistic fuzzy sets.
#' \emph{Applied Mathematical Modelling}, 36(9), 4466-4472.
#' @examples
#' a <- ifv(c(0.6, 0.5), c(0.3, 0.2)); b <- ifv(c(0.5, 0.4), c(0.3, 0.3))
#' if_dist(a, b, "hamming"); if_dist(a, b, "euclidean"); if_dist(a, b, "hausdorff")
#' @export
if_dist <- function(a, b,
                    method = c("hamming", "euclidean", "hausdorff",
                               "ye", "ye_cosine", "park"),
                    w = NULL) {
  method <- match.arg(method)
  p <- .pair_ifv(a, b)
  A <- p$a; B <- p$b
  n <- nrow(A)
  dmu <- abs(A$mu - B$mu); dnu <- abs(A$nu - B$nu)
  dpi <- abs((1 - A$mu - A$nu) - (1 - B$mu - B$nu))
  wt <- if (is.null(w)) rep(1 / n, n) else .norm_w(w, n)

  d <- switch(method,
    hamming   = sum(wt * (dmu + dnu + dpi) / 2),
    euclidean = sqrt(sum(wt * (dmu^2 + dnu^2 + dpi^2) / 2)),
    hausdorff = sum(wt * pmax(dmu, dnu, dpi)),
    park      = sum(wt * (dmu + dnu + dpi) / 2),
    ye        = {
      num <- dmu + dnu
      den <- dmu + dnu + pmin(A$mu, B$mu) + pmin(A$nu, B$nu) +
             pmin(1 - A$mu - A$nu, 1 - B$mu - B$nu) + 1
      sum(wt * num / den)
    },
    ye_cosine = 1 - .ye_cosine_sim(A, B, wt)
  )
  as.numeric(d)
}

#' Cosine core of Ye similarity (internal)
#' @keywords internal
.ye_cosine_sim <- function(A, B, wt) {
  num <- A$mu * B$mu + A$nu * B$nu +
         (1 - A$mu - A$nu) * (1 - B$mu - B$nu)
  den <- sqrt(A$mu^2 + A$nu^2 + (1 - A$mu - A$nu)^2) *
         sqrt(B$mu^2 + B$nu^2 + (1 - B$mu - B$nu)^2)
  s <- num / den
  s[den == 0] <- 1  # both degenerate -> identical
  sum(wt * s)
}

