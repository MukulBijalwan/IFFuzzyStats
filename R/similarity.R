#' Similarity between intuitionistic fuzzy sets
#'
#' Similarities in \eqn{[0,1]} (larger = more similar), complementing
#' \code{\link{if_dist}}.
#'
#' \itemize{
#'   \item \code{"chen"}: \eqn{1 - D_H} (one minus normalised Hamming).
#'   \item \code{"hong_kim"}: \eqn{1 - D_E} (one minus normalised Euclidean).
#'   \item \code{"ye_cosine"} (Ye, 2011): weighted cosine
#'     \eqn{S_C=\sum w_i
#'       \frac{\mu_A\mu_B+\nu_A\nu_B+\pi_A\pi_B}
#'            {\sqrt{\mu_A^2+\nu_A^2+\pi_A^2}\sqrt{\mu_B^2+\nu_B^2+\pi_B^2}}}.
#'   \item \code{"ye_weighted"} (Ye, 2010 correlation):
#'     \eqn{S_W=\frac{C(A,B)}{\max(C(A,A),C(B,B))}} with
#'     \eqn{C(A,B)=\sum w_i(\mu_A\mu_B+\nu_A\nu_B+\pi_A\pi_B)}.
#'   \item \code{"deng"} / \code{"liang_shi"}: Jaccard-style
#'     \eqn{S_J=\sum w_i
#'       \frac{\mu_A\mu_B+\nu_A\nu_B+\pi_A\pi_B}
#'            {(\mu_A^2+\nu_A^2+\pi_A^2)+(\mu_B^2+\nu_B^2+\pi_B^2)
#'             -(\mu_A\mu_B+\nu_A\nu_B+\pi_A\pi_B)}}.
#'   \item \code{"dice"}: Dice-style
#'     \eqn{S_D=\sum w_i
#'       \frac{2(\mu_A\mu_B+\nu_A\nu_B+\pi_A\pi_B)}
#'            {(\mu_A^2+\nu_A^2+\pi_A^2)+(\mu_B^2+\nu_B^2+\pi_B^2)}}.
#' }
#'
#' @param a,b objects coercible to \code{ifv}.
#' @param method similarity name (partial matching allowed).
#' @param w optional element weights.
#' @return numeric scalar similarity in \eqn{[0,1]}.
#' @references
#' Ye, J. (2011). Cosine similarity measures for intuitionistic fuzzy sets
#' and their applications. \emph{Mathematical and Computer Modelling},
#' 53(1-2), 91-97.
#'
#' Ye, J. (2010). Fuzzy decision-making method based on the weighted
#' correlation coefficient under intuitionistic fuzzy environment.
#' \emph{European J. Operational Research}, 205(1), 202-204.
#'
#' Liang, Z. & Shi, P. (2003). Similarity measures on intuitionistic
#' fuzzy sets. \emph{Pattern Recognition Letters}, 24(15), 2687-2693.
#' @examples
#' a <- ifv(c(0.6, 0.5), c(0.3, 0.2)); b <- ifv(c(0.5, 0.4), c(0.3, 0.3))
#' if_sim(a, b, "chen"); if_sim(a, b, "ye_cosine"); if_sim(a, b, "dice")
#' @export
if_sim <- function(a, b,
                   method = c("chen", "hong_kim", "ye_cosine",
                              "ye_weighted", "deng", "liang_shi", "dice"),
                   w = NULL) {
  method <- match.arg(method)
  p <- .pair_ifv(a, b)
  A <- p$a; B <- p$b
  n <- nrow(A)
  wt <- if (is.null(w)) rep(1 / n, n) else .norm_w(w, n)

  piA <- 1 - A$mu - A$nu; piB <- 1 - B$mu - B$nu
  dot <- A$mu * B$mu + A$nu * B$nu + piA * piB
  qA <- A$mu^2 + A$nu^2 + piA^2
  qB <- B$mu^2 + B$nu^2 + piB^2

  s <- switch(method,
    chen = 1 - if_dist(A, B, "hamming", w = wt),
    hong_kim = 1 - if_dist(A, B, "euclidean", w = wt),
    ye_cosine = {
      den <- sqrt(qA) * sqrt(qB)
      t <- dot / den
      t[den == 0] <- 1
      sum(wt * t)
    },
    ye_weighted = {
      cab <- sum(wt * dot); caa <- sum(wt * qA); cbb <- sum(wt * qB)
      m <- max(caa, cbb)
      if (m == 0) 1 else cab / m
    },
    deng = , liang_shi = {
      den <- qA + qB - dot
      t <- dot / den
      t[den == 0] <- 1
      sum(wt * t)
    },
    dice = {
      den <- qA + qB
      t <- 2 * dot / den
      t[den == 0] <- 1
      sum(wt * t)
    }
  )
  as.numeric(s)
}
