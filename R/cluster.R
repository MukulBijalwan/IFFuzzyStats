#' Intuitionistic fuzzy c-means (IFCM) clustering
#'
#' Clusters \code{ifv} data with intuitionistic fuzzy memberships.
#' Distance between an observation \eqn{a} and centre \eqn{v} is the
#' normalised Euclidean IF distance (Szmidt-Kacprzyk). An optional
#' hesitation weight \code{lambda} inflates distances by
#' \eqn{\exp(\lambda \cdot \bar\pi)} where \eqn{\bar\pi} is the mean
#' hesitation of the pair, emphasising uncertain points when
#' \code{lambda > 0} (Xu et al. style modification).
#'
#' @param x object coercible to \code{ifv} (n observations).
#' @param k number of clusters.
#' @param m fuzzifier > 1 (default 2).
#' @param lambda hesitation penalty >= 0 (default 0 = plain IFCM).
#' @param maxit maximum iterations.
#' @param tol convergence tolerance on membership change.
#' @param nstart number of random starts (best kept).
#' @param seed random seed.
#' @return A list with \code{centers} (\code{ifv}, k rows),
#'   \code{membership} (n x k matrix), \code{cluster} (hard labels),
#'   \code{within_ss} (sum of weighted squared distances),
#'   \code{iter}, \code{converged}.
#' @references
#' Xu, Z., Chen, J. & Wu, J. (2008). Clustering algorithm for
#' intuitionistic fuzzy sets. \emph{Information Sciences}, 178(19),
#' 3775-3790.
#' @examples
#' set.seed(1)
#' mu <- runif(30, 0.3, 0.7); nu <- runif(30, 0.05, 0.25)
#' x <- ifv(mu, nu)
#' r <- ifcmeans(x, k = 2, nstart = 2)
#' r$cluster; r$centers
#' @export
ifcmeans <- function(x, k, m = 2, lambda = 0, maxit = 100,
                     tol = 1e-5, nstart = 1L, seed = NULL) {
  a <- as_ifv(x)
  n <- nrow(a)
  if (k < 2L || k > n) stop("`k` must be in [2, n].")
  if (m <= 1) stop("`m` must exceed 1.")
  if (!is.null(seed)) set.seed(seed)
  best <- NULL; best_ss <- Inf
  for (s in seq_len(nstart)) {
    init <- sample.int(n, k)
    V <- a[init, , drop = FALSE]
    U <- matrix(0, n, k)
    converged <- FALSE; iter <- 0L
    for (it in seq_len(maxit)) {
      iter <- it
      D2 <- .if_dist2_mat(a, V, lambda)
      D2[D2 < 1e-300] <- 1e-300
      # standard FCM update: u_ik = 1/sum_j (d_ik/d_ij)^{2/(m-1)}
      pw <- 2 / (m - 1)
      Unew <- matrix(0, n, k)
      for (i in seq_len(n)) {
        di <- D2[i, ]
        if (any(di <= 1e-290)) {
          z <- which.min(di); Unew[i, ] <- 0; Unew[i, z] <- 1
        } else {
          inv <- (1 / di)^(pw / 2)
          Unew[i, ] <- inv / sum(inv)
        }
      }
      if (max(abs(Unew - U)) < tol) { U <- Unew; converged <- TRUE; break }
      U <- Unew
      # update centres: IFWA of points weighted by u_ik^m
      Vm <- numeric(k); Vn <- numeric(k)
      for (j in seq_len(k)) {
        wj <- U[, j]^m
        if (sum(wj) <= 0) {
          pick <- sample.int(n, 1L)
          Vm[j] <- a$mu[pick]; Vn[j] <- a$nu[pick]
        } else {
          wj <- wj / sum(wj)
          Vm[j] <- 1 - prod((1 - a$mu)^wj)
          Vn[j] <- prod(a$nu^wj)
        }
      }
      V <- ifv(Vm, Vn)
    }
    D2 <- .if_dist2_mat(a, V, lambda)
    ss <- sum((U^m) * D2)
    if (ss < best_ss) {
      best_ss <- ss
      best <- list(centers = V, membership = U,
                   cluster = max.col(U, ties.method = "first"),
                   within_ss = ss, iter = iter, converged = converged)
    }
  }
  colnames(best$membership) <- paste0("C", seq_len(k))
  best
}

#' Pairwise squared IF distances with hesitation penalty (internal)
#' @keywords internal
.if_dist2_mat <- function(A, V, lambda = 0) {
  n <- nrow(A); k <- nrow(V)
  D2 <- matrix(0, n, k)
  piA <- 1 - A$mu - A$nu
  piV <- 1 - V$mu - V$nu
  for (j in seq_len(k)) {
    d2 <- ((A$mu - V$mu[j])^2 + (A$nu - V$nu[j])^2 +
           (piA - piV[j])^2) / 2
    if (lambda > 0) d2 <- d2 * exp(lambda * (piA + piV[j]) / 2)
    D2[, j] <- d2
  }
  D2
}

#' Intuitionistic fuzzy k-means (hard labels, IF centroids)
#'
#' Lloyd-style k-means with normalised Euclidean IF distance and IFWA
#' centroid updates.
#'
#' @param x object coercible to \code{ifv}.
#' @param k number of clusters.
#' @param maxit maximum iterations.
#' @param nstart random starts.
#' @param seed seed.
#' @return list with \code{centers} (\code{ifv}), \code{cluster},
#'   \code{within_ss}, \code{iter}, \code{converged}.
#' @examples
#' set.seed(2)
#' mu <- runif(20, 0.2, 0.7); nu <- runif(20, 0.0, 0.25)
#' x <- ifv(mu, nu)
#' ifkmeans(x, k = 2, nstart = 2)
#' @export
ifkmeans <- function(x, k, maxit = 50, nstart = 5L, seed = NULL) {
  a <- as_ifv(x)
  n <- nrow(a)
  if (k < 2L || k > n) stop("`k` must be in [2, n].")
  if (!is.null(seed)) set.seed(seed)
  best <- NULL; best_ss <- Inf
  for (s in seq_len(nstart)) {
    V <- a[sample.int(n, k), , drop = FALSE]
    cl <- integer(n); converged <- FALSE; iter <- 0L
    for (it in seq_len(maxit)) {
      iter <- it
      D2 <- .if_dist2_mat(a, V, 0)
      newcl <- max.col(-D2, ties.method = "first")
      if (all(newcl == cl) && it > 1L) { converged <- TRUE; break }
      cl <- newcl
      for (j in seq_len(k)) {
        idx <- which(cl == j)
        if (length(idx) == 0L) {
          V$mu[j] <- a$mu[sample.int(n, 1L)]; V$nu[j] <- a$nu[sample.int(n, 1L)]
        } else {
          wj <- rep(1 / length(idx), length(idx))
          V$mu[j] <- 1 - prod((1 - a$mu[idx])^wj)
          V$nu[j] <- prod(a$nu[idx]^wj)
        }
      }
      V <- ifv(V$mu, V$nu)
    }
    D2 <- .if_dist2_mat(a, V, 0)
    ss <- sum(D2[cbind(seq_len(n), cl)])
    if (ss < best_ss) {
      best_ss <- ss
      best <- list(centers = V, cluster = cl, within_ss = ss,
                   iter = iter, converged = converged)
    }
  }
  best
}
