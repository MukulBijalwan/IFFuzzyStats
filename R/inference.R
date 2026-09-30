#' Intuitionistic fuzzy relations: composition and inference
#'
#' Max-min / max-product composition of IF relations and the
#' compositional rule of inference (Zadeh, 1973, extended to IFS).
#'
#' An IF relation on \eqn{X \times Y} is a matrix of IFVs. Composition
#' \eqn{R \circ S} on \eqn{X \times Z} is:
#' \deqn{(R \circ S)(x,z) =
#'   \bigoplus_{y} (R(x,y) \otimes S(y,z))}
#' where \eqn{\oplus,\otimes} are max-min (default) or max-product.
#' Membership composes with t-conorm over t-norm, non-membership with the
#' dual operation (min over max for max-min; product-sum dual for max-product).
#'
#' @param R,S matrices (or data.frames) of IFVs stored as \code{ifv} objects
#'   with \code{nrow = nx*ny} in row-major order, or numeric matrices of
#'   membership values with matching non-membership matrices
#'   \code{R_nu}, \code{S_nu}. Simpler: pass \code{ifv} vectors plus
#'   dimensions.
#' @param nx,ny,nz dimensions.
#' @param op \code{"maxmin"} or \code{"maxprod"}.
#' @return An \code{ifv} vector of length \code{nx*nz} (row-major).
#' @examples
#' R <- ifv(c(0.6, 0.3, 0.8, 0.2), c(0.2, 0.5, 0.1, 0.6))  # 2x2
#' S <- ifv(c(0.5, 0.7, 0.4, 0.9), c(0.3, 0.1, 0.4, 0.0))  # 2x2
#' if_compose(R, S, nx = 2, ny = 2, nz = 2)
#' @export
if_compose <- function(R, S, nx, ny, nz, op = c("maxmin", "maxprod")) {
  op <- match.arg(op)
  R <- as_ifv(R); S <- as_ifv(S)
  if (nrow(R) != nx * ny) stop("length(R) must equal nx*ny.")
  if (nrow(S) != ny * nz) stop("length(S) must equal ny*nz.")
  Rm <- matrix(seq_len(nx * ny), nrow = nx, ncol = ny, byrow = TRUE)
  Sm <- matrix(seq_len(ny * nz), nrow = ny, ncol = nz, byrow = TRUE)
  out_mu <- numeric(nx * nz); out_nu <- numeric(nx * nz)
  k <- 1L
  for (i in seq_len(nx)) for (j in seq_len(nz)) {
    mu_r <- R$mu[Rm[i, ]]; nu_r <- R$nu[Rm[i, ]]
    mu_s <- S$mu[Sm[, j]]; nu_s <- S$nu[Sm[, j]]
    if (op == "maxmin") {
      out_mu[k] <- max(pmin(mu_r, mu_s))
      out_nu[k] <- min(pmax(nu_r, nu_s))
    } else {
      out_mu[k] <- max(mu_r * mu_s)
      out_nu[k] <- min(nu_r + nu_s - nu_r * nu_s)
    }
    k <- k + 1L
  }
  ifv(out_mu, out_nu)
}

#' Compositional rule of inference for IFVs
#'
#' Given a fact (IF set on X, length \code{nx}) and an IF implication
#' relation on \eqn{X \times Y} (\code{nx*ny} values row-major), infer the
#' consequent on Y via max-min composition.
#'
#' @param fact \code{ifv} of length \code{nx}.
#' @param relation \code{ifv} of length \code{nx*ny}.
#' @param nx,ny dimensions.
#' @param op composition operator.
#' @return \code{ifv} of length \code{ny}.
#' @examples
#' fact <- ifv(c(0.8, 0.2), c(0.1, 0.7))
#' rel <- ifv(c(0.6, 0.3, 0.8, 0.2), c(0.2, 0.5, 0.1, 0.6))
#' if_infer(fact, rel, nx = 2, ny = 2)
#' @export
if_infer <- function(fact, relation, nx, ny, op = c("maxmin", "maxprod")) {
  op <- match.arg(op)
  fact <- as_ifv(fact); relation <- as_ifv(relation)
  if (nrow(fact) != nx) stop("length(fact) must equal nx.")
  if_compose(fact, relation, nx = 1L, ny = nx, nz = ny, op = op)
}

#' Mamdani-style IF rule block evaluation
#'
#' Evaluate \code{nrules} rules, each mapping an \code{nx}-vector input fact
#' through its own \code{nx*ny} relation, then aggregate consequents by
#' element-wise max (mu) / min (nu) union.
#'
#' @param fact \code{ifv} length \code{nx}.
#' @param relations list of \code{nrules} \code{ifv} relations (each length
#'   \code{nx*ny}).
#' @param nx,ny dimensions.
#' @param op composition operator.
#' @return \code{ifv} length \code{ny}.
#' @export
if_rule_block <- function(fact, relations, nx, ny, op = c("maxmin", "maxprod")) {
  op <- match.arg(op)
  cons <- lapply(relations, function(r) if_infer(fact, r, nx, ny, op = op))
  mu <- sapply(cons, `[[`, "mu"); nu <- sapply(cons, `[[`, "nu")
  if (is.null(dim(mu))) { mu <- matrix(mu, nrow = ny); nu <- matrix(nu, nrow = ny) }
  ifv(apply(mu, 1L, max), apply(nu, 1L, min))
}
