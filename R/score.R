#' Score, accuracy and ranking of IFVs
#'
#' Score \eqn{S=\mu-\nu \in [-1,1]}, accuracy \eqn{H=\mu+\nu \in [0,1]}
#' (Chen & Tan, 1994; Hong & Choi, 2000). Ranking is lexicographic:
#' larger score first, ties broken by larger accuracy, then larger
#' hesitation-handling via \code{tie = "pi_min"} (smaller hesitation wins).
#'
#' @param x object coercible to \code{ifv}.
#' @return \code{if_score()}/\code{if_accuracy()}/\code{if_hesitation()}
#'   numeric vectors; \code{if_rank()} integer ranks (1 = best);
#'   \code{if_order()} ordering permutation (best first);
#'   \code{if_best()} best element(s).
#' @references
#' Chen, S. M. & Tan, J. M. (1994). Handling multicriteria fuzzy
#' decision-making problems based on vague set theory.
#' \emph{Fuzzy Sets and Systems}, 67(2), 163-172.
#'
#' Hong, D. H. & Choi, C.-H. (2000). Multicriteria fuzzy decision-making
#' problems based on vague set theory. \emph{Fuzzy Sets and Systems},
#' 114(1), 103-113.
#' @examples
#' x <- ifv(c(0.6, 0.5, 0.7), c(0.3, 0.2, 0.1))
#' if_score(x); if_accuracy(x); if_rank(x); if_order(x)
#' @export
if_score <- function(x) {
  a <- as_ifv(x)
  a$mu - a$nu
}

#' @rdname if_score
#' @export
if_accuracy <- function(x) {
  a <- as_ifv(x)
  a$mu + a$nu
}

#' @rdname if_score
#' @export
if_hesitation <- function(x) {
  a <- as_ifv(x)
  1 - a$mu - a$nu
}

#' @rdname if_score
#' @export
if_rank <- function(x) {
  a <- as_ifv(x)
  s <- a$mu - a$nu; h <- a$mu + a$nu
  # rank 1 = best: order by -s, -h
  o <- order(-s, -h)
  r <- integer(length(s)); r[o] <- seq_along(o)
  r
}

#' @rdname if_score
#' @export
if_order <- function(x) {
  a <- as_ifv(x)
  order(-(a$mu - a$nu), -(a$mu + a$nu))
}

#' @rdname if_score
#' @export
if_best <- function(x) {
  a <- as_ifv(x)
  o <- if_order(a)
  a[o[1L], , drop = FALSE]
}
