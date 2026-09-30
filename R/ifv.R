#' Create an intuitionistic fuzzy value (IFV)
#'
#' An intuitionistic fuzzy value is a pair \eqn{(\mu, \nu)} with
#' \eqn{\mu \ge 0}, \eqn{\nu \ge 0} and \eqn{\mu + \nu \le 1}.
#' Hesitation is \eqn{\pi = 1 - \mu - \nu}.
#'
#' @param mu numeric vector of membership degrees in \eqn{[0,1]}.
#' @param nu numeric vector of non-membership degrees in \eqn{[0,1]}.
#' @return An object of class \code{"ifv"}: a \code{data.frame} with columns
#'   \code{mu}, \code{nu}, \code{pi}.
#' @examples
#' ifv(0.6, 0.3)
#' ifv(c(0.5, 0.7), c(0.3, 0.1))
#' @export
ifv <- function(mu, nu) {
  mu <- as.numeric(mu); nu <- as.numeric(nu)
  if (length(mu) != length(nu)) {
    if (length(mu) == 1L) mu <- rep(mu, length(nu))
    else if (length(nu) == 1L) nu <- rep(nu, length(mu))
    else stop("`mu` and `nu` must have the same length (or length 1).")
  }
  if (any(!is.finite(mu)) || any(!is.finite(nu)))
    stop("`mu` and `nu` must be finite.")
  if (any(mu < -1e-12 | mu > 1 + 1e-12) || any(nu < -1e-12 | nu > 1 + 1e-12))
    stop("`mu` and `nu` must lie in [0, 1].")
  mu <- pmin(pmax(mu, 0), 1); nu <- pmin(pmax(nu, 0), 1)
  if (any(mu + nu > 1 + 1e-9))
    stop("Invalid IFV: need mu + nu <= 1 (got mu + nu = ",
         paste(round(mu + nu, 4), collapse = ", "), ").")
  out <- data.frame(mu = mu, nu = nu, pi = 1 - mu - nu)
  class(out) <- c("ifv", "data.frame")
  rownames(out) <- NULL
  out
}

#' Test / coerce intuitionistic fuzzy values
#' @param x object to test or coerce.
#' @return \code{is_ifv()} logical; \code{as_ifv()} an \code{ifv} object.
#' @export
is_ifv <- function(x) inherits(x, "ifv")

#' @rdname is_ifv
#' @param ... ignored.
#' @export
as_ifv <- function(x, ...) UseMethod("as_ifv")

#' @export
as_ifv.ifv <- function(x, ...) x

#' @export
as_ifv.data.frame <- function(x, ...) {
  if (!all(c("mu", "nu") %in% names(x)))
    stop("data.frame must contain columns `mu` and `nu`.")
  ifv(x$mu, x$nu)
}

#' @export
as_ifv.matrix <- function(x, ...) {
  if (ncol(x) < 2L) stop("matrix must have at least 2 columns (mu, nu).")
  ifv(x[, 1L], x[, 2L])
}

#' @export
as_ifv.default <- function(x, ...) {
  x <- as.matrix(x)
  as_ifv.matrix(x)
}

#' @export
print.ifv <- function(x, ...) {
  cat(sprintf("<intuitionistic fuzzy vector: %d value(s)>\n", nrow(x)))
  xx <- as.data.frame(x); class(xx) <- "data.frame"
  print(xx, ...)
  invisible(x)
}

#' @export
as.data.frame.ifv <- function(x, ...) {
  class(x) <- "data.frame"
  x
}

#' @export
`[.ifv` <- function(x, i, j, drop = FALSE) {
  cl <- class(x)
  class(x) <- "data.frame"
  out <- x[i, j, drop = drop]
  if (is.data.frame(out) && all(c("mu", "nu") %in% names(out))) {
    return(ifv(out$mu, out$nu))
  }
  out
}

#' Hesitation degree
#'
#' @param x an \code{ifv} object (or coercible).
#' @return numeric vector of \eqn{\pi = 1 - \mu - \nu}.
#' @export
if_pi <- function(x) {
  x <- as_ifv(x)
  1 - x$mu - x$nu
}

#' Validate IFVs
#' @param x object coercible to \code{ifv}.
#' @param tol numeric tolerance.
#' @return invisibly \code{TRUE}; errors if invalid.
#' @export
if_check <- function(x, tol = 1e-9) {
  a <- as_ifv(x)
  if (any(a$mu < -tol | a$mu > 1 + tol | a$nu < -tol | a$nu > 1 + tol))
    stop("mu/nu outside [0,1].")
  if (any(a$mu + a$nu > 1 + tol)) stop("mu + nu > 1.")
  invisible(TRUE)
}

#' Complement of an IFV
#' @param x object coercible to \code{ifv}.
#' @return \code{ifv} with mu/nu swapped.
#' @examples
#' if_comp(ifv(0.6, 0.2))
#' @export
if_comp <- function(x) {
  a <- as_ifv(x)
  ifv(a$nu, a$mu)
}
