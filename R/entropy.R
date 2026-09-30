#' Entropy of intuitionistic fuzzy sets
#'
#' Fuzziness / intuitionism of a set \eqn{A=\{a_1,\dots,a_n\}}.
#' All measures lie in \eqn{[0,1]} with larger = more fuzzy.
#'
#' \itemize{
#'   \item \code{"burillo"} (Burillo & Bustince, 1996):
#'     \eqn{E = \frac{1}{n}\sum \pi_i}.
#'   \item \code{"szmidt"} (Szmidt & Kacprzyk, 2001):
#'     \eqn{E = \frac{1}{n}\sum
#'       \frac{\min(\mu_i,\nu_i)+\pi_i}{\max(\mu_i,\nu_i)+\pi_i}}
#'     (define \eqn{0/0:=1}).
#'   \item \code{"vlachos"} (Vlachos & Sergiadis, 2007; De Luca-Termini extension):
#'     \eqn{E = \frac{-1}{n\ln 2}\sum
#'       [\mu_i\ln\mu_i+\nu_i\ln\nu_i-(1-\pi_i)\ln(1-\pi_i)-\pi_i\ln 2]},
#'     with \eqn{0\ln 0:=0}.
#'   \item \code{"zeng_li"} (Zeng & Li, 2006):
#'     \eqn{E = \frac{1}{n}\sum (1-|\mu_i-\nu_i|)}.
#' }
#'
#' @param x object coercible to \code{ifv} (the whole set).
#' @param method one of \code{"burillo"}, \code{"szmidt"}, \code{"vlachos"},
#'   \code{"zeng_li"} (partial matching allowed).
#' @return numeric scalar in \eqn{[0,1]}.
#' @references
#' Burillo, P. & Bustince, H. (1996). Entropy on intuitionistic fuzzy
#' sets and on interval-valued fuzzy sets.
#' \emph{Fuzzy Sets and Systems}, 78(3), 305-316.
#'
#' Szmidt, E. & Kacprzyk, J. (2001). Entropy for intuitionistic fuzzy sets.
#' \emph{Fuzzy Sets and Systems}, 118(3), 467-477.
#'
#' Vlachos, I. K. & Sergiadis, G. D. (2007). Intuitionistic fuzzy
#' information - applications to pattern recognition.
#' \emph{Pattern Recognition Letters}, 28(2), 197-206.
#'
#' Zeng, L. & Li, H. (2006). Normalized distance, similarity measure,
#' intuitionistic fuzzy entropy and application.
#' Proc. World Congress on Intelligent Control and Automation, 3857-3860.
#' @examples
#' x <- ifv(c(0.5, 0.6, 0.4), c(0.3, 0.2, 0.4))
#' if_entropy(x, "burillo"); if_entropy(x, "szmidt")
#' if_entropy(x, "vlachos"); if_entropy(x, "zeng_li")
#' @export
if_entropy <- function(x, method = c("burillo", "szmidt", "vlachos", "zeng_li")) {
  method <- match.arg(method)
  a <- as_ifv(x)
  mu <- a$mu; nu <- a$nu; pi <- 1 - mu - nu
  n <- length(mu)
  if (method == "burillo") {
    return(mean(pi))
  }
  if (method == "szmidt") {
    num <- pmin(mu, nu) + pi
    den <- pmax(mu, nu) + pi
    r <- num / den
    r[den == 0] <- 1  # degenerate (mu=nu=1 impossible, but guard)
    return(mean(r))
  }
  if (method == "vlachos") {
    t1 <- ifelse(mu <= 0, 0, mu * log(mu))
    t2 <- ifelse(nu <= 0, 0, nu * log(nu))
    om <- 1 - pi  # mu + nu
    t3 <- ifelse(om <= 0 | om >= 1, ifelse(om <= 0, 0, 0), om * log(om))
    # careful: om=1 -> 1*log(1)=0; handle explicitly
    t3 <- ifelse(om <= 0, 0, ifelse(om >= 1, 0, om * log(om)))
    s <- t1 + t2 - t3 - pi * log(2)
    return(mean(-s / log(2)))
  }
  # zeng_li
  mean(1 - abs(mu - nu))
}
