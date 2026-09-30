#' IFFuzzyStats: Intuitionistic Fuzzy Statistics Toolkit
#'
#' A comprehensive toolkit for intuitionistic fuzzy sets (Atanassov, 1986),
#' where each element carries membership \eqn{\mu}, non-membership \eqn{\nu}
#' and hesitation \eqn{\pi = 1 - \mu - \nu}.
#'
#' The package provides:
#' \itemize{
#'   \item constructors and arithmetic for intuitionistic fuzzy values
#'     (\code{\link{ifv}}, \code{\link{if_add}}, ...),
#'   \item Xu and Yager aggregation operators
#'     (\code{\link{ifwa}}, \code{\link{ifwg}}, \code{\link{ifowa}}, ...),
#'   \item entropy, distance and similarity measures
#'     (\code{\link{if_entropy}}, \code{\link{if_dist}}, \code{\link{if_sim}}),
#'   \item intuitionistic fuzzy clustering
#'     (\code{\link{ifcmeans}}, \code{\link{ifkmeans}}),
#'   \item approximate reasoning / compositional inference
#'     (\code{\link{if_compose}}, \code{\link{if_infer}}),
#'   \item multi-criteria decision helpers
#'     (\code{\link{if_topsis}}, \code{\link{if_rank_df}}).
#' }
#'
#' @references
#' Atanassov, K. (1986). Intuitionistic fuzzy sets.
#' \emph{Fuzzy Sets and Systems}, 20(1), 87-96.
#'
#' Xu, Z. (2007). Intuitionistic fuzzy aggregation operators.
#' \emph{IEEE Transactions on Fuzzy Systems}, 15(6), 1179-1187.
#'
#' Szmidt, E. & Kacprzyk, J. (2000). Distances between intuitionistic
#' fuzzy sets. \emph{Fuzzy Sets and Systems}, 114(3), 505-518.
#'
#' Vlachos, I. K. & Sergiadis, G. D. (2007). Intuitionistic fuzzy
#' information - applications to pattern recognition.
#' \emph{Pattern Recognition Letters}, 28(2), 197-206.
#'
#' Ye, J. (2010). Fuzzy decision-making method based on the weighted
#' correlation coefficient under intuitionistic fuzzy environment.
#' \emph{European Journal of Operational Research}, 205(1), 202-204.
#' @keywords internal
"_PACKAGE"
