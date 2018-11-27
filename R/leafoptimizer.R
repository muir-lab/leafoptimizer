#' \code{leafoptimizer} package
#'
#' Optimize leaf traits to different environments in silico
#'
#' See the README on
#' \href{https://github.com/cdmuir/leafoptimizer}{GitHub}
#'
#' @docType package
#' @name leafoptimizer
#' @importFrom magrittr %>% %<>%
#' @importFrom methods is
#' @importFrom rlang .data
#' @importFrom stats optim
#' @importFrom units drop_units set_units
#' @importFrom zeallot %<-%
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c(".", "lower", "upper"))
