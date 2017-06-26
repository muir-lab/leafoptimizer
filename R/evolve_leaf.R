#' Find evolutionarily optimal leaf traits
#'
#' @importFrom stats optim
#' 
#' @param traits A vector of traits to optimize. Must one or more of stomatal conductance (\code{g_sw}), stomatal ratio (\code{sr}), and leaf size (\code{leafsize}). Partial matching allowed.
#' @param constraints A list containing lambda (i.e. the cost of water), which must be a scalar. No other constraints are currently implemented.
#' @param leaf_par A list of leaf parameters. This can be generated using the \code{make_leafpar} function.
#' @param enviro_par A list of environmental parameters. This can be generated using the \code{make_enviropar} function.
#' @param constants A list of physical constants. This can be generated using the \code{make_constants} function.
#'
#' @export
#'

evolve_leaf <- function(traits, constraints, leaf_par, enviro_par, constants, ...) {

  ##### Initial guess at traits -----

  ##### Find traits that maximize fitness -----
  soln <- optim(traits, leaf_fitness, constraints, leaf_par, enviro_par, constants, return_negative = TRUE, ...)

  soln

}

