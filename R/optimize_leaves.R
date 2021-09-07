#' Optimize leaf photosynthesis
#' 
#' \code{optimize_leaves}: optimize C3 photosynthesis over multiple parameter sets
#' 
#' @param traits A vector of one or more character strings indicating which trait(s) to optimize. Stomatal conductance (\code{g_sc}) and stomatal ratio (\code{logit_sr}) are currently supported.
#' 
#' @param carbon_costs A named list of resources with their costs in terms of carbon (e.g. mol C / mol H2O). Currently only H2O and SR are supported. See details below.
#' 
#' @param constants A list of physical constants inheriting class \code{constants}. This can be generated using the \code{make_constants} function.
#' 
#' @param bake_par A list of temperature response parameters inheriting class \code{bake_par}. This can be generated using the \code{make_bakepar} function.
#' 
#' @param enviro_par A list of environmental parameters inheriting class \code{enviro_par}. This can be generated using the \code{make_enviropar} function.
#' 
#' @param leaf_par A list of leaf parameters inheriting class \code{leaf_par}. This can be generated using the \code{make_leafpar} function.
#' 
#' @param set_units Logical. Should \code{units} be set? The function is faster when FALSE, but input must be in correct units or else results will be incorrect without any warning.

#' @param check Logical. Should arguments checks be done? This is intended to be disabled when \code{optimize_leaf} is called from \code{\link{optimize_leaves}} Default is TRUE.
#' 
#' @param n_init Integer. Number of initial values for each trait to try during optimization. If there are multiple traits, these initial values are crossed. For example, if \code{n_init = 3}, the total number of intitial values sets is 3, 9, 27 for 1, 2, 3 traits, respectively. This significantly increases the time, but may be important if the surface is rugged. Default is 1L.
#' 
#' @param progress Logical. Should a progress bar be displayed?
#' 
#' @param quiet Logical. Should messages be displayed?
#' 
#' @param parallel Logical. Should parallel processing be used via \code{\link[furrr]{future_map}}?
#' 
#' @return 
#' A data.frame with the following \code{units} columns \cr
#' 
#' \tabular{ll}{
#' 
#' \bold{Input:} \tab \cr
#' \cr
#' \code{C_air} \tab atmospheric CO2 concentration (Pa) \cr
#' \code{g_mc25} \tab mesophyll conductance to CO2 at 25 °C (\eqn{\mu}mol CO2 / (m\eqn{^2} s Pa)) \cr
#' \code{g_sc} \tab stomatal conductance to CO2 (\eqn{\mu}mol CO2 / (m\eqn{^2} s Pa)) \cr
#' \code{g_uc} \tab cuticular conductance to CO2 (\eqn{\mu}mol CO2 / (m\eqn{^2} s Pa)) \cr
#' \code{gamma_star25} \tab chloroplastic CO2 compensation point at 25 °C (Pa) \cr
#' \code{J_max25} \tab potential electron transport at 25 °C (\eqn{\mu}mol CO2) / (m\eqn{^2} s) \cr
#' \code{K_C25} \tab Michaelis constant for carboxylation at 25 °C (\eqn{\mu}mol / mol) \cr
#' \code{K_O25} \tab Michaelis constant for oxygenation at 25 °C (\eqn{\mu}mol / mol) \cr
#' \code{k_mc} \tab partition of \eqn{g_\mathrm{mc}}{g_mc} to lower mesophyll (unitless) \cr
#' \code{k_sc} \tab partition of \eqn{g_\mathrm{sc}}{g_sc} to lower surface (unitless) \cr
#' \code{k_uc} \tab partition of \eqn{g_\mathrm{uc}}{g_uc} to lower surface (unitless) \cr
#' \code{leafsize} \tab leaf characteristic dimension (m) \cr
#' \code{O} \tab atmospheric O2 concentration (kPa) \cr
#' \code{P} \tab atmospheric pressure (kPa) \cr
#' \code{phi} \tab initial slope of the response of J to PPFD (unitless) \cr
#' \code{PPFD} \tab photosynthetic photon flux density (umol quanta / (m\eqn{^2} s)) \cr
#' \code{R_d25} \tab nonphotorespiratory CO2 release  at 25 °C (\eqn{\mu}mol CO2 / (m\eqn{^2} s)) \cr
#' \code{RH} \tab relative humidity (unitless) \cr
#' \code{theta_J} \tab curvature factor for light-response curve (unitless) \cr
#' \code{T_air} \tab air temperature (K) \cr
#' \code{T_leaf} \tab leaf tempearture (K) \cr
#' \code{V_cmax25} \tab maximum rate of carboxylation at 25 °C (\eqn{\mu}mol CO2 / (m\eqn{^2} s)) \cr
#' \code{V_tpu25} \tab rate of triose phosphate utilisation at 25 °C (\eqn{\mu}mol CO2 / (m\eqn{^2} s)) \cr
#' \code{wind} \tab wind speed (m / s) \cr
#' \cr
#' \bold{Baked Input:} \tab \cr
#' \cr
#' \code{g_mc} \tab mesophyll conductance to CO2 at \code{T_leaf} (\eqn{\mu}mol CO2 / (m\eqn{^2} s Pa)) \cr
#' \code{gamma_star} \tab chloroplastic CO2 compensation point at \code{T_leaf} (Pa) \cr
#' \code{J_max} \tab potential electron transport at \code{T_leaf} (\eqn{\mu}mol CO2) / (m\eqn{^2} s) \cr
#' \code{K_C} \tab Michaelis constant for carboxylation at \code{T_leaf} (\eqn{\mu}mol / mol) \cr
#' \code{K_O} \tab Michaelis constant for oxygenation at \code{T_leaf}(\eqn{\mu}mol / mol) \cr
#' \code{R_d} \tab nonphotorespiratory CO2 release  at \code{T_leaf} (\eqn{\mu}mol CO2 / (m\eqn{^2} s)) \cr
#' \code{V_cmax} \tab maximum rate of carboxylation at \code{T_leaf} (\eqn{\mu}mol CO2 / (m\eqn{^2} s)) \cr
#' \code{V_tpu} \tab rate of triose phosphate utilisation at \code{T_leaf} (\eqn{\mu}mol CO2 / (m\eqn{^2} s)) \cr
#' \cr
#' \bold{Output:} \tab \cr
#' \cr
#' \code{A} \tab photosynthetic rate at \code{C_chl} (\eqn{\mu}mol CO2 / (m\eqn{^2} s)) \cr
#' \code{C_chl} \tab chloroplastic CO2 concentration where \code{A_supply} intersects \code{A_demand} (Pa) \cr
#' \code{g_tc} \tab total conductance to CO2 at \code{T_leaf} (\eqn{\mu}mol CO2 / (m\eqn{^2} s Pa)) \cr
#' \code{value} \tab \code{A_supply} - \code{A_demand} (\eqn{\mu}mol CO2 / (m\eqn{^2} s)) at \code{C_chl} \cr
#' \code{convergence} \tab convergence code (0 = converged)
#' }
#' 
#' @details 
#' 
#' \code{optimize_leaf}: This function optimizes leaf traits using an integrated leaf temperature and C3 photosynthesis model under a set of environmental conditions. The leaf temperature model is described in the \code{\link[tealeaves]{tealeaves}} package. The C3 photosynthesis model is described in the \code{\link[photosynthesis]{photosynthesis-package}} package\cr
#' \cr
#' \code{optimize_leaves}: This function uses \code{optimize_leaf} to optimize over multiple parameter sets that are generated using \code{\link[tidyr]{crossing}}. \cr
#' 
#' @examples 
#' # Single parameter set with 'optimize_leaf'
#' 
#' bp <- make_bakepar()
#' cs <- make_constants()
#' ep <- make_enviropar()
#' lp <- make_leafpar()
#' traits <- "g_sc"
#' carbon_costs <- list(H2O = 1000, SR = 0)
#' optimize_leaf("g_sc", carbon_costs, bp, cs, ep, lp, n_init = 1L)
#' 
#' # Multiple parameter sets with 'optimize_leaves'
#' 
#' ep <- make_enviropar(
#'   replace = list(
#'     T_air = set_units(c(293.14, 298.15), "K")
#'   )
#' )
#' optimize_leaves(traits, carbon_costs, bp, cs, ep, lp, n_init = 1L)
#' 
#' @encoding UTF-8
#' 
#' @export
#' 

optimize_leaves <- function(traits, carbon_costs, bake_par, constants, 
                            enviro_par, leaf_par, set_units = TRUE, 
                            check = TRUE, n_init = 1L, progress = TRUE, 
                            quiet = FALSE, parallel = FALSE) {
  
  checkmate::assert_flag(check)
  
  if (check) {
    
    checkmate::assert_flag(set_units)
    checkmate::assert_flag(progress)
    checkmate::assert_flag(quiet)
    checkmate::assert_flag(parallel)
    checkmate::assert_integerish(n_init, len = 1L, lower = 1L)
    
    # Check traits ----
    checkmate::assert_character(traits)
    checkmate::assert_vector(traits, min.len = 1L, max.len = 3L, unique = TRUE,
                             any.missing = FALSE)
    
    # Check carbon costs ----
    check_carbon_costs(carbon_costs, quiet)
    
    # Check parameters ----
    checkmate::assert_class(bake_par, "bake_par")
    checkmate::assert_class(constants, "constants")
    checkmate::assert_class(enviro_par, "enviro_par")
    checkmate::assert_class(leaf_par, "leaf_par")
    
  }
  
  # Set units ----
  if (set_units) {
    bake_par %<>% photosynthesis::bake_par()
    constants %<>% leafoptimizer::constants()
    enviro_par %<>% leafoptimizer::enviro_par()
    leaf_par %<>% leafoptimizer::leaf_par()
  }
  
  # Capture units ----
  pars <- c(leaf_par, enviro_par)
  par_units <- purrr::map(pars, units) %>%
    magrittr::set_names(names(pars))
  
  # Make parameter sets ----
  ## cross_df() removes units. 
  ## This code will cause errors if units are not properly set
  pars %<>% purrr::cross_df()
  
  # Optimize ----
  soln <- find_optima(traits, carbon_costs, pars, bake_par, constants, 
                      par_units, n_init, progress, quiet, parallel)
  
  # Return ----
  soln
  
}

find_optima <- function(traits, carbon_costs, par_sets, bake_par, constants, 
                        par_units, n_init, progress, quiet, parallel) {
  
  if (!quiet) {
    glue::glue("\nOptimizing leaf trait{s1} from {n} parameter set{s2} ...", 
               s1 = ifelse(length(traits) > 1, "s", ""), n = length(par_sets), 
               s2 = dplyr::if_else(length(par_sets) > 1, "s", "")) %>%
      crayon::green() %>%
      message(appendLF = FALSE)
  }
  
  if (parallel) future::plan("multiprocess")
  
  if (progress & !parallel) pb <- dplyr::progress_estimated(length(pars))
  
  soln <- suppressWarnings(
    par_sets %>%
      as.list() %>%
      purrr::transpose() %>%
      furrr::future_map_dfr(~{
        
        ret <- optimize_leaf(traits, carbon_costs, bake_par, constants, 
                             .x, .x, set_units = FALSE, n_init, 
                             check = FALSE, quiet = TRUE)
        if (progress & !parallel) pb$tick()$print()
        ret
        
      }, .progress = progress)
  )
  
  # Reassign units ----
  colnames(soln) %>%
    glue::glue("units(soln${x}) <<- par_units${x}", x = .) %>%
    parse(text = .) %>%
    eval()
  
  soln %>%
    dplyr::select(tidyselect::ends_with("25")) %>%
    colnames() %>%
    stringr::str_remove("25$") %>%
    glue::glue("units(soln${x}) <<- par_units${x}25", x = .) %>%
    parse(text = .) %>%
    eval()
  
  soln
  
}

