#' Optimize leaf photosynthesis
#' 
#' \code{optimize_leaves}: optimize C3 photosynthesis over multiple parameter sets
#' 
#' @param traits A vector of one or more character strings indicating which trait(s) to optimize. Stomatal conductance (\code{g_sc}) and stomatal ratio (\code{logit_sr}) are currently supported.
#' 
#' @param carbon_costs A named list of resources with their costs in terms of carbon (e.g. mol C / mol H2O). Currently only H2O is supported. See details below.
#' 
#' @param leaf_par A list of leaf parameters inheriting class \code{leaf_par}. This can be generated using the \code{make_leafpar} function.
#' 
#' @param enviro_par A list of environmental parameters inheriting class \code{enviro_par}. This can be generated using the \code{make_enviropar} function.
#' 
#' @param bake_par A list of temperature response parameters inheriting class \code{bake_par}. This can be generated using the \code{make_bakepar} function.
#' 
#' @param constants A list of physical constants inheriting class \code{constants}. This can be generated using the \code{make_constants} function.
#' 
#' @param progress Logical. Should a progress bar be displayed?
#' 
#' @param quiet Logical. Should messages be displayed?
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
#' # Single parameter set with 'photo'
#' 
#' cs <- make_constants()
#' lp <- make_leafpar(cs)
#' ep <- make_enviropar()
#' bp <- make_bakepar()
#' traits <- "g_sc"
#' carbon_costs <- list(H2O = 0.004)
#' optimize_leaf("g_sc", carbon_costs, lp, ep, bp, cs)
#' 
#' # Multiple parameter sets with 'photosynthesis'
#' 
#' ep <- make_enviropar(
#'   replace = list(
#'     T_air = set_units(c(293.14, 298.15), "K")
#'   )
#' )
#' optimize_leaves(traits, carbon_costs, lp, ep, bp, cs)
#' 
#' @encoding UTF-8
#' 
#' @export
#' 

optimize_leaves <- function(traits, carbon_costs, leaf_par, enviro_par, bake_par, 
                            constants, progress = TRUE, quiet = FALSE) {
  
  # Check inputs ----
  leaf_par %<>% leaf_par()
  enviro_par %<>% enviro_par()
  bake_par %<>% photosynthesis::bake_par()
  constants %<>% constants()
  
  # Capture units ----
  pars <- c(leaf_par, enviro_par)
  par_units <- purrr::map(pars, units) %>%
    magrittr::set_names(names(pars))
  
  # Make parameter sets ----
  pars %<>% make_parameter_sets(constants, par_units)
  
  # Optimize ----
  soln <- find_optima(traits, carbon_costs, pars, bake_par, constants, 
                      par_units, progress, quiet)
  
  # Return ----
  soln
  
}

make_parameter_sets <- function(pars, constants, par_units) {
  
  pars %<>%
    names() %>%
    glue::glue("{x} = pars${x}", x = .) %>%
    stringr::str_c(collapse = ", ") %>%
    glue::glue("tidyr::crossing({x})", x = .) %>%
    parse(text = .) %>%
    eval() %>%
    # Exclude mismatched parameter sets generated in tidyr::crossing
    dplyr::mutate(
      PPFD1 = drop_units(sun2ppfd(
        set_units(.data$S_sw, par_units[["S_sw"]], mode = "standard"), 
        set_units(.data$f_par, par_units[["f_par"]], mode = "standard"), 
        set_units(.data$E_q, par_units[["E_q"]], mode = "standard")
      )),
      g_sw1 = drop_units(gc2gw(
        set_units(.data$g_sc, par_units[["g_sc"]], mode = "standard"),
        constants$D_c0, constants$D_w0, unitless = FALSE
      )),
      g_uw1 = drop_units(gc2gw(
        set_units(.data$g_uc, par_units[["g_uc"]], mode = "standard"),
        constants$D_c0, constants$D_w0, unitless = FALSE
      ))
    ) %>%
    dplyr::filter(round(.data$PPFD, 6) == round(.data$PPFD1, 6),
                  round(.data$g_sw, 6) == round(.data$g_sw1, 6),
                  round(.data$g_uw, 6) == round(.data$g_uw1, 6)) %>%
    dplyr::select(-"PPFD1", -"g_sw1", -"g_uw1") %>%
    purrr::transpose()
  
  tidyr::crossing(i = seq_len(length(pars)),
                  par = names(pars[[1]])) %>%
    dplyr::transmute(ex = glue::glue("units(pars[[{i}]]${par}) <<- par_units${par}", 
                                     i = .data$i, par = .data$par)) %>%
    dplyr::pull("ex") %>%
    parse(text = .) %>%
    eval()
  
  pars
  
}

find_optima <- function(traits, carbon_costs, pars, bake_par, constants, 
                        par_units, progress, quiet) {
  
  if (!quiet) {
    glue::glue("\nOptimizing leaf trait{s1} from {n} parameter set{s2} ...", 
               s1 = ifelse(length(traits) > 1, "s", ""), n = length(pars), 
               s2 = dplyr::if_else(length(pars) > 1, "s", "")) %>%
      crayon::green() %>%
      message(appendLF = FALSE)
  }
  
  if (progress) pb <- dplyr::progress_estimated(length(pars))
  
  soln <- suppressWarnings(
    pars %>%
      purrr::map_dfr(~{
        
        ret <- optimize_leaf(traits, carbon_costs, leaf_par(.x), enviro_par(.x), 
                             bake_par, constants, quiet = TRUE)
        if (progress) pb$tick()$print()
        ret
        
      })
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

#' Optimize C3 photosynthesis
#' @description \code{optimize_leaf}: simulate C3 photosynthesis over a single parameter set
#' @rdname optimize_leaves
#' @export

optimize_leaf <- function(traits, carbon_costs, leaf_par, enviro_par, bake_par, 
                          constants, quiet = FALSE) {
  
  # Check traits ----
  traits %<>% 
    match.arg(c("g_sc", "logit_sr"), TRUE) %>%
    sort()

  # Check carbon costs ----
  check_carbon_costs(carbon_costs, quiet)
  
  # Check parameters ----
  constants %<>% constants()
  leaf_par %<>% leaf_par()
  bake_par %<>% photosynthesis::bake_par()
  enviro_par %<>% enviro_par()
  
  # Concatenate parameters ----
  pars <- c(constants, leaf_par, enviro_par, bake_par)
  
  # Find optimum ----
  soln <- find_optimum(g_sc = ("g_sc" %in% traits), 
                       logit_sr = ("logit_sr" %in% traits), 
                       carbon_costs, pars, quiet)
  
  # Check results ----
  check_results(soln)
  pars$carbon_balance <- -soln$value
    
  # Concatenate optimized traits in pars to calculate T_leaf, A, and E ----
  pars %<>% c_optimized_traits(traits, soln) 
  unitless_pars <- pars %>% purrr::map_if(function(x) is(x, "units"), drop_units)
  
  # Calculate T_leaf, A, and E ----
  unitless_pars$T_leaf <- unitless_pars %>% 
    find_tleaf(., . , .) %>%
    magrittr::use_series("T_leaf")
  pars$T_leaf <- set_units(unitless_pars$T_leaf, "K")

  ph <- unitless_pars %>% 
    c(bake(., ., ., unitless = TRUE)) %>%
    find_A()

  pars$A <- set_units(ph$A, "umol/m^2/s")
  pars$C_chl <- set_units(ph$C_chl, "Pa")

  eb <- tealeaves::energy_balance(
    pars$T_leaf, tealeaves::leaf_par(pars), tealeaves::enviro_par(pars),
    tealeaves::constants(pars), components = TRUE
  )
  stopifnot(round(drop_units(eb$energy_balance), 1) == 0)
  
  pars %<>% c(eb$components)
  
  blp <- bake(photosynthesis::leaf_par(pars), 
              photosynthesis::bake_par(pars),
              photosynthesis::constants(pars),
              unitless = FALSE)
  pars %<>% c(blp[!(names(blp) %in% names(.))])
  
  # Return ----
  keep <- names(pars)[pars %>%
    names() %>%
    magrittr::is_in(c(parameter_names("constants"), parameter_names("bake"))) %>%
    magrittr::not()]
  
  as.data.frame(pars[sort(keep)])

}

find_optimum <- function(g_sc, logit_sr, carbon_costs, pars, quiet) {
  
  unitless_pars <- pars %>% 
    purrr::map_if(function(x) is(x, "units"), drop_units)
  traits <- c("g_sc", "logit_sr")[c(g_sc, logit_sr)]
  
  # Find traits that maximize carbon gain ----
  .f <- function(traits, g_sc, logit_sr, carbon_costs, upars) {

    if (g_sc) {
      upars$g_sc <- traits[1]
      upars$g_sw <- gc2gw(upars$g_sc, upars$D_c0, upars$D_w0, unitless = TRUE)
    }
    
    if (logit_sr) {
      upars$logit_sr <- ifelse(g_sc, traits[2], traits[1])
      upars$k_sc <- upars$logit_sr %>%
        stats::plogis() %>%
        magrittr::divide_by(1 - .)
    }
    
    upars$T_leaf <- find_tleaf(upars, upars, upars)$T_leaf
    
    upars %<>%
      c(bake(., ., ., TRUE) %>%
          purrr::map_if(function(x) is(x, "units"), drop_units))
    
    A <- find_A(upars)$A

    E <- tealeaves::E(upars$T_leaf, upars, unitless = TRUE)
    
    -(A - E * 1e6 * carbon_costs$H2O)
    
  }
  
  if (!quiet) {
    glue::glue("\nOptimizing leaf trait{s} ...",
               s = ifelse(length(traits) > 1, "s", "")) %>%
      crayon::green() %>%
      message(appendLF = FALSE)
  }
  
  fit <- tryCatch({
    
    c(init, lb, ub) %<-% list(c(g_sc = 3, logit_sr = 0), 
                              c(g_sc = 0, logit_sr = -10), 
                              c(g_sc = 10, logit_sr = 10))
    
    stats::optim(init[traits], .f, carbon_costs = carbon_costs, 
                 upars = unitless_pars, g_sc = g_sc, logit_sr = logit_sr,
                 method = ifelse(length(traits) == 1, "Brent", "L-BFGS-B"),
                 lower = lb[traits], upper = ub[traits])
    
  }, finally = {
    fit <- list(par = stats::setNames(rep(NA, length(traits)), traits), 
                value = NA, convergence = 1)
  })
  
  soln <- fit$par %>% 
    as.data.frame() %>%
    t() %>%
    as.data.frame() %>%
    magrittr::set_colnames(traits) %>%
    dplyr::mutate(value = fit$value, 
                  convergence = fit$convergence)
  
  if (!quiet) {
    " done" %>%
      crayon::green() %>%
      message()
  }
  
  soln
  
}

c_optimized_traits <- function(pars, traits, soln) {
  
  if ("g_sc" %in% traits) {
    pars$g_sc <- soln$g_sc
    pars$g_sw <- gc2gw(pars$g_sc, pars$D_c0, pars$D_w0, unitless = TRUE) 
  }
  
  if ("logit_sr" %in% traits) {
    pars$logit_sr <- soln$logit_sr
    pars$k_sc <- pars$logit_sr %>%
      stats::plogis() %>%
      magrittr::divide_by(1 - .)
  }
  
  pars$g_sc %<>% set_units("umol/m^2/s/Pa")
  pars$g_sw %<>% set_units("umol/m^2/s/Pa")
  pars$logit_sr %<>% set_units()
  pars$k_sc %<>% set_units()
  
  pars
  
}

find_tleaf <- function(leaf_par, enviro_par, constants) {

  # For this version, all parameters must arrive unitless  
  
  # Balance energy fluxes -----
  fit <- tryCatch({
    stats::uniroot(f = tealeaves::energy_balance, leaf_par = leaf_par, 
                   enviro_par = enviro_par, constants = constants, 
                   quiet = TRUE, unitless = TRUE, check = FALSE,
                   lower = enviro_par$T_air - 30, upper = enviro_par$T_air + 30)
  }, finally = {
    fit <- list(root = NA, f.root = NA, convergence = 1)
  })
  
  soln <- data.frame(T_leaf = fit$root, value = fit$f.root, 
                     convergence = dplyr::if_else(is.null(fit$convergence), 0, 1))
  
  soln
  
}

find_A <- function(pars) {
  
  # For this version, all parameters must arrive unitless  
  
  .f <- function(C_chl, pars) {
    photosynthesis::A_supply(C_chl, pars, unitless = TRUE) - 
      photosynthesis::A_demand(C_chl, pars, unitless = TRUE)
  }
  
  fit <- tryCatch({
    stats::uniroot(.f, pars = pars, lower = 0.1, upper = max(c(10, pars$C_air)), 
                   check.conv = TRUE)
  }, finally = {
    fit <- list(root = NA, f.root = NA, convergence = 1)
  })
  
  soln <- data.frame(C_chl = fit$root, value = fit$f.root, 
                     convergence = dplyr::if_else(is.null(fit$convergence), 0, 1))
  
  soln$A <- photosynthesis::A_supply(soln$C_chl, pars, unitless = TRUE)
  
  soln
  
}





