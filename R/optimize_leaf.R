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
#' constants <- make_constants()
#' leaf_par <- make_leafpar(constants)
#' enviro_par <- make_enviropar()
#' bake_par <- make_bakepar()
#' # optimize_leaf(leaf_par, enviro_par, bake_par, constants)
#' 
#' # Multiple parameter sets with 'photosynthesis'
#' 
#' leaf_par <- make_leafpar(constants,
#'   replace = list(
#'     T_leaf = set_units(c(293.14, 298.15), "K")
#'     )
#'   )
#' # optimize_leaves(leaf_par, enviro_par, bake_par, constants)
#' 
#' @encoding UTF-8
#' 
#' @export
#' 

optimize_leaves <- function(traits, carbon_costs, leaf_par, enviro_par, bake_par, 
                            constants, progress = TRUE, quiet = FALSE) {
  
  # Capture units ----
  pars <- c(leaf_par, enviro_par)
  par_units <- purrr::map(pars, units) %>%
    magrittr::set_names(names(pars))
  
  # Make parameter sets ----
  pars %<>%
    names() %>%
    glue::glue("{x} = pars${x}", x = .) %>%
    stringr::str_c(collapse = ", ") %>%
    glue::glue("tidyr::crossing({x})", x = .) %>%
    parse(text = .) %>%
    eval() %>%
    purrr::transpose()
  
  tidyr::crossing(i = seq_len(length(pars)),
                  par = names(pars[[1]])) %>%
    dplyr::transmute(ex = glue::glue("units(pars[[{i}]]${par}) <<- par_units${par}", 
                                     i = .data$i, par = .data$par)) %>%
    dplyr::pull("ex") %>%
    parse(text = .) %>%
    eval()
  
  # Optimize ----
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
  
  # Return ----
  soln
  
}

#' Optimize C3 photosynthesis
#' @description \code{optimize_leaf}: simulate C3 photosynthesis over a single parameter set
#' @rdname optimize_leaves
#' @export

optimize_leaf <- function(traits, carbon_costs, leaf_par, enviro_par, bake_par, 
                          constants, quiet = FALSE) {
  
  # Check traits ----
  traits %<>% match.arg(c("g_sc", "logit_sr"), TRUE)  

  pars <- c(leaf_par, enviro_par, bake_par, constants)
  
  # Find traits that maximize carbon gain ----
  .f <- function(traits, carbon_costs, pars) {
    
    if ("g_sc" %in% names(traits)) {
      pars$g_sc <- set_units(traits[["g_sc"]], "umol/m^2/s/Pa")
      pars$g_sw <- gc2gw(pars$g_sc, pars$D_c0, pars$D_w0) 
    }
    
    if ("logit_sr" %in% names(traits)) {
      pars$logit_sr <- set_units(traits[["logit_sr"]])
      pars$k_sc <- pars$logit_sr %>%
        stats::plogis() %>%
        drop_units() %>%
        magrittr::divide_by(1 - .) %>%
        set_units()
    }
    
    tealeaves::tleaf(tealeaves::leaf_par(pars),
                     tealeaves::enviro_par(pars),
                     tealeaves::constants(pars), quiet = TRUE) %>%
      magrittr::use_series("T_leaf") %>%
      carbon_balance(carbon_costs, pars, quiet = TRUE) %>%
      set_units("umol/m^2/s") %>%
      drop_units() %>%
      magrittr::multiply_by(-1)
    
  }
  
  if (!quiet) {
    glue::glue("\nOptimizing leaf trait{s} ...",
               s = ifelse(length(traits) > 1, "s", "")) %>%
      crayon::green() %>%
      message(appendLF = FALSE)
  }
  
  fit <- tryCatch({
    
    init <- purrr::map(pars[traits], drop_units)
    c(lower, upper) %<-% list(c(g_sc = 0, logit_sr = -10), c(g_sc = 10, logit_sr = 10))
    
    stats::optim(init, .f, carbon_costs = carbon_costs, pars = pars,
                 method = ifelse(length(traits) == 1, "Brent", "L-BFGS-B"),
                 lower = lower[traits], upper = upper[traits])
    
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
  
  # Check results ----
  if (soln$convergence == 1) {
    "stats::optim did not converge, NA returned. Inspect parameters carefully." %>%
      crayon::red() %>%
      message()
  }
  
  # Put optimized traits in pars to calculate T_leaf, A, and E ----
  if ("g_sc" %in% traits) {
    pars$g_sc <- set_units(soln$g_sc, "umol/m^2/s/Pa")
    pars$g_sw <- gc2gw(pars$g_sc, pars$D_c0, pars$D_w0) 
  }
  
  if ("logit_sr" %in% traits) {
    pars$logit_sr <- set_units(soln$logit_sr)
    pars$k_sc <- pars$logit_sr %>%
      stats::plogis() %>%
      drop_units() %>%
      magrittr::divide_by(1 - .) %>%
      set_units()
  }
  
  # Calculate T_leaf, A, and E ----
  tl <- tealeaves::tleaf(tealeaves::leaf_par(pars), 
                         tealeaves::enviro_par(pars),
                         tealeaves::constants(pars))
  pars$T_leaf <- set_units(tl$T_leaf, "K")
  ph <- photosynthesis::photo(photosynthesis::leaf_par(pars),
                              photosynthesis::enviro_par(pars),
                              photosynthesis::bake_par(pars),
                              photosynthesis::constants(pars))
  
  # Return ----
  soln %<>%
    dplyr::transmute(carbon_balance = -.data$value) %>%
    dplyr::bind_cols(as.data.frame(
      photosynthesis::bake(photosynthesis::leaf_par(pars), 
                           photosynthesis::bake_par(pars),
                           photosynthesis::constants(pars))
    )) %>%
    dplyr::bind_cols(as.data.frame(enviro_par(pars))) %>%
    dplyr::bind_cols(dplyr::select(tl, .data$E)) %>%
    dplyr::bind_cols(dplyr::select(ph, .data$C_chl, .data$g_tc, .data$A))
  
  soln  
  
}

#' Calculate carbon balance
#' 
#' @inheritParams optimize_leaves
#' @param T_leaf Leaf temperature in degree Kelvin
#' @param pars Concatenated parameters (\code{leaf_par}, \code{enviro_par}, \code{bake_par}, and \code{constants})
#' 
#' @return Value of class \code{units} indicating the carbon balance.
#' 
#' @details 
#' 
#' Currently only carbon a cost of water lost to transpiration is supported. Functions calculate the instantaneous leaf-level carbon gain and water loss per area. In the future, I plan to extend functionality to other resources (e.g. nitrogen) and integrate over time courses.
#' 
#' The basic equation is:
#' 
#' Carbon Balance = Carbon gain - (Carbon cost of water) Water loss
#' 
#' @examples 
#' cnstnts <- make_constants()
#' lp <- make_leafpar(cnstnts)
#' bp <- make_bakepar()  
#' ep <- make_enviropar()
#' 
#' T_leaf <- tealeaves::tleaf(lp, ep, cnstnts)$T_leaf
#' carbon_costs <- list(H2O = 0.003)
#' pars <- c(cnstnts, lp, bp, ep)
#' 
#' carbon_balance(T_leaf, carbon_costs, pars)
#' 
#' @export

carbon_balance <- function(T_leaf, carbon_costs, pars, quiet = FALSE) {
  
  # Checks ----
  T_leaf %<>% set_units("K")
  stopifnot(is.list(carbon_costs))
  stopifnot("H2O" %in% names(carbon_costs))
  stopifnot(length(carbon_costs$H2O) == 1L)
  stopifnot(is.numeric(carbon_costs$H2O))
  if (!quiet & length(carbon_costs) > 1L) {
    "Only the carbon cost of H2O is currently supported. Other elements will be ignored." %>%
      crayon::green() %>%
      message()
  }
  cnstnts <- constants(pars)
  lp <- leaf_par(pars)
  bp <- photosynthesis::bake_par(pars)
  ep <- enviro_par(pars)
  
  # Carbon gain ----
  ph <- photosynthesis::photo(c(lp, T_leaf = T_leaf), ep, bp, cnstnts, 
                              quiet = TRUE)
  C_gain <- set_units(ph$A, "umol/m^2/s")
  
  # Carbon costs ----
  tl <- tealeaves::energy_balance(T_leaf, lp, ep, cnstnts, quiet = TRUE, 
                                  components = TRUE)
  C_cost <- carbon_costs$H2O * set_units(tl$components$E, "umol/m^2/s")
  
  # Carbon balance ----
  C_gain - C_cost
  
}






