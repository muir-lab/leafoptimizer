#' Optimize leaf photosynthesis
#' 
#' \code{optimize_leaves}: optimize C3 photosynthesis over multiple parameter sets
#' 
#' @param traits A vector of one or more character strings indicating which trait(s) to optimize. Stomatal conductance (\code{g_sc}) and stomatal ratio (\code{logit_sr}) are currently supported.
#' 
#' @param carbon_costs A named list of resources with their costs in terms of carbon (e.g. mol C / mol H2O). Currently only H2O and SR are supported. See details below.
#' 
#' @param leaf_par A list of leaf parameters inheriting class \code{leaf_par}. This can be generated using the \code{make_leafpar} function.
#' 
#' @param enviro_par A list of environmental parameters inheriting class \code{enviro_par}. This can be generated using the \code{make_enviropar} function.
#' 
#' @param bake_par A list of temperature response parameters inheriting class \code{bake_par}. This can be generated using the \code{make_bakepar} function.
#' 
#' @param n_init Integer. Number of initial values for each trait to try during optimization. If there are multiple traits, these initial values are crossed. For example, if \code{n_init = 3}, the total number of intitial values sets is 3, 9, 27 for 1, 2, 3 traits, respectively. This significantly increases the time, but may be important if the surface is rugged. Default is 1L.
#' 
#' @param constants A list of physical constants inheriting class \code{constants}. This can be generated using the \code{make_constants} function.
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
#' # Single parameter set with 'photo'
#' 
#' bp <- make_bakepar()
#' cs <- make_constants()
#' ep <- make_enviropar()
#' lp <- make_leafpar()
#' traits <- "g_sc"
#' carbon_costs <- list(H2O = 1000, SR = 0)
#' optimize_leaf("g_sc", carbon_costs, lp, ep, bp, cs, n_init = 1L)
#' 
#' # Multiple parameter sets with 'photosynthesis'
#' 
#' ep <- make_enviropar(
#'   replace = list(
#'     T_air = set_units(c(293.14, 298.15), "K")
#'   )
#' )
#' optimize_leaves(traits, carbon_costs, lp, ep, bp, cs, n_init = 1L)
#' 
#' @encoding UTF-8
#' 
#' @export
#' 

optimize_leaves <- function(traits, carbon_costs, leaf_par, enviro_par, bake_par,
                            constants, n_init = 1L, progress = TRUE, 
                            quiet = FALSE, parallel = FALSE) {
  
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
                      par_units, n_init, progress, quiet, parallel)
  
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
                        par_units, n_init, progress, quiet, parallel) {
  
  if (!quiet) {
    glue::glue("\nOptimizing leaf trait{s1} from {n} parameter set{s2} ...", 
               s1 = ifelse(length(traits) > 1, "s", ""), n = length(pars), 
               s2 = dplyr::if_else(length(pars) > 1, "s", "")) %>%
      crayon::green() %>%
      message(appendLF = FALSE)
  }
  
  if (parallel) future::plan("multiprocess")
  
  if (progress & !parallel) pb <- dplyr::progress_estimated(length(pars))
  
  soln <- suppressWarnings(
    pars %>%
      furrr::future_map_dfr(~{
        
        ret <- optimize_leaf(traits, carbon_costs, leaf_par(.x), enviro_par(.x), 
                             bake_par, constants, n_init, quiet = TRUE)
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

#' Optimize C3 photosynthesis
#' @description \code{optimize_leaf}: simulate C3 photosynthesis over a single parameter set
#' @rdname optimize_leaves
#' 
#' #' @param check Logical. Should arguments checkes be done? This is intended to be disabled when \code{optimize_leaf} is called from \code{\link{optimize_leaves}} Default is TRUE.
#' 
#' @export

optimize_leaf <- function(traits, carbon_costs, bake_par, constants, enviro_par,
                          leaf_par, set_units = TRUE, n_init, check = TRUE,
                          quiet = FALSE) {

  checkmate::assert_flag(check)
  
  if (check) {
  
    checkmate::assert_flag(set_units)
    checkmate::assert_flag(quiet)
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
  
  traits %<>% 
    match.arg(c("g_sc", "leafsize", "sr"), TRUE) %>%
    sort()
  
  # Set units ----
  if (set_units) {
    bake_par %<>% photosynthesis::bake_par()
    constants %<>% leafoptimizer::constants()
    enviro_par %<>% leafoptimizer::enviro_par()
    leaf_par %<>% leafoptimizer::leaf_par()
  }
  
  # Concatenate parameters and drop units ----
  pars <- c(bake_par, constants, enviro_par, leaf_par)
  upars <- pars %>%
    purrr::map_if(~ is(.x, "units"), drop_units)
  
  # Find optimum ----
  soln <- find_optimum(g_sc = ("g_sc" %in% traits), 
                       leafsize = ("leafsize" %in% traits), 
                       sr = ("sr" %in% traits), 
                       carbon_costs, upars, n_init, quiet)
  
  # Check results ----
  check_results(soln)
  pars$carbon_balance <- -soln$value
    
  # Concatenate optimized traits in pars to calculate T_leaf, A, and E ----
  pars %<>% c_optimized_traits(traits, soln) 
  pars$S_sw <- set_units(pars[["PPFD"]] * pars[["E_q"]] / pars[["f_par"]], W/m^2)
  if (!("g_sc" %in% traits)) {
    pars$g_sw <- gc2gw(pars[["g_sc"]], pars[["D_c0"]], pars[["D_w0"]], 
                       unitless = FALSE) 
  }
  pars$g_uw <- gc2gw(pars[["g_uc"]], pars[["D_c0"]], pars[["D_w0"]], 
                     unitless = FALSE) 
  if (!("sr" %in% traits)) {
    pars$logit_sr <- stats::qlogis(pars[["k_sc"]] / 
                                     (set_units(1) + pars[["k_sc"]]))
  }
  
  # Calculate T_leaf, A, and E ----
  unitless_pars <- pars %>% purrr::map_if(~ is(.x, "units"), drop_units)
  unitless_pars$T_leaf <- unitless_pars %>% 
    find_tleaf(., . , .) %>%
    magrittr::use_series("T_leaf")
  pars$T_leaf <- set_units(unitless_pars$T_leaf, "K")

  ph <- unitless_pars %>% 
    c(photosynthesis::bake(., ., ., set_units = FALSE)) %>%
    find_A()

  pars$A <- set_units(ph$A, umol/m^2/s)
  pars$C_chl <- set_units(ph$C_chl, Pa)

  eb <- tealeaves::energy_balance(
    unitless_pars$T_leaf, unitless_pars,  unitless_pars, unitless_pars, 
    components = TRUE, set_units = FALSE
  )
  stopifnot(round(drop_units(eb$energy_balance), 1) == 0)
  
  pars %<>% c(eb$components)
  
  blp <- photosynthesis::bake(pars, pars, pars, set_units = TRUE)
  pars %<>% c(blp[!(names(blp) %in% names(.))])
  
  # Return ----
  keep <- names(pars)[pars %>%
    names() %>%
    magrittr::is_in(c(parameter_names("constants"), parameter_names("bake"))) %>%
    magrittr::not()]
  
  as.data.frame(pars[sort(keep)])

}

find_optimum <- function(g_sc, leafsize, sr, carbon_costs, upars, n_init,
                         quiet) {
  
  traits <- c("g_sc", "leafsize", "logit_sr")[c(g_sc, leafsize, sr)]
  
  # Initial values ----
  init <- get_init(traits, n_init)
  
  # Parameter bounds ----
  bounds <- get_bounds()
    
  if (!quiet) {
    glue::glue("\nOptimizing leaf trait{s} ...",
               s = ifelse(length(traits) > 1, "s", "")) %>%
      crayon::green() %>%
      message(appendLF = FALSE)
  }
  
  # Minimize carbon_balance() ----
  if (length(traits) == 1L) {
    
    soln <- purrr::map_dfr(init, function(.x, ...) {
      
      fit <- stats::optim(.x, carbon_balance, ...)
      
      soln <- fit$par %>%
        as.data.frame() %>%
        t() %>%
        as.data.frame() %>%
        magrittr::set_colnames(traits) %>%
        dplyr::mutate(value = fit$value, convergence = fit$convergence)
      
      soln
      
    }, find_gsc = g_sc, find_leafsize = leafsize, find_sr = sr,
    carbon_costs = carbon_costs, upars = upars,
    method = "Brent", lower = bounds$lower[traits], upper = bounds$upper[traits]
    )
    
    soln %<>% dplyr::top_n(-1, .data$value)
    
  } else {
    
    soln <- purrr::map_dfr(1:nrow(init), function(.x, ...) {
      
      fit <- optimx::optimx(init[.x,], carbon_balance, ...)
      
      soln <- fit %>%
        dplyr::filter(.data$value == min(.data$value)) %>%
        dplyr::select(c(traits, "value", convergence = "convcode"))
      
      soln
      
    }, find_gsc = g_sc, find_leafsize = leafsize, find_sr = sr,
    carbon_costs = carbon_costs, upars = upars, 
    method = "nlm", lower = bounds$lower[traits], upper = bounds$upper[traits]
    )
    
    soln %<>% dplyr::top_n(-1, .data$value)
    
  }
    
  if (!quiet) {
    " done" %>%
      crayon::green() %>%
      message()
  }
  
  soln
  
}

carbon_balance <- function(trait_values, find_gsc, find_leafsize, find_sr,
                           carbon_costs, upars) {
  
  if (find_gsc) {
    upars$g_sc <- trait_values[1]
    upars$g_sw <- gc2gw(upars$g_sc, upars$D_c0, upars$D_w0, unitless = TRUE)
  }
  
  if (find_leafsize) {
    upars$leafsize <- dplyr::if_else(find_gsc, trait_values[2], trait_values[1])
  }
  
  if (find_sr) {
    upars$logit_sr <- dplyr::last(trait_values)
    upars$k_sc <- upars$logit_sr %>%
      stats::plogis() %>%
      magrittr::divide_by(1 - .)
  }
  
  ph <- photosynthesis::photo(upars, upars, upars, upars, use_tealeaves = TRUE,
                              quiet = TRUE, set_units = TRUE, check = FALSE,
                              prepare_for_tleaf = TRUE)
  
  upars$g_sw <- drop_units(ph[["g_sw"]])
  upars$g_uw <- drop_units(ph[["g_uw"]])
  upars$logit_sr <- drop_units(ph[["logit_sr"]])
  
  E <- tealeaves::E(ph[["T_leaf"]], upars, unitless = TRUE)
  
  drop_units(ph[["g_sw"]] * stats::plogis(ph[["logit_sr"]]) * 
               carbon_costs[["SR"]]) +
    E * 1e6 - drop_units(carbon_costs[["H2O"]] * ph[["A"]])
  
}

get_init <- function(traits, n_init) {
  
  init <- tidyr::crossing(
    g_sc = seq(0, 10, length.out = n_init + 2L)[-c(1, n_init + 2L)], 
    leafsize = seq(0.0005, 0.4, length.out = n_init + 2L)[-c(1, n_init + 2L)],
    logit_sr = seq(-10, 10, length.out = n_init + 2L)[-c(1, n_init + 2L)]
  ) %>%
    dplyr::select(traits) %>%
    unique() %>%
    as.matrix()
  stopifnot(nrow(init) == n_init ^ length(traits))
  init
  
}

get_bounds <- function() {
  
  # Based on Wright et al 2017, leaf size varies from 0.01 cm ^ 2 to 5000 cm ^ 2
  # so minimum characteristic leaf dimension (radius of largest circle) is approximately:
  # sqrt(0.01 / pi) / 100 = 0.0005
  # sqrt(5000 / pi) / 100 = 0.4
  list(
    lower = c(g_sc = 0, leafsize = 0.0005, logit_sr = -10), 
    upper = c(g_sc = 10, leafsize = 0.4, logit_sr = 10)
  )
  
}
  
c_optimized_traits <- function(pars, traits, soln) {
  
  if ("g_sc" %in% traits) {
    pars$g_sc <- set_units(soln$g_sc, umol/m^2/s/Pa)
    pars$g_sw <- gc2gw(pars$g_sc, pars$D_c0, pars$D_w0, unitless = FALSE) 
  }
  
  if ("leafsize" %in% traits) {
    pars$leafsize <- set_units(soln$leafsize, m)
  }
  
  if ("sr" %in% traits) {
    pars$logit_sr <- set_units(soln$logit_sr)
    pars$k_sc <- pars$logit_sr %>%
      stats::plogis() %>%
      magrittr::divide_by(set_units(1) - .)
  }
  
  pars
  
}

find_tleaf <- function(leaf_par, enviro_par, constants) {

  # For this version, all parameters must arrive unitless  
  
  # Balance energy fluxes -----
  fit <- tryCatch({
    stats::uniroot(f = tealeaves::energy_balance, leaf_par = leaf_par, 
                   enviro_par = enviro_par, constants = constants, 
                   quiet = TRUE, set_units = FALSE,
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





