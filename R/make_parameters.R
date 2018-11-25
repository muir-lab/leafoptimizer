#' Make lists of parameters for \code{optimize_leaf}
#'
#' @param constants A list of physical constants. This can be generated using the \code{make_constants} function. This is needed to ensure that CO2 and H2O conductances are internally consistent with each other.
#' @param replace A named list of parameters to replace defaults. If \code{NULL}, defaults will be used.
#' @param default_to A character string, either 'photosynthesis' or 'tealeaves', to indicate which package's default parameter to use when they conflict. Default to 'photosynthesis'.
#' @param quiet Logical. Should messages about parameters be displayed?
#' 
#' @name make_parameters
#' 
#' @encoding UTF-8

NULL

#' make_leafpar
#' @rdname make_parameters
#'
#' @return 
#' 
#' \code{make_leafpar}: An object inheriting from class \code{\link{leaf_par}}\cr
#' \code{make_enviropar}: An object inheriting from class \code{\link{enviro_par}}\cr
#' \code{make_bakepar}: An object inheriting from class \code{\link{bake_par}}\cr
#' \code{make_constants}: An object inheriting from class \code{\link{constants}}
#'
#' @details
#'
#' \bold{Leaf parameters:}
#'
#' \tabular{lllll}{
#' \emph{Symbol} \tab \emph{R} \tab \emph{Description} \tab \emph{Units} \tab \emph{Default}\cr
#' \eqn{d} \tab \code{leafsize} \tab leaf characteristic dimension \tab m \tab 0.1 \cr
#' \eqn{\Gamma*} \tab \code{gamma_star} \tab chloroplastic CO2 compensation point (T_leaf) \tab Pa \tab \link[=bake]{calculated} \cr
#' \eqn{\Gamma*_{25}}{\Gamma_25*} \tab \code{gamma_star25} \tab chloroplastic CO2 compensation point (25 °C) \tab Pa \tab 3.743 \cr
#' \eqn{g_\mathrm{mc}}{g_mc} \tab \code{g_mc} \tab mesophyll conductance to CO2 (T_leaf) \tab \eqn{\mu}mol CO2 / (m\eqn{^2} s Pa) \tab \link[=bake]{calculated} \cr
#' \eqn{g_\mathrm{mc}}{g_mc} \tab \code{g_mc25} \tab mesophyll conductance to CO2 (25 °C) \tab \eqn{\mu}mol CO2 / (m\eqn{^2} s Pa) \tab 4 \cr
#' \eqn{g_\mathrm{sc}}{g_sc} \tab \code{g_sc} \tab stomatal conductance to CO2 \tab \eqn{\mu}mol CO2 / (m\eqn{^2} s Pa) \tab 4 \cr
#' \eqn{g_\mathrm{uc}}{g_uc} \tab \code{g_uc} \tab cuticular conductance to CO2 \tab \eqn{\mu}mol CO2 / (m\eqn{^2} s Pa) \tab 0.1 \cr
#' \eqn{J_\mathrm{max25}}{J_max25} \tab \code{J_max25} \tab potential electron transport (25 °C) \tab \eqn{\mu}mol CO2 / (m\eqn{^2} s) \tab 200 \cr
#' \eqn{J_\mathrm{max}}{J_max} \tab \code{J_max} \tab potential electron transport (T_leaf) \tab \eqn{\mu}mol CO2 / (m\eqn{^2} s) \tab \link[=bake]{calculated} \cr
#' \eqn{k_\mathrm{mc}}{k_mc} \tab \code{k_mc} \tab partition of \eqn{g_\mathrm{mc}}{g_mc} to lower mesophyll \tab none \tab 1 \cr
#' \eqn{k_\mathrm{sc}}{k_sc} \tab \code{k_sc} \tab partition of \eqn{g_\mathrm{sc}}{g_sc} to lower surface \tab none \tab 1 \cr
#' \eqn{k_\mathrm{uc}}{k_uc} \tab \code{k_uc} \tab partition of \eqn{g_\mathrm{uc}}{g_uc} to lower surface \tab none \tab 1 \cr
#' \eqn{K_\mathrm{C25}}{K_C25} \tab \code{K_C25} \tab Michaelis constant for carboxylation (25 °C) \tab \eqn{\mu}mol / mol \tab 268.3 \cr
#' \eqn{K_\mathrm{C}}{K_C} \tab \code{K_C} \tab Michaelis constant for carboxylation (T_leaf) \tab \eqn{\mu}mol / mol \tab \link[=bake]{calculated} \cr
#' \eqn{K_\mathrm{O25}}{K_O25} \tab \code{K_O25} \tab Michaelis constant for oxygenation (25 °C) \tab \eqn{\mu}mol / mol \tab 165084.2\cr
#' \eqn{K_\mathrm{O}}{K_O} \tab \code{K_O} \tab Michaelis constant for oxygenation (T_leaf) \tab \eqn{\mu}mol / mol \tab \link[=bake]{calculated} \cr
#' \eqn{\phi} \tab \code{phi} \tab initial slope of the response of J to PPFD \tab none \tab 0.331 \cr
#' \eqn{R_\mathrm{d25}}{R_d25} \tab \code{R_d25} \tab nonphotorespiratory CO2 release (25 °C) \tab \eqn{\mu}mol CO2 / (m\eqn{^2} s) \tab 2 \cr
#' \eqn{R_\mathrm{d}}{R_d} \tab \code{R_d} \tab nonphotorespiratory CO2 release (T_leaf) \tab \eqn{\mu}mol CO2 / (m\eqn{^2} s) \tab \link[=bake]{calculated} \cr
#' \eqn{\theta_J} \tab \code{theta_J} \tab curvature factor for light-response curve \tab none \tab 0.825 \cr
#' \eqn{T_\mathrm{leaf}}{T_leaf} \tab \code{T_leaf} \tab leaf temperature \tab K \tab 298.15 \cr
#' \eqn{V_\mathrm{c,max25}}{V_c,max25} \tab \code{V_cmax25} \tab maximum rate of carboxylation (25 °C) \tab \eqn{\mu}mol CO2 / (m\eqn{^2} s) \tab 150 \cr
#' \eqn{V_\mathrm{c,max}}{V_c,max} \tab \code{V_cmax} \tab maximum rate of carboxylation (T_leaf) \tab \eqn{\mu}mol CO2 / (m\eqn{^2} s) \tab \link[=bake]{calculated} \cr
#' \eqn{V_\mathrm{tpu25}}{V_tpu25} \tab \code{V_tpu25} \tab rate of triose phosphate utilisation (25 °C) \tab \eqn{\mu}mol CO2 / (m\eqn{^2} s) \tab 200 \cr
#' \eqn{V_\mathrm{tpu}}{V_tpu} \tab \code{V_tpu} \tab rate of triose phosphate utilisation (T_leaf) \tab \eqn{\mu}mol CO2 / (m\eqn{^2} s) \tab \link[=bake]{calculated}
#' }
#'
#' \bold{Environment parameters:}
#'
#' \tabular{lllll}{
#' \emph{Symbol} \tab \emph{R} \tab \emph{Description} \tab \emph{Units} \tab \emph{Default}\cr
#' \eqn{C_\mathrm{air}}{C_air} \tab \code{C_air} \tab atmospheric CO2 concentration \tab Pa \tab 41 \cr
#' \eqn{O} \tab \code{O} \tab atmospheric O2 concentration \tab kPa \tab 21.27565 \cr
#' \eqn{P} \tab \code{P} \tab atmospheric pressure \tab kPa \tab 101.3246 \cr
#' PPFD \tab \code{PPFD} \tab photosynthetic photon flux density \tab \eqn{\mu}mol quanta / (m^2 s) \tab 1500 \cr
#' \eqn{\mathrm{RH}}{RH} \tab \code{RH} \tab relative humidity \tab none \tab 0.50 \cr
#' \eqn{T_\mathrm{air}}{T_air} \tab \code{T_air} \tab air temperature \tab K \tab 298.15 \cr
#' \eqn{u} \tab \code{wind} \tab windspeed \tab m / s \tab 2
#' }
#'
#' \bold{Baking (i.e. temperature response) parameters:}
#'
#' \tabular{lllll}{
#' \emph{Symbol} \tab \emph{R} \tab \emph{Description} \tab \emph{Units} \tab \emph{Default}\cr
#' \eqn{D_\mathrm{s,gmc}}{Ds_gmc} \tab \code{Ds_gmc} \tab empirical temperature response parameter \tab J / (mol K) \tab 487.29 \cr
#' \eqn{D_\mathrm{s,Jmax}}{Ds_Jmax} \tab \code{Ds_Jmax} \tab empirical temperature response parameter \tab J / (mol K) \tab 388.04 \cr
#' \eqn{E_\mathrm{a,\Gamma *}}{Ea_gammastar} \tab \code{Ea_gammastar} \tab empirical temperature response parameter \tab J / mol \tab 24459.97 \cr
#' \eqn{E_\mathrm{a,gmc}}{Ea_gmc} \tab \code{Ea_gmc} \tab empirical temperature response parameter \tab J / mol \tab 68901.56 \cr
#' \eqn{E_\mathrm{a,Jmax}}{Ea_Jmax} \tab \code{Ea_Jmax} \tab empirical temperature response parameter \tab J / mol \tab 56095.18 \cr
#' \eqn{E_\mathrm{a,KC}}{Ea_KC} \tab \code{Ea_KC} \tab empirical temperature response parameter \tab J / mol \tab 80989.78 \cr
#' \eqn{E_\mathrm{a,KO}}{Ea_KO} \tab \code{Ea_KO} \tab empirical temperature response parameter \tab J / mol \tab 23719.97 \cr
#' \eqn{E_\mathrm{a,Rd}}{Ea_Rd} \tab \code{Ea_Rd} \tab empirical temperature response parameter \tab J / mol \tab 40446.75 \cr
#' \eqn{E_\mathrm{a,Vcmax}}{Ea_Vcmax} \tab \code{Ea_Vcmax} \tab empirical temperature response parameter \tab J / mol \tab 52245.78 \cr
#' \eqn{E_\mathrm{d,gmc}}{Ed_gmc} \tab \code{Ed_gmc} \tab empirical temperature response parameter \tab J / mol \tab 148788.56 \cr
#' \eqn{E_\mathrm{d,Jmax}}{Ed_Jmax} \tab \code{Ed_Jmax} \tab empirical temperature response parameter \tab J / mol \tab 121244.79
#' }
#' 
#' \bold{Constants:}
#' \tabular{lllll}{
#' \emph{Symbol} \tab \emph{R} \tab \emph{Description} \tab \emph{Units} \tab \emph{Default}\cr
#' \eqn{D_{c,0}}{D_c0} \tab \code{D_c0} \tab diffusion coefficient for CO2 in air at 0 °C \tab m\eqn{^2} / s \tab 12.9 \cr
#' \eqn{D_{h,0}}{D_h0} \tab \code{D_h0} \tab diffusion coefficient for heat in air at 0 °C \tab m\eqn{^2} / s \tab 1.9e-5 \cr
#' \eqn{D_{m,0}}{D_m0} \tab \code{D_m0} \tab diffusion coefficient for momentum in air at 0 °C \tab m\eqn{^2} / s \tab 13.3e-06 \cr
#' \eqn{\epsilon} \tab \code{epsilon} \tab ratio of water to air molar masses \tab none \tab 0.622 \cr
#' \eqn{G} \tab \code{G} \tab gravitational acceleration \tab m / s\eqn{^2} \tab 9.8 \cr
#' \eqn{eT} \tab \code{eT} \tab exponent for temperature dependence of diffusion \tab none \tab 1.75 \cr
#' \eqn{R} \tab \code{R} \tab ideal gas constant \tab J / (mol K) \tab 8.3144598 \cr
#' \eqn{\sigma} \tab \code{s} \tab Stephan-Boltzmann constant \tab W / (m\eqn{^2} K\eqn{^4}) \tab 5.67e-08 \cr
#' \eqn{Sh} \tab \code{Sh} \tab Sherwood number \tab none \tab \link[=.get_sh]{calculated}
#' }
#'
#' @references 
#' 
#' Buckley TN and Diaz-Espejo A. 2015. Partitioning changes in photosynthetic rate into contributions from different variables. Plant, Cell & Environment 38: 1200-11.
#' 
#' @examples 
#' constants <- make_constants()
#' leaf_par <- make_leafpar(constants)
#' enviro_par <- make_enviropar()
#' bake_par <- make_bakepar()
#' 
#' leaf_par <- make_leafpar(constants,
#'   replace = list(
#'     g_sc = set_units(3, "umol/m^2/s/Pa"),
#'     V_cmax25 = set_units(100, "umol/m^2/s")
#'   )
#' )
#' 
#' @export

make_leafpar <- function(constants, replace = NULL, 
                         default_to = "photosynthesis", quiet = FALSE) {
  
  constants %<>% constants()
  
  default_to %<>% match.arg(c("photosynthesis", "tealeaves"))
  
  # Combine defaults -----
  tl_leafpar <- tealeaves::make_leafpar()
  ph_leafpar <- photosynthesis::make_leafpar()
  ph_leafpar[["T_leaf"]] <- NULL
  obj <- combine_defaults(tl_leafpar, ph_leafpar, default_to, quiet)
  
  # Replace defaults ----
  obj %<>% replace_defaults(replace)
  
  # Harmonize conductances ----
  if (default_to == "photosynthesis") {
    obj$g_sw <- gc2gw(obj$g_sc, constants$D_c0, constants$D_w0)
    obj$g_uw <- gc2gw(obj$g_uc, constants$D_c0, constants$D_w0)
  } else {
    obj$g_sc <- gw2gc(obj$g_sw, constants$D_c0, constants$D_w0)
    obj$g_uc <- gw2gc(obj$g_uw, constants$D_c0, constants$D_w0)
  }
  
  # Notify if g_sw or g_sc are in replace ----
  if (("g_sw" %in% replace | "g_sc" %in% replace) & !quiet) {
    glue::glue("{x1} automatically converted from {x2} based on constants D_c0 and D_w0",
               x1 = switch(default_to,
                           photosynthesis = "g_sw",
                           tealeaves = "g_sc"),
               x2 = switch(default_to,
                           photosynthesis = "g_sc",
                           tealeaves = "g_sw")) %>%
      message()
  }
  
  # Notify if g_uw or g_uc are in replace ----
  if (("g_uw" %in% replace | "g_uc" %in% replace) & !quiet) {
    glue::glue("{x1} automatically converted from {x2} based on constants D_c0 and D_w0",
               x1 = switch(default_to,
                           photosynthesis = "g_uw",
                           tealeaves = "g_uc"),
               x2 = switch(default_to,
                           photosynthesis = "g_uc",
                           tealeaves = "g_uw")) %>%
      message()
  }
  
  # Assign class and return -----
  obj %<>% leaf_par()
  
  obj
  
}

#' make_enviropar
#' @rdname make_parameters
#' @export

make_enviropar <- function(replace = NULL, default_to = "photosynthesis", 
                           quiet = FALSE) {
  
  default_to %<>% match.arg(c("photosynthesis", "tealeaves"))
  
  # Combine defaults -----
  tl_enviropar <- tealeaves::make_enviropar()
  ph_enviropar <- photosynthesis::make_enviropar()
  obj <- combine_defaults(tl_enviropar, ph_enviropar, default_to, quiet)
  
  # Add new leafoptimizer-specific parameters
  obj$f_par = set_units(0.5)
  obj$E_q = set_units(220, "kJ/mol")
  
  # Replace defaults ----
  obj %<>% replace_defaults(replace)

  # Harmonize PPFD AND S_sw ----
  if (default_to == "photosynthesis") {
    obj$S_sw <- ppfd2sun(obj$PPFD, obj$f_par, obj$E_q)
  } else {
    obj$PPFD <- sun2ppfd(obj$S_sw, obj$f_par, obj$E_q)
  }
  
  # Notify if PPFD or S_sw are in replace ----
  if (("S_sw" %in% replace | "PPFD" %in% replace) & !quiet) {
    glue::glue("{x1} automatically converted from {x2} based on f_par and E_q",
               x1 = switch(default_to,
                           photosynthesis = "S_sw",
                           tealeaves = "PPFD"),
               x2 = switch(default_to,
                           photosynthesis = "PPFD",
                           tealeaves = "S_sw")) %>%
      message()
  }
  
  # Assign class and return ----
  obj %<>% enviro_par()
  
  obj
  
}

#' make_bakepar
#' @rdname make_parameters
#' @export

make_bakepar <- function(replace = NULL) {
  
  photosynthesis::make_bakepar(replace)

}

#' make_constants
#' @rdname make_parameters
#' @export

make_constants <- function(replace = NULL, default_to = "photosynthesis",
                           quiet = FALSE) {
  
  default_to %<>% match.arg(c("photosynthesis", "tealeaves"))
  
  # Combine defaults -----
  tl_constants <- tealeaves::make_constants()
  ph_constants <- photosynthesis::make_constants()
  obj <- combine_defaults(tl_constants, ph_constants, default_to, quiet)
  
  # Replace defaults -----
  if ("nu_constant" %in% names(replace)) {
    stopifnot(is.function(replace$nu_constant))
    obj$nu_constant <- replace$nu_constant
    replace$nu_constant <- NULL
  }
  
  if ("sh_constant" %in% names(replace)) {
    stopifnot(is.function(replace$sh_constant))
    obj$sh_constant <- replace$sh_constant
    replace$sh_constant <- NULL
  }
  
  obj %<>% replace_defaults(replace)
  
  # Assign class and return -----
  obj %<>% constants()
  
  obj
  
}

#' Combine default parameters
#' 
#' @param tl_pars List of {tealeaves} parameters.
#' @param ph_pars List of {photosynthesis} parameters.
#' @inheritParams make_parameters

combine_defaults <- function(tl_pars, ph_pars, default_to, quiet) {
  
  stopifnot(identical(class(tl_pars), class(ph_pars)))
  
  shared_pars <- intersect(names(tl_pars), names(ph_pars))
  
  tl_fxns <- tl_pars %>%
    purrr::map(class) %>%
    magrittr::equals("function") %>%
    which() %>%
    names()
  
  ph_fxns <- ph_pars %>%
    purrr::map(class) %>%
    magrittr::equals("function") %>%
    which() %>%
    names()
  
  shared_fxns <- intersect(tl_fxns, ph_fxns)
  
  shared_pars %<>% setdiff(shared_fxns)
  
  defaults_identical <- purrr::map_lgl(
    shared_pars, function(x) identical(tl_pars[x], ph_pars[x])
  )
  
  if (any(!defaults_identical) & !quiet) {
    
    shared_pars %>%
      magrittr::extract(!defaults_identical) %>%
      tl_pars[.] %>%
      cbind(ph_pars[names(.)]) %>%
      as.data.frame() %>%
      tibble::rownames_to_column("par") %>%
      magrittr::set_colnames(
        switch(default_to,
               photosynthesis = c("par", "tealeaves (from)", "photosynthesis (to)"),
               tealeaves = c("par", "photosynthesis (from)", "tealeaves (to)"))
      ) %>%
      print()

  }
  
  if (default_to == "photosynthesis") {
    ph_pars %<>% c(tl_pars[!(names(tl_pars) %in% names(.))])
    return(ph_pars)
  } else {
    tl_pars %<>% c(ph_pars[!(names(ph_pars) %in% names(.))])
    return(tl_pars)
  }

}

#' Replace default parameters
#'
#' @param obj List of default values
#' @param replace List of replacement values
#'

replace_defaults <- function(obj, replace) {
  
  if (!is.null(replace)) {
    
    stopifnot(is.list(replace))
    stopifnot(all(sapply(replace, inherits, what = "units")))
    stopifnot(all(sapply(replace, is.numeric)))
    x <- names(replace)
    if (any(!x %in% names(obj))) {
      warning(sprintf("The following parameters in 'replace' were not recognized:\n%s", paste0(x[!x %in% names(obj)], collapse = "\n")))
      x %<>% .[. %in% names(obj)]
    }
    obj[x] <- replace[x]
    
  }
  
  obj
  
}
