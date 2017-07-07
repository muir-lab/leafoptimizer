#' Make lists of parameters for \code{evolve_leaf}
#'
#' @inheritParams evolve_leaf
#' @param replace A named list of parameters to replace defaults. If \code{NULL}, defaults will be used.
#'
#' @name make_parameters

NULL

#' make_leafpar
#' @rdname make_parameters
#'
#' @return An object inheriting from class leaf_par.
#'
#' @details
#'
#' \bold{Leaf parameters:}
#'
#' \tabular{llll}{
#' \emph{R} \tab \emph{Description} \tab \emph{Units} \tab \emph{Default}\cr
#' \code{abs_s} \tab absortivity of shortwave radiation (0.3 - 4 \eqn{\mu}m) \tab none \tab 0.80\cr
#' \code{abs_l} \tab absortivity of longwave radiation (4 - 80 \eqn{\mu}m) \tab none \tab 0.97\cr
#' \code{g_uw} \tab cuticular conductance to CO2 \tab mol H2O m\eqn{^{-2}} s\eqn{^{-1}} \tab 0.01\cr
#' \code{g_xc} \tab intercellular conductance to CO2 \tab mol CO2 m\eqn{^{-2}} s\eqn{^{-1}} \tab 1\cr
#' \code{g_ic} \tab intracellular conductance to CO2 \tab mol CO2 m\eqn{^{-2}} s\eqn{^{-1}} \tab 1\cr
#' \code{k_x} \tab partition of gx to spongy mesophyll \tab none \tab 1\cr
#' \code{V_cmax} \tab maximum rate of carboxylation \tab mol CO2 m\eqn{^{-2}} s\eqn{^{-1}} \tab 50\cr
#' \code{J_max} \tab potential electron transport \tab mol CO2 m\eqn{^{-2}} s\eqn{^{-1}} \tab 100\cr
#' \code{R_d} \tab Mitochondrial (CHECK) Respiration \tab mol CO2 m\eqn{^{-2}} s\eqn{^{-1}} \tab 2\cr
#' \code{K_c} \tab Michaelis constant for carboxylation \tab \eqn{\mu}mol mol\eqn{^{-1}} \tab 268.3\cr
#' \code{K_o} \tab Michaelis constant for oxygenation \tab \eqn{\mu}mol mol\eqn{^{-1}} \tab 165084.2\cr
#' \code{gamma_star} \tab Chloroplastic CO2 compensation point \tab \eqn{\mu}mol CO2 mol\eqn{^{-1} air} \tab 37.3\cr
#' }
#'
#' \bold{Environment parameters:}
#'
#' \tabular{llll}{
#' \emph{R} \tab \emph{Description} \tab \emph{Units} \tab \emph{Default}\cr
#' \code{T_air} \tab air temperature \tab K \tab 298.15\cr
#' \code{RH} \tab relative humidity \tab \% \tab 0.50\cr
#' \code{R_sw} \tab incident short-wave (solar) radiation flux density \tab W m\eqn{^{-2}} \tab 1000\cr
#' \code{R_lw} \tab incident long-wave radiation flux density \tab W m\eqn{^{-2}} \tab 825\cr
#' \code{wind} \tab windspeed \tab m s\eqn{^{-1}} \tab 2\cr
#' \code{C_air} \tab atmospheric CO2 concentration \tab \eqn{\mu}mol CO2 mol\eqn{^{-1}} air \tab 400\cr
#' \code{P} \tab atmospheric pressure \tab kPa \tab 101.3246\cr
#' \code{O} \tab atmospheric O2 concentration \tab \eqn{\mu}mol O2 mol\eqn{^{-1}} air \tab 210000\cr
#' }
#'
#' \bold{Constants:}
#' \tabular{llll}{
#' \code{thetaJ} \tab curvature factor for light-response curve \tab none \tab 0.86\cr
#' \code{phi} \tab effective maximum quantum yield of electrons from incident irradiance \tab e- / hv \tab 0.25\cr
#' \code{s} \tab Stephan-Boltzmann constant \tab W m\eqn{^{-2}} K\eqn{^{-4}} \tab 5.67e-08\cr
#' \code{R} \tab ideal gas constant \tab J / (mol K) \tab 8.3144598\cr
#' \code{R_air} \tab specific gas constant for dry air \tab J / (kg K) \tab 287.058\cr
#' \code{eT} \tab exponent for temperature dependence of diffusion \tab none? \tab 1.75\cr
#' \code{Nu} \tab Nusselt number \tab none \tab *\cr
#' \code{D_m0} \tab diffusion coefficient for momentum in air at 0 C \tab m\eqn{^2} s\eqn{^{-1}} \tab 13.3 \times 10\eqn{^{-6}}\cr
#' \code{t_air} \tab coefficient of thermal expansion of air \tab 1 / K \tab 3.66e-3\cr
#' \code{G} \tab gravitational acceleration \tab m s\eqn{^{-2}} \tab 9.8\cr
#' \code{Sh} \tab Sherwood number \tab none \tab *\cr
#' \code{D_h0} \tab diffusion coefficient for heat in air at 0 C \tab m\eqn{^2} s\eqn{^{-1}} \tab 1.9e-5\cr
#' \code{D_w0} \tab diffusion coefficient for water vapour in air at 0 C \tab m\eqn{^2} s\eqn{^{-1}} \tab 21.2\cr
#' \code{c_p} \tab heat capacity of air \tab J g\eqn{^{-1}} K\eqn{^{-1}} \tab 1.01\cr
#' }
#'
#' * see manual for further detail on calculation
#'
#' @export
#' @importFrom magrittr %<>% %>%
#' @importFrom units kPa m mol Pa s set_units umol unitless

make_leafpar <- function(replace = NULL, traits = NULL) {

  ##### Defaults -----
  obj <- list(abs_s = set_units(0.8, unitless),
              abs_l = set_units(0.97, unitless),
              g_xc = set_units(10, umol / (m^2 * s * Pa)), # CHECK DEFAULT in Pa^-1
              g_ic = set_units(10, umol / (m^2 * s * Pa)), # CHECK DEFAULT in Pa^-1
              g_uw = set_units(0.1, umol / (m^2 * s * Pa)), # CHECK DEFAULT in Pa^-1
              k_x = set_units(1, unitless),
              V_cmax = set_units(50, umol / (m^2 * s)),
              J_max = set_units(100, umol / (m^2 * s)),
              R_d = set_units(2, umol / (m^2 * s)),
              K_c = set_units(27.238, Pa), # From Sharkey et al. 2007. Newer source? Check bayCi
              K_o = set_units(16.582, kPa), # From Sharkey et al. 2007. Newer source? Check bayCi
              gamma_star = set_units(3.73, Pa), # From Sharkey et al. 2007. Newer source? Check bayCi
              g_sw = set_units(5, umol / (m^2 * s * Pa)), # CHECK DEFAULT in Pa^-1
              leafsize = set_units(0.1, m),
              sr = set_units(1, unitless))

  ##### Replace defaults -----
  obj %<>% replace_defaults(replace)

  ##### Check values ------
  stopifnot(obj$abs_s >= set_units(0, unitless) & obj$abs_s <= set_units(1, unitless))
  stopifnot(obj$abs_l >= set_units(0, unitless) & obj$abs_l <= set_units(1, unitless))
  stopifnot(obj$g_xc >= set_units(0, mol / (m^2 * s * Pa)))
  stopifnot(obj$g_ic >= set_units(0, mol / (m^2 * s * Pa)))
  stopifnot(obj$k_x >= set_units(0, unitless))
  stopifnot(obj$V_cmax >= set_units(0, umol / (m^2 * s)))
  stopifnot(obj$J_max >= set_units(0, umol / (m^2 * s)))
  stopifnot(obj$R_d >= set_units(0, umol / (m^2 * s)))
  stopifnot(obj$K_c >= set_units(0, Pa))
  stopifnot(obj$K_o >= set_units(0, kPa))
  stopifnot(obj$gamma_star >= set_units(0, Pa))
  stopifnot(obj$g_sw >= set_units(0, umol / (m^2 * s * Pa)))
  stopifnot(obj$leafsize >= set_units(0, m))
  stopifnot(obj$sr >= set_units(0, unitless))
  
  ##### Remove traits to be optimized -----
  for (i in traits) obj[[i]] <- NULL
  
  ##### Assign class and return -----
  class(obj) <- "leaf_par"

  obj

}

#' make_enviropar
#' @rdname make_parameters
#' @export
#' @importFrom magrittr %<>% %>%
#' @importFrom units K mset_units umol unitless W


make_enviropar <- function(replace = NULL) {

  ##### Defaults -----
  obj <- list(T_air = set_units(298.15, K),
              RH = set_units(0.50, unitless),
              R_sw = set_units(1000, W / m^2),
              R_lw = set_units(825, W / m^2),
              wind = set_units(2, m / s),
              C_air = set_units(4e-4, unitless), # in proportion
              P = set_units(101.3246, kPa),
              O = set_units(0.21, unitless)) # in proportion
  
  ##### Replace defaults -----

  obj %<>% replace_defaults(replace)

  ##### Check values ------
  stopifnot(obj$T_air >= set_units(0, K))
  stopifnot(obj$RH >= set_units(0, unitless) & obj$RH <= set_units(1, unitless))
  stopifnot(obj$R_sw >= set_units(0, W / m^2))
  stopifnot(obj$R_lw >= set_units(0, W / m^2))
  stopifnot(obj$wind >= set_units(0, m / s))
  stopifnot(obj$C_air >= set_units(0, unitless) & obj$C_air <= set_units(1, unitless))
  stopifnot(obj$P >= set_units(0, kPa))
  stopifnot(obj$O >= set_units(0, unitless) & obj$O <= set_units(1, unitless))

  ##### Assign class and return -----
  class(obj) <- "enviro_par"

  obj

}

#' make_constants
#' @rdname make_parameters
#' @export
#' @importFrom magrittr %<>% %>%
#' @importFrom units g J K kg m s set_units unitless W

make_constants <- function(replace = NULL) {

  ##### Defaults -----
  obj <- list(thetaJ = set_units(0.86, unitless),
              phi = set_units(0.25, unitless), # Foster and Smith reported as e / hv
              s = set_units(5.67e-08, W / (m ^ 2 * K ^ 4)),
              R = set_units(8.3144598, J / (mol * K)),
              R_air = set_units(287.058, J / (kg * K)),
              eT = set_units(1.75, unitless),
              nu_constant = function(Re, type, T_air, T_leaf, surface) {

                stopifnot(units(T_air)$numerator == "K" & 
                            length(units(T_air)$denominator) == 0L)
                stopifnot(units(T_leaf)$numerator == "K" & 
                            length(units(T_leaf)$denominator) == 0L)

                type %<>% match.arg(c("free", "forced"))

                if (identical(type, "forced")) {
                  if (Re <= set_units(4000, unitless)) ret <- list(a = 0.6, b = 0.5)
                  if (Re > set_units(4000, unitless)) ret <- list(a = 0.032, b = 0.8)
                  return(ret)
                }

                if (identical(type, "free")) {
                  surface %<>% match.arg(c("lower", "upper"))
                  if ((surface == "upper" & T_leaf > T_air) |
                      (surface == "lower" & T_leaf < T_air)) {
                    ret <- list(a = 0.5, b = 0.25)
                  } else {
                    ret <- list(a = 0.23, b = 0.25)
                  }
                  return(ret)
                }

              },
              D_h0 = set_units(1.9e-5, m ^ 2 / s),
              D_m0 = set_units(13.3e-6, m ^ 2 / s),
              D_w0 = set_units(21.2e-6, m ^ 2 / s),
              t_air = set_units(3.66e-3, 1 / K),
              G = set_units(9.8, m / s ^ 2),
              sh_constant = function(type) {

                type %<>% match.arg(c("free", "forced"))

                if (type == "forced") return(0.33)
                if(type == "free") return(0.25)

              },
              c_p = set_units(1.01, J / (g * K)))

  ##### Replace defaults -----

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

  ##### Check values ------
  stopifnot(obj$thetaJ >= set_units(0, unitless) & obj$thetaJ <= set_units(1, unitless))
  stopifnot(obj$phi >= set_units(0, unitless) & obj$phi <= set_units(1, unitless))
  stopifnot(obj$s >= set_units(0, W / (m ^ 2 * K ^ 4)))
  stopifnot(obj$R >= set_units(0, J / (mol * K)))
  stopifnot(obj$R_air >= set_units(0, J / (kg * K)))
  stopifnot(obj$eT >= set_units(0, unitless))
  stopifnot(obj$D_h0 >= set_units(0, m ^ 2 / s))
  stopifnot(obj$D_m0 >= set_units(0, m ^ 2 / s))
  stopifnot(obj$D_w0 >= set_units(0, m ^ 2 / s))
  stopifnot(obj$t_air >= set_units(0, 1 / K))
  stopifnot(obj$G >= set_units(0, m / s ^ 2))
  stopifnot(obj$c_p >= set_units(0, J / (g * K)))

  ##### Assign class and return -----
  class(obj) <- "constants"

  obj

}

#' Traits missing from traits argument (i.e. traits to be fixed rather than optimized)
#'
#' @importFrom magrittr %<>% %>%
#' @inheritParams evolve_leaf
#'

.missing_traits <- function(traits) {

  possible_traits <- c("g_sw", "sr", "leafsize")
  if (length(traits) == 0) return(possible_traits)
  ret <- possible_traits %>%
    base::setdiff(., base::match.arg(traits, ., several.ok = TRUE))

  ret

}

#' Replace default parameters
#'
#' @param obj List of default values
#' @param replace List of replacement values
#'
#' @importFrom magrittr %<>% %>%
#'

replace_defaults <- function(obj, replace) {

  stopifnot(all(sapply(replace, inherits, what = "units")))
  stopifnot(all(sapply(replace, is.numeric)))
  stopifnot(all(sapply(replace, function(X) length(X) == 1)))

  if (!is.null(replace)) {
    x <- names(replace)
    if (any(!x %in% names(obj))) {
      warning(sprintf("The following parameters in 'replace' were not recognized:\n%s", paste0(x[!x %in% names(obj)], collapse = "\n")))
      x %<>% .[. %in% names(obj)]
    }
    for (i in x) obj[[i]] <- replace[[i]]

  }

  obj

}

# Don't think I need this:
mol_CO2 <- make_unit("mol CO2")
# Ymol_CO2 <- make_unit("Ymol CO2")
# Zmol_CO2 <- make_unit("Zmol CO2")
# Emol_CO2 <- make_unit("Emol CO2")
# Pmol_CO2 <- make_unit("Pmol CO2")
# Tmol_CO2 <- make_unit("Tmol CO2")
# Gmol_CO2 <- make_unit("Gmol CO2")
# Mmol_CO2 <- make_unit("Mmol CO2")
# kmol_CO2 <- make_unit("kmol CO2")
# hmol_CO2 <- make_unit("hmol CO2")
# damol_CO2 <- make_unit("damol CO2")
# dmol_CO2 <- make_unit("dmol CO2")
# cmol_CO2 <- make_unit("cmol CO2")
mmol_CO2 <- make_unit("mmol CO2")
umol_CO2 <- make_unit("umol CO2")
# nmol_CO2 <- make_unit("nmol CO2")

install_conversion_constant("mol CO2", "mmol CO2", 1e3)
install_conversion_constant("mol CO2", "umol CO2", 1e6)
install_conversion_constant("mmol CO2", "umol CO2", 1e3)

mol_O2 <- make_unit("mol O2")
mmol_O2 <- make_unit("mmol O2")
umol_O2 <- make_unit("umol O2")

install_conversion_constant("mol O2", "mmol O2", 1e3)
install_conversion_constant("mol O2", "umol O2", 1e6)
install_conversion_constant("mmol O2", "umol O2", 1e3)
