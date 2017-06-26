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
#' \code{R_air} \tab specific gas constant for dry air \tab J / (kg K) \tab 287.058\cr
#' \code{eT} \tab exponent for temperature dependence of diffusion \tab none? \tab 1.75\cr
#' \code{Nu} \tab Nusselt number \tab none \tab *\cr
#' \code{D_m0} \tab diffusion coefficient for momentum in air at 0 C \tab m\eqn{^2} s\eqn{^{-1}} \tab 13.3\cr
#' \code{t_air} \tab coefficient of thermal expansion of air \tab none? \tab 3.66e-3\cr
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
#' @importFrom dplyr select

make_leafpar <- function(replace = NULL, traits = NULL) {

  ##### Defaults -----
  obj <- list(abs_s = 0.8,
              abs_l = 0.97,
              g_xc = 1,
              g_ic = 1,
              g_uw = 0.01,
              k_x = 1,
              V_cmax = 50,
              J_max = 100,
              R_d = 2,
              K_c = 268.3,
              K_o = 165084.2,
              gamma_star = 37.3,
              g_sw = 0.5,
              leafsize = 0.1,
              sr = 1)

  ##### Replace defaults -----
  obj %<>% replace_defaults(replace)

  ##### Remove traits to be optimized -----
  for (i in traits) obj[[i]] <- NULL

  ##### Check values ------
  stopifnot(obj$abs_s >= 0 & obj$abs_s <= 1)
  stopifnot(obj$abs_l >= 0 & obj$abs_l <= 1)
  stopifnot(obj$g_xc >= 0)
  stopifnot(obj$g_ic >= 0)
  stopifnot(obj$k_x >= 0)
  stopifnot(obj$V_cmax >= 0)
  stopifnot(obj$J_max >= 0)
  stopifnot(obj$R_d >= 0)
  stopifnot(obj$K_c >= 0)
  stopifnot(obj$K_o >= 0)
  stopifnot(obj$gamma_star >= 0)

  ##### Assign class and return -----
  class(obj) <- "leaf_par"

  obj

}

#' make_enviropar
#' @rdname make_parameters
#' @export
#' @importFrom magrittr %<>% %>%

make_enviropar <- function(replace = NULL) {

  ##### Defaults -----
  obj <- list(T_air = 298.15,
              RH = 0.50,
              R_sw = 1000,
              R_lw = 825,
              wind = 2,
              C_air = 400,
              P = 101.3246,
              O = 210000)

  ##### Replace defaults -----

  obj %<>% replace_defaults(replace)

  ##### Check values ------
  stopifnot(obj$T_air >= 0)
  stopifnot(obj$RH >= 0 & obj$RH <= 1)
  stopifnot(obj$R_sw >= 0)
  stopifnot(obj$R_lw >= 0)
  stopifnot(obj$wind >= 0)
  stopifnot(obj$C_air >= 0)
  stopifnot(obj$P >= 0)
  stopifnot(obj$O >= 0)

  ##### Assign class and return -----
  class(obj) <- "enviro_par"

  obj

}

#' make_constants
#' @rdname make_parameters
#' @export
#' @importFrom magrittr %<>% %>%

make_constants <- function(replace = NULL) {

  ##### Defaults -----
  obj <- list(thetaJ = 0.86,
              phi = 0.25,
              s = 5.67e-08,
              R_air = 287.058,
              eT = 1.75,
              nu_constant = function(Re, type, T_air, T_leaf, surface) {

                type %<>% match.arg(c("free", "forced"))

                if (identical(type, "forced")) {
                  if (Re <= 4000) ret <- list(a = 0.6, b = 0.5)
                  if (Re > 4000) ret <- list(a = 0.032, b = 0.8)
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
              D_h0 = 1.9e-5,
              D_m0 = 13.3,
              D_w0 = 21.2,
              t_air = 3.66e-3,
              G = 9.8,
              sh_constant = function(type) {

                type %<>% match.arg(c("free", "forced"))

                if (type == "forced") return(0.33)
                if(type == "free") return(0.25)

              },
              c_p = 1.01)

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
  stopifnot(obj$T_air >= 0)
  stopifnot(obj$RH >= 0 & obj$RH <= 1)
  stopifnot(obj$R_sw >= 0)
  stopifnot(obj$R_lw >= 0)
  stopifnot(obj$wind >= 0)
  stopifnot(obj$C_air >= 0)
  stopifnot(obj$P >= 0)
  stopifnot(obj$O >= 0)

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
