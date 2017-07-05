#' Find leaf temperature
#'
#' @inheritParams evolve_leaf
#'
#' @importFrom magrittr %<>% %>%
#'
#' @export
#'

find_Tleaf <- function(leaf_par, enviro_par, constants) {

  ##### Balance energy fluxes -----
  soln <- stats::optim(enviro_par$T_air, engery_balance, leaf_par = leaf_par,
                       enviro_par = enviro_par, constants = constants,
                       abs_val = TRUE, method = "Brent",
                       lower = enviro_par$T_air - 30,
                       upper = enviro_par$T_air + 30)

  ##### Check or report on convergence?? -----
  # ?

  ##### Return -----
  # return evaporation?
  T_leaf <- soln$par

}

#' Calculate leaf energy balance
#'
#' @inheritParams evolve_leaf
#' @param abs_val Return absolute value? Useful for finding leaf temperature that balances heat transfer.
#' @importFrom magrittr %<>% %>%
#'
#' @param T_leaf Leaf temperature in Kelvin
#'
#' @export
#'

engery_balance <- function(T_leaf, leaf_par, enviro_par, constants, abs_val = FALSE) {

  ##### Checks -----
  traits <- .missing_traits(character(0))
  check_leafpar(leaf_par, traits)
  check_enviropar(enviro_par)
  check_constants(constants)

  pars <- c(leaf_par, enviro_par, constants)

  ##### R_abs: total absorbed radiation (W m^-2) -----
  R_abs <- .get_Rabs(pars)

  ##### R_r: longwave re-radiation (W m^-2) -----
  R_r <- .get_Rr(pars)

  ##### H: sensible heat flux density (W m^-2) -----
  H <- .get_H(T_leaf, pars)

  ##### L: latent heat flux density (W m^-2) -----
  L <- .get_L(T_leaf, pars)

  ##### Return -----
  if (abs_val) return(abs(R_abs - (R_r + H + L)))
  R_abs - (R_r + H + L)

}

#' R_abs: total absorbed radiation (W m^-2)
#'
#' @param R_sw total incident shortwave (solar) radiation flux density
#' @param R_lw total incident longwave radiation flux density
#' @param abs_s absortivity of shortwave radiation
#' @param abs_l absortivity of longwave radiation
#'

.get_Rabs <- function(pars) {
  R_abs <- with(pars, abs_s * R_sw + abs_l * R_lw)
  R_abs
}

#' R_r: longwave re-radiation (W m^-2)
#'
#' @param T_air Air temperature in Kelvin
#' @param s Stephan-Boltzmann constant
#' @param abs_l leaf emissivity (e = abs_l)
#'

.get_Rr <- function(pars) pars$s * pars$abs_l * pars$T_air ^ 4

#' H: sensible heat flux density (W m^-2)
#'
#' @inheritParams .get_Rr
#' @param T_leaf Leaf temperature in Kelvin
#' @param RH Relative humidity
#' @param leafsize Leaf characteristic dimension in meters
#' @param wind Windspeed in m s^-1
#' @param P Atmospheric pressure in kPa
#' @param c_p Heat capacity of air in J g^-1 K^-1
#'

.get_H <- function(T_leaf, pars) {

  P_a <- .get_Pa(T_leaf, pars) # correct

  warning("g_h is several orders of magnitude too low and should be in units of m/s")
  g_h <- sum(.get_gh(T_leaf, "lower", pars), .get_gh(T_leaf, "upper", pars))

  H <- P_a * pars$c_p * g_h * (T_leaf - pars$T_air)
  H %<>% set_units(W / m ^ 2)
  H
  
}

#' P_a: density of dry air (g m^-3)
#'
#' @inheritParams .get_H
#' @param R_air Specific gas constant for dry air in J kg^-1 K^-1
#'

.get_Pa <- function(T_leaf, pars) {
  P_a <- pars$P / (pars$R_air * (pars$T_air + T_leaf) / 2)
  P_a %<>% set_units(g / m^3)
}

#' g_h: boundary layer conductance to heat (m s^-1)
#'
#' @inheritParams .get_H
#' @param surface Leaf surface (upper or lower)
#' @param D_h0 Diffusion coefficient of heat in air at 0C in m^2 s^-1
#' @param D_m0 Diffusion coefficient for momentum in air at 0C in m^2 s^-1
#' @param eT Exponent for temperature dependence of diffusion
#' @param G Gravitational acceleration in m s^-2
#' @param nu_constant Function to calculate Nusselt number constants
#' @param t_air Coefficient of thermal expansion of air in 1 / K
#'

.get_gh <- function(T_leaf, surface, pars) {

  # Calculate diffusion coefficient to heat
  D_h <- .get_Dx(pars$D_h0, (pars$T_air + T_leaf) / 2, pars$eT, pars$P)

  # Calculate Nusselt numbers
  Nu <- .get_nu(T_leaf, surface, pars)

  D_h * Nu / pars$leafsize

}

#' D_x: Calculate diffusion coefficient for a given temperature and pressure
#'
#' @inheritParams .get_H
#' @inheritParams .get_gh
#' @param Temp Temperature in Kelvin
#'

.get_Dx <- function(D_0, Temp, eT, P) {

  D_0 * 
    as.numeric((set_units(Temp, K) / set_units(273.15, K))) ^ as.numeric(eT) * 
    as.numeric((set_units(101.3246, kPa) / set_units(P, kPa)))

}

#' Gr: Grashof number
#'
#' @inheritParams .get_H
#' @inheritParams .get_gh
#'

.get_gr <- function(T_leaf, pars) {

  # Calculate virtual temperature
  Tv_leaf <- .get_Tv(T_leaf, .get_ps(T_leaf, pars$P), pars$P)
  Tv_air <-	.get_Tv(pars$T_air, pars$RH * .get_ps(pars$T_air, pars$P), pars$P)
  D_m <- .get_Dx(pars$D_m0, (pars$T_air + T_leaf) / 2, pars$eT, pars$P)
  Gr <- pars$t_air * pars$G * pars$leafsize ^ 3 * abs(Tv_leaf - Tv_air) / D_m ^ 2

  Gr

}

#' Calculate virtual temperature
#'
#' @inheritParams .get_Dx
#'

.get_Tv <- function(Temp, p_air, P) {

  set_units(Temp, K) / 
    (set_units(1, unitless) - (set_units(p_air, kPa) / set_units(P, kPa)) * 0.388)

}

#' Saturation water vapour pressure in kPa
#'
#' @inheritParams .get_Dx
#'

.get_ps <- function(Temp, P) {

  # Goff-Gratch equation (see http://cires1.colorado.edu/~voemel/vp.html)
  # This assumes P = 1 atm = 101.3246 kPa, otherwise boiling temperature needs to change
  # This returns p_s in hPa
  Temp %<>% set_units(K) %>% as.numeric()
  P %<>% set_units(hPa) %>% as.numeric()
  p_s <- 10 ^ (-7.90298 * (373.16 / Temp - 1) +
                 5.02808 * log10(373.16 / Temp) -
                 1.3816e-7 * (10 ^ (11.344 * (1 - Temp / 373.16) - 1)) +
                 8.1328e-3 * (10 ^ (-3.49149 * (373.16 / Temp - 1)) - 1) +
                 log10(P))
  # Convert from hPa to mol m ^ -3 using ideal gas law
  # (p_s * 100) / (8.314 * Temp)
  # Convert to kPa
  p_s %<>% set_units(kPa)
  p_s

}

#' Re: Reynolds number
#'
#' @inheritParams .get_H
#' @inheritParams .get_gh
#'

.get_re <- function(T_leaf, pars) {

  D_m <- .get_Dx(pars$D_m0, (pars$T_air + T_leaf) / 2, pars$eT, pars$P)
  Re <- pars$wind * pars$leafsize / D_m

  Re

}

#' Nu: Nusselt number
#'
#' @inheritParams .get_H
#' @inheritParams .get_gh
#'

.get_nu <- function(T_leaf, surface, pars) {

  Gr <- .get_gr(T_leaf, pars)
  Re <- .get_re(T_leaf, pars)

  # Archemides number
  Ar <- Gr / Re ^ 2

  # Forced or free convection? Cutoffs based on Nobel (2009) pg.344
  if (Ar < set_units(0.1, unitless)) {
    type <- "forced"
    cons <- pars$nu_constant(Re, type, pars$T_air, T_leaf, surface)
    Nu <- cons$a * Re ^ cons$b
    return(Nu)
  }

  if (Ar >= set_units(0.1, unitless) & Ar <= set_units(10, unitless)) {
    type <- "forced"
    cons <- pars$nu_constant(Re, type, pars$T_air, T_leaf, surface)
    Nu_forced <- cons$a * Re ^ cons$b

    type <- "free"
    cons <- pars$nu_constant(Re, type, pars$T_air, T_leaf, surface)
    Nu_free <- cons$a * Gr ^ cons$b

    Nu <- (Nu_forced ^ 3.5 + Nu_free ^ 3.5) ^ (1 / 3.5)
    return(Nu)
  }

  if (Ar > set_units(10, unitless)) {
    type <- "free"
    cons <- pars$nu_constant(Re, type, pars$T_air, T_leaf, surface)
    Nu <- cons$a * Gr ^ cons$b
    return(Nu)
  }

}


#' L: Latent heat flux density (W m^-2)
#'
#' @inheritParams .get_H
#' @param h_vap latent heat of vapourization in J mol^-1
#' @param g_sw stomatal conductance in m s^-1
#' @param g_uw stomatal conductance in m s^-1
#' @param g_tw: total conductance to water vapour in m s^-1
#'

.get_L <- function(T_leaf, pars) {

  g_bw <- sum(.get_gbw(T_leaf, "lower", pars), .get_gbw(T_leaf, "upper", pars))
  # Equation from Foster and Smith 1986 seems to be off:
  # h_vap <- 4.504e4 - 41.94 * T_leaf
  
  # Instead, using regression based on data from Nobel (2009, 4th Ed, Appendix 1)
  # T_K <- 273.15 + c(0, 10, 20, 25, 30, 40, 50, 60)
  # h_vap <- 1e3 * c(45.06, 44.63, 44.21, 44, 43.78, 43.35, 42.91, 42.47) # (in J / mol)
  # fit <- lm(Hvap ~ temp)
  h_vap <- set_units(56847.68250, J / mol) - set_units(43.12514, J / mol / K) * set_units(T_leaf, K)
  h_vap %<>% set_units(J / mol)
  
  warning("Incorporate stomatal ratio into .get_L")
  warning("convert to engineering units using ideal gas law")
  g_tw <- 1 / (1 / pars$g_sw + 1 / pars$g_uw) + g_bw

  L <- prod(h_vap,
            g_tw,
            (.get_ps(T_leaf, pars$P) - pars$RH * .get_ps(pars$T_air, pars$P)))

  L

}

#' g_bw: Boundary layer conductance to water vapour
#'
#' @inheritParams .get_H
#'

.get_gbw <- function(T_leaf, surface, pars) {

  D_w <- .get_Dx(pars$D_w0, (pars$T_air + T_leaf) / 2, pars$eT, pars$P)

  # Calculate Nusselt numbers
  Sh <- .get_sh(T_leaf, surface, pars)

  D_w * Sh / pars$leafsize

}


#' Sh: Sherwood number
#'
#' @inheritParams .get_H
#' @inheritParams .get_gh
#'

.get_sh <- function(T_leaf, surface, pars) {

  Gr <- .get_gr(T_leaf, pars)
  Re <- .get_re(T_leaf, pars)

  # Archemides number
  Ar <- Gr / Re ^ 2

  D_h <- .get_Dx(pars$D_h0, (pars$T_air + T_leaf) / 2, pars$eT, pars$P)
  D_w <- .get_Dx(pars$D_w0, (pars$T_air + T_leaf) / 2, pars$eT, pars$P)

  # Forced or free convection? Cutoffs based on Nobel (2009) pg.344
  if (Ar < set_units(0.1, unitless)) {
    type <- "forced"
    cons <- pars$nu_constant(Re, type, pars$T_air, T_leaf, surface)
    Nu <- cons$a * Re ^ cons$b
    Sh <- Nu * as.numeric(D_h / D_w) ^ pars$sh_constant(type)
    return(Sh)
  }

  if (Ar >= set_units(0.1, unitless) & Ar <= set_units(10, unitless)) {
    type <- "forced"
    cons <- pars$nu_constant(Re, type, pars$T_air, T_leaf, surface)
    Nu_forced <- cons$a * Re ^ cons$b
    Sh_forced <- Nu_forced * as.numeric(D_h / D_w) ^ pars$sh_constant(type)

    type <- "free"
    cons <- pars$nu_constant(Re, type, pars$T_air, T_leaf, surface)
    Nu_free <- cons$a * Gr ^ cons$b
    Sh_free <- Nu_free * as.numeric(D_h / D_w) ^ pars$sh_constant(type)

    warning("check on exponents in mixed convection Sherwood equation in .get_sh")
    Sh <- (as.numeric(Sh_forced) ^ 3.5 + as.numeric(Sh_free) ^ 3.5) ^ (1 / 3.5)
    Sh %<>% set_units(unitless)
    return(Sh)
  }

  if (Ar > set_units(10, unitless)) {
    type <- "free"
    cons <- pars$nu_constant(Re, type, pars$T_air, T_leaf, surface)
    Nu <- cons$a * Gr ^ cons$b
    Sh <- Nu * as.numeric(D_h / D_w) ^ pars$sh_constant(type)
    return(Sh)
  }

}
