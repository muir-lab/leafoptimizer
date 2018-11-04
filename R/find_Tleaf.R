#' Find leaf temperature
#'
#' @inheritParams evolve_leaf
#'
#' @export
#'

find_Tleaf <- function(leaf_par, enviro_par, constants) {

  ##### Balance energy fluxes -----
  enviro_par$T_air %<>% set_units("K") # convert to Kelvin
  
  soln <- stats::optim(drop_units(enviro_par$T_air), engery_balance, leaf_par = leaf_par,
                       enviro_par = enviro_par, constants = constants,
                       abs_val = TRUE, quiet = TRUE, method = "Brent",
                       lower = drop_units(enviro_par$T_air - set_units(30, "K")),
                       upper = drop_units(enviro_par$T_air + set_units(30, "K")))

  ##### Check or report on convergence?? -----
  # ?

  ##### Return -----
  # return evaporation?
  T_leaf <- soln$par

}

#' Calculate leaf energy balance
#'
#' @inheritParams evolve_leaf
#' @param T_leaf Leaf temperature in Kelvin. If input is numeric, it will be automatically converted to \code{units}.
#' @param quiet Logical. Should a message appear about conversion from \code{numeric} to \code{units}?
#' @param abs_val Return absolute value? Useful for finding leaf temperature that balances heat transfer.
#'
#' @export
#'

engery_balance <- function(T_leaf, leaf_par, enviro_par, constants, 
                           quiet = FALSE, abs_val = FALSE) {

  ##### Checks -----
  warning("implement checks in energy_balance")
  #traits <- .missing_traits(character(0))
  #check_leafpar(leaf_par, traits)
  #check_enviropar(enviro_par)
  #check_constants(constants)

  ##### Convert T_leaf to units and message
  if (!is(T_leaf, "units")) {
    if (!quiet) {
      glue::glue("T_leaf converted from numeric to {X} K", X = T_leaf) %>%
        message()
    }
    T_leaf %<>% set_units("K")
  }
  pars <- c(leaf_par, enviro_par, constants)

  ##### R_abs: total absorbed radiation (W m^-2) -----
  R_abs <- .get_Rabs(pars) %>% drop_units()

  ##### R_r: longwave re-radiation (W m^-2) -----
  R_r <- .get_Rr(pars) %>% drop_units()

  ##### H: sensible heat flux density (W m^-2) -----
  H <- .get_H(T_leaf, pars) %>% drop_units()

  ##### L: latent heat flux density (W m^-2) -----
  L <- .get_L(T_leaf, pars) %>% drop_units()

  ##### Return -----
  if (abs_val) return(abs(R_abs - (R_r + H + L)))
  R_abs - (R_r + H + L)

}

#' R_abs: total absorbed radiation (W m^-2)
#'
#' @param pars Concatenated parameters (\code{leaf_par}, \code{enviro_par}, and \code{constants})
# #' @param R_sw total incident shortwave (solar) radiation flux density
# #' @param R_lw total incident longwave radiation flux density
# #' @param abs_s absortivity of shortwave radiation
# #' @param abs_l absortivity of longwave radiation

.get_Rabs <- function(pars) {
  R_abs <- with(pars, abs_s * R_sw + abs_l * R_lw)
  R_abs
}

#' R_r: longwave re-radiation (W m^-2)
#'
#' @inheritParams .get_Rabs
# #' @param T_air Air temperature in Kelvin
# #' @param s Stephan-Boltzmann constant
# #' @param abs_l leaf emissivity (e = abs_l)

.get_Rr <- function(pars) pars$s * pars$abs_l * pars$T_air ^ 4

#' H: sensible heat flux density (W m^-2)
#'
#' @inheritParams .get_Rr
#' @param T_leaf Leaf temperature in Kelvin
# #' @param RH Relative humidity
# #' @param leafsize Leaf characteristic dimension in meters
# #' @param wind Windspeed in m s^-1
# #' @param P Atmospheric pressure in kPa
# #' @param c_p Heat capacity of air in J g^-1 K^-1

.get_H <- function(T_leaf, pars) {

  # Density of dry air
  P_a <- .get_Pa(T_leaf, pars)

  # Boundary layer conductance to heat
  warning("check that I am supposed to sum conductances from each surface")
  g_h <- sum(.get_gh(T_leaf, "lower", pars), .get_gh(T_leaf, "upper", pars))

  H <- P_a * pars$c_p * g_h * (T_leaf - pars$T_air)
  H %<>% set_units("W / m ^ 2")
  H
  
}

#' P_a: density of dry air (g m^-3)
#'
#' @inheritParams .get_H
# #' @param R_air Specific gas constant for dry air in J kg^-1 K^-1

.get_Pa <- function(T_leaf, pars) {
  P_a <- pars$P / (pars$R_air * (pars$T_air + T_leaf) / 2)
  P_a %<>% set_units("g / m^3")
}

#' g_h: boundary layer conductance to heat (m s^-1)
#'
#' @inheritParams .get_H
#' @param surface Leaf surface (lower or upper)
# #' @param D_h0 Diffusion coefficient of heat in air at 0C in m^2 s^-1
# #' @param D_m0 Diffusion coefficient for momentum in air at 0C in m^2 s^-1
# #' @param eT Exponent for temperature dependence of diffusion
# #' @param G Gravitational acceleration in m s^-2
# #' @param nu_constant Function to calculate Nusselt number constants
# #' @param t_air Coefficient of thermal expansion of air in 1 / K

.get_gh <- function(T_leaf, surface, pars) {

  surface %<>% match.arg(c("lower", "upper"))
  
  # Calculate diffusion coefficient to heat
  D_h <- .get_Dx(pars$D_h0, (pars$T_air + T_leaf) / 2, pars$eT, pars$P)

  # Calculate Nusselt numbers
  Nu <- .get_nu(T_leaf, surface, pars)

  D_h * Nu / pars$leafsize

}

#' D_x: Calculate diffusion coefficient for a given temperature and pressure
#'
#' @param D_0 Diffusion coefficient at 273.15 K and 101.3246 kPa
#' @param Temp Temperature in Kelvin
#' @param eT Exponent for temperature dependence of diffusion
#' @param P Atmospheric pressure in kPa
#'

.get_Dx <- function(D_0, Temp, eT, P) {

  D_0 * 
    drop_units((set_units(Temp, "K") / set_units(273.15, "K"))) ^ drop_units(eT) * 
    drop_units((set_units(101.3246, "kPa") / set_units(P, "kPa")))

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
#' @param p_air Saturation water vapour pressure of air in kPa
#'

.get_Tv <- function(Temp, p_air, P) {

  set_units(Temp, "K") / 
    (set_units(1) - (set_units(p_air, "kPa") / set_units(P, "kPa")) * 0.388)

}

#' Saturation water vapour pressure in kPa
#'
#' @inheritParams .get_Dx
#'

.get_ps <- function(Temp, P) {

  # Goff-Gratch equation (see http://cires1.colorado.edu/~voemel/vp.html)
  # This assumes P = 1 atm = 101.3246 kPa, otherwise boiling temperature needs to change
  # This returns p_s in hPa
  Temp %<>% set_units("K") %>% drop_units()
  P %<>% set_units("hPa") %>% drop_units()
  p_s <- 10 ^ (-7.90298 * (373.16 / Temp - 1) +
                 5.02808 * log10(373.16 / Temp) -
                 1.3816e-7 * (10 ^ (11.344 * (1 - Temp / 373.16) - 1)) +
                 8.1328e-3 * (10 ^ (-3.49149 * (373.16 / Temp - 1)) - 1) +
                 log10(P))

  # Convert to kPa
  p_s %<>% set_units("kPa")
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

  surface %<>% match.arg(c("lower", "upper"))
  
  Gr <- .get_gr(T_leaf, pars)
  Re <- .get_re(T_leaf, pars)

  # Archemides number
  Ar <- Gr / Re ^ 2

  # Forced or free convection? Cutoffs based on Nobel (2009) pg.344
  if (Ar < set_units(0.1)) {
    type <- "forced"
    cons <- pars$nu_constant(Re, type, pars$T_air, T_leaf, surface)
    Re %<>% as.numeric()
    Nu <- cons$a * Re ^ cons$b
    Nu %<>% set_units()
    return(Nu)
  }

  if (Ar >= set_units(0.1) & Ar <= set_units(10)) {
    type <- "forced"
    cons <- pars$nu_constant(Re, type, pars$T_air, T_leaf, surface)
    Re %<>% as.numeric()
    Nu_forced <- cons$a * Re ^ cons$b

    type <- "free"
    Re %<>% set_units()
    cons <- pars$nu_constant(Re, type, pars$T_air, T_leaf, surface)
    Gr %<>% as.numeric()
    Nu_free <- cons$a * Gr ^ cons$b

    Nu <- (Nu_forced ^ 3.5 + Nu_free ^ 3.5) ^ (1 / 3.5)
    Nu %<>% set_units()
    return(Nu)
  }

  if (Ar > set_units(10)) {
    type <- "free"
    cons <- pars$nu_constant(Re, type, pars$T_air, T_leaf, surface)
    Gr %<>% as.numeric()
    Nu <- cons$a * Gr ^ cons$b
    Nu %<>% set_units()
    return(Nu)
  }

}


#' L: Latent heat flux density (W m^-2)
#'
#' @inheritParams .get_H
# #' @param h_vap latent heat of vapourization in J mol^-1
# #' @param g_sw stomatal conductance in m s^-1
# #' @param g_uw stomatal conductance in m s^-1
# #' @param g_tw: total conductance to water vapour in m s^-1

.get_L <- function(T_leaf, pars) {

  # Equation from Foster and Smith 1986 seems to be off:
  # h_vap <- 4.504e4 - 41.94 * T_leaf
  # Instead, using regression based on data from Nobel (2009, 4th Ed, Appendix 1)
  # T_K <- 273.15 + c(0, 10, 20, 25, 30, 40, 50, 60)
  # h_vap <- 1e3 * c(45.06, 44.63, 44.21, 44, 43.78, 43.35, 42.91, 42.47) # (in J / mol)
  # fit <- lm(Hvap ~ temp)
  h_vap <- set_units(56847.68250, "J / mol") - 
    set_units(43.12514, "J / mol / K") * set_units(T_leaf, "K")
  h_vap %<>% set_units("J / mol")
  
  # Lower surface ----
  g_bw_lower <- .get_gbw(T_leaf, "lower", pars)
  
  # Convert stomatal and cuticular conductance from molar to 'engineering' units
  # See email from Tom Buckley (July 4, 2017)
  g_sw_lower <- set_units(pars$g_sw * (set_units(1) - plogis(pars$sr)) * pars$R * 
                            ((T_leaf + pars$T_air) / 2), "m / s")
  g_uw_lower <- set_units(pars$g_uw * 0.5 * pars$R * ((T_leaf + pars$T_air) / 2), "m / s")
  g_tw_lower <- 1 / (1 / (g_sw_lower + g_uw_lower) + 1 / g_bw_lower)

  # Upper surface ----
  g_bw_upper <- .get_gbw(T_leaf, "upper", pars)
  
  # Convert stomatal and cuticular conductance from molar to 'engineering' units
  # See email from Tom Buckley (July 4, 2017)
  g_sw_upper <- set_units(pars$g_sw * plogis(pars$sr) * pars$R * 
                            ((T_leaf + pars$T_air) / 2), "m / s")
  g_uw_upper <- set_units(pars$g_uw * 0.5 * pars$R * ((T_leaf + pars$T_air) / 2), "m / s")
  g_tw_upper <- 1 / (1 / (g_sw_upper + g_uw_upper) + 1 / g_bw_upper)
  
  # Lower and upper surface are in parallel
  g_tw <- g_tw_lower + g_tw_upper
    
  # Water vapour differential converted from kPa to mol m ^ -3 using ideal gas law
  dWV <- .get_ps(T_leaf, pars$P) / (pars$R * T_leaf) - 
    pars$RH * .get_ps(pars$T_air, pars$P) / (pars$R * pars$T_air)
  dWV %<>% set_units("mol / m ^ 3")

  L <- h_vap * g_tw * dWV
  L %<>% set_units("W / m ^ 2")
  L

}

#' g_bw: Boundary layer conductance to water vapour
#'
#' @inheritParams .get_H
#' @inheritParams .get_gh

.get_gbw <- function(T_leaf, surface, pars) {

  surface %<>% match.arg(c("lower", "upper"))
  
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

  surface %<>% match.arg(c("lower", "upper"))
  
  Gr <- .get_gr(T_leaf, pars)
  Re <- .get_re(T_leaf, pars)

  # Archemides number
  Ar <- Gr / Re ^ 2

  D_h <- .get_Dx(pars$D_h0, (pars$T_air + T_leaf) / 2, pars$eT, pars$P)
  D_w <- .get_Dx(pars$D_w0, (pars$T_air + T_leaf) / 2, pars$eT, pars$P)

  # Forced or free convection? Cutoffs based on Nobel (2009) pg.344
  if (Ar < set_units(0.1)) {
    type <- "forced"
    cons <- pars$nu_constant(Re, type, pars$T_air, T_leaf, surface)
    Re %<>% drop_units()
    Nu <- cons$a * Re ^ cons$b
    Nu %<>% set_units()
    Sh <- Nu * drop_units(D_h / D_w) ^ pars$sh_constant(type)
    return(Sh)
  }

  if (Ar >= set_units(0.1) & Ar <= set_units(10)) {
    type <- "forced"
    cons <- pars$nu_constant(Re, type, pars$T_air, T_leaf, surface)
    Re %<>% drop_units()
    Nu_forced <- cons$a * Re ^ cons$b
    Nu_forced %<>% set_units()
    Sh_forced <- Nu_forced * drop_units(D_h / D_w) ^ pars$sh_constant(type)

    type <- "free"
    Re %<>% set_units()
    cons <- pars$nu_constant(Re, type, pars$T_air, T_leaf, surface)
    Gr %<>% drop_units()
    Nu_free <- cons$a * Gr ^ cons$b
    Nu_free %<>% set_units()
    Sh_free <- Nu_free * drop_units(D_h / D_w) ^ pars$sh_constant(type)

    warning("check on exponents in mixed convection Sherwood equation in .get_sh")
    Sh <- (drop_units(Sh_forced) ^ 3.5 + drop_units(Sh_free) ^ 3.5) ^ (1 / 3.5)
    Sh %<>% set_units()
    return(Sh)
  }

  if (Ar > set_units(10)) {
    type <- "free"
    Re %<>% drop_units()
    cons <- pars$nu_constant(Re, type, pars$T_air, T_leaf, surface)
    Gr %<>% drop_units()
    Nu <- cons$a * Gr ^ cons$b
    Nu %<>% set_units()
    Sh <- Nu * drop_units(D_h / D_w) ^ pars$sh_constant(type)
    return(Sh)
  }

}
