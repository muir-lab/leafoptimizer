#' Convert irradiance (W/m^2) to PPFD (umol/m^2/s)
#'
#' @param S_sw incident short-wave (solar) radiation flux density with units (W / m^2) of class \code{units}.
#' @param f_par fraction of \code{S_sw} that is photosynthetic active radiation (PAR). Must be unitless value between 0 and 1.
#' @param E_q energy per mole quanta. Value in kJ/mol of class \code{units}.
#'
#' @return Value with units \eqn{\mu}mol / (m^2 s) of class \code{units}.
#' 
#' @details 
#' 
#' Shortwave radiation is (at first approximation) the sum of photosynthetically active radiation (PAR) and near-infrared radiation (NIR):
#' 
#' \deqn{S_\mathrm{sw} = S_\mathrm{PAR} + S_\mathrm{NIR}}{S_sw = S_par + S_nir}
#' 
#' Most sources (e.g. Jones 2013) assume that S_nir = S_par for sunlight, so \eqn{f_\mathrm{PAR} = 0.5}{f_par = 0.5}.
#' 
#' To convert PAR to PPFD, divide by the energy per mol quanta. Gutschick (2016) suggests ~220 kJ/mol quanta for PAR:
#' 
#' \deqn{\mathrm{PPFD} = S_\mathrm{PAR} / E_q}{PPFD = S_par / E_q}
#' 
#' @references 
#' 
#' Jones HG. 2013. Plants and microclimate: a quantitative approach to environmental plant physiology. Cambridge University Press.
#' 
#' Gutschick VP. 2016. Leaf energy balance: basics, and modeling from leaves to Canopies. In Canopy Photosynthesis: From Basics to Applications (pp. 23-58). Springer, Dordrecht.
#' 
#' @examples 
#' S_sw <- set_units(1000, "W/m^2")
#' f_par <- set_units(0.5)
#' E_q <- set_units(220, "kJ/mol")
#' sun2ppfd(S_sw, f_par, E_q)
#' 
#' @export
#'

sun2ppfd <- function(S_sw, f_par, E_q) {
  
  f_par %<>% drop_units()
  stopifnot(f_par >= 0 & f_par <= 1)
  E_q %<>% set_units("kJ/mol")
  
  S_sw %>% 
    set_units("W/m^2") %>%
    magrittr::multiply_by(f_par) %>%
    magrittr::divide_by(E_q) %>% 
    set_units("umol/m^2/s")
  
}

#' Convert PPFD (umol/m^2/s) to irradiance (W/m^2)
#' 
#' @rdname sun2ppfd
#' 
#' @inheritParams sun2ppfd
#' @param PPFD hotosynthetic photon flux density in \eqn{\mu}mol quanta / (m^2 s) of class \code{units}.
#' 
#' @export
#' 

ppfd2sun <- function(PPFD, f_par, E_q) {
  
  f_par %<>% drop_units()
  stopifnot(f_par >= 0 & f_par <= 1)
  E_q %<>% set_units("kJ/mol")
  
  PPFD %>% 
    set_units("umol/m^2/s") %>%
    magrittr::multiply_by(E_q) %>%
    magrittr::divide_by(f_par) %>% 
    set_units("W/m^2")
  
}

#' Convert g_c (\eqn{\mu}mol CO2/m^2/s/Pa) to g_w (\eqn{\mu}mol H2O /m^2/s/Pa)
#'
#' @param g_w conductance to water vapor in units (\eqn{\mu}mol H2O / (m^2 s Pa)) of class \code{units}.
#' @param D_c diffusion coefficient for CO2 in air in units of m^2/s of call \code{units}
#' @param D_w diffusion coefficient for H2O in air in units of m^2/s of call \code{units}
#'
#' @return Value with units \eqn{\mu}mol / (m^2 s Pa) of class \code{units}.
#' 
#' @details 
#' 
#' Diffusive conductance to CO2 is generally about ~1.6x that of H2O because of the higher molecular weight. To convert, multiply conductance by the ratio of diffusion coefficients:
#' 
#' \deqn{g_\mathrm{c} = g_\mathrm{w} D_\mathrm{c} / D_\mathrm{w}}{g_c = g_w D_c / D_w}
#' \deqn{g_\mathrm{w} = g_\mathrm{c} D_\mathrm{w} / D_\mathrm{c}}{g_w = g_c D_w / D_c}
#' 
#' @examples 
#' D_c <- set_units(1.29e-05, "m^2/s")
#' D_w <- set_units(2.12e-05, "m^2/s")
#' g_c <- set_units(3, "umol/m^2/s/Pa")
#' g_w <- gc2gw(g_c, D_c, D_w)
#' g_w
#' 
#' gw2gc(g_w, D_c, D_w)
#' 
#' @export
#'

gw2gc <- function(g_w, D_c, D_w) {
  
  g_w %<>% set_units("umol/m^2/s/Pa")
  D_c %<>% set_units("m^2/s")
  D_w %<>% set_units("m^2/s")
  
  g_w %>% 
    magrittr::multiply_by(D_c / D_w) %>%
    set_units("umol/m^2/s/Pa")
  
}

#' Convert g_c (umol CO2/m^2/s/Pa) to g_w (umol H2O /m^2/s/Pa)
#' 
#' @rdname gw2gc
#' 
#' @inheritParams gw2gc
#' @param g_c conductance to CO2 in units (\eqn{\mu}mol H2O / (m^2 s Pa)) of class \code{units}.
#' @export
#' 

gc2gw <- function(g_c, D_c, D_w) {
  
  g_c %<>% set_units("umol/m^2/s/Pa")
  D_c %<>% set_units("m^2/s")
  D_w %<>% set_units("m^2/s")

  g_c %>% 
    magrittr::multiply_by(D_w / D_c) %>%
    set_units("umol/m^2/s/Pa")
  
}
