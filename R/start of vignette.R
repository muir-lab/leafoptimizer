library(units)
library(magrittr)
source("R/make_parameters.R")

# Test that I can replace defaults

  # Should fail (no units)
  leaf_par <- make_leafpar(replace = list(abs_s = 1))
  leaf_par <- make_leafpar(replace = list(abs_l = 1))
  leaf_par <- make_leafpar(replace = list(g_xc = 1))
  leaf_par <- make_leafpar(replace = list(g_ic = 1))
  leaf_par <- make_leafpar(replace = list(g_uw = 1))
  leaf_par <- make_leafpar(replace = list(k_x = 1))
  leaf_par <- make_leafpar(replace = list(V_cmax = 1))
  leaf_par <- make_leafpar(replace = list(J_max = 1))
  leaf_par <- make_leafpar(replace = list(R_d = 1))
  leaf_par <- make_leafpar(replace = list(K_c = 1))
  leaf_par <- make_leafpar(replace = list(K_o = 1))
  leaf_par <- make_leafpar(replace = list(gamma_star = 1))
  leaf_par <- make_leafpar(replace = list(g_sw = 1))
  leaf_par <- make_leafpar(replace = list(leafsize = 1))
  leaf_par <- make_leafpar(replace = list(sr = 1))
  
  # Should work
  leaf_par <- make_leafpar(replace = list(abs_s = set_units(1, unitless)))
  
              # abs_l = set_units(0.97, unitless),
              # g_xc = set_units(1, mol / (m^2 * s * Pa)), # CHECK DEFAULT in Pa^-1
              # g_ic = set_units(1, mol / (m^2 * s * Pa)), # CHECK DEFAULT in Pa^-1
              # g_uw = set_units(0.01, mol / (m^2 * s * Pa)), # CHECK DEFAULT in Pa^-1
              # k_x = set_units(1, unitless),
              # V_cmax = set_units(50, umol / (m^2 * s)),
              # J_max = set_units(100, umol / (m^2 * s)),
              # R_d = set_units(2, umol / (m^2 * s)),
              # K_c = set_units(27.238, Pa), # From Sharkey et al. 2007. Newew source? Check bayCi
              # K_o = set_units(16.582, kPa), # From Sharkey et al. 2007. Newew source? Check bayCi
              # gamma_star = set_units(3.73, Pa), # From Sharkey et al. 2007. Newew source? Check bayCi
              # g_sw = set_units(0.5, mol / (m^2 * s * Pa)), # CHECK DEFAULT in Pa^-1
              # leafsize = set_units(0.1, m),
              # sr = set_units(1, unitless))
  
enviro_par <- make_enviropar()
constants <- make_constants()


# Example: find leaf temperature

pars <- c(leaf_par, enviro_par, constants)


x <- numeric(100)
T_leaf <- seq(280, 300, length.out = 100)
for (i in 1:100) {
##### R_abs: total absorbed radiation (W m^-2) -----
R_abs <- .get_Rabs(pars)

##### R_abs: total absorbed radiation (W m^-2) -----
R_r <- .get_Rr(pars)

##### H: -----
H <- .get_H(T_leaf[i], pars) # I think H is way too low.

##### H: -----
L <- .get_L(T_leaf[i], pars) # L may be too high??

x[i] <- R_abs - (R_r + H + L)
}

T_leaf <- find_Tleaf(leaf_par, enviro_par, constants)


P_a (g m^-3)
pars$c_p J g^-1 K^-1
g_h m^2 s^-1
(T_leaf - pars$T_air) K

m^-1 * J * s^-1
J / m s


1.292 kg m^-3
1010 J kg^-1 K^-1
3.3 K


# L from worksheet
(p * Cp/ y) * (ei - ea) / (rbl + rst)

p = 1.292 kg m^-3
Cp = 1010 J kg^-1 K^-1
y = 0.066 kPa K^-1
ei = 5.1 kPa
ea = 1.1 kPa
rbl = 50.4 (0.02)
rst = 500 (0.002)
