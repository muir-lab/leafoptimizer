library(units)
library(magrittr)
source("R/make_parameters.R")
source("R/find_Tleaf.R")

leaf_par <- make_leafpar()
enviro_par <- make_enviropar()
constants <- make_constants()
pars <- c(leaf_par, enviro_par, constants)

##### Components of energy balance
##### R_abs: total absorbed radiation (W m^-2) -----
R_abs <- .get_Rabs(pars)

##### R_abs: total absorbed radiation (W m^-2) -----
R_r <- .get_Rr(pars)

##### H: -----
H <- .get_H(set_units(295, K), pars)

##### L: -----
L <- .get_L(set_units(295, K), pars) # L may be too high??

leaf_par$g_sw<- set_units(0.5, umol/m^2/Pa/s)
engery_balance(set_units(290, K), leaf_par, enviro_par, constants)

T_leaf <- find_Tleaf(leaf_par, enviro_par, constants)


