library(units)

source("R/make_parameters.R")

# Example: find leaf temperature

leaf_par <- make_leafpar(replace = list(leafsize = 1),
                         traits = character(0))
enviro_par <- make_enviropar()
constants <- make_constants()
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
