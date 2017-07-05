# A supply curve

As <- function(cc, gc, ca) {

	# gc: total conductance to CO2 (mol CO2 m^-2 s^-1)
	# ca: atmospheric CO2 concentration (umol CO2 mol^-1 air)
	# cc: chloroplastic CO2 concentration (umol CO2 mol^-1 air)
	gc * (ca - cc)

}

# Total conductance to CO2

gc <- function(gs, gx, gi, ks, kx) {

	# gs: stomatal conductance to CO2 (mol CO2 m^-2 s^-1)
	# gx: intercellular conductance to CO2 (mol CO2 m^-2 s^-1)
	# gi: intracellular conductance to CO2 (mol CO2 m^-2 s^-1)
	# ks: partitioning of gs to abaxial surface
	# kx: partition of gx to spongy mesophyll
	gs_ad <- gs * (ks / (1 + ks))
	gs_ab <- gs * (1 / (1 + ks))
	gx_pa <- gx * (kx / (1 + kx))
	gx_sp <- gx * (1 / (1 + kx))
	gupp <- 1 / ((1 / gs_ad) + (1 / gx_pa))
	glow <- 1 / ((1 / gs_ab) + (1 / gx_sp))
	gc <- 1 / ((1 / gi) + (1 / (gupp + glow)))
	return(gc)

}

# A demand curve

Ad <- function(cc, gammaStar, Wc, Wr, Rd) {

	# gammaStar: chloroplastic CO2 compensation point (umol CO2 mol^-1 air)
	# cc: chloroplastic CO2 concentration (umol CO2 mol^-1 air)
	# Rd: respiration (mol CO2 m^-2 s^-1)
	# Wc: carboxylation limited assimilation rate (mol CO2 m^-2 s^-1)
	# Wr: RuBP regeneration limited assimilation rate (mol CO2 m^-2 s^-1)

	.A <- function(par, Wc, Wr, thetaA = 0.99) {
		# thetaA: dimensionless curvature factor
		X <- thetaA * par ^ 2 - par * (Wc + Wr) + Wc * Wr
		X
		#abs(X - 0)
	}

	#A <- optim(mean(c(Wc, Wr)), .A, lower = 0.5 * min(Wc, Wr), upper = 2 * max(Wc, Wr), method = "Brent", Wc = Wc, Wr = Wr)
	A <- uniroot(.A, c(0, max(Wc, Wr)), Wc = Wc, Wr = Wr)

	(1 - gammaStar / cc) * A$root - Rd

}

# Assimilation functions (equations A4-6)

Wcarbox <- function(cc, Vcmax, Kc, Ko, O) {

	# cc: chloroplastic CO2 concentration (umol CO2 mol^-1 air)
	# Vcmax: maximum rate of carboxylation (mol CO2 m^-2 s^-1)
	# Kc: Michaelis constant for carboxylation (umol mol ^ -1)
	# Ko: Michaelis constant for oxygenation (umol mol ^ -1)
	# O: atmospheric concentration of oxygen (umol mol ^ -1)
	(Vcmax * cc) / (cc + Kc * (1 + O / Ko))

}

Wregen <- function(cc, Irradiance, Jmax, gammaStar) {

	# cc: chloroplastic CO2 concentration (umol CO2 mol^-1 air)
	# Irriadiance: incident irradiance (umol quanta m^-2 s^-1)
	# Jmax: potential electron transport (mol CO2 m^-2 s^-1)
	# gammaStar: chloroplastic CO2 compensation point (umol CO2 mol^-1 air)

	J <- function(par, Irradiance, Jmax, thetaJ = 0.86, phi = 0.25) {
		# thetaJ: dimensionless curvature factor
		# phi: effective maximum quantum yield of electrons from incident irradiance (e-/hv)
		X <- thetaJ * par ^ 2 - par * (Jmax + Irradiance * phi) + Jmax * Irradiance * phi

		abs(X - 0)
	}

	JI <- optim(Jmax / 2, J, lower = 0, upper = Jmax, method = "Brent",
							Irradiance = Irradiance, Jmax = Jmax)

	(JI$par * cc) / (4 * (cc + 2 * gammaStar))

}

# Find chloroplastic CO2 concentration

findCc <- function(cc, leafVar, enviroPar, leafPar) {

	# enviroPar should have ca, O, Irradiance
	# leafPar should have Kc, Ko, gammaStar
	# leafVar should have gc, Vcmax, Jmax, Rd

	# Supply
	A1 <- As(cc, leafVar$gc, enviroPar$ca)

	# Demand
	W1 <- Wcarbox(cc, leafVar$Vcmax, leafPar$Kc, leafPar$Ko, enviroPar$O)
	W2 <- Wregen(cc, enviroPar$Irradiance, leafVar$Jmax, leafPar$gammaStar)
	A2 <- Ad(cc, leafPar$gammaStar, W1, W2, leafVar$Rd)

	return((A1 - A2) ^ 2)

}

A <- function(leafVar, enviroPar, leafPar) {

	# enviroPar should have ca, O, Irradiance
	# leafPar should have Kc, Ko, gammaStar
	# leafVar should have gs, gx, gi, ks, kx, Vcmax, Jmax, Rd

	leafVar$gc <- with(leafVar, gc(gs, gx, gi, ks, kx))
	fit <- optim(enviroPar$ca / 2, findCc,
							 leafVar = leafVar, enviroPar = enviroPar, leafPar = leafPar,
							 method = "Brent", upper = enviroPar$ca, lower = 0)
	cc <- fit$par
	A <- As(cc, leafVar$gc, enviroPar$ca)
	return(A)
}

# Light response curve

light_response <- function(Irradiance, leafVar, enviroPar, leafPar) {

	ret <- list(Irradiance = Irradiance)

	ret$A <- sapply(Irradiance, function(irr, leafVar, enviroPar, leafPar) {
		enviroPar$Irradiance <- irr
		A(leafVar, enviroPar, leafPar)
	}, leafVar = leafVar, enviroPar = enviroPar, leafPar = leafPar)

	ret

}


### Foster and Smith 1986 model

			# D_h: diffusion coefficient of heat in air (m^2 s^-1)
			# D_w: diffusion coefficient of water vapour in air (m^2 s^-1)
			get_Dx <- function(D_0, T_a, T_l, eT = 1.75, P = 101.3246) {
				#
			  # Generic function for calculating diffusion coefficients at a given temperature
				# Assume temperature is average of T_a and T_l
				D_0 * (((T_a + T_l) / 2) / 273.15) ^ eT * (101.3246 / P)
			}

			# Nu: Nusselt number
			get_nu <- function(number, constants, type) {

				if (type == "forced") {

					# forced convection
					# Re: Reynolds number
					Re <- number
					# Re < 4000: a = 0.6, b = 0.5: empirical constants
					# Re > 4000: a = 0.032, b = 0.8: empirical constants
					a <- constants[1]
					b <- constants[2]
					Nu <- a * Re ^ b

				}

				if (type == "free") {

					# free convection
					# Gr: Grashof number
					Gr <- number
					# c = 0.5, d = 0.25: top surface of leaves warmer than air or bottom surface of leaves cooler than air
					# lower surface: c = 0.23, d = 0.25: top surface of leaves cooler than air or bottom surface of leaves warmer than air
					a <- constants[1] # using a and b to avoid using 'c' as variable
					b <- constants[2]
					Nu <- a * Gr ^ b

				}

				Nu

			}

			get_re <- function(leafsize, wind, D_m) {

				# Reynolds number (forced convection)
				# leafsize: leaf characteristic dimension (m)
				# wind: windspeed (m s^-1)
				# D_m: diffusion coefficient for momentum in air (m^2 s^-1)
			  wind * leafsize / D_m

			}

			get_ps <- function(T_K, P = 101.3246) {
				# Goff-Gratch equation (see http://cires1.colorado.edu/~voemel/vp.html)
				# T_K: temperature in K
				# This assumes P = 1 atm = 101.3246 kPa, otherwise boiling temperature needs to change
				# This returns p_s in hPa
				p_s <- 10 ^ (-7.90298 * (373.16 / T_K - 1) +
												5.02808 * log10(373.16 / T_K) -
												1.3816e-7 * (10 ^ (11.344 * (1 - T_K / 373.16) - 1)) +
												8.1328e-3 * (10 ^ (-3.49149 * (373.16 / T_K - 1)) - 1) +
												log10(P * 10))
				# Convert from hPa to mol m ^ -3 using ideal gas law
				# (p_s * 100) / (8.314 * T_K)
				# Convert to kPa
				p_s / 10

			}

			get_Tv <- function(T_K, p_air, P = 101.3246) {
				# T_K: temperature in Kelvin
				# e: water vapour pressure in kPa
				# P: atmospheric pressure in kPa
				T_K / (1 - (p_air / P) * 0.388)
			}

			get_gr <- function(leafsize, T_leaf, T_air, RH, t_air = 3.66e-3, G = 9.8, P = 101.3246) {

				# Grashof number (free convection)
				# d: leaf characteristic dimension (m)
				# T_l: leaf temperature (K)
				# T_a: atmospheric temperature (K)
				# T_lv: virtual leaf temperature (K)
				# T_av: virtual atmospheric temperature (K)
				# RH: relative humidity
				# t_a: coefficient of thermal expansion of air (3.66 * 10^-3)
				# G: gravitational acceleration (9.8 m s^-2)
				# P: air pressure (kPa)
				# D_m: diffusion coefficient for momentum in air
					# (13.3 m^2 s^-1 at 0C in Monteith and Unger Table A3)
			  # Calculate virtual temperature
			  Tv_leaf <- get_Tv(T_leaf, get_ps(T_leaf, P), P)
			  Tv_air <-	get_Tv(T_air, RH * get_ps(T_air, P), P)
			  D_m <- get_Dx(D_m0, T_air, T_leaf)
				abs(t_air * G * d ^ 3 * (Tv_leaf - Tv_air) / D_m ^ 2)

				}

			get_sh <- function(Nu, D_h, D_w, type) {

				# Sh: Sherwood number
				# forced convection
				if (type == "forced") Sh <- Nu * (D_h / D_w) ^ 0.33
				# free convection
				if(type == "free") Sh <- Nu * (D_h / D_w) ^ 0.25

				Sh

			}

			# Diffusion coefficients as a function of temperature
			# d: leaf characteristic dimension (m)

	# g_b: Boundary layer conductance to water vapour (m s^-1) -------------------

			get_gb <- function(T_l, T_a, RH, d, u, surface, t_a = 3.66e-3, G = 9.8, P = 101.3246) {
				# D_h0 = 1.9e-5: diffusion coefficient of heat in air at 0C and P = 101.3246 kPa (m^2 s^-1)
				D_h <- get_Dx(1.9e-5, T_a, T_l, P = P)
				# D_w0 = 21.2: diffusion coefficient of water vapour in air at 0C and P = 101.3246 kPa (m^2 s^-1)
				# Monteith and Unsworth 2013
				D_w <- get_Dx(21.2, T_a, T_l, P = P)

				# Forced or free convection? Calculate Archemedes number: Ar = Gr / Re ^ 2
				T_lv <- get_Tv(T_l, get_ps(T_l, P = P), P = P)
				T_av <-	get_Tv(T_a, RH * get_ps(T_a, P = P), P = P)
				Ar <- abs((t_a * G * (T_lv - T_av)) * d / (u ^ 2))

				# Cutoffs based on Nobel (2009) pg.344
				if (Ar < 0.1) {
					type <- "forced"
					D_m <- get_Dx(D_0 = 13.3, T_a, T_l)
					Re <- get_re(d, u, D_m)
					# Re < 4000: a = 0.6, b = 0.5: empirical constants
					# Re > 4000: a = 0.032, b = 0.8: empirical constants
					if (Re < 4e3) constants <- c(0.6, 0.5)
					if (Re >= 4e3) constants <- c(0.032, 0.8)
					Nu <- get_nu(Re, constants, type)
					Sh <- get_sh(Nu, D_h, D_w, type)
				}

				if (Ar >= 0.1 & Ar <= 10) {
					type <- "forced"
					D_m <- get_Dx(D_0 = 13.3, T_a, T_l)
					Re <- get_re(d, u, D_m)
					# Re < 4000: a = 0.6, b = 0.5: empirical constants
					# Re > 4000: a = 0.032, b = 0.8: empirical constants
					if (Re < 4e3) constants <- c(0.6, 0.5)
					if (Re >= 4e3) constants <- c(0.032, 0.8)
					Nu_forced <- get_nu(Re, constants, type)
					Sh_forced <- get_sh(Nu_forced, D_h, D_w, type)

					type <- "free"
					Gr <- get_gr(d, T_l, T_a, RH, P = P)
					if ((surface == "upper" & T_l > T_a) |
							(surface == "lower" & T_l < T_a)) {
						constants <- c(0.5, 0.25)
					} else {
						constants <- c(0.23, 0.25)
					}
					Nu_free <- get_nu(Gr, constants, type)
					Sh_free <- get_sh(Nu_free, D_h, D_w, type)

					Nu <- (Nu_forced ^ 3.5 + Nu_free ^ 3.5) ^ (1 / 3.5)
					Sh <- (Sh_forced ^ 3.5 + Sh_free ^ 3.5) ^ (1 / 3.5)

				}

				if (Ar > 10) {
					type <- "free"
					Gr <- get_gr(d, T_l, T_a, RH, P = P)
					if ((surface == "upper" & T_l > T_a) |
							(surface == "lower" & T_l < T_a)) {
						constants <- c(0.5, 0.25)
					} else {
						constants <- c(0.23, 0.25)
					}
					Nu <- get_nu(Gr, constants, type)
					Sh <- get_sh(Nu, D_h, D_w, type)
				}

				D_w * Sh / d

			}


# L: Latent heat flux density (W m^-2) -----------------------------------------
	# h_v: latent heat of vapourization (J mol^-1)
	# g_t: total conductance to water vapour (m s^-1)
	get_L <- function(g_s, T_l, T_a, RH, d, u, g_c = 0.01, t_a = 3.66e-3, G = 9.8, P = 101.3246) {
		g_b <- get_gb(T_l, T_a, RH, d, u, "lower", t_a, G, P) +
			get_gb(T_l, T_a, RH, d, u, "upper", t_a, G, P)
		h_v <- 4.504e4 - 41.94 * T_l
		g_t <- 1 / (1 / g_s + 1 / g_c) + g_b

		h_v * g_t * (get_ps(T_l, P = P) - RH * get_ps(T_a, P = P))
	}

# minimize this function -------------------------------------------------------

energy_balance <- function(g_s, T_l, T_a, RH, S_t, R_l, d, u) {

	R_abs <- get_Rabs(S_t, R_l)
	R_r <- get_Rr(T_a)
	H <- get_H(T_l, T_a, RH, d, u)
	L <- get_L(g_s, T_l, T_a, RH, d, u)

	R_abs - (R_r + H + L)

}

	S_t <- 1000
	R_l <- 825
	T_a <- 298
	RH <- 0.2
	d <- 0.1
	u <- 2
	g_s <- 0.5

energy_balance(g_s = 0.5, T_l = 270:300, T_a = 298, RH = 0.2, S_t = 1000, R_l = 825, d = 0.1, u = 2)
x <- seq(270, 330, length.out = 1e3)
plot(x, sapply(x, energy_balance, g_s = 0.5, T_a = 298, RH = 0.2, S_t = 1000, R_l = 825, d = 0.1, u = 2), type = "l")

uniroot(energy_balance, c(200, 400), g_s = 0.5, T_a = 298, RH = 0.2, S_t = 1000, R_l = 825, d = 1, u = 2)
