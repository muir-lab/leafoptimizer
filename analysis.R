# Get A-light response working
# How to translate from A to fitness?

setwd("~/Google Drive/StomatalRatio/BEF/Model")

source("functions.R")

# Parameters

# enviroPar should have ca, O, Irradiance
# leafPar should have Kc, Ko, gammaStar
# leafVar should have gs, gx, gi, ks, kx, Vcmax, Jmax, Rd

enviroPar <- list(ca = 400,
									Irradiance = 500,
									O = 210000)

leafPar <- list(gammaStar = 37.3,
								Kc = 268.3,
								Ko = 165084.2)

leafVar <- list(gs = 1,
								gi = 1,
								gx = 0.5,
								ks = 0,
								kx = 1,
								Rd = 1,
								Vcmax = 75,
								Jmax = 150)

# Simple light gradient
Irradiance <- seq(0, 2000, by = 100)

lr <- light_response(Irradiance, leafVar, enviroPar, leafPar)

plot(lr$Irradiance, lr$A, type = "l")

ks2sr <- function(ks) ks / (1 + ks)

# Find gs that maximizes A - cost of water
# model assumes kx is fixed and 'a' modulates additional cost of upper stomata
fn1 <- function(stomTrait, lambda, a, gx, gi, kx, ca, Vcmax, Kc, Ko, O, Jmax,
								gammaStar, Rd)
{
	gs <- stomTrait[1]
	ks <- stomTrait[2]
	benefit <- A(gs = gs, gx = gx, gi = gi, ks = ks, kx = kx, ca = ca, 
							 Vcmax = Vcmax,	Kc = Kc, Ko = Ko, O = O, Jmax = Jmax,
							 gammaStar = gammaStar, Rd = Rd)
	cost <- gs * lambda * (1 + a * ks) / (1 + ks)
	ret <- -(benefit - cost)
	return(ret)
}







##### OLD ----------------------------------------------------------------------

# Evaporation per area
gs * VPD # really just need single term (gsc to gsw conversion x VPD x cost of water)

# parameters
Pars <- list(
	ca = 400,
	gammaStar = 37.3,
	Kc = 268.3,
	Ko = 165084.2,
	O = 210000,
	gi = 0.5,
	gx = 0.5,
	kx = 1,
	Rd = 1,
	Vcmax = 50,
	Jmax = 75)

ks2sr <- function(ks) ks / (1 + ks)

# Find gs that maximizes A - cost of water
# model assumes kx is fixed and 'a' modulates additional cost of upper stomata
fn1 <- function(stomTrait, lambda, a, gx, gi, kx, ca, Vcmax, Kc, Ko, O, Jmax,
								gammaStar, Rd)
{
	gs <- stomTrait[1]
	ks <- stomTrait[2]
	benefit <- A(gs = gs, gx = gx, gi = gi, ks = ks, kx = kx, ca = ca, 
							 Vcmax = Vcmax,	Kc = Kc, Ko = Ko, O = O, Jmax = Jmax,
							 gammaStar = gammaStar, Rd = Rd)
	cost <- gs * lambda * (1 + a * ks) / (1 + ks)
	ret <- -(benefit - cost)
	return(ret)
}

fn2 <- function(stomTrait, lambda, a, gx, gi, kx, ca, Vcmax, Kc, Ko, O, 
								Jmax, gammaStar, Rd)
{
	gs <- stomTrait[1]
	benefit <- A(gs = gs, gx = gx, gi = gi, ks = 0, kx = kx, ca = ca, 
							 Vcmax = Vcmax,	Kc = Kc, Ko = Ko, O = O, Jmax = Jmax,
							 gammaStar = gammaStar, Rd = Rd)
	cost <- gs * lambda
	ret <- -(benefit - cost)
	return(ret)
}

# Optimum under ca = 400
fit1 <- with(Pars, optim(c(0.5, 1), fn1, lambda = 1, a = 3, gx = gx, gi = gi, 
												 kx = kx, ca = ca, Vcmax = Vcmax, Kc = Kc, Ko = Ko, 
												 O = O, Jmax = Jmax, gammaStar = gammaStar, Rd = Rd, 
												 method = "L-BFGS-B", lower = c(0, 0), upper = c(10, 10)))

fit1a <- with(Pars, optim(0.5, fn2, lambda = 1, a = 3, gx = gx, gi = gi, 
													kx = kx, ca = ca, Vcmax = Vcmax, Kc = Kc, Ko = Ko, 
													O = O, Jmax = Jmax, gammaStar = gammaStar, Rd = Rd, 
													method = "Brent", lower = 0, upper = 1))

# Optimum under ca = 700
fit2 <- with(Pars, optim(c(0.5, 1), fn1, lambda = 1, a = 3, gx = gx, gi = gi, 
												 kx = kx, ca = 700, Vcmax = Vcmax, Kc = Kc, Ko = Ko, 
												 O = O, Jmax = Jmax, gammaStar = gammaStar, Rd = Rd, 
												 method = "L-BFGS-B", lower = c(0, 0), upper = c(10, 10)))
fit2a <- with(Pars, optim(0.5, fn2, lambda = 1, a = 3, gx = gx, gi = gi, 
													kx = kx, ca = 700, Vcmax = Vcmax, Kc = Kc, Ko = Ko, 
													O = O, Jmax = Jmax, gammaStar = gammaStar, Rd = Rd, 
													method = "Brent", lower = 0, upper = 1))

# Optimum under ca = 280
fit3 <- with(Pars, optim(c(0.5, 1), fn1, lambda = 1, a = 3, gx = gx, gi = gi, 
												 kx = kx, ca = 280, Vcmax = Vcmax, Kc = Kc, Ko = Ko, 
												 O = O, Jmax = Jmax, gammaStar = gammaStar, Rd = Rd, 
												 method = "L-BFGS-B", lower = c(0, 0), upper = c(10, 10)))
fit3a <- with(Pars, optim(0.5, fn2, lambda = 1, a = 3, gx = gx, gi = gi, 
													kx = kx, ca = 280, Vcmax = Vcmax, Kc = Kc, Ko = Ko, 
													O = O, Jmax = Jmax, gammaStar = gammaStar, Rd = Rd, 
													method = "Brent", lower = 0, upper = 1))

# Fitness surfaces
gs <- seq(0, 0.7, length.out = 5e2)
ks <- seq(0.01, 2, length.out = 5e2)
traitSpace <- expand.grid(gs, ks)
colnames(traitSpace) <- c("gs", "ks")

# fitness surface under ca = 400
traitSpace$W1 <- with(Pars, -apply(traitSpace, 1, fn1, lambda = 1, a = 3, gx = gx, 
																	 gi = gi, kx = kx, ca = ca, Vcmax = Vcmax, 
																	 Kc = Kc, Ko = Ko, O = O, Jmax = Jmax,
																	 gammaStar = gammaStar, Rd = Rd))
traitSpace$w1 <- -traitSpace$W1 / fit1$value
W1a <- sapply(gs, function(X) with(Pars, -fn2(X, lambda = 1, a = 3, gx, gi, kx, 
																							ca, Vcmax, Kc, Ko, O, Jmax, 
																							gammaStar, Rd)))
w1a <- -W1a / fit1a$value


# fitness surface under ca = 700
traitSpace$W2 <- with(Pars, -apply(traitSpace, 1, fn1, lambda = 1, a = 3, gx = gx, 
																	 gi = gi, kx = kx, ca = 700, Vcmax = Vcmax, 
																	 Kc = Kc, Ko = Ko, O = O, Jmax = Jmax,
																	 gammaStar = gammaStar, Rd = Rd))
traitSpace$w2 <- -traitSpace$W2 / fit2$value
W2a <- sapply(gs, function(X) with(Pars, -fn2(X, lambda = 1, a = 3, gx, gi, kx, 
																							ca = 700, Vcmax, Kc, Ko, O, Jmax, 
																							gammaStar, Rd)))
w2a <- -W2a / fit2a$value

# fitness surface under ca = 280
traitSpace$W3 <- with(Pars, -apply(traitSpace, 1, fn1, lambda = 1, a = 3, gx = gx, 
																	 gi = gi, kx = kx, ca = 280, Vcmax = Vcmax, 
																	 Kc = Kc, Ko = Ko, O = O, Jmax = Jmax,
																	 gammaStar = gammaStar, Rd = Rd))
traitSpace$w3 <- -traitSpace$W3 / fit3$value
W3a <- sapply(gs, function(X) with(Pars, -fn2(X, lambda = 1, a = 3, gx, gi, kx, 
																							ca = 280, Vcmax, Kc, Ko, O, Jmax, 
																							gammaStar, Rd)))
w3a <- -W3a / fit3a$value

# w = 0.99 isocline ranges
c1 <- range(contourLines(gs, ks2sr(ks), 
												 matrix(traitSpace$w1, ncol = 5e2, byrow = F), 
												 levels = 0.99)[[1]]$x)
c1a <- range(gs[w1a >= 0.99])

c2 <- range(contourLines(gs, ks2sr(ks), 
												 matrix(traitSpace$w2, ncol = 5e2, byrow = F), 
												 levels = 0.99)[[1]]$x)
c2a <- range(gs[w2a >= 0.99])

c3 <- range(contourLines(gs, ks2sr(ks), 
												 matrix(traitSpace$w3, ncol = 5e2, byrow = F), 
												 levels = 0.99)[[1]]$x)
c3a <- range(gs[w3a >= 0.99])

pdf("~/Documents/Talks/Evolution2016/Fitness surfaces 0.pdf", 5, 5)
contour(gs, ks2sr(ks), matrix(traitSpace$w1, ncol = 5e2, byrow = F), 
				ylim = c(0, 2/3), xlim = c(0, 0.7), las = 1, lwd = 2,
				levels = c(1 - 10 ^ seq(-1, -2, length.out = 10)),
				col = rgb(seq(1, 0, length.out = 10), seq(1, 0, length.out = 10), 
									seq(1, 0, length.out = 10)), drawlabels = F)
points(fit1$par[1], fit1$par[2] / (1 + fit1$par[2]), pch = 19, cex = 2)
text(fit1$par[1], fit1$par[2] / (1 + fit1$par[2]), labels = "optimum", pos = 1)
dev.off()

pdf("~/Documents/Talks/Evolution2016/Fitness surfaces 0.5.pdf", 5, 5)
contour(gs, ks2sr(ks), matrix(traitSpace$w1, ncol = 5e2, byrow = F), 
				ylim = c(0, 2/3), level = 0.99, xlim = c(0, 0.7), las = 1, lwd = 2)
points(fit1$par[1], fit1$par[2] / (1 + fit1$par[2]), pch = 19, cex = 2)
text(fit1$par[1], fit1$par[2] / (1 + fit1$par[2]), labels = "optimum", pos = 1)
dev.off()

pdf("~/Documents/Talks/Evolution2016/Fitness surfaces 1.pdf", 5, 5)
contour(gs, ks2sr(ks), matrix(traitSpace$w1, ncol = 5e2, byrow = F), 
				ylim = c(0, 2/3), level = 0.99, xlim = c(0, 0.7), las = 1, lwd = 2)
points(fit1$par[1], fit1$par[2] / (1 + fit1$par[2]), pch = 19, cex = 2)

contour(gs, ks2sr(ks), matrix(traitSpace$w3, ncol = 5e2, byrow = F), col = "blue", 
				add = T, level = 0.99, lwd = 2)
points(fit3$par[1], ks2sr(fit3$par[2]), col = "blue", pch = 19, cex = 2)

legend(0.7, 2/3, lwd = 2, col = c("blue", "black", "red"), bty = "n", xjust = 1, 
			 legend = c("Preindustrial", "Today", "2100 (Projected)"))
dev.off()


pdf("~/Documents/Talks/Evolution2016/Fitness surfaces 2.pdf", 5, 5)
contour(gs, ks2sr(ks), matrix(traitSpace$w1, ncol = 5e2, byrow = F), 
				ylim = c(0, 2/3), level = 0.99, xlim = c(0, 0.7), las = 1, lwd = 2)
points(fit1$par[1], fit1$par[2] / (1 + fit1$par[2]), pch = 19, cex = 2)

contour(gs, ks2sr(ks), matrix(traitSpace$w2, ncol = 5e2, byrow = F), col = "red", 
				add = T, level = 0.99, lwd = 2)
points(fit2$par[1], ks2sr(fit2$par[2]), col = "red", pch = 19, cex = 2)

contour(gs, ks2sr(ks), matrix(traitSpace$w3, ncol = 5e2, byrow = F), col = "grey", 
				add = T, level = 0.99, lwd = 2)
points(fit3$par[1], ks2sr(fit3$par[2]), col = "grey", pch = 19, cex = 2)

legend(0.7, 2/3, lwd = 2, col = c("grey", "black", "red"), bty = "n", xjust = 1, 
			 legend = c("Preindustrial", "Today", "2100 (Projected)"))
dev.off()

x <- seq(200, 700, 10)
out <- sapply(x, function(X)
{
	fit <- with(Pars, optim(c(0.5, 1), fn1, lambda = 0.5, a = 3, gx = gx, 
													gi = gi, kx = kx, ca = X, Vcmax = Vcmax, Kc = Kc, 
													Ko = Ko, O = O, Jmax = Jmax, gammaStar = gammaStar, 
													Rd = Rd, method = "L-BFGS-B", lower = c(0, 0), 
													upper = c(10, 10)))
	return(setNames(fit$par, c("gs", "ks")))
})

plot(x, out["gs", ], type = "l")
plot(x, out["ks", ], type = "l", col = "red")
plot(x, out["ks", ] / (1 + out["ks", ]), type = "l", col = "red")
plot(out["gs", ], out["ks", ], type = "l")





# Now simulate standing variation
# Absolute fitness is directly proportional carbon gain

burnIn <- function(z0, N, muSig, ngen, initPars)
{
	
	t0 <- Sys.time()
	# z0 should be named vector
	z <- data.frame(gs = rep(z0["gs"], N), ks = rep(z0["ks"], N))
	gs_ab <- exp(log(z$gs / (1 + z$ks)) + rnorm(N, 0, muSig))
	gs_ad <- exp(log(z$ks * z$gs / (1 + z$ks)) + rnorm(N, 0, muSig))
	z$gs <- gs_ab + gs_ad
	z$ks <- gs_ad / gs_ab
	
	Zbar <- Zsig <- data.frame(gs = numeric(ngen), ks = numeric(ngen))
	Wbar <- numeric(ngen)
	for (i in 1:ngen)
	{
		W <- with(initPars, -apply(z, 1, fn1, lambda = lambda, a = a, gx = gx, 
															 gi = gi, kx = kx, ca = ca, Vcmax = Vcmax, 
															 Kc = Kc, Ko = Ko, O = O, Jmax = Jmax,
															 gammaStar = gammaStar, Rd = Rd))
		Wbar[i] <- mean(W)
		w <- W / max(W)
		z <- z[sample(N, replace = T, prob = w), 1:2]
		rownames(z) <- 1:N
		Zbar[i, ] <- apply(z, 2, mean)
		Zsig[i, ] <- apply(z, 2, var)
		gs_ab <- exp(log(z$gs / (1 + z$ks)) + rnorm(N, 0, muSig))
		gs_ad <- exp(log(z$ks * z$gs / (1 + z$ks)) + rnorm(N, 0, muSig))
		z$gs <- gs_ab + gs_ad
		z$ks <- gs_ad / gs_ab
		if (i %% (round(ngen / 10, 0)) == 0)
		{
			t1 <- Sys.time()
			timePerIter <- difftime(t1, t0, units = "mins") / i
			timeRemaining <- timePerIter * (ngen - i)
			cat(sprintf("Generation %s, %s minutes remaining\n", i, round(timeRemaining, 1)))
		}
	}
	
	ret <- list(z = z, Zbar = Zbar, Zsig = Zsig, Wbar = Wbar)
	return(ret)
}

z0 <- setNames(fit1$par, c("gs", "ks"))
N <- 1e3
ngen <- 1e3
muSig <- 0.1
Pars$lambda <- 1
Pars$a <- 2
bi <- burnIn(z0 = z0, N = N, muSig = muSig, ngen = ngen, initPars = Pars)

contour(gs, ks2sr(ks), matrix(traitSpace$w1, ncol = 1e2, byrow = F))
points(fit1$par[1], ks2sr(fit1$par[2]))
points(bi$Zbar[, 1], ks2sr(bi$Zbar[, 2]), type = "l", col = "grey")

plot(1:ngen, bi$Zbar[, 1], type = "l")
plot(1:ngen, ks2sr(bi$Zbar[, 2]), type = "l")

plot(1:ngen, bi$Zsig[, 1], type = "l")
plot(1:ngen, bi$Zsig[, 2], type = "l")

plot(1:ngen, bi$Wbar, type = "l")
plot(1:ngen, -bi$Wbar / fit1$value, type = "l")

doSim <- function(z, N, muSig, ngen, newPars)
{
	
	t0 <- Sys.time()
	
	Zbar <- Zsig <- data.frame(gs = numeric(ngen), ks = numeric(ngen))
	Wbar <- numeric(ngen)
	for (i in 1:ngen)
	{
		W <- with(newPars, -apply(z, 1, fn1, lambda = lambda, a = a, gx = gx, 
															gi = gi, kx = kx, ca = ca, Vcmax = Vcmax, 
															Kc = Kc, Ko = Ko, O = O, Jmax = Jmax,
															gammaStar = gammaStar, Rd = Rd))
		Wbar[i] <- mean(W)
		w <- W / max(W)
		z <- z[sample(N, replace = T, prob = w), 1:2]
		rownames(z) <- 1:N
		Zbar[i, ] <- apply(z, 2, mean)
		Zsig[i, ] <- apply(z, 2, var)
		gs_ab <- exp(log(z$gs / (1 + z$ks)) + rnorm(N, 0, muSig))
		gs_ad <- exp(log(z$ks * z$gs / (1 + z$ks)) + rnorm(N, 0, muSig))
		z$gs <- gs_ab + gs_ad
		z$ks <- gs_ad / gs_ab
		if (i %% (round(ngen / 10, 0)) == 0)
		{
			t1 <- Sys.time()
			timePerIter <- difftime(t1, t0, units = "mins") / i
			timeRemaining <- timePerIter * (ngen - i)
			cat(sprintf("Generation %s, %s minutes remaining\n", i, round(timeRemaining, 1)))
		}
	}
	
	ret <- list(z = z, w = w, Zbar = Zbar, Zsig = Zsig, Wbar = Wbar)
	return(ret)
}


newPars <- Pars
newPars$ca <- 700
sim <- doSim(bi$z, N, muSig, ngen, newPars)

contour(gs, ks2sr(ks), matrix(traitSpace$w2, ncol = 1e2, byrow = F))
points(fit2$par[1], ks2sr(fit2$par[2]))
points(sim$Zbar[, 1], ks2sr(sim$Zbar[, 2]), type = "l", col = "grey")

plot(sim$Zbar, col = "grey", type = "l")
plot(-sim$Wbar / fit2$value, type = "l")

plot(c(bi$Zbar[, "gs"], sim$Zbar[, "gs"]), type = "l")
lines(c(1, ngen), rep(fit1$par[1], 2))
lines(c(ngen + 1, 2 * ngen), rep(fit2$par[1], 2))

plot(ks2sr(c(bi$Zbar[, "ks"], sim$Zbar[, "ks"])), type = "l")
lines(c(1, ngen), ks2sr(rep(fit1$par[2], 2)))
lines(c(ngen + 1, 2 * ngen), ks2sr(rep(fit2$par[2], 2)))
