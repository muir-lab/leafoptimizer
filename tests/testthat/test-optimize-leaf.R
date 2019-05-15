context("optimize_leaf")
library(leafoptimizer)


# Revise and uncomment

# bake_par <- make_bakepar()
# constants <- make_constants()
# enviro_par <- make_enviropar()
# leaf_par <- make_leafpar()
# 
# carbon_costs <- list(H2O = 1000, SR = 0)
# 
# ol1 <- optimize_leaf("g_sc", carbon_costs, bake_par, constants, enviro_par,
#                      leaf_par)
# 
# ol2 <- optimize_leaf("leafsize", carbon_costs, bake_par, constants, enviro_par,
#                      leaf_par)
# 
# ol3 <- optimize_leaf("sr", carbon_costs, bake_par, constants, enviro_par,
#                      leaf_par)
# 
# ol4 <- optimize_leaf(c("g_sc", "leafsize"), carbon_costs, bake_par, constants,
#                      enviro_par, leaf_par)
# 
# ol5 <- optimize_leaf(c("g_sc", "sr"), carbon_costs, bake_par, constants,
#                      enviro_par, leaf_par)
# 
# ol6 <- optimize_leaf(c("leafsize", "sr"), carbon_costs, bake_par, constants,
#                      enviro_par, leaf_par)
# 
# ol7 <- optimize_leaf(c("g_sc", "leafsize", "sr"), carbon_costs, bake_par,
#                      constants, enviro_par, leaf_par)


test_that("optimize_leaf calculates T_leaf and A correctly", {
  
  cs <- make_constants()
  lp <- make_leafpar(cs)
  bp <- make_bakepar()  
  ep <- make_enviropar()
  
  pars <- c(cs, lp, bp, ep)
  upars <- pars %>% purrr::map_if(function(x) is(x, "units"), drop_units)
  
  # Calculate T_leaf, A, and E ----
  tl1 <- tealeaves::tleaf(
    tealeaves::leaf_par(pars), 
    tealeaves::enviro_par(pars), 
    tealeaves::constants(pars))$T_leaf
  
  upars$T_leaf <- upars %>% 
    find_tleaf(., . , .) %>%
    magrittr::use_series("T_leaf")
  tl2 <- upars$T_leaf
  
  expect_equal(drop_units(tl1), tl2)
  
  pars$T_leaf <- tl1
  upars$T_leaf <- tl2
  
  A1 <- photosynthesis::photo(
    photosynthesis::leaf_par(pars), 
    photosynthesis::enviro_par(pars), 
    photosynthesis::bake_par(pars),
    photosynthesis::constants(pars))$A

  ph <- upars %>% 
    c(bake(., ., ., unitless = TRUE)) %>%
    find_A()
  A2 <- ph$A
  
  expect_equal(drop_units(A1), A2)
  
})

test_that("carbon_balance calculates E correctly", {
  
  # cs <- make_constants()
  # lp <- make_leafpar(cs)
  # bp <- make_bakepar()  
  # ep <- make_enviropar()
  # 
  # T_leaf <- set_units(300, "K")
  # 
  # blp <- lp %>% 
  #   c(T_leaf = T_leaf) %>% 
  #   photosynthesis::bake(bp, cs, unitless = FALSE)
  # 
  # baked_pars <- c(cs, lp[!(names(lp) %in% names(blp))], blp, ep) %>%
  #   purrr::map_if(function(x) is(x, "units"), drop_units)
  # 
  # eb1 <- tealeaves::energy_balance(T_leaf, lp, ep, cs, quiet = TRUE, 
  #                                  unitless = FALSE, components = TRUE)
  # E1 <- eb1$components$E
  # T_leaf %<>% drop_units()
  # eb2 <- tealeaves::energy_balance(T_leaf, baked_pars, baked_pars, baked_pars, quiet = TRUE, 
  #                                 components = TRUE, unitless = TRUE, check = FALSE)
  # E2 <- eb2$components$E
  # expect_equal(E1, E2)
  
})
