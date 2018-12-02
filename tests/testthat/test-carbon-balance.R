context("carbon_balance")
library(leafoptimizer)

test_that("carbon_balance need baked, unitless pars", {
  
  cs <- make_constants()
  lp <- make_leafpar(cs)
  bp <- make_bakepar()  
  ep <- make_enviropar()
  
  T_leaf <- set_units(300, "K")
  
  carbon_costs <- list(H2O = 0.003)
  
  unbaked_pars <- c(cs, lp, ep) %>%
    purrr::map_if(function(x) is(x, "units"), drop_units)
  
  blp <- lp %>% 
    c(T_leaf = T_leaf) %>% 
    photosynthesis::bake(bp, cs, unitless = FALSE)
  
  baked_pars1 <- c(cs, lp[!(names(lp) %in% names(blp))], blp, ep)
  
  baked_pars2 <- baked_pars1 %>%
    purrr::map_if(function(x) is(x, "units"), drop_units)
  
  # T_leaf has units
  expect_error(carbon_balance(T_leaf, carbon_costs, baked_pars2))
  
  # pars has units
  expect_error(carbon_balance(drop_units(T_leaf), carbon_costs, baked_pars1))

  # pars not baked units
  expect_error(carbon_balance(drop_units(T_leaf), carbon_costs, unbaked_pars))
  
  # Should work
  cb <- carbon_balance(drop_units(T_leaf), carbon_costs, baked_pars2)
  expect_is(cb, "numeric")
  
})
  
test_that("carbon_balance calculates A correctly", {
  
  cs <- make_constants()
  lp <- make_leafpar(cs)
  bp <- make_bakepar()  
  ep <- make_enviropar()
  
  T_leaf <- set_units(300, "K")
  
  blp <- lp %>% 
    c(T_leaf = T_leaf) %>% 
    photosynthesis::bake(bp, cs, unitless = FALSE)
  
  baked_pars <- c(cs, lp[!(names(lp) %in% names(blp))], blp, ep) %>%
    purrr::map_if(function(x) is(x, "units"), drop_units)
  
  A1 <- photosynthesis::photo(c(lp, T_leaf = T_leaf), ep, bp, cs, quiet = TRUE)$A
  A2 <- find_A(baked_pars)$A
  
  expect_equal(drop_units(A1), A2)
  
})

test_that("carbon_balance calculates E correctly", {
  
  cs <- make_constants()
  lp <- make_leafpar(cs)
  bp <- make_bakepar()  
  ep <- make_enviropar()
  
  T_leaf <- set_units(300, "K")
  
  blp <- lp %>% 
    c(T_leaf = T_leaf) %>% 
    photosynthesis::bake(bp, cs, unitless = FALSE)
  
  baked_pars <- c(cs, lp[!(names(lp) %in% names(blp))], blp, ep) %>%
    purrr::map_if(function(x) is(x, "units"), drop_units)
  
  eb1 <- tealeaves::energy_balance(T_leaf, lp, ep, cs, quiet = TRUE, 
                                   unitless = FALSE, components = TRUE)
  E1 <- eb1$components$E
  T_leaf %<>% drop_units()
  eb2 <- tealeaves::energy_balance(T_leaf, baked_pars, baked_pars, baked_pars, quiet = TRUE, 
                                  components = TRUE, unitless = TRUE, check = FALSE)
  E2 <- eb2$components$E
  expect_equal(E1, E2)
  
})

test_that("carbon_balance calculates carbon balance correctly", {
  
  cs <- make_constants()
  lp <- make_leafpar(cs)
  bp <- make_bakepar()  
  ep <- make_enviropar()
  
  T_leaf <- set_units(300, "K")
  
  carbon_costs <- list(H2O = 0.003)
  
  blp <- lp %>% 
    c(T_leaf = T_leaf) %>% 
    photosynthesis::bake(bp, cs, unitless = FALSE)
  
  baked_pars <- c(cs, lp[!(names(lp) %in% names(blp))], blp, ep) %>%
    purrr::map_if(function(x) is(x, "units"), drop_units)
  
  E1 <- tealeaves::energy_balance(T_leaf, lp, ep, cs, quiet = TRUE, 
                                  unitless = FALSE, components = TRUE)$components$E
  A1 <- photosynthesis::photo(c(lp, T_leaf = T_leaf), ep, bp, cs, quiet = TRUE)$A
  
  C_gain <- A1
  C_cost <- carbon_costs$H2O * set_units(E1, "umol/m^2/s")
  
  # Carbon balance ----
  cb1 <- C_gain - C_cost
  
  cb2 <- carbon_balance(drop_units(T_leaf), carbon_costs, baked_pars)
  expect_equal(cb1, cb2)
  
})