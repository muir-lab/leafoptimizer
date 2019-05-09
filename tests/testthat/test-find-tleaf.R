context("optimize_leaf")
library(leafoptimizer)

test_that("find_tleaf calculated leaf temperature correctly", {
  
  bp <- make_bakepar()
  cs <- make_constants()
  ep <- make_enviropar()
  lp <- make_leafpar(cs)
  
  traits <- "g_sc"
  carbon_costs <- list(H2O = 333.3)
  
  cs %<>% constants()
  lp %<>% leaf_par()
  bp %<>% photosynthesis::bake_par()
  ep %<>% enviro_par()
  
  # Concatenate parameters ----
  pars <- c(lp, ep, bp, cs)
  unitless_pars <- pars %>% purrr::map_if(function(x) is(x, "units"), drop_units)
  
  # Find optimum ----
  # soln <- find_optimum(traits, carbon_costs, unitless_pars, quiet = FALSE)
  # 
  # # Concatenate optimized traits in pars to calculate T_leaf, A, and E ----
  # pars %<>% c_optimized_traits(traits, soln) 
  # 
  # # Leaf temperature ----
  # tl1 <- tealeaves::tleaf(tealeaves::leaf_par(pars[tealeaves::parameter_names("leaf")]), 
  #                         tealeaves::enviro_par(pars[tealeaves::parameter_names("enviro")]),
  #                         tealeaves::constants(pars[tealeaves::parameter_names("constants")]),
  #                         quiet = TRUE, unitless = TRUE)
  # 
  # unitless_pars <- pars %>% purrr::map_if(function(x) is(x, "units"), drop_units)
  # tl2 <- find_tleaf(unitless_pars, unitless_pars, unitless_pars)
  # expect_equal(drop_units(tl1$T_leaf), tl2$T_leaf)
  # 
  # # Leaf photosynthesis
  # pars$T_leaf <- tl1$T_leaf
  # unitless_pars$T_leaf <- tl2$T_leaf
  # 
  # ph1 <- photosynthesis::photo(photosynthesis::leaf_par(pars),
  #                              photosynthesis::enviro_par(pars),
  #                              photosynthesis::bake_par(pars),
  #                              photosynthesis::constants(pars))
  # ph2 <- unitless_pars %>% 
  #   bake(., ., ., unitless = TRUE) %>%
  #   c(unitless_pars) %>%
  #   find_A()
  # 
  # expect_equal(drop_units(ph1$A), ph2$A)
  
})