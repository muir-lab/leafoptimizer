context("make_parameter_sets")
library(leafoptimizer)

test_that("make_parameter_sets makes the correct number of set", {
  
  cs <- make_constants()
  lp <- make_leafpar(cs)
  bp <- make_bakepar()
  ep <- make_enviropar()
  
  pars <- c(lp, ep)
  par_units <- purrr::map(pars, units) %>%
    magrittr::set_names(names(pars))
  
  pars %<>% make_parameter_sets(cs, par_units)
  
  expect_true(is.list(pars))
  expect_true(length(pars) == 1L)
  
  ep <- make_enviropar(replace = list(
    S_sw = set_units(1, "W/m^2")
  ))
  
  pars <- c(lp, ep)
  par_units <- purrr::map(pars, units) %>%
    magrittr::set_names(names(pars))
  
  pars %<>% make_parameter_sets(cs, par_units)
  
  expect_true(is.list(pars))
  expect_true(length(pars) == 1L)
  
  ep <- make_enviropar(replace = list(
    PPFD = set_units(1500, "umol/m^2/s"),
    S_sw = set_units(660, "W/m^2")
  ))
  
  pars <- c(lp, ep)
  par_units <- purrr::map(pars, units) %>%
    magrittr::set_names(names(pars))
  pars %<>% make_parameter_sets(cs, par_units)
  
  expect_true(is.list(pars))
  expect_true(length(pars) == 1L)

  ep <- make_enviropar(replace = list(
    PPFD = set_units(c(500, 1500), "umol/m^2/s"),
    S_sw = set_units(c(220, 660), "W/m^2")
  ))
  
  pars <- c(lp, ep)
  par_units <- purrr::map(pars, units) %>%
    magrittr::set_names(names(pars))
  pars %<>% make_parameter_sets(cs, par_units)
  
  expect_true(is.list(pars))
  expect_true(length(pars) == 2L)
  
  ep <- make_enviropar()
  lp <- make_leafpar(replace = list(
    g_sw = set_units(1, "umol/m^2/s/Pa"),
    g_sc = gw2gc(set_units(1, "umol/m^2/s/Pa"), cs$D_c0, cs$D_w0, FALSE)
  ), constants = cs)
  
  pars <- c(lp, ep)
  par_units <- purrr::map(pars, units) %>%
    magrittr::set_names(names(pars))
  pars %<>% make_parameter_sets(cs, par_units)
  
  expect_true(is.list(pars))
  expect_true(length(pars) == 1L)

  lp <- make_leafpar(replace = list(
    g_sw = set_units(1:2, "umol/m^2/s/Pa"),
    g_sc = gw2gc(set_units(1:2, "umol/m^2/s/Pa"), cs$D_c0, cs$D_w0, FALSE)
  ), constants = cs)
  
  pars <- c(lp, ep)
  par_units <- purrr::map(pars, units) %>%
    magrittr::set_names(names(pars))
  pars %<>% make_parameter_sets(cs, par_units)
  
  expect_true(is.list(pars))
  expect_true(length(pars) == 2L)
  
  lp <- make_leafpar(replace = list(
    g_uw = set_units(1, "umol/m^2/s/Pa"),
    g_uc = gw2gc(set_units(1, "umol/m^2/s/Pa"), cs$D_c0, cs$D_w0, FALSE)
  ), constants = cs)
  
  pars <- c(lp, ep)
  par_units <- purrr::map(pars, units) %>%
    magrittr::set_names(names(pars))
  pars %<>% make_parameter_sets(cs, par_units)
  
  expect_true(is.list(pars))
  expect_true(length(pars) == 1L)
  
  lp <- make_leafpar(replace = list(
    g_uw = set_units(1:2, "umol/m^2/s/Pa"),
    g_uc = gw2gc(set_units(1:2, "umol/m^2/s/Pa"), cs$D_c0, cs$D_w0, FALSE)
  ), constants = cs)
  
  pars <- c(lp, ep)
  par_units <- purrr::map(pars, units) %>%
    magrittr::set_names(names(pars))
  pars %<>% make_parameter_sets(cs, par_units)
  
  expect_true(is.list(pars))
  expect_true(length(pars) == 2L)
  
  ep <- make_enviropar(replace = list(
    PPFD = set_units(c(500, 1500), "umol/m^2/s"),
    S_sw = set_units(c(220, 660), "W/m^2")
  ))
  lp <- make_leafpar(replace = list(
    g_sw = set_units(1:2, "umol/m^2/s/Pa"),
    g_sc = gw2gc(set_units(1:2, "umol/m^2/s/Pa"), cs$D_c0, cs$D_w0, FALSE)
  ), constants = cs)
  
  pars <- c(lp, ep)
  par_units <- purrr::map(pars, units) %>%
    magrittr::set_names(names(pars))
  pars %<>% make_parameter_sets(cs, par_units)
  
  expect_true(is.list(pars))
  expect_true(length(pars) == 4L)
  
  ep <- make_enviropar(replace = list(
    PPFD = set_units(c(500, 1500), "umol/m^2/s")
  ))
  lp <- make_leafpar(constants = cs)
  
  pars <- c(lp, ep)
  par_units <- purrr::map(pars, units) %>%
    magrittr::set_names(names(pars))
  pars %<>% make_parameter_sets(cs, par_units)
  
  expect_true(is.list(pars))
  expect_true(length(pars) == 2L)
  
})
  