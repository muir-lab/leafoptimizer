context("water to CO2 conductance")
library(leafoptimizer)

test_that("conversion works correctly", {
  
  constants <- make_constants()
  
  gw1 <- gc2gw(3, constants$D_c0, constants$D_w0, FALSE)
  gw2 <- gc2gw(3, constants$D_c0, constants$D_w0, TRUE)
  expect_equal(drop_units(gw1), gw2)

  gw3 <- gc2gw(3, constants$D_c0, constants$D_w0, FALSE)
  gw4 <- gc2gw(3, drop_units(constants$D_c0), drop_units(constants$D_w0), TRUE)
  expect_equal(drop_units(gw3), gw4)
  
  gc1 <- gw2gc(gw2, constants$D_c0, constants$D_w0, FALSE)
  gc2 <- gw2gc(gw1, constants$D_c0, constants$D_w0, TRUE)
  expect_equal(drop_units(gc1), gc2)

  gc3 <- gw2gc(gw2, constants$D_c0, constants$D_w0, FALSE)
  gc4 <- gw2gc(gw1, drop_units(constants$D_c0), drop_units(constants$D_w0), TRUE)
  expect_equal(drop_units(gc3), gc4)
  
  expect_equal(gc2, 3)
  expect_equal(gc4, 3)
  
})
  