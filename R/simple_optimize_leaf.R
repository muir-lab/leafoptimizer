#' Optimize C3 photosynthesis, simply
#' @description \code{simple_optimize_leaf}: simulate C3 photosynthesis over a single parameter set
#' @rdname optimize_leaves
#' @export

simple_optimize_leaf <- function(traits, carbon_costs, leaf_par, enviro_par, bake_par, 
                                 constants) {
  
  traits %<>% match.arg("g_sc")
  
  pars <- c(constants, leaf_par, enviro_par, bake_par)
  
  .f <- function(g_sc, carbon_costs, pars) {
    
    pars$g_sc <- set_units(g_sc, "umol/m^2/s/Pa")
    pars$g_sw <- gc2gw(pars$g_sc, pars$D_c0, pars$D_w0, unitless = FALSE)
    tl <- tealeaves::tleaf(tealeaves::leaf_par(pars),
                           tealeaves::enviro_par(pars),
                           tealeaves::constants(pars), 
                           quiet = TRUE, unitless = TRUE)
    pars$T_leaf <- tl$T_leaf
    ph <- photosynthesis::photo(photosynthesis::leaf_par(pars),
                                photosynthesis::enviro_par(pars),
                                photosynthesis::bake_par(pars),
                                photosynthesis::constants(pars),
                                quiet = TRUE)
    
    ret <- ph$A - set_units(tl$E, "umol/m^2/s") * carbon_costs$H2O
    
    ret %<>% 
      drop_units() %>%
      magrittr::multiply_by(-1)
    
    ret
    
  }
  
  soln <- optim(4, .f, carbon_costs = carbon_costs, pars = pars, method = "Brent",
                lower = 0, upper = 10)
  
  soln
   
}
  
