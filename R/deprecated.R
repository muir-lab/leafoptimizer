#' Calculate carbon balance
#' 
#' @inheritParams optimize_leaves
#' @param pars Concatenated parameters (\code{leaf_par}, \code{enviro_par}, \code{bake_par}, and \code{constants}) with \code{units} dropped
#' 
#' @return Value of class \code{numeric} indicating the carbon balance.
#' 
#' @details 
#' 
#' This function is not intended to be called directly because most checks have been removed for speed in \code{\link{optimize_leaf}}.
#' 
#' Currently only carbon a cost of water lost to transpiration is supported. Functions calculate the instantaneous leaf-level carbon gain and water loss per area. In the future, I plan to extend functionality to other resources (e.g. nitrogen) and integrate over time courses.
#' 
#' The basic equation is:
#' 
#' Carbon Balance = Carbon gain - (Carbon cost of water) Water loss
#' 
#' @examples 
#' library(magrittr)
#' cs <- make_constants()
#' lp <- make_leafpar(cs)
#' bp <- make_bakepar()  
#' ep <- make_enviropar()
#' 
#' lp$T_leaf <- tealeaves::tleaf(lp, ep, cs, unitless = TRUE)$T_leaf
#' blp <- lp %>% 
#'   bake(bp, cs)
#' carbon_costs <- list(H2O = 0.003)
#' pars <- c(cs, lp[!(names(lp) %in% names(blp))], blp, ep) %>%
#'   purrr::map_if(function(x) is(x, "units"), drop_units)
#' 
#' # everything must be unitless
#' # pars must already be baked
#' carbon_balance(pars, carbon_costs)
#' 
#' @export

carbon_balance <- function(pars, carbon_costs) {
  
  names(pars) %>% 
    stringr::str_replace("25$", "") %>% 
    magrittr::is_in(names(pars)) %>% 
    all() %>%
    stopifnot()
  
  # Carbon gain ----
  C_gain <- find_A(pars)$A
  
  # Carbon costs ----
  tl <- tealeaves::energy_balance(pars$T_leaf, pars, pars, pars, quiet = TRUE, 
                                  components = TRUE, unitless = TRUE, check = FALSE)
  C_cost <- carbon_costs$H2O * drop_units(tl$components$E) * 1e6
  
  # Carbon balance ----
  C_gain - C_cost
  
}
