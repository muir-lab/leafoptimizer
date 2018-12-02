#' Leaf parameter temperature responses
#' 
#' @description a wrapper for \code{\link[photosynthesis]{bake}}
#' 
#' @name bake
#' 
#' @inheritParams optimize_leaves
#' @param check Logical. Should all parameter sets be checked? TRUE is safer, but FALSE is faster.
#' 
#' @export


bake <- function(leaf_par, bake_par, constants, unitless = FALSE, 
                 check = !unitless) {
  
  photosynthesis::bake(leaf_par, bake_par, constants, unitless, check)
  
}
