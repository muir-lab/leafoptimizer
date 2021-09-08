check_carbon_costs <- function(carbon_costs, quiet) {

  checkmate::assert_flag(quiet)
  
  checkmate::assert_list(carbon_costs, types = "double", len = 2L,
                         any.missing = FALSE, names = "named")
  checkmate::assert_subset(names(carbon_costs), choices = c("H2O", "SR"))
  checkmate::assert_double(carbon_costs$H2O, lower = 0, len = 1L)
  checkmate::assert_double(carbon_costs$SR, lower = 0, len = 1L)

  carbon_costs
  
}

check_results <- function(soln) {
  
  if (soln$convergence == 1) {
    "Optimization did not converge, NA returned. Inspect parameters carefully." %>%
      crayon::red() %>%
      message()
  }
  
}
