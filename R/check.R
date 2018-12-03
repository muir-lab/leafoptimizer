check_carbon_costs <- function(carbon_costs, quiet) {
  
  stopifnot(is.list(carbon_costs))
  stopifnot("H2O" %in% names(carbon_costs))
  stopifnot(length(carbon_costs$H2O) == 1L)
  stopifnot(is.numeric(carbon_costs$H2O))
  if (!quiet & length(carbon_costs) > 1L) {
    "Only the carbon cost of H2O is currently supported. Other elements will be ignored." %>%
      crayon::green() %>%
      message()
  }
  
  carbon_costs
  
}

check_results <- function(soln) {
  
  if (soln$convergence == 1) {
    "stats::optim did not converge, NA returned. Inspect parameters carefully." %>%
      crayon::red() %>%
      message()
  }
  
}