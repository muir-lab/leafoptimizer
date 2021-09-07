#' Get vector of parameter names
#' 
#' @param which A character string indicating which parameter names to retrieve, "constants, "bake", "enviro", or "leaf". Partial matching allowed.
#' 
#' @examples 
#' parameter_names("leaf")
#' 
#' @export
#' 

parameter_names <- function(which) {
  
  checkmate::assert_choice(which, 
                           choices = c("bake", "constants", "enviro", "leaf"))
  
  bakepar_names <- photosynthesis::parameter_names("bake", use_tealeaves = TRUE)

  constants_names <- photosynthesis::parameter_names("constants", 
                                                     use_tealeaves = TRUE)
  
  enviropar_names <- photosynthesis::parameter_names("enviro", 
                                                     use_tealeaves = TRUE)
  
  leafpar_names <- photosynthesis::parameter_names("leaf", 
                                                   use_tealeaves = TRUE)
  
  which %>% 
    match.arg(c("leaf", "enviro", "bake", "constants")) %>%
    switch(
         leaf = leafpar_names,
         enviro = enviropar_names,
         bake = bakepar_names,
         constants = constants_names
  )
  
}
