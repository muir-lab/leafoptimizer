#' Get vector of parameter names
#' 
#' @param which A character string indicating which parameter names to retreive, "leaf", "enviro", "bake", or "constants". Partial matching allowed.
#' 
#' @examples 
#' parameter_names("leaf")
#' 
#' @export
#' 

parameter_names <- function(which) {
  
  which %>% 
    match.arg(c("leaf", "enviro", "bake", "constants")) %>%
    switch(
         leaf = unique(c(tealeaves::parameter_names("leaf"), 
                         photosynthesis::parameter_names("leaf"))) %>%
           magrittr::extract(. != "T_leaf"),
         enviro = unique(c(tealeaves::parameter_names("enviro"), 
                           photosynthesis::parameter_names("enviro"),
                           "E_q", "f_par")),
         bake = photosynthesis::parameter_names("bake"),
         constants = unique(c(tealeaves::parameter_names("constants"), 
                              photosynthesis::parameter_names("constants")))
  )
  
}
