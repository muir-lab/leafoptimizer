#' S3 class enviro_par
#' @exportClass enviro_par
#

#' @param .x A list to be constructed into \strong{enviro_par}.
#' 
#' @description 
#' 
#' Constructor function for enviro_par class. This function ensures that environmental parameter inputs are properly formatted.
#' 
#' @export

enviro_par <- function(.x) {
  
  which <- "enviro"
  nms <- parameter_names(which)
  
  stopifnot(is.list(.x))
  
  stopifnot(all(nms %in% names(.x)))
  
  repeated_tab <- plyr::count(names(.x)) %>%
    dplyr::filter(.data$freq > 1)
  if (nrow(repeated_tab) > 0) {
    repeated_tab$x %>%
      as.character() %>%
      stringr::str_c(collapse = ", ") %>%
      glue::glue("{x} ha{suffix} more than one entry. Only one named entry is allowed per parameter.", x = ., suffix = dplyr::if_else(stringr::str_detect(., ", "), "ve", "s")) %>%
      stop()
  }
  
  .x %<>% magrittr::extract(nms)
  
  # Set units ----
  .x$E_q %<>% set_units("kJ/mol")
  .x$f_par %<>% set_units()
  
  # Check units ----
  stopifnot(.x$E_q >= set_units(0, "kJ/mol"))
  stopifnot(.x$f_par >= set_units(0) & .x$f_par <= set_units(1))
  
  tl_enviropar <- tealeaves::enviro_par(.x)
  ph_enviropar <- photosynthesis::enviro_par(.x)
  shared_enviropar <- intersect(names(tl_enviropar), names(ph_enviropar))
  stopifnot(identical(tl_enviropar[shared_enviropar], 
                      ph_enviropar[shared_enviropar]))
  
  structure(.x, class = c(stringr::str_c(which, "_par"), "list"))
  
}

