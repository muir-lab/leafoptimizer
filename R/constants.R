#' S3 class constants
#' @exportClass constants
#

#' @param .x A list to be constructed into \strong{constants}.
#' 
#' @description 
#' 
#' Constructor function for constants class. This function ensures that physical constant inputs are properly formatted.
#' 
#' @export

constants <- function(.x) {
  
  checkmate::assert_list(.x)
  
  which <- "constants"
  nms <- leafoptimizer::parameter_names(which)
  
  if (!all(nms %in% names(.x))) {
    nms[!(nms %in% names(.x))] %>%
      stringr::str_c(collapse = ", ") %>%
      glue::glue("{x} not in parameter names required for {which}",
                 x = ., which = which) %>%
      stop()
  }
  
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
  
  structure(.x[nms], class = c(which, "list"))
  
}

