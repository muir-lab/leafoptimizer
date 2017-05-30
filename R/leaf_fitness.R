#' Leaf fitness
#'
#' @inheritParams evolve_leaf
#'
#' @param return_negative Logical. Should negative fitness be returned? This is useful for optimization, which usually search for minimum.
#'
#' @importFrom magrittr %<>% %>%
#'
#' @export
#'

leaf_fitness <- function(traits, constraints, leaf_par, enviro_par, constants, return_negative = FALSE, ...) {

  ##### Checks -----
  check_traits(traits)
  check_constraints(constraints)
  check_leafpar(leaf_par, traits)
  check_enviropar(enviro_par)
  check_constants(constants)

  ##### Add traits to leaf_par
  leaf_par %<>% c(traits)

  ##### Find leaf temperature -----
  T_leaf <- find_Tleaf(leaf_par, enviro_par, constants)

  ##### Find evaporation -----
  # Should this be calculated in find_Tleaf??

  ##### Find photosynthetic rate -----
  ##### Calculate fitness and return -----

  if (return_negative) return(-fitness)

  fitness

}

check_traits <- function(traits) {

  match.arg(names(traits), c("g_sw", "sr", "leafsize"),
            several.ok = TRUE)

  stopifnot(all(sapply(traits, is.numeric)))
  stopifnot(all(sapply(traits, function(X) length(X) == 1)))

}

check_constraints <- function(constraints) {

  stopifnot(is.list(constraints))
  stopifnot(length(constraints) == 1)
  if (names(constraints) != "lambda") stop("Only lambda constraint is currently implemented.")
  stopifnot(is.numeric(constraints$lambda))
  stopifnot(length(constraints$lambda) == 1)

}

check_leafpar <- function(leaf_par, traits) {

  if (!inherits(leaf_par, "leaf_par")) stop("Use `make_leafpar` function to generate leaf_par")
  mt <- .missing_traits(traits)
  stopifnot(all(mt %in% names(leaf_par)))
  if (length(mt) > 0) {
    stopifnot(all(sapply(traits, function(X) is.numeric(leaf_par[[X]]))))
    stopifnot(all(sapply(traits, function(X) length(leaf_par[[X]]) == 1)))
  }

}

check_enviropar <- function(enviro_par) {

  if (!inherits(enviro_par, "enviro_par")) stop("Use `make_enviropar` function to generate enviro_par")

}

check_constants <- function(constants) {

  if (!inherits(constants, "constants")) stop("Use `make_constants` function to generate constants")

}
