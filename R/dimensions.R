## Dimensions

dimension_to_basis <- function(dim) {
  Dimensions[[dim]]$vector
}

## Reduce a vector of the form (mass = 1, length = 2, ...) to basis dimensions
dimv_to_basis <- function(dimv) {
  if (identical(length(dimv), 0L)) {
    dimension_to_basis(DIMENSIONLESS)
  } else {
    Reduce(`+`,
           Map(`*`, dimv, lapply(names(dimv), dimension_to_basis)))
  }
}


is.dimension <- function(dimension) {
  !is.null(Dimensions[[dimension]])
}

add_dimension <- function(name, definition) {
  if (name %in% names(Dimensions)) {
    stop("dimension [", name, "] already defined")
  }
  Dimensions[[name]] <<- list(definition = definition,
                              vector = dimv_to_basis(definition))
}

## Definitions of dimensions
## Should be package local in the end

Basis.Dimensions <- data.frame(
  symbol = c("L", "M", "T", "I", "Th", "N", "J"),
  name = c("length", "mass", "time", "electric current",
    "thermodynamic temperature", "amount of substance", "luminous intensity"),
  dimension = c("length", "mass", "time", "electric_current",
    "temperature", "amount", "luminous_intensity"),
  stringsAsFactors = FALSE) 

## TODO: Need a "Dimensions -> default Unit" map.

DIMENSIONLESS = "ONE"

Dimensions <- list(
  ONE = list(
    definition = NULL,
    vector = c(0,0,0,0,0,0,0)),
  length = list(
    definition = NULL,
    vector = c(1,0,0,0,0,0,0)),
  mass = list(
    definition = NULL,
    vector = c(0,1,0,0,0,0,0)),
  time = list(
    definition = NULL,
    vector = c(0,0,1,0,0,0,0)),
  electric_current = list(
    definition = NULL,
    vector = c(0,0,0,1,0,0,0)),
  temperature = list(
    definition = NULL,
    vector = c(0,0,0,0,1,0,0)),
  amount = list(
    definition = NULL,
    vector = c(0,0,0,0,0,1,0)),
  luminous_intensity = list(
    definition = NULL,
    vector = c(0,0,0,0,0,0,1)))

