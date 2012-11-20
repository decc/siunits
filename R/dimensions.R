#' @import units.R

## A unit is a compatible unit for a dimension just in case the reduction of
## the dimensions of the unit to basis dimensions is the same as the reduction
## of the dimensions of the, er  ... dimension.

to_basis <- function(dim_expr) {
  Reduce(`+`,                                           # Add up ...
         Map(`*`,                                       # the powers of ..
             dim_expr,                            
             lapply(Dimensions[names(dim_expr)],        # the basis dimensions
                    function(x) {`[[`(x, "vector")} ))) # (extracting the vectors).   
}

## unit: vector-of (atomic_unit, power)
is.compatible_unit <- function(unit, dimension) {
  unit.dimensions <- unit
  names(unit.dimensions) <- Units$dimension[match(names(unit), Units$symbol)]
  
  identical(to_basis(unit.dimensions),
            Dimensions[[dimension]]$vector)
}

## 
## atomic_measure := pair (unit, dimension) such that unit is a compatible unit for dimension
## 
## measure := list-of (atomic_measure, power) such that no power is zero
##                

is.dimension <- function(dimension) {
  !is.null(Dimensions[[dimension]])
}

add_dimension <- function(name, definition) {
  if (name %in% names(Dimensions)) {
    stop("dimension [", name, "] already defined")
  }
  Dimensions[[name]] <<- list(definition = definition,
                              vector = to_basis(definition))
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

Dimensions <- list(
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

