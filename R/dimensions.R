#' @export add_dimension

## Reduce a dimension vector to basis dimensions 
## dv : a dimension vector (a generic vector is allowed)

to_basis_dimensions <- function(dv) {
  Reduce(`+`,                                           # Add up ...
         Map(`*`,                                       # the powers of ..
             dv,                            
             lapply(Dimensions[names(dv)],        # the basis dimensions
                    function(x) {`[[`(x, "vector")} ))) # (extracting the vectors).   
}


is.dimension <- function(dimension) {
  !is.null(Dimensions[[dimension]])
}

add_dimension <- function(name, definition) {
  if (name %in% names(Dimensions)) {
    stop("dimension [", name, "] already defined")
  }
  Dimensions[[name]] <<- list(definition = definition,
                              vector = to_basis_dimensions(definition))
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

