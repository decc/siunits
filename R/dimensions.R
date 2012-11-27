##' Add a dimensions
##'
##' Create a new dimension. Dimensions must be added before the corresponding
##' coherent unit. The definition of the dimension is used to work out its basis
##' and cannot reference a component dimension until it has been added. 
##'  
##' @param name The name of this dimension as a character.  
##' @param definition The definition of this dimension in terms of previously
##' defined dimensions. Expressed as a named numeric vector: the names are the
##' previous dimensions and the values are the power of the named dimensions.
##' @examples
##' \dontrun{add_dimension("velocity", c(length = 1, time = -1))}
##' @export
add_dimension <- function(name, definition) {
  if (name %in% names(units.env$Dimensions)) {
    stop("dimension [", name, "] already defined")
  }
  dimensions <- units.env$Dimensions
  dimensions[[name]] <- list(definition = definition,
                             vector = dimv_to_basis(definition))
  assign("Dimensions", dimensions, envir = units.env)
}


## Return the 7 element vector representing this dimension in basis dimensions
dimension_to_basis <- function(dim) {
  units.env$Dimensions[[dim]]$vector
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
  !is.null(units.env$Dimensions[[dimension]])
}


