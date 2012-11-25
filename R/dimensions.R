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

