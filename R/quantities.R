#' @import units.R dimensions.R measures.R

as.Quantity <- function(q, measure) {
  if (!inherits(q, "Quantity")) stop("'q' must be a Quantity")

  old.measure <- attr(q, "measure")
  
  if (is.character(measure)) {
    new.measure <- as.measure(measure, old.measure[[1]][[1]][[2]])
  } else {
    new.measure <- measure
  }
  
  if (!is.simple_measure(new.measure)) stop("'measure' must be a simple measure")

    if (!is.compatible.measure(new.measure, old.measure)) {
    stop("'q' and 'measure' must be unit compatible")
  }

  result <- q * unit.si_multiple(old.measure[[1]][[1]][[1]]) /
    unit.si_multiple(new.measure[[1]][[1]][[1]])

  attr(result, "measure") <- new.measure
  result
}
   

## Quantity: make a numeric object of class Quantity
## measure: either a measure, or a unit, or a string representing a unit

Quantity <- function(vec, measure, dimension = NULL) {
  if (class(vec) == "Quantity") warn("'vec' is already a Quantity")
  if (!is.numeric(vec)) stop("'vec' must be numeric")
  if (is.character(measure)) {
    measure <- as.measure(measure, dimension)
  } 
  if (!is.measure(measure)) stop("'measure' is not a valid measure")

  class(vec) <- c("Quantity", "numeric")
  attr(vec, "measure") <- measure
  vec
}

print.Quantity <- function(vec, verbose = FALSE, ...) {
  cat("Units:", format_measure(attr(vec, "measure"), verbose), "\n")
  print(c(vec), ...)
  invisible(vec)
}
