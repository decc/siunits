##' A class to represent physical quantities
##'
##' Provides a class suitable for representating physical quantities: numerical values having
##' dimension and units. 
##'
##' @name quantities
##' @rdname quantities
##' @aliases is.Quantity as.Quantity
##' @param q Object to be checked for membership in the Quantity class.
##' @seealso The package documentation for \code{\link{siunits}}.
##' @examples
##' \dontrun{as.Quantity(1, "(kW h)_[energy]")}
##' @export is.Quantity
is.Quantity <- function(q) {
  inherits(q, "Quantity")
}

##' @rdname quantities
##' @usage as.Quantity(value, unit = "")
##' @param value
##' A numeric value or a Quantity. In the case of a numberic value
##' a Quantity will be created with the given units. A Quantity argument will be
##' converted to the given units if they are compatible with \code{value}'s
##' existing units.
##' @param unit
##' An object of class Unit, or a character representation which
##' will be coerced (by \code{as.Unit}).
##' @export as.Quantity
as.Quantity <- function(value, unit = "") {
  UseMethod("as.Quantity")
}

## Internal function to create an object of class Quantity. `vaue` must be
## numeric, `unit` must be a Unit. Attributes of value are preserved, including
## `names` in particular.
make_quantity <- function(value, unit) {
  structure(value, class = c("Quantity", "numeric"), unit = unit)
}

##' @S3method as.Quantity Quantity
as.Quantity.Quantity <- function(value, unit) {

  if (nargs() == 1L) return(value)

  old.unit <- as.Unit(value)
  unit <- as.Unit(unit)
  if (!is.compatible_unit(unit, old.unit)) {
    stop("can't convert ", format(old.unit), " to ", format(unit), call. = FALSE)
  }
  
  make_quantity(unclass(value) * si_multiple.unit(old.unit) /
                si_multiple.unit(unit), unit)
}

##' @S3method as.Quantity numeric
as.Quantity.numeric <- function(value, unit) {
  make_quantity(value, as.Unit(unit))
}

##' @S3method print Quantity
print.Quantity <- function(x, verbose = FALSE, ...) {
  unit <- as.Unit(x)
  attr(x, "unit") <- NULL
  print(unclass(x), ...)
  cat("Units:", format(unit, verbose), "\n")
  invisible(x)
}

##' @S3method format Quantity
format.Quantity <- function(x, digits = NULL, na.encode = FALSE,
                            justify = justify, ...) {
  paste(NextMethod(x), format.Unit(as.Unit(x)))
}

##' @S3method as.character Quantity
as.character.Quantity <- function(x, ...) {
  x <- vapply(x, function(xx) {
    paste(xx, format(as.Unit(x), ...)) }, character(1))
  NextMethod()
}

