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

##' @S3method as.Quantity Quantity
as.Quantity.Quantity <- function(value, unit) {

  if (nargs() == 1L) return(value)
  
  unit <- as.Unit(unit)
  old.unit <- as.Unit(value)
  if (!is.compatible_unit(unit, old.unit)) {
    stop("can't convert ", format(old.unit), " to ", format(unit), call. = FALSE)
  }
  
  result <- as.numeric(value) * si_multiple.unit(old.unit) /
    si_multiple.unit(unit)
  
  as.Quantity(result, unit)

}

##' @S3method as.Quantity numeric
as.Quantity.numeric <- function(value, unit) {
  unit <- as.Unit(unit)
  structure(value, class = c("Quantity", "numeric"), unit = unit)
}

##' @S3method print Quantity
print.Quantity <- function(x, verbose = FALSE, ...) {
  cat("Units:", format(as.Unit(x), verbose), "\n")
  print(c(x), ...)
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

