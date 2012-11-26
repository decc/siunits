## Physical Quantities

is.Quantity <- function(q) {
  inherits(q, "Quantity")
}

## Quantity: make a numeric object of class Quantity
## measure: either a unit, or a string representing a unit

as.Quantity <- function(value, unit = "") {
  UseMethod("as.Quantity")
}

##' @S3method as.Quantity Quantity
as.Quantity.Quantity <- function(value, unit) {

  if (nargs() == 1L) return(value)
  
  unit <- as.Unit(unit)
  old.unit <- as.Unit(value)
  if (!is.compatible_unit(unit, old.unit)) {
    stop("can't convert ", old.unit, " to ", unit)
  }
  
  result <- as.numeric(value) * si_multiple.unit(old.unit) /
    si_multiple.unit(unit)
  
  as.Quantity(result, unit)

}

as.Quantity.numeric <- function(value, unit) {
  unit <- as.Unit(unit)
  structure(value, class = c("Quantity", "numeric"), unit = unit)
}

##' @S3method print Quantity
print.Quantity <- function(q, verbose = FALSE, ...) {
  cat("Units:", format(as.Unit(q), verbose), "\n")
  print(c(q), ...)
  invisible(q)
}

##' @S3method format Quantity
format.Quantity <- function(q, digits = NULL, na.encode = FALSE,
                            justify = justify) {
  paste(NextMethod(q), format.Unit(as.Unit(q)))
}

as.character.Quantity <- function(q) {
  q <- vapply(q, function(qq) {
    paste(qq, format(as.Unit(q))) }, character(1))
  NextMethod()
}

