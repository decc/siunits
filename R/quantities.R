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

`+.Quantity` <- function(e1, e2) {
  if (nargs() == 1L) return(e1)
  
  if (!is.compatible_unit(as.Unit(e1), as.Unit(e2))) {
    stop("arguments to '+' must be unit compatible", call. = FALSE) 
  }
  
  as.Quantity(as.numeric(e1) + as.numeric(as.Quantity(e2, as.Unit(e1))),
           as.Unit(e1))
}

`-.Quantity` <- function(e1, e2) {
  if (nargs() == 1L) return(as.Quantity(e1))
  
  if (!is.compatible_unit(as.Unit(e1), as.Unit(e2))) {
    stop("arguments to '-' must be unit compatible", call. = FALSE) 
  }
  
  as.Quantity(as.numeric(e1) - as.numeric(as.Quantity(e2, as.Unit(e1))),
           as.Unit(e1))
}

`*.Quantity` <- function(e1, e2) {
  if (!is.Quantity(e2)) { # Assume that e2 is numeric and put the numeric in
                                        # first position
    tmp <- e2
    e2 <- e1
    e1 <- tmp
  }
  
  if (!is.Quantity(e1)) {
    return(as.Quantity(e1 * as.numeric(e2), as.Unit(e2)))
  } else {
    return(as.Quantity(as.numeric(e1) * as.numeric(e2), product_unit(as.Unit(e1), as.Unit(e2))))
  }
}

`/.Quantity` <- function(e1, e2) {
  if (!is.Quantity(e2)) {
    return(as.Quantity(as.numeric(e1) / e2, as.Unit(e1)))
  } else if (!is.Quantity(e1)) {
    return(as.Quantity(e1 / as.numeric(e2), inverse_unit(as.Unit(e2))))
  }
  
  if (!is.compatible_unit(as.Unit(q1), as.Unit(q2))) {
    stop("arguments to '+' must be unit compatible", call. = FALSE) 
  }

  as.Quantity(as.numeric(q1) + as.numeric(as.Quantity(q2, as.Unit(q1))),
           as.Unit(q1))
}

`^.Quantity` <- function(q, num) {

  as.Quantity(as.numeric(q)^num, power_unit(as.Unit(q), num))
}
