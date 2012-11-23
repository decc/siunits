#' @export Quantity
#' @export as.Quantity
#' @export print.Quantity
#' @export `+.Quantity`
#' @export `-.Quantity`
#' @export `*.Quantity`
#' @export `/.Quantity`
#' @export `^.Quantity`
#' @export as.character.Quantity

## Physical Quantities

is.Quantity <- function(q) {
  inherits(q, "Quantity")
}

## Quantity: make a numeric object of class Quantity
## measure: either a unit, or a string representing a unit

Quantity <- function(vec, unit) {
  if (is.Quantity(vec)) warn("'vec' is already a Quantity")
  if (!is.numeric(vec)) stop("'vec' must be numeric")

  unit <- as.Unit(unit)
  structure(vec, class = c("Quantity", "numeric"), unit = unit)
}

print.Quantity <- function(q, verbose = FALSE, ...) {
  cat("Units:", format_unit(as.Unit(q), verbose), "\n")
  print(c(q), ...)
  invisible(q)
}

as.Quantity <- function(q, new.unit) {
  if (!is.Quantity(q)) stop("'q' must be a Quantity")
  
  old.unit <- as.Unit(q)
  
  if (is.character(new.unit)) {
    new.unit <- as.Unit(new.unit)
  } else if (!is.unit(new.unit)) {
    stop ("'new.unit' must be a unit or a string")
  }
  
  if (!is.compatible_unit(new.unit, old.unit)) {
    stop("'q' and 'new.unit' must be unit compatible")
  }
  
  result <- as.numeric(q) * si_multiple.unit(old.unit) /
    si_multiple.unit(new.unit)
  
  Quantity(result, new.unit)
}

as.character.Quantity <- function(q) {
  q <- vapply(q, function(qq) {
    paste(qq, format_unit(as.Unit(q))) }, character(1))
  NextMethod()
}

`+.Quantity` <- function(e1, e2) {
  if (nargs() == 1L) return(e1)
  
  if (!is.compatible_unit(as.Unit(e1), as.Unit(e2))) {
    stop("arguments to '+' must be unit compatible", call. = FALSE) 
  }
  
  Quantity(as.numeric(e1) + as.numeric(as.Quantity(e2, as.Unit(e1))),
           as.Unit(e1))
}

`-.Quantity` <- function(e1, e2) {
  if (!is.compatible_unit(as.Unit(e1), as.Unit(e2))) {
    stop("arguments to '-' must be unit compatible", call. = FALSE) 
  }
  
  Quantity(as.numeric(e1) - as.numeric(as.Quantity(e2, as.Unit(e1))),
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
    return(Quantity(e1 *  as.numeric(e2), as.Unit(e2)))
  } else {
    return(Quantity(as.numeric(e1) * as.numeric(e2), as.Unit(e1)))
  }
}

`/.Quantity` <- function(q1, q2) {
  if (!is.compatible_unit(as.Unit(q1), as.Unit(q2))) {
    stop("arguments to '+' must be unit compatible", call. = FALSE) 
  }

  Quantity(as.numeric(q1) + as.numeric(as.Quantity(q2, as.Unit(q1))),
           as.Unit(q1))
}

`^.Quantity` <- function(q, e) {

  Quantity(as.numeric(q1) + as.numeric(as.Quantity(q2, as.Unit(q1))),
           as.Unit(q1))
}
