##' Convert units to a different dimensional basis.
##'
##' Given a unit, convert to a unit with a given \code{\link{signature}}. The
##' signature of a unit is an expression involving dimensions, combined with
##' products and powers.  
##'
##' @note Since conversion of units typically introduces a numerical factor, an
##' argument of class \code{Unit} will be coerced to \code{Quantity} before conversion.
##'
##'
##' @param x An object whose units are to be converted. Either a \code{Unit} or a \code{Quantity}.
##' @param to A dimensional expression (see below) to convert to. 
##' @param with A list of default units for specified dimensions. 
##' @return A `Quantity`.
##'
##'
##'
##'
##'
##'
##'

convert <- function(x, to = NA, with = NA) {


}


## What is this unit as a multiple of SI basis units? 

si_multiple.unit <- function(u) {
  if (identical(length(u), 0L)) {
    1.0
  } else if (is.singleton(u)) {
    si_multiple.atomic_unit(u)
  } else if (is.derived(u)) {
    Reduce(`*`, lapply(u[-1], si_multiple.unit))
  } else if (is.to_power(u)) {
    si_multiple.unit(u[[2]])^u[[3]]
  } else if (is.dimensioned(u)) {
    si_multiple.unit(u[[2]])
  }
}

