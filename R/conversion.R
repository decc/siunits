##' Convert units to a different dimensional basis.
##'
##' Given a Quantity, convert to units with a given \code{\link{signature}}. The
##' signature of a unit is an expression involving dimensions, combined with
##' products and powers. If no signature is given, the quantity is converted to
##' the units in the default list \code{with}. If no default list is given, or a
##' dimension is not found in it, the SI default units for each dimension are used.  
##'
##' @param x An object whose units are to be converted. Either a \code{Unit} or a \code{Quantity}.
##' @param to A dimensional expression (see below) to convert to. 
##' @param with A list of default units for specified dimensions. 
##' @return An object of class Quantity.
##' @examples
##' \dontrun{
##' qq <- as.Quantity(1, "N m")
##' convert(as.Quantity"1 N m", to = "energy")
##' convert("1 N m", to = "energy", with = list(energy = "ktoe"))
##'
##' qe <- as.Quantity(1, "(N m)_[energy]") # Converts to SI units of energy
##' }
##' @export
convert <- function(x, to = NA, with = NULL) {
  if (!is.na(to)) {
    to <- as.Signature(to)
  } else {
    to <- as.Signature(x)
  }
  
  if (!is.null(with)) {
    with <- c(Map(as.Unit, with), units.env$SI.Defaults) # Prepend 'with' to 'SI.Defaults'
  } else {
    with <- units.env$SI.Defaults
  }
  
  out.unit <- make_unit_from_signature(to, with)
  as.Quantity(x, out.unit)
}


## Given unit = list(dimensions = unit, ...), go through 'sig' looking for dimensions,
## and replace them with the respective definitions in 'units' to make a Unit.
make_unit_from_signature <- function(sig, units) {
  structure(apply_units_to_signature(sig, units), class = "Unit")
}

apply_units_to_signature <- function(sig, units) {
  if (is.empty(sig)) {
    make_atomic_unit("")
  } else if (is.singleton(sig)) {
    retrieve_dimension(sig, units)
  } else if (is.derived(sig)) {
    make_derived_unit(lapply(sig[-1], function(x) {apply_units_to_signature(x, units)}))
  } else if (is.to_power(sig)) {
    make_unit_to_power(apply_units_to_signature((sig[2])[[1]], units), sig[[3]])
  }
}

## Given an atomic dimension and a list of units, return the Unit
retrieve_dimension <- function(dim, units) {
  units[[match(dim, names(units))]]
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

