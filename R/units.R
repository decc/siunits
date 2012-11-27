##' The Units class
##' 
##' Functions to create a Unit and to check membership of the Units class. A
##' Unit is either an atomic unit (eg, "kg", or "s") or powers and products of
##' units, possibly with associated dimensions for compound units.
##'
##' @name units
##' @rdname units
##' @aliases is.Unit as.Unit
##' @param u Object to be checked for membership in the Unit class.
##' @return In the case of \code{is.Unit}, a boolean; in the case of
##' \code{as.Unit}, an object of class \code{Unit}.
##' @seealso The package documentation for \code{\link{siunits}}.
##' @examples
##' \dontrun{is.Unit(as.Unit("kg (m s^-2)_[acceleration]"))} 
##' @export
##' 
is.Unit <- function(u) {
  inherits(u, "Unit")
}

##' @rdname units
##' @usage as.Unit(e)
##' @param e Object (character or Quantity) to be coerced to Unit. A Unit may
##' also be given as \code{e}, in which case it is returned unchanged. 
##' @export
as.Unit <- function(e) {
  UseMethod("as.Unit")
}

## <unit> ::= ATOMIC UNIT 
##          | list(*, <unit>, ... ) 
##          | list(^, <unit>, NUMBER)
##          | list(_, <unit>, DIMENSION) where DIMENSION is compatible with <unit>
##
## ATOMIC UNIT examples: "kg", "m", "s", "J"
## DIMENSION examples: "mass", "length", "time"
##
## The dimension of a <unit> is:
## ATOMIC UNIT : the dimension of the atomic unit
## (*, <unit>, ...) or (^, <unit>, NUMBER) : the derived dimension
## (_, <unit>, DIMENSION) : DIMENSION
##

##' @S3method as.Unit character
as.Unit.character <- function(e) {
  structure(check_unit(parse_unit(lexify_unit(e))$tree), class = "Unit")
}

##' @S3method as.Unit Unit
as.Unit.Unit <- function(e) {
  e
}

##' @S3method as.Unit Quantity
as.Unit.Quantity <- function(e) {
  attr(e, "unit")
}

## check_unit stops (with error) if its argument is not a unit, or if the atomic
## units or dimensions are not known. 

check_unit <- function(unit) {
  if (!is.unit(unit)) {
    stop("'unit' is not a well-formed unit (this error is likely due to a bug)")
  } else if (identical(length(unit), 0L)) {
    list()
  } else {
    check_unit0(unit)
  }
}

## Check unit stops on error
  
check_unit0 <- function(unit) {
  if (is.singleton(unit)) {
    check_atomic_unit(unit)
  } else if (is.derived(unit)) {
    check_derived_unit(unit)
  } else if (is.to_power(unit)) {
    check_unit_to_power(unit)
  } else if (is.dimensioned(unit)) {
    check_dimensioned_unit(unit)
  } else {
    stop("'unit' is not well-formed and this wasn't caught. Suspect bug in check_unit")
  }
}        

check_atomic_unit <- function(unit) {
  if (is.atomic_unit(unit)) {
    unit
  } else {
    stop('"', unit, '" is not a known unit', call. = FALSE)
  }
}

check_derived_unit <- function(unit) {
  make_derived_unit(lapply(unit[-1], check_unit0))
}

check_dimensioned_unit <- function(unit) {
  if (!is.dimension(unit[[3]])) {
    stop ('"', unit[[3]], '" is not a known dimension', call. = FALSE)
  } else if (!identical(dimension_to_basis(unit[[3]]),
                        unit_to_basis(unit[[2]]))) {
    stop ('"', unit[[3]], '" is not compatible with ',
          format_unit0(unit[[2]], verbose = TRUE, parens = TRUE),
          call. = FALSE)
  } 
  
  make_dimensioned_unit(check_unit0(unit[[2]]), unit[[3]])
}

check_unit_to_power <- function(unit) {
  if (identical(unit[[3]], 0)) {
    stop("zero is not an allowed power", call. = FALSE)
  }
  make_unit_to_power(check_unit0(unit[[2]]), unit[[3]])
}




