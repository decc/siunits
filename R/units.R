##' The Units class
##'
##' @param u Object to be checked for membership in the Unit class
##' @return Boolean.
##' @examples
##' \dontrun{is.Unit(as.Unit("kg"))} 
##' @export
##' 
is.Unit <- function(u) {
  inherits(u, "Unit")
}

##' Create a Unit 
##'
##' Functions to create Units from character, Unit, or Quantities.
##'
##' @param e A character vector of length one, or a Unit, or a Quantity. A
##' character vector will be parsed as a string representation of a unit.
##' @return A Unit. In the case of a Unit or Quantity argument, the Unit of the argument.
##' @export as.Unit
 
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

as.Unit <- function(e) {
  UseMethod("as.Unit")
}

##' @S3method as.Unit character
as.Unit.character <- function(e) {
  structure(check_unit(parse_unit(lexify_unit(e))$tree), class = "Unit")
}

##' @S3method as.Unit Unit
as.Unit.Unit <- function(e) {
  e
}

##' @S3method as.Unit Unit
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
    stop('"', unit, '" is not a known unit')
  }
}

check_derived_unit <- function(unit) {
  make_derived_unit(lapply(unit[-1], check_unit0))
}

check_dimensioned_unit <- function(unit) {
  if (!is.dimension(unit[[3]])) {
    stop ('"', unit[[3]], '" is not a known dimension')
  } else if (!identical(dimension_to_basis(unit[[3]]),
                        unit_to_basis(unit[[2]]))) {
    stop ('"', unit[[3]], '" is not compatible with ', format_unit0(unit[[2]],
                                                                    verbose = TRUE,
                                                                    parens = TRUE))
  } 
  
  make_dimensioned_unit(check_unit0(unit[[2]]), unit[[3]])
}

check_unit_to_power <- function(unit) {
  if (identical(unit[[3]], 0)) {
    stop("zero is not an allowed power")
  }
  make_unit_to_power(check_unit0(unit[[2]]), unit[[3]])
}




