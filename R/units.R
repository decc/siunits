## Units
##
## <unit> ::= ATOMIC UNIT 
##          | (*, <unit>, ... ) 
##          | (^, <unit>, NUMBER)
##          | (_, <unit>, DIMENSION) where DIMENSION is compatible with <unit>
##
## ATOMIC UNIT examples: "kg", "m", "s", "J"
## DIMENSION examples: "mass", "length", "time"

## The dimension of a <unit> is:
## ATOMIC UNIT : the dimension of the atomic unit
## (*, <unit>, ...) or (^, <unit>, NUMBER) : the derived dimension
## (_, <unit>, DIMENSION) : DIMENSION
##

is.Unit <- function(u) {
  inherits(u, "Unit")
}

as.Unit <- function(e) {
  UseMethod("as.Unit")
}

as.Unit.character <- function(e) {
  structure(check_unit(parse_unit(lexify_unit(e))$tree), class = "Unit")
}

as.Unit.Unit <- function(e) {
  e
}

as.Unit.Quantity <- function(e) {
  attr(e, "unit")
}

ensure_unit <- function(u) {
  u <- as.Unit(u)
  if (is.Unit(u)) {
    u
  } else {
    FALSE
  }
}

## is.unit checks syntax only, does not check whether ATOMIC UNITs or
## DIMENSIONSs are defined. 

is.unit <- function(u) {
  (identical(length(u), 0L)
   || is.single_atomic_unit(u) 
   || is.derived_unit(u)
   || is.unit_to_power(u)
   || is.dimensioned_unit(u))
}

## check.unit stops (with error) if its argument is not a unit. check.unit does
## check whether the atomic units and dimensions are known

check_unit <- function(unit) {
  if (!is.unit(unit)) {
    stop("'unit' is not a well-formed unit (this error is likely due to a bug)")
  } else if (identical(length(unit), 0L)) {
    list()
  } else {
    check_unit0(unit)
  }
}
             
is.single_atomic_unit <- function(u) {
  (identical(length(u), 1L) && is.character(u))
}

## Is u of the form (_, <unit>, DIMENSION)
is.dimensioned_unit <- function(u) {
  (is.list(u) && identical(length(u), 3L)
   && u[[1]] == quote(`_`)
   && is.unit(u[[2]])
   && (is.character(u[[3]]) && identical(length(u[[3]]), 1L)))
}

## Is u of the form list(*, <unit> , ... )
is.derived_unit<- function(u) {
  (is.list(u) && (length(u) > 1)
   && u[[1]] == quote(`*`)
   && all(vapply(u[-1], is.unit, logical(1))))
}

## Is u of the form list(^, <unit>, power)
is.unit_to_power <- function(u) {
  (is.list(u) && identical(length(u), 3L)
   && u[[1]] == quote(`^`)
   && is.unit(u[[2]])
   && is.numeric(u[[3]])
   && !identical(u[[2]], 0))
}

check_unit0 <- function(unit) {
  if (is.single_atomic_unit(unit)) {
    check_atomic_unit(unit)
  } else if (is.derived_unit(unit)) {
    check_derived_unit(unit)
  } else if (is.unit_to_power(unit)) {
    check_unit_to_power(unit)
  } else if (is.dimensioned_unit(unit)) {
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

## Writing units as strings
## ------------------------

print.Unit <- function(u, verbose = TRUE) {
  cat(format(u, verbose), "\n")
  invisible(u)
}

format.Unit <- function(u, verbose = FALSE) {
  if (identical(length(u), 0L)) {
    ""
  } else {
    format_unit0(unclass(u), verbose, parens = FALSE)
  }
}

format_unit0 <- function(u, verbose, parens) {
  if (is.character(u)) {
    u
  } else if (u[[1]] == quote(`*`)) {
    format_derived_unit(u, verbose, parens)
  } else if (u[[1]] == quote(`_`)) {
    format_dimensioned_unit(u, verbose, parens)
  } else if (u[[1]] == quote(`^`)) {
    format_unit_to_power(u, verbose, parens)
  }
}

format_derived_unit <- function(u, verbose, parens) {
  str <- paste(vapply(u[-1], format_unit0, character(1), verbose = verbose,
                      parens = TRUE), collapse = " ")
  if (parens && (length(u) > 2)) {
    paste("(", str, ")", sep = "")
  } else {
    str
  }
}

format_dimensioned_unit <- function(u, verbose, parens) {
  if (verbose) {
    str <-  paste(format_unit0(u[[2]], verbose, parens = !parens), "_[", u[[3]], "]", sep = "")
  } else {
    str <- format_unit0(u[[2]], verbose, parens = !parens)
  }
  
  ##  if (parens) {
  ##    paste("(", str, ")", sep = "")
  ##  } else {
  str
  ##  }
}

format_unit_to_power <- function(u, verbose, parens) {
  if (identical(u[[3]], 1)) {
    format_unit0(u[[2]], verbose, parens = TRUE)
  } else {
    paste(format_unit0(u[[2]], verbose, parens = TRUE), "^", u[[3]], sep = "")
  }
}


## Unit manipulations
## ------------------

## The units of u^-1
inverse_unit <- function(u) {
  power_unit(u, -1)
}

power_unit <- function(u, n) {
  if (is.single_atomic_unit(u)){
    structure(make_unit_to_power(u, n), class = "Unit")
  } else if (is.unit_to_power(u)) {
    structure(make_unit_to_power(u[[2]], n * u[[3]]), class = "Unit")
  } else {
    structure(make_unit_to_power(u, n), class = "Unit")
  }
}

## The units of u1 u2
product_unit <- function(u1, u2) {
  structure(make_derived_unit(list(u1,u2)), class = "Unit")
}




## Functions for extracting information from units
## -----------------------------------------------

## Return the 7-element dimension basis vector of this unit

unit_to_basis <- function(u) {
  if (identical(length(u), 0L)) {
    dimension_to_basis(DIMENSIONLESS)
  } else if (is.single_atomic_unit(u)) {
    dimension_to_basis(dimension.atomic_unit(u))
  } else if (is.derived_unit(u)) {
    Reduce(`+`, lapply(u[-1], unit_to_basis))
  } else if (is.unit_to_power(u)) {
    u[[3]] * unit_to_basis(u[[2]])
  } else if (is.dimensioned_unit(u)) {
    dimension_to_basis(u[[3]])
  }
}

is.compatible_unit <- function(u1, u2) {
  identical(unit_to_basis(ensure_unit(u1)),
            unit_to_basis(ensure_unit(u2)))
}


## What is this unit as a multiple of SI basis units? 
si_multiple.unit <- function(u) {
  u <- ensure_unit(u)
  if (identical(length(u), 0L)) {
    1.0
  } else if (is.single_atomic_unit(u)) {
    si_multiple.atomic_unit(u)
  } else if (is.derived_unit(u)) {
    Reduce(`*`, lapply(u[-1], si_multiple.unit))
  } else if (is.unit_to_power(u)) {
    si_multiple.unit(u[[2]])^u[[3]]
  } else if (is.dimensioned_unit(u)) {
    si_multiple.unit(u[[2]])
  }
}

