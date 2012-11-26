## Operations on units
## -------------------

## The units of u^-1
inverse_unit <- function(u) {
  power_unit(u, -1)
}

## The units of u^n
power_unit <- function(u, n) {
  if (is.singleton(u)){
    structure(make_unit_to_power(u, n), class = "Unit")
  } else if (is.to_power(u)) {
    structure(make_unit_to_power(u[[2]], n * u[[3]]), class = "Unit")
  } else {
    structure(make_unit_to_power(u, n), class = "Unit")
  }
}

## The units of u1 u2
product_unit <- function(u1, u2) {
  structure(make_derived_unit(list(u1,u2)), class = "Unit")
}

## Returns a vector of 7 elements, representing the SI basis of this unit
unit_to_basis <- function(u) {
  if (identical(length(u), 0L)) {
    dimension_to_basis(DIMENSIONLESS)
  } else if (is.singleton(u)) {
    dimension_to_basis(dimension.atomic_unit(u))
  } else if (is.derived(u)) {
    Reduce(`+`, lapply(u[-1], unit_to_basis))
  } else if (is.to_power(u)) {
    u[[3]] * unit_to_basis(u[[2]])
  } else if (is.dimensioned(u)) {
    dimension_to_basis(u[[3]])
  }
}

## Checks arguments for equality of fundamental SI bases 
is.compatible_unit <- function(u1, u2) {
  identical(unit_to_basis(as.Unit(u1)),
            unit_to_basis(as.Unit(u2)))
}


