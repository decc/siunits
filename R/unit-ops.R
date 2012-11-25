## Operations on units
## -------------------

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

## Return the 7-element dimension basis vector of this unit

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

is.compatible_unit <- function(u1, u2) {
  identical(unit_to_basis(as.Unit(u1)),
            unit_to_basis(as.Unit(u2)))
}


