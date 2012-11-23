#' SI (and other) units and tools for dealing with physical quantities
#'
#' @docType package
#' @name siunits
NULL

## Notes:
##
## - Ratio scales only -- no degrees Celsius to Kelvin
## - Uses SI as the underlying basis
##
## Main user functions:
## Quantity(v, unit_string, dimension_string) : make an object of class Quantity
## print(q, verbose = FALSE) : print an object q of class Quantity
## 
## add_unit
## add_dimension

## TODO
##
## most functions work only with simple measures
## measures should be recursive
## perhaps Units should have named rows?
## functions which take 'measure, dimension = NULL', should just take 'measure' and
## accept a string representation 
## write unit tests (aha ha)
## unit.Quantity
## dimension.Quantity (?)
## unit.String --> return details of unit
