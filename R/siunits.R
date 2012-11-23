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
## is.similar_unit(u1, u2) to check, eg, if looks like [power] [time], that is,
## has the same "dimensional signature"
## * + / -
## to_SI : "(N m)_[energy] ktoe^-1" -> "J J^-1" (retains signature)
## write unit tests (aha ha)
##
## defaults, so, for example, print(quantity, defaults = TRUE),
## or print(quantity, defaults = list(area = "wales", energy = "Mtoe"))
##
## as.Quantity(str)
##
## simplify.unit: looks for units to cancel: doesn't change units.
## simplify.Q : cancels and changes
