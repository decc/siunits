##' Functions for dealing with "atomic units"
##'
##' Atomic units are the units of defined dimensions. They are represented by a
##' single symbol
##' 
##' 
##'
##' 

## atomic_unit := one of a list of named units (kg, J, N, Gt, mHz, ...)
##
is.atomic_unit <- function(au) {
  au %in% Units$symbol
}

is.coherent_atomic_unit <- function(au) {
  (type.atomic_unit(au) == "coherent") | is.basis_atomic_unit(au)
}

is.basis_atomic_unit <- function(au) {
  (type.atomic_unit(au) == "basis")
}


## Accessors
## ---------

## Type of atomic unit
##
type.atomic_unit <- function(au) {
  Units$type[match(au, Units$symbol)]
}

## atomic_unit.dimension : atomic unit -> dimension
## Extract the dimension of an atomic unit
##
dimension.atomic_unit <- function(au) {
  Units$dimension[match(au, Units$symbol)]
}

## The name of the unit
name.unit <- function(au, singular = FALSE) {
  if (singular) {
    Units$name[match(au, Units$symbol)]
  } else {
    Units$plural.name[match(au, Units$symbol)]
  }
}

## Conversions

## si_multiple.atomic_unit: unit -> number: the multiple these units are of SI basis units 
si_multiple.atomic_unit <- function(au) {
  Units$multiple[match(au, Units$symbol)]
}  


##' Add a new atomic unit to the list of known atomic units
##' 
##' Creates a new atomic unit and adds it to the database. Units are units of a
##' specific dimension, and this dimension must exist already (if not, it may be
##' added by \code{\link{add_dimension}}).
##'
##' @param dimension The dimension for which this unit is defined. 
##' @param symbol The symbol to use for this unit. Must not already exists and,
##' if \code{gen.prefixes = TRUE}, none of the prefixed versions can be a name of
##' an existing unit.
##' @param name The English name of the unit.
##' @param plural.name The plural of the name. May be omitted, in which case it
##' will be derived from \code{name} by suffixing "s". 
##' @param is.coherent Whether this unit is an "SI coherent" unit -- that is,
##' the SI unit for \code{dimension}.
##' @param multiple The ratio between the size of this unit and the size of the
##' SI coherent unit. May be given as a \code{\link{Quantity}}, in which case
##' the unit will have the size of the given \code{Quantity}. 
##' @param gen.prefixes Whether to create SI-prefixed versions of this unit
##' ("giga-", "mega-", "kilo-", ...).
##' @param true.basis Internal.
##' @param series Not used.
##' @export
add_unit <- function(dimension, symbol, name, plural.name = "",
                     is.coherent = FALSE,
                     multiple = 1.0,
                     gen.prefixes = FALSE,
                     true.basis = NA,
                     series = NA) {
  if (!is.dimension(dimension)) {
    stop(dimension, " is not a known dimension")
  }
  
  if (is.Quantity(multiple)) {
    unit <- as.Unit(multiple)
    multiple <- as.numeric(multiple) * si_multiple.unit(unit) 
  }
  
  if (plural.name == "") {
    plural.name <- paste(name, "s", sep = "")
  }

  type <- ifelse(is.coherent, "coherent", "other")

  ## Coherent units should be added to the list of default units for each
  ## dimension, on the assumption that there is not more than one coherent unit
  ## for each dimension.
  if (is.coherent) {
    SI.Defaults[[dimension]] <- structure(symbol, class = "Unit")
  } 
  
  if (!gen.prefixes) {
    add_unit0(dimension, symbol, name, plural.name, type, multiple, series) 
  } else {
    
    ## Prefixed units are added one at a time so that add_unit0 can report
    ## sensible errors on duplicates. We don't expect new units to be added
    ## very frequently.

    if (is.na(true.basis)) {
      add_unit0(dimension, symbol, name, plural.name, type, multiple, series)
      prefix.range <- 1:length(SI.Prefixes$prefix)
      skip.multiple <- 1
    } else if (true.basis == symbol) {
      prefix.range <- 1:length(SI.Prefixes$prefix)
      skip.multiple <- 1
    } else {
      skip <- match(true.basis, paste(SI.Prefixes$prefix, symbol, sep = ""))
      prefix.range <- (1:length(SI.Prefixes$prefix))[-skip]
      skip.multiple <- SI.Prefixes$multiple[[skip]]
      add_unit0(dimension, symbol, name, plural.name, "other", 1/skip.multiple, series)
    }
    
    for (i in prefix.range) {
      add_unit0(dimension,
                paste(SI.Prefixes$prefix[[i]], symbol, sep = ""),
                paste(SI.Prefixes$name[[i]], name, sep = ""),
                paste(SI.Prefixes$name[[i]], plural.name, sep = ""),
                "other", # These are not coherent, derived units
                SI.Prefixes$multiple[[i]] * multiple / skip.multiple,
                series)
    }
  }
}


## Add a single unit

add_unit0 <- function(dimension, symbol, name, plural.name, type, multiple, series) {

  if (any(is.atomic_unit(symbol))) {
    type <- type.atomic_unit(symbol)
    stop("unit '", symbol, "' is already defined as a(n) ",
         type.atomic_unit(symbol), " unit")
  }
  
  Units <<- rbind(Units,
                  data.frame(symbol = symbol,
                             dimension = dimension,
                             name = name,
                             plural.name = plural.name,
                             type = type,
                             multiple = multiple,
                             series = series))
}








