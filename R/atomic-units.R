### Units - A physical quantity package for SI units (and extensions)

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

indef.article <- function(str) {
  if (substr(str, 1, 1) %in% c("a", "A", "e", "E", "i", "I", "o", "O", "u",
                               "U")) {
    "an"
  } else {
    "a" }
}
    
## atomic_unit := one of a list of named units (kg, J, N, Gt, mHz, ...)

is.basis_unit <- function(au) {
  au %in% Units$symbol[Units$type == "basis"]
}

is.coherent_unit <- function(au) {
  au %in% Units$symbol[Units$type == "coherent"] | is.basis_unit(au)
}

is.atomic_unit <- function(au) {
  au %in% Units$symbol
}

atomic_unit.type <- function(au) {
  Units$type[Units$symbol == au]
}

## unit := vector-of (atomic_unit = power) such that no atomic_unit occurs twice and no
## power is zero

is.unit <- function(u) {
  (is.numeric(u)
   && all(is.atomic_unit(names(u))) 
   && !anyDuplicated(names(u))
   && !any(u == 0))
}

## unit.dimension : unit -> dimension
## Extract the dimension of a unit

unit.dimension <- function(symb) {
  Units$dimension[match(symb, Units$symbol)]
}



## as.unit: string -> unit (eg, "kg m^2 s^-2") 
as.unit <- function(str) {
  unlist(
    lapply(strsplit(
      unlist(strsplit(str, " ", fixed = TRUE)),
      "^", fixed = TRUE),
           make_unit_part))
}

## make_unit_part: list(unit, power) -> c(unit = power)
make_unit_part <- function(ll) {
  if (length(ll) == 1L) {
    power <- 1
  } else {
    power <- as.numeric(ll[[2]])
  }

  if (!is.atomic_unit(ll[[1]])) stop (ll[[1]], " is not a known unit")
  if (identical(power, 0L)) stop ("powers of 0 are not allowed in unit definitions")
  
  names(power) <- ll[[1]]
  power
}


## TODO: Need to capture the numeric-relevant options to 'print'



## format_unit: unit -> "kg m^2 s^-2" (eg)
format_unit <- function(unit) {
  paste(
    mapply(format_unit_part, names(unit), unit), collapse = " ")
}

## format_unit_part: -> m^2 (eg)
format_unit_part <- function(symbol, power) {
  if (power == 1) {
    symbol
  } else {
    paste(symbol, "^", power, sep = "")
  }
}

## to_SI: Quantity -> Quantity expressed in SI units, using the SI unit for each
## corresponding dimension, where possible

## unit.si_multiple: unit -> number: the multiple these units are of SI basis units 
unit.si_multiple <- function(unit) {
  Reduce(`*`,
         mapply(function(au, power) {
           Units$multiple[Units$symbol == au]^power
         }, names(unit), unit))
}  





add_unit0 <- function(dimension, symbol, name, plural.name, type, multiple, series) {

  if (any(is.atomic_unit(symbol))) {
    type <- atomic_unit.type(symbol)
    article <- indef.article(type) 
    stop("unit '", symbol, "' is already defined (as", article, atomic_unit.type(symbol), "unit)")
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

## Add units, including making up all the prefixed versions, calling add_unit0
## for each individual unit (and to check for duplicates).
## TODO: Vectorise

add_unit <- function(dimension, symbol, name, plural.name = "",
                     is.coherent = FALSE,
                     multiple = 1.0,
                     gen.prefixes = FALSE,
                     true.basis = NA,
                     series = NA) {

  if (inherits(multiple, "Quantity")) {
    measure <- attr(multiple, "measure")
    if (!is.simple_measure(measure)) {
      stop("'multiple' must be a simple measure or numeric")
    }
    multiple <- as.numeric(multiple) * unit.si_multiple(measure[[1]][[1]][[1]]) 
  }
  
  if (plural.name == "") {
    plural.name <- paste(name, "s", sep = "")
  }
    
  type <- if (is.coherent) "coherent" else "other"
  
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
      add_unit0(dimension, symbol, name, plural.name, "other", multiple, series)
      skip <- match(true.basis, paste(SI.Prefixes$prefix, symbol, sep = ""))
      prefix.range <- (1:length(SI.Prefixes$prefix))[-skip]
      skip.multiple <- SI.Prefixes$multiple[[skip]]
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



## Units in basis.units are the default for those dimensions whose
## definition is NULL. The order should be precisely the same as the order in
## Basis.Dimensions

Units <- data.frame(
  symbol = c("m", "kg", "s", "A", "K", "mol", "cd"),
  dimension = c("length", "mass", "time", "electric_current",
    "temperature", "amount", "luminous_intensity"), 
  name = c("metre", "kilogram", "second", "ampere", "kelvin", "mole",
    "candela"),
  plural.name = c("metres", "kilograms", "seconds", "amperes", "kelvins",
    "moles", "candelas"),
  type = "basis",
  multiple = 1.0,
  series = NA,
  stringsAsFactors = FALSE
  )

SI.Prefixes <- data.frame(
  name = c("yotta", "zetta", "exa", "peta", "tera", "giga", "mega", "kilo",
    "hecto", "deca", "deci", "centi", "milli", "micro", "nano", "pico", "femto",
    "atto", "zepto", "yocto"),
  multiple = c(1e24, 1e21, 1e18, 1e15, 1e12, 1e9, 1e6, 1e3, 1e2, 10, 0.1, 1e-2,
    1e-3, 1e-6, 1e-9, 1e-12, 1e-15, 1e-18, 1e-21, 1e-24), 
  prefix = c("Y", "Z", "E", "P", "T", "G", "M", "k", "h", "da", "d", "c", "m",
    "µ", "n", "p", "f", "a", "z", "y"),
  stringsAsFactors = FALSE) 







