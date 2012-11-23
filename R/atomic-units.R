## Functions for dealing with "atomic units"

## indef.article : either "a" or "an" depending on the first letter of the word.
indef.article <- function(str) {
  if (substr(str, 1, 1) %in% c("a", "A", "e", "E", "i", "I", "o", "O", "u",
                               "U")) {
    "an"
  } else {
    "a" }
}


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


## Conversions

## si_multiple.atomic_unit: unit -> number: the multiple these units are of SI basis units 
si_multiple.atomic_unit <- function(au) {
  Units$multiple[match(au, Units$symbol)]
}  


## Making new units

## Add units, including making up all the prefixed versions, calling add_unit0
## for each individual unit (and to check for duplicates).
## TODO: Vectorise

add_unit <- function(dimension, symbol, name, plural.name = "",
                     is.coherent = FALSE,
                     multiple = 1.0,
                     gen.prefixes = FALSE,
                     true.basis = NA,
                     series = NA) {

  if (is.Quantity(multiple)) {
    unit <- as.Unit(multiple)
    multiple <- as.numeric(multiple) * si_multiple.unit(unit) 
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


## Add a single unit

add_unit0 <- function(dimension, symbol, name, plural.name, type, multiple, series) {

  if (any(is.atomic_unit(symbol))) {
    type <- type.atomic_unit(symbol)
    article <- indef.article(type) 
    stop("unit '", symbol, "' is already defined (as", article, type.atomic_unit(symbol), "unit)")
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







