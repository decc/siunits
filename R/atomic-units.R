## Functions for dealing with "atomic units"

## indef.article : either "a" or "an" depending on the first letter of the word.
indef.article <- function(str) {
  if (substr(str, 1, 1) %in% c("a", "A", "e", "E", "i", "I", "o", "O", "u",
                               "U")) {
    "an"
  } else {
    "a" }
}

## Types
## -----

## <unit_vector> ::= vector-of (atomic_unit = power) such that no atomic_unit
## occurs twice and no power is zero

is.unit_vector <- function(uv) {
  (is.numeric(uv)
   && all(is.atomic_unit(names(uv))) 
   && !anyDuplicated(names(uv))
   && !any(uv == 0))
}

check_unit_vector <- function(uv) {

  if (is.unit_vector(uv)) {
    return(uv)
  } else {
    not.matched <- !is.atomic_unit(names(uv))
    if (any(not.matched)) {
      stop(names(uv)[not.matched], " is not a known atomic unit")
    } else if (any(uv == 0)) {
      stop("powers of 0 are not allowed in a unit vector")
    } else if (anyDuplicated(names(uv))) {
      stop("duplicate elements are not allowed in a unit vector")
    } else {
      stop("something is not a unit vector (but I don't know why not)")
    }
  }
}

## atomic_unit := one of a list of named units (kg, J, N, Gt, mHz, ...)
##
is.atomic_unit <- function(au) {
  au %in% Units$symbol
}

is.coherent_atomic_unit <- function(au) {
  (atomic_unit.type(au) == "coherent") | is.basis_atomic_unit(au)
}

is.basis_atomic_unit <- function(au) {
  (atomic_unit.type(au) == "basis")
}

## is.compatible_unit_vector : check whether a unit vector is compatible with a dimension
is.compatible_unit_vector <- function(uv, dimension) {
  uv.dimensions <- uv
  names(uv.dimensions) <- atomic_unit.dimension(names(uv))
  
  identical(to_basis_dimensions(uv.dimensions),
            Dimensions[[dimension]]$vector)
}

## Accessors
## ---------

## Type of atomic unit
##
atomic_unit.type <- function(au) {
  Units$type[match(au, Units$symbol)]
}

## atomic_unit.dimension : atomic unit -> dimension
## Extract the dimension of an atomic unit
##
atomic_unit.dimension <- function(au) {
  Units$dimension[match(au, Units$symbol)]
}



## Reading and writing strings
## ---------------------------

## as.unit_vector: string -> unit_vector (eg, "kg m^2 s^-2") 
##
as.unit_vector<- function(str) {
  check_unit_vector(parse_simple_vector(str))
}

## format_unit_vector: unit -> "kg m^2 s^-2" (eg)
format_unit_vector <- function(uv) {
  paste(
    mapply(format_unit_vector_part, names(uv), uv), collapse = " ")
}

## format_unit_vector_part: -> m^2 (eg)
format_unit_vector_part <- function(symbol, power) {
  if (power == 1) {
    symbol
  } else {
    paste(symbol, "^", power, sep = "")
  }
}

## Conversions

## unit.si_multiple: unit -> number: the multiple these units are of SI basis units 
unit_vector.si_multiple <- function(uv) {
  Reduce(`*`,
         mapply(function(au, power) {
           Units$multiple[Units$symbol == au]^power
         }, names(uv), uv))
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

  if (inherits(multiple, "Quantity")) {
    measure <- attr(multiple, "measure")
    if (!is.simple_measure(measure)) {
      stop("'multiple' must be a simple measure or numeric")
    }
    multiple <- as.numeric(multiple) * unit_vector.si_multiple(measure[[1]][[1]][[1]]) 
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







