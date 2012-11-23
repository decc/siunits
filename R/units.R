## <unit> ::= <single atomic unit>
##          | <derived unit>
##          | <dimensioned unit>
##
## <single atomic unit> ::= <atomic unit> of length 1
##
## <derived unit> ::= <unit with power> *
##
## <dimensioned unit> ::= (<dimension>, <unit>)
## where the basis form of dimension is the same as the basis form of the
## dimension of unit

## The dimension of a <unit> is:
## <atomic unit> : the dimension of the <atomic unit>
## list-of (<unit>, power) : the derived dimension
## pair (<unit>, <dimension>) : <dimension>
##
## Examples: 
## "kg"
## (("kg", 2))
## (("m", 2), ("s", -1))
## ( ((("kg", 2)), 3)) , ...)
## ("mass", "kg")
## ( "velocity", (("m", 1), ("s", -1)))
##
## <atomic unit> : string
## pair (<dimension>, <unit>) : list of length 2 with character as first element
## list-of pair (<unit>, power) : list of pairs

is.unit <- function(u) {
  (
    (is.single_atomic_unit(u)
     || is.dimensioned_unit(u) 
     || is.derived_unit(u)))
}

is.single_atomic_unit <- function(u) {
  (identical(length(u), 1L) && is.character(u) && is.atomic_unit(u))
}

## Is u of the form list(<dimension>, <unit>)
is.dimensioned_unit <- function(u) {
  (is.list(u)
   && identical(length(u), 2L) 
   && is.character(u[[1]])
   && identical(length(u[[1]]), 1L)
   && is.dimension(u[[1]])
   && is.unit(u[[2]])) ## NEED TO CHECK COMPATIBILITY
}

## Is u of the form list( list(<unit>, power), ... )
is.derived_unit<- function(u) {
  (is.list(u)
   && all(vapply(u, is.unit_to_power, logical(1))))
}

## Is u of the form list(<unit>, power)
is.unit_to_power <- function(u) {
  (is.list(u)
   && identical(length(u), 2L)
   && is.unit(u[[1]])
   && is.numeric(u[[2]])
   && identical(length(u[[2]]), 1L)
   && !identical(u[[2]], 0))
}

## Writing units as strings

format_unit <- function(u, verbose = TRUE) {
  format_unit0(u, verbose, parens = FALSE)
}

format_unit0 <- function(u, verbose, parens) {
  if (is.single_atomic_unit(u)) {
    u
  } else if (is.derived_unit(u)) {
    format_derived_unit(u, verbose, parens)
  } else if (is.dimensioned_unit(u)) {
    if (verbose) {
      paste(format_unit0(u[[2]], verbose, parens = TRUE), "_[", u[[1]], "]", sep = "")
    } else {
      format_unit0(u[[2]], verbose, parens = TRUE)
    }
  }
}

format_derived_unit <- function(u, verbose, parens) {
  str <- paste(vapply(u, format_unit_to_power, character(1), verbose = verbose,
                      parens = parens), collapse = " ")
  if (parens && (length(u) > 1)) {
    paste("(", str, ")", sep = "")
  } else {
    str
  }
}

format_unit_to_power <- function(u, verbose, parens) {
  if (identical(u[[2]], 1)) {
    format_unit0(u[[1]], verbose, parens = TRUE)
  } else {
    paste(format_unit0(u[[1]], verbose, parens = TRUE), "^", u[[2]], sep = "")
  }
}




##

is.atomic_measure <- function(am) {
  (is.unit_vector(am[[1]]) 
   && is.dimension(am[[2]])
   && is.compatible_unit_vector(am[[1]], am[[2]]))
}

is.measure <- function(m) {
  (is.list(m)
   && all(vapply(m, is.measure_part, TRUE)))
}

is.measure_part <- function(mp) {
  (is.list(mp)
   && length(mp) == 2L
   && is.atomic_measure(mp[[1]])
   && !identical(mp[[2]], 0))
}

## Assumes 'm' is a measure
is.simple_measure <- function(m) {
  (identical(length(m), 1L)        # m contains only one part ...
   && identical(m[[1]][[2]], 1))  # and the power of that part is 1
}
  

## Example atomic measures:
## N_[force]
## kg_[mass]
## (kg m s^-2)_[force]
## (m s^-1)_[velocity]
## (N m)_[energy]
## (N m)_[torque]
##
## Example non-atomic measures:
## kg_[mass] (m s^-1)_[velocity]
## N_[force] m_[displacement]
## 
## Things that are not measures:
## * (N_[force] m_[displacement])_[energy] 
## * (N_[force] m_[position])_[torque] 

## as.Quantity : Quantity -> same quantity, different units.
## TODO: allow quantities other than those with simple measures
##

basis_vector <- function(m) {
  ## Return the 7-element basis vector of this measure
  ## assumes measure is a simple measure
  dim_expr <- as.numeric(m[[1]][[1]][[1]])
  names(dim_expr) <- unit.dimension(names(m[[1]][[1]][[1]]))
  to_basis_dimensions(dim_expr)
}

is.compatible.measure <- function(m1, m2) {
  (identical(basis_vector(m1), basis_vector(m2)))
}


## as.measure: return a measure given a character string of the form "kg
## m^2 s^-2"
## TODO: Allow, eg, "kg_[mass]"

as.measure <- function(str, dimension = NULL) {
  unit <- as.unit_vector(str)
  measure <- if (!is.null(dimension)) {
    if (!is.dimension(dimension)) {
      stop("'", dimension, "' is not a known dimension")
    }
    if (!is.compatible_unit_vector(unit, dimension)) {
      stop ("the dimensions of ", unit, " are not ", dimension)
    }
    list(list(list(unit, dimension), 1))
  } else {
    unpack.unit(unit)
  }
  
  measure
}

## unpack.unit: convert, eg, "m s^-2" to "m_[length] (s_[time])^-2"
unpack.unit <- function(unit) {
  mapply(function(au, power) {
    list(
      list({v <- c(1); names(v) <- au; v}, atomic_unit.dimension(au)),
      power)},
         names(unit),
         unit,
         USE.NAMES = FALSE, SIMPLIFY = FALSE)
}

format_measure <- function(measure, verbose = FALSE) {
  parens <- length(measure) > 1L
  paste(lapply(measure, format_measure_part, verbose, parens = parens), collapse = " ")
}

format_measure_part <- function(mp, verbose, parens = FALSE) {
  
  is.longunit <- !identical(length(mp[[1]][[1]][[1]]), 1L)
  
  if (mp[[2]] == 1) {
    if (parens && is.longunit) {
      paste("(", format_atomic_measure(mp[[1]], verbose), ")", sep = "")
    } else {
      format_atomic_measure(mp[[1]], verbose)
    }
  } else {
    if (is.longunit) {
      paste("(", format_atomic_measure(mp[[1]], verbose), ")^", mp[[2]], sep = "")
    } else {
      paste(format_atomic_measure(mp[[1]], verbose), "^", mp[[2]], sep = "")
    }
  }
}
  
  
format_atomic_measure <- function(am, verbose) {
  ff <- format_unit_vector(am[[1]])
  if (!verbose) {
    ff 
  } else {
    if (length(am[[1]]) > 1L) {
      paste("(", ff, ")_[", am[[2]], "]", sep = "")
    } else {
      paste(ff, "_[", am[[2]], "]", sep = "")
    }   
  }
}

