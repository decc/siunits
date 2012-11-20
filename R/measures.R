#' @import units.R dimensions.R

is.atomic_measure <- function(am) {
  (is.unit(am[[1]]) 
   && is.dimension(am[[2]])
   && is.compatible_unit(am[[1]], am[[2]]))
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
  to_basis(dim_expr)
}

is.compatible.measure <- function(m1, m2) {
  (identical(basis_vector(m1), basis_vector(m2)))
}


## as.measure: return a measure given a character string of the form "kg
## m^2 s^-2"
## TODO: Allow, eg, "kg_[mass]"

as.measure <- function(str, dimension = NULL) {
  unit <- as.unit(str)
  measure <- if (!is.null(dimension)) {
    if (!is.dimension(dimension)) {
      stop("'", dimension, "' is not a known dimension")
    }
    if (!is.compatible_unit(unit, dimension)) {
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
      list({v <- c(1); names(v) <- au; v}, unit.dimension(au)),
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
  ff <- format_unit(am[[1]])
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

