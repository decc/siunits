## Operations on objects of class Quantity
## =======================================

## Arithmetic
## ----------

## + and - silently coerce the second argument to have the units of the first,
## so long as the arguments are unit-compatible. 

##' @S3method + Quantity
`+.Quantity` <- function(e1, e2) {
  if (nargs() == 1L) return(e1)
  
  if (!is.compatible_unit(as.Unit(e1), as.Unit(e2))) {
    stop("arguments to '+' must be unit compatible", call. = FALSE) 
  }

  ## Keep the units of the longest vector, or the first one (matches what `+`
  ## does with names).
  out.units <- if (length(e2) > length(e1)) as.Unit(e2) else as.Unit(e1)
  make_quantity(unclass(as.Quantity(e1, out.units)) + unclass(as.Quantity(e2, out.units)),
                out.units)
}

##' @S3method - Quantity
`-.Quantity` <- function(e1, e2) {
  if (nargs() == 1L) return(as.Quantity(e1))
  
  if (!is.compatible_unit(as.Unit(e1), as.Unit(e2))) {
    stop("arguments to '-' must be unit compatible", call. = FALSE) 
  }
  
  ## Keep the units of the longest vector, or the first one (matches what `+`
  ## does with names).
  out.units <- if (length(e2) > length(e1)) as.Unit(e2) else as.Unit(e1)
  make_quantity(unclass(as.Quantity(e1, out.units)) - unclass(as.Quantity(e2, out.units)),
                out.units)
}

##' @S3method * Quantity
`*.Quantity` <- function(e1, e2) {
  if (!is.Quantity(e2)) { # Assume that e2 is numeric and put the numeric in
                                        # first position
    tmp <- e2; e2 <- e1; e1 <- tmp
  }
  
  if (!is.Quantity(e1)) {
    return(make_quantity(e1 * unclass(e2), as.Unit(e2)))
  } else {
    return(make_quantity(unclass(e1) * unclass(e2), product_unit(as.Unit(e1), as.Unit(e2))))
  }
}

##' @S3method / Quantity
`/.Quantity` <- function(e1, e2) {
  if (!is.Quantity(e2)) {
    return(make_quantity(unclass(e1) / e2, as.Unit(e1)))
  } else if (!is.Quantity(e1)) {
    return(make_quantity(e1 / unclass(e2), inverse_unit(as.Unit(e2))))
  } else {
    return(make_quantity(unclass(e1) / unclass(e2),
                       product_unit(as.Unit(e1),
                                    inverse_unit(as.Unit(e2)))))
  }
}

##' @S3method ^ Quantity
`^.Quantity` <- function(e, num) {
  make_quantity(unclass(e)^num, power_unit(as.Unit(e), num))
}

## Summary methods
## ---------------

##' @S3method Summary Quantity
Summary.Quantity <- function(..., na.rm) {
  ok <- switch(.Generic, max = , min = , range = , sum = TRUE, FALSE)
  if (!ok)
    stop(.Generic, " not defined for Quantity objects", call. = FALSE)
  args <- list(...)
  ## Argument list must all be of compatible units; will be converted to the
  ## units of the first argument
  unit <- as.Unit(args[[1]])
  args[[1]] <- unclass(args[[1]])
  args[-1] <- lapply(args[-1],
                     function(arg) {
                       if (!is.compatible_unit(unit, as.Unit(arg))) 
                         stop("Arguments to ", .Generic, " must have compatible units")
                       unlcass(as.Quantity(arg, unit))
                     })
  make_quantity(NextMethod(.Generic), unit)
}

## Indexing and subsetting operations
## ----------------------------------

##' @S3method [ Quantity
`[.Quantity` <- function(x, ..., drop = TRUE) {
  cl <- class(x)
  u <- attr(x, "unit")
  structure(NextMethod(), class = cl, unit = u)
}

##' @S3method [<- Quantity
`[<-.Quantity` <- function(x, ..., value) {
  if (is.Quantity(value)) {
    if (!is.compatible_unit(x, value)) {
      stop("assignment to a Quantity requires replacement value to be unit-compatible")
    }
  } else if (!(is.numeric(value) || is.null(value))) {
    stop("only numeric quantities may be assigned to a Quantity")
  }          
  cl <- class(x)
  u <- attr(x, "unit")
  class(x) <- NULL
  x[...] <- as.numeric(as.Quantity(value, u))
  structure(x, class = cl, unit = u) 
}

##' @S3method [[ Quantity
`[[.Quantity` <- function(x, ..., drop = TRUE) {
  cl <- class(x)
  u <- attr(x, "unit")
  structure(NextMethod(), class = cl, unit = u)
}

##' @S3method [[<- Quantity
`[[<-.Quantity` <- function(x, ..., value) {
  if (is.Quantity(value)) {
    if (!is.compatible_unit(x, value)) {
      stop("assignment to a Quantity requires replacement value to be unit-compatible")
    }
  } else if (!(is.numeric(value) || is.null(value))) {
    stop("only numeric quantities may be assigned to a Quantity")
  }          
  cl <- class(x)
  u <- attr(x, "unit")
  class(x) <- NULL
  x[[...]] <- as.numeric(as.Quantity(value, u))
  structure(x, class = cl, unit = u) 
}

## Vector concatenation
## --------------------

##' @S3method c Quantity
c.Quantity <- function(..., recursive = FALSE) {
  unit = as.Unit(list(...)[[1]])  
  make_quantity(
    c(unlist(lapply(list(...),
                    function(arg) {
                      if (!is.compatible_unit(unit, as.Unit(arg))) 
                        stop("Arguments to ", .Generic,
                             " must have compatible units", call. = FALSE)
                      unclass(as.Quantity(arg, unit))}))),
    unit)
}

