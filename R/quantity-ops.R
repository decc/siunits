## + and - silently coerce the second argument to have the units of the first,
## so long as the arguments are unit-compatible. 

##' @S3method + Quantity
`+.Quantity` <- function(e1, e2) {
  if (nargs() == 1L) return(e1)
  
  if (!is.compatible_unit(as.Unit(e1), as.Unit(e2))) {
    stop("arguments to '+' must be unit compatible", call. = FALSE) 
  }
  
  as.Quantity(as.numeric(e1) + as.numeric(as.Quantity(e2, as.Unit(e1))),
           as.Unit(e1))
}

##' @S3method - Quantity
`-.Quantity` <- function(e1, e2) {
  if (nargs() == 1L) return(as.Quantity(e1))
  
  if (!is.compatible_unit(as.Unit(e1), as.Unit(e2))) {
    stop("arguments to '-' must be unit compatible", call. = FALSE) 
  }
  
  as.Quantity(as.numeric(e1) - as.numeric(as.Quantity(e2, as.Unit(e1))),
           as.Unit(e1))
}

##' @S3method * Quantity
`*.Quantity` <- function(e1, e2) {
  if (!is.Quantity(e2)) { # Assume that e2 is numeric and put the numeric in
                                        # first position
    tmp <- e2; e2 <- e1; e1 <- tmp
  }
  
  if (!is.Quantity(e1)) {
    return(as.Quantity(e1 * as.numeric(e2), as.Unit(e2)))
  } else {
    return(as.Quantity(as.numeric(e1) * as.numeric(e2), product_unit(as.Unit(e1), as.Unit(e2))))
  }
}

##' @S3method / Quantity
`/.Quantity` <- function(e1, e2) {
  if (!is.Quantity(e2)) {
    return(as.Quantity(as.numeric(e1) / e2, as.Unit(e1)))
  } else if (!is.Quantity(e1)) {
    return(as.Quantity(e1 / as.numeric(e2), inverse_unit(as.Unit(e2))))
  }
  
  if (!is.compatible_unit(as.Unit(q1), as.Unit(q2))) {
    stop("arguments to '+' must be unit compatible", call. = FALSE) 
  }

  as.Quantity(as.numeric(q1) + as.numeric(as.Quantity(q2, as.Unit(q1))),
           as.Unit(q1))
}

##' @S3method ^ Quantity
`^.Quantity` <- function(q, num) {

  as.Quantity(as.numeric(q)^num, power_unit(as.Unit(q), num))
}

## Indexing and subsetting operations

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
