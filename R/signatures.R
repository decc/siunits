##' The Signatures class
##'
##' Functions to create Signatures and check membership in the class. A
##' Signature is formed of products of powers of dimensions, possibly parenthesised. 
##'
##' @name signatures
##' @rdname signatures
##' @aliases as.Signature is.Signature
##' @param s Object to be checked for membership in the Signature class.
##' @return In the case of \code{is.Signature}, a boolean; in the case of
##' \code{as.Signature}, an object of class \code{Signature}.
##' @seealso The package documentation for \code{\link{siunits}}.
##' @examples
##' \dontrun{is.Signature(as.Signature("mass acceleration"))}
##' @export is.Signature
is.Signature <- function(s) {
  inherits(s, "Signature")
}

##' @rdname signatures
##' @TODO Fix the method by which character strings are parsed (currently uses
##' parse_unit).
##' @usage as.Signature(e)
##' @param e Object (character, Unit, or Quantity) to be coerced to Signature.
##' @export as.Signature
as.Signature <- function(e) {
  UseMethod("as.Signature", e)
}

## Given a unit, extract the named dimensions.
## signature ATOMIC UNIT -> dimension(ATOMIC UNIT)
## signature (_, <unit>, dimension) -> <dimension>
## signature (^, <unit>, number) -> (^, signature(<unit>), number)
## signature (*, <unit> , ...) -> (*, signature(<unit>), ...)

##' @S3method as.Signature Unit
as.Signature.Unit <- function(e) {
  structure(as_signature(e), class = "Signature")
 }

##' @S3method as.Signature Quantity
as.Signature.Quantity <- function(e) {
  structure(as_signature(as.Unit(e)), class = "Signature")
}

##' @S3method as.Signature Signature
as.Signature.Signature <- function(e) {
  e
}

##' @S3method as.Signature character
## This is presently a cheat -- it uses parse_unit, so is not guaranteed to
## return a Signature!
as.Signature.character <- function(e) {
  sig <- parse_unit(lexify_unit(e))[[1]]
  structure(sig, class = "Signature")
}

as_signature <- function(u) {
  if (is.singleton(u)) {
    dimension.atomic_unit(u)
  } else if (is.dimensioned(u)) {
    u[[3]]
  } else if (is.to_power(u)) {
    list(quote(`^`), as_signature(u[[2]]), u[[3]])
  } else if (is.derived(u)) {
    c(quote(`*`), lapply(u[-1], as_signature))
  }
}

##' @method format Signature
format.Signature <- function(s) {
  format_signature0(s, parens = FALSE)
}

format_signature0 <- function(s, parens = FALSE) {
  if (identical(length(s), 0)) {
    ""
  } else if (is.singleton(s)) {
    as.character(s)
  } else if (is.to_power(s)) {
    paste(format_signature0(s[[2]], parens = TRUE), "^", s[[3]], sep = "")
  } else if (is.derived(s)) {
    str <- paste0(vapply(s[-1], format_signature0, character(1), parens = TRUE), collapse = " ")
    if (parens) {
      paste("(", str, ")", sep = "")
    } else {
      str
    }
  }
}

##' @S3method print Signature
print.Signature <- function(x, ...) {
  cat(format(x, ...), "\n")
  invisible(x)
}
