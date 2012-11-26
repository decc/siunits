## Signatures

##' @export
is.Signature <- function(s) {
  inherits(s, "Signature")
}

##' @export as.Signature
as.Signature <- function(x) {
  UseMethod("as.Signature", x)
}


## Given a unit, extract the named dimensions.
## signature ATOMIC UNIT -> dimension(ATOMIC UNIT)
## signature (_, <unit>, dimension) -> <dimension>
## signature (^, <unit>, number) -> (^, signature(<unit>), number)
## signature (*, <unit> , ...) -> (*, signature(<unit>), ...)

##' @S3method as.Signature Unit
as.Signature.Unit <- function(u) {
  structure(as_signature(u), class = "Signature")
 }

##' @S3method as.Signature Quantity
as.Signature.Quantity <- function(q) {
  structure(as_signature(as.Unit(q)), class = "Signature")
}

##' @S3method as.Signature Signature
as.Signature.Signature <- function(s) {
  s
}

##' @S3method as.Signature character
## This is presently a cheat -- it uses parse_unit, so is not guaranteed to
## return a Signature!
as.Signature.character <- function(s) {
  sig <- parse_unit(lexify_unit(s))[[1]]
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
print.Signature <- function(s) {
  cat(format(s), "\n")
  invisible(s)
}
