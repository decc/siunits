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
  if (identical(length(s), 0)) {
    ""
  } else if (is.singleton(s)) {
    as.character(s)
  } else if (is.to_power(s)) {
    paste(format.Signature(s[[2]]), "^", s[[3]], sep = "")
  } else if (is.derived(s)) {
    paste(vapply(s[-1], format.Signature, character(1)), collapse = " ")
  }
}

