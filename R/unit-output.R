## Writing units as strings
## ------------------------

##' @S3method print Unit
print.Unit <- function(x, verbose = TRUE, ...) {
  cat(format(x, verbose, ...), "\n")
  invisible(x)
}

##' @S3method format Unit
format.Unit <- function(x, digits = NULL, na.encode = FALSE, justify = justify,
                        verbose = FALSE, ...) {
  if (identical(length(x), 0L)) {
    ""
  } else {
    format_unit0(unclass(x), verbose, parens = FALSE)
  }
}

## Recursively convert components of the unit to character representation.
format_unit0 <- function(u, verbose, parens) {
  if (is.character(u)) {
    u
  } else if (u[[1]] == quote(`*`)) {
    format_derived_unit(u, verbose, parens)
  } else if (u[[1]] == quote(`_`)) {
    format_dimensioned_unit(u, verbose, parens)
  } else if (u[[1]] == quote(`^`)) {
    format_unit_to_power(u, verbose, parens)
  }
}

## Produce character representation of units of the form "(X Y Z)".
## Parentheses are included unless unit would be of the form "(X)" or we are at
## the top level.
format_derived_unit <- function(u, verbose, parens) {
  str <- paste0(vapply(u[-1], format_unit0, character(1), verbose = verbose,
                      parens = TRUE), collapse = " ")
  if (parens && (length(u) > 2)) {
    paste("(", str, ")", sep = "")
  } else {
    str
  }
}

## Produce character representation of units of the form "X_[dimension]"
format_dimensioned_unit <- function(u, verbose, parens) {
  if (verbose) {
    str <-  paste(format_unit0(u[[2]], verbose, parens = !parens), "_[", u[[3]], "]", sep = "")
  } else {
    str <- format_unit0(u[[2]], verbose, parens = !parens)
  }
  str
}

## Produce character representation of units of the form "X^n"
format_unit_to_power <- function(u, verbose, parens) {
  if (identical(u[[3]], 1)) {
    format_unit0(u[[2]], verbose, parens = TRUE)
  } else {
    paste(format_unit0(u[[2]], verbose, parens = TRUE), "^", u[[3]], sep = "")
  }
}
