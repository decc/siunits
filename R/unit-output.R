## Writing units as strings
## ------------------------

##' @S3method print Unit
print.Unit <- function(u, verbose = TRUE) {
  cat(format(u, verbose), "\n")
  invisible(u)
}

##' @S3method format Unit
format.Unit <- function(u, digits = NULL, na.encode = FALSE, justify = justify,
                        verbose = FALSE, ...) {
  if (identical(length(u), 0L)) {
    ""
  } else {
    format_unit0(unclass(u), verbose, parens = FALSE)
  }
}

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

format_derived_unit <- function(u, verbose, parens) {
  str <- paste0(vapply(u[-1], format_unit0, character(1), verbose = verbose,
                      parens = TRUE), collapse = " ")
  if (parens && (length(u) > 2)) {
    paste("(", str, ")", sep = "")
  } else {
    str
  }
}

format_dimensioned_unit <- function(u, verbose, parens) {
  if (verbose) {
    str <-  paste(format_unit0(u[[2]], verbose, parens = !parens), "_[", u[[3]], "]", sep = "")
  } else {
    str <- format_unit0(u[[2]], verbose, parens = !parens)
  }
  
  ##  if (parens) {
  ##    paste("(", str, ")", sep = "")
  ##  } else {
  str
  ##  }
}

format_unit_to_power <- function(u, verbose, parens) {
  if (identical(u[[3]], 1)) {
    format_unit0(u[[2]], verbose, parens = TRUE)
  } else {
    paste(format_unit0(u[[2]], verbose, parens = TRUE), "^", u[[3]], sep = "")
  }
}
