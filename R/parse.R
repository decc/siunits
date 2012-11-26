## Functions for reading character representations of Units.

## Read a complex structure,
## eg, (kg m s^-2)_[force]

## Regular expression
NAME <- "[[:alpha:]%]+"
SPACE <- "[[:space:]]+"
NUMBER <- "\\-?[[:digit:]]+"
OPENSUBUNIT <- "\\("
CLOSESUBUNIT <- "\\)"
OPENDIM <- "\\["
CLOSEDIM <- "\\]"
TOTHE <- "\\^"
OFDIM <- "\\_"

TOKENS <- paste(NAME, SPACE, NUMBER, OPENSUBUNIT, CLOSESUBUNIT, OPENDIM,
                CLOSEDIM, TOTHE, OFDIM, sep = "|")

## BNF for input --
##
## <unit> ::= <complete unit> [ SPACE <complete unit> ]* 
##          
## <complete unit> ::= <dimensioned unit> "^" NUMBER
##                   | <dimensioned unit>
##
## <dimensioned unit> ::= <subunit> "_" <dimension>
##                      | <subunit>
##
## <subunit> ::= NAME
##             | "(" <unit> ")"
##
## <dimension> ::= "[" NAME "]"
##
## NAME, NUMBER, SPACE, and "things in quotes" are terminals 

is.empty <- function(toks) {
  identical(length(toks), 0L)
}

parse_error <- function(msg, toks) {
  stop(msg, " with '", toks, "' left", call. = FALSE)
}

## Higher-order functions for building parsers

Literal <- function(str) {
  function(toks) {
    if (is.empty(toks)) {
      return(list())
    }

    if (toks[[1]][[1]] == str) {
      return(list(tree = list(),
                  toks = toks[-1]))
    } else {
      return(list())
    }
  }
}

Value <- function(str) {
  function(toks) {
    if (is.empty(toks)) {
      return(list())
    }

    if (toks[[1]][[1]] == str) {
      return(list(tree = toks[[1]][[2]],
                  toks = toks[-1]))
    } else {
      return(list())
    }
  }
}

parse_SPACE <- Literal("SPACE")
parse_TOTHE <- Literal("TOTHE")
parse_OFDIM <- Literal("OFDIM")
parse_OPENSUBUNIT <- Literal("OPENSUBUNIT")
parse_CLOSESUBUNIT <- Literal("CLOSESUBUNIT")
parse_OPENDIM <- Literal("OPENDIM")
parse_CLOSEDIM <- Literal("CLOSEDIM")

parse_NUMBER <- Value("NUMBER")
parse_NAME <- Value("NAME")


## Testers for parts of Units

## is.unit checks syntax only, any character string is accepted for dimensions
## and atomic units  

is.unit <- function(u) {
  (identical(length(u), 0L)
   || is.singleton(u) 
   || (is.derived(u)
       && all(vapply(u[-1], is.unit, logical(1)))) 
   || (is.to_power(u)
       && is.unit(u[[2]])
       && is.numeric(u[[3]])
       && !identical(u[[3]], 0))
   || (is.dimensioned(u)
       && is.unit(u[[2]])
       && is.character(u[[3]])
       && identical(length(u[[3]]), 1L)))
}

## Is u of the form "kg", or "m", etc
is.singleton <- function(u) {
  (identical(length(u), 1L) && is.character(u))
}

## Is u of the form list(_, ...)

is.dimensioned <- function(u) {
  (is.list(u)
   && identical(length(u), 3L)
   && u[[1]] == quote(`_`))
 }
  
## Is u of the form list(*, ... )
is.derived <- function(u) {
  (is.list(u)
   && length(u) > 1
   && u[[1]] == quote(`*`))
}

## Is u of the form list(^, )
is.to_power <- function(u) {
  (is.list(u)
   && identical(length(u), 3L)
   && u[[1]] == quote(`^`))
}

## Constructors for parts of Units

make_atomic_unit <- function(unit) {
  unit
}

make_derived_unit <- function(list_of_units) {
  c(quote(`*`), list_of_units)
}

make_dimensioned_unit <- function(unit, dimension) {
  list(quote(`_`), unit, dimension)
}

make_unit_to_power <- function(unit, num) {
  list(quote(`^`), unit, num)
}
  

## Parser

parse_unit <- function(toks) {
  if (is.empty(toks)) {
    return(list())
  }
  
  first <- parse_complete_unit(toks)
  if (is.empty(first)) {
    return(list())
  }
  
  mult <- parse_SPACE(first$toks)
  if (is.empty(mult)) {
    return(first)
  }
  
  second <- parse_unit(mult$toks)
  if (is.empty(second)) {
    parse_error("expecting <unit>", mult$toks)
  } else {
    if (is.derived(first$tree)) {
      first$tree <- first$tree[-1]
    } else {
      first$tree <- list(first$tree)
    }
    if (is.derived(second$tree)) {
      second$tree <- second$tree[-1]
    } else {
      second$tree <- list(second$tree)
    }
    
    return(list(tree = make_derived_unit(c(first$tree, second$tree)),
                  toks = second$toks))
  }
}


parse_complete_unit <- function(toks) {
  if (is.empty(toks)) {
    return(list())
  }
  
  dimunit <- parse_dimensioned_unit(toks)
  if (is.empty(dimunit)) {
    return(list())
  }

  tothe <- parse_TOTHE(dimunit$toks)
  if (!is.empty(tothe)) {
    number <- parse_NUMBER(tothe$toks)
    if (!is.empty(number)) {
      return(list(tree = make_unit_to_power(dimunit$tree, number$tree),
                  toks = number$toks))
    } else {
      parse_error("expecting NUMBER", tothe$toks)
    }
  }

  return(dimunit)
}

parse_dimensioned_unit <- function(toks) {
  if (is.empty(toks)) {
    return(list())
  }

  subunit <- parse_subunit(toks)
  if (is.empty(subunit)) {
    return(list())
  }
  
  ofdim <- parse_OFDIM(subunit$toks)
  if (!is.empty(ofdim)) {
    dimension <- parse_DIMENSION(ofdim$toks)
    if (!is.empty(dimension)) {
      return(list(tree = make_dimensioned_unit(subunit$tree, dimension$tree),
                  toks = dimension$toks))
    } else {
      parse_error("expecting <dimension>", ofdim$toks)
    }
  }

  return(subunit)
}

parse_DIMENSION <- function(toks) {
  if (is.empty(toks)) {
    return(list())
  }

  opendim <- parse_OPENDIM(toks)
  if (is.empty(opendim)) {
    return(list())
  }

  dimension <- parse_NAME(opendim$toks)
  if (is.empty(dimension)) {
    parse_error("expecting a dimension", opendim$toks)
  }

  closedim <- parse_CLOSEDIM(dimension$toks)
  if (is.empty(closedim)) {
    parse_error("expecing a ']'", dimension$toks)
  }

  return(list(tree = dimension$tree,
              toks = closedim$toks))
}


parse_subunit <- function(toks) {
  if (is.empty(toks)) {
    return(list())
  }

  name = parse_NAME(toks)
  if (!is.empty(name)) {
    return(name)
  }

  opensubunit <- parse_OPENSUBUNIT(toks)
  if (is.empty(opensubunit)) {
    return(list())
  }

  unit <- parse_unit(opensubunit$toks)
  if (is.empty(unit)) {
    parse_error("expecting a unit", opensubunit$toks)
  }
  
  closesubunit <- parse_CLOSESUBUNIT(unit$toks)
  if (is.empty(closesubunit)) {
    parse_error("expecting ')'", dimension$toks)
  } else {
    return(list(tree = make_derived_unit(list(unit$tree)),
                toks = closesubunit$toks))
  }
}
 
  
  
lexify_unit <- function(str) {
  tokens <- tokenise_unit(str)
  lapply(tokens, lexify_token)
}

tokenise_unit <- function(str) {
  regmatches(str, gregexpr(TOKENS, str))[[1]]
}

lexify_token <- function(str) {
  if (grepl(NAME, str)) {
    list("NAME", str)
  } else if (grepl(NUMBER, str)) {
    list("NUMBER", as.numeric(str))
  } else if (grepl(SPACE, str)) {
    list("SPACE")
  } else if (grepl(OPENSUBUNIT, str)) {
    list("OPENSUBUNIT")
  } else if (grepl(CLOSESUBUNIT, str)) {
    list("CLOSESUBUNIT")
  } else if (grepl(OPENDIM, str)) {
    list("OPENDIM")
  } else if (grepl(CLOSEDIM, str)) {
    list("CLOSEDIM")
  } else if (grepl(TOTHE, str)) {
    list("TOTHE")
  } else if (grepl(OFDIM, str)) {
    list("OFDIM")
  }
}
  
