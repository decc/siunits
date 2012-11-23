## Functions for reading strings

## Read a simple series of powers and return a vector
## eg, "kg m s^-2" -> c(kg = 1, m = 1, s = -2)
## eg, "power time" -> c(power = 1, time = 1)

parse_simple_vector <- function(str) {
  unlist(
    lapply(strsplit(
      unlist(strsplit(str, " ", fixed = TRUE)),
      "^", fixed = TRUE),
           parse_power))
}

parse_power <- function(ll) {
  if (length(ll) == 1L) {
    power <- 1
  } else {
    power <- as.numeric(ll[[2]])
  }
  
  structure(power, names = ll[[1]])
}

## Read a complex structure,
## eg, (kg m s^-2)_[force]
## ( kg m s ^ -2 ) _ [ force ]

## Regular expression
NAME <- "[[:alpha:]]+"
SPACE <- "[[:space:]]+"
NUMBER <- "\\-?[[:digit:]]+"
OPENSUBUNIT <- "\\("
CLOSESUBUNIT <- "\\)"
OPENDIM <- "\\["
CLOSEDIM <- "\\]"
TOTHE <- "\\^"
OFDIM <- "\\_"

TOKENS <- paste(NAME, SPACE, NUMBER, POWER, OPENSUBUNIT, CLOSESUBUNIT, OPENDIM,
                CLOSEDIM, POWER, DIM, sep = "|")

## BNF for input --
##
## <unit> ::= <complete unit> [ SPACE <complete unit> ]* 
##          
##
## <complete unit> ::= <subunit> "^" NUMBER
##                   | <subunit> "_" <dimension>
##                   | <subunit>
##
## <subunit> ::= NAME
##             | "(" <unit> ")"
##
## <dimension> ::= "[" NAME "]"
##
## NAME, NUMBER, SPACE, and "things in quotes" are terminals 

as.unit <- function(str) {
  parse_unit(lexify_unit(str))
}

is.empty <- function(toks) {
  identical(length(toks), 0L)
}

## Higher-order functions for building parsers

Succeed <- function(v) {
  function(toks) { list(list(v, toks)) }
}

Fail <- function() {
  function(toks) { list() }
}

Satisfy <- function(predicate) {
  function(toks) {
    if (is.empty(toks)) {
      (Fail())(list())
    } else if (predicate(toks[[1]])) {
      (Succeed(toks[[1]]))(toks[-1])
    } else {
      (Fail())(toks[-1])
    }
  }
}


parse_error <- function(msg, toks) {
  stop(msg, " with '", toks, "' left")
}

done <- function(toks) {
  identical(length(toks), 0L)
}

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
    if (second$tree[[1]] == quote(`*`)) {
      return(list(tree = c(quote(`*`), first$tree, second$tree[-1]),
                  toks = second$toks))
    } else {
      return(list(tree = list(quote(`*`), first$tree, second$tree),
                  toks = second$toks))
    }
  }
}
  

parse_complete_unit <- function(toks) {
  if (is.empty(toks)) {
    return(list())
  }

  subunit <- parse_subunit(toks)
  if (is.empty(subunit)) {
    return(list())
  }

  tothe <- parse_TOTHE(subunit$toks)
  if (!is.empty(tothe)) {
    number <- parse_NUMBER(tothe$toks)
    if (!is.empty(number)) {
      return(list(tree = list(quote(`^`), subunit$tree, number$tree),
                  toks = number$toks))
    } else {
      parse_error("expecting NUMBER", tothe$toks)
    }
  }

  ofdim <- parse_OFDIM(subunit$toks)
  if (!is.empty(ofdim)) {
    dimension <- parse_DIMENSION(ofdim$toks)
    if (!is.empty(dimension)) {
      return(list(tree = list(quote(`_`), subunit$tree, dimension$tree),
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
    parse_error("expecting a dimension NAME", opendim$toks)
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
  }

  return(list(tree = unit$tree, toks = closesubunit$toks))
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
  } else if (grepl(POWER, str)) {
    list("TOTHE")
  } else if (grepl(DIM, str)) {
    list("OFDIM")
  }
}
  
