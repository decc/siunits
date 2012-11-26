## Initial set of units, dimensions, and defaults
## ==============================================

## Dimensions
## ----------

Basis.Dimensions <- data.frame(
  symbol = c("L", "M", "T", "I", "Th", "N", "J"),
  name = c("length", "mass", "time", "electric current",
    "thermodynamic temperature", "amount of substance", "luminous intensity"),
  dimension = c("length", "mass", "time", "electric_current",
    "temperature", "amount", "luminous_intensity"),
  stringsAsFactors = FALSE) 

DIMENSIONLESS = "ONE"

Dimensions <- list(
  ONE = list(
    definition = NULL,
    vector = c(0,0,0,0,0,0,0)),
  length = list(
    definition = NULL,
    vector = c(1,0,0,0,0,0,0)),
  mass = list(
    definition = NULL,
    vector = c(0,1,0,0,0,0,0)),
  time = list(
    definition = NULL,
    vector = c(0,0,1,0,0,0,0)),
  electric_current = list(
    definition = NULL,
    vector = c(0,0,0,1,0,0,0)),
  temperature = list(
    definition = NULL,
    vector = c(0,0,0,0,1,0,0)),
  amount = list(
    definition = NULL,
    vector = c(0,0,0,0,0,1,0)),
  luminous_intensity = list(
    definition = NULL,
    vector = c(0,0,0,0,0,0,1)))

## Units
## -----

## Units with type = "basis" are the default for those dimensions whose
## definition is NULL. The order should be precisely the same as the order in
## Basis.Dimensions

Units <- data.frame(
  symbol = c("m", "kg", "s", "A", "K", "mol", "cd"),
  dimension = c("length", "mass", "time", "electric_current",
    "temperature", "amount", "luminous_intensity"), 
  name = c("metre", "kilogram", "second", "ampere", "kelvin", "mole",
    "candela"),
  plural.name = c("metres", "kilograms", "seconds", "amperes", "kelvins",
    "moles", "candelas"),
  type = "basis",
  multiple = 1.0,
  series = NA,
  stringsAsFactors = FALSE
  )

SI.Prefixes <- data.frame(
  name = c("yotta", "zetta", "exa", "peta", "tera", "giga", "mega", "kilo",
    "hecto", "deca", "deci", "centi", "milli", "micro", "nano", "pico", "femto",
    "atto", "zepto", "yocto"),
  multiple = c(1e24, 1e21, 1e18, 1e15, 1e12, 1e9, 1e6, 1e3, 1e2, 10, 0.1, 1e-2,
    1e-3, 1e-6, 1e-9, 1e-12, 1e-15, 1e-18, 1e-21, 1e-24), 
  prefix = c("Y", "Z", "E", "P", "T", "G", "M", "k", "h", "da", "d", "c", "m",
    "µ", "n", "p", "f", "a", "z", "y"),
  stringsAsFactors = FALSE) 


## Defaults for conversion
## -----------------------

SI.Defaults <- list(ONE = as.Unit(""),
                    length = as.Unit("m"),
                    mass = as.Unit("kg"),
                    time = as.Unit("s"),
                    electric_current = as.Unit("A"),
                    temperature = as.Unit("K"),
                    amount = as.Unit("mol"),
                    luminous_intensity = as.Unit("cd"))


## SI-prefixed versions of the base units
## --------------------------------------

add_unit("mass", "g", "gram", is.coherent = TRUE, gen.prefixes = TRUE, true.basis = "kg")
add_unit("length", "m", "metre", is.coherent = TRUE, gen.prefixes = TRUE, true.basis = "m")
add_unit("time", "s", "second", is.coherent = TRUE, gen.prefixes = TRUE, true.basis = "s")
add_unit("electric_current", "A", "ampere", is.coherent = TRUE, gen.prefixes = TRUE, true.basis = "A")
add_unit("temperature", "K", "kelvin", is.coherent = TRUE, gen.prefixes = TRUE, true.basis =
         "K")
add_unit("amount", "mol", "mole", is.coherent = TRUE, gen.prefixes = TRUE, true.basis =
         "mol") 
add_unit("luminous_intensity", "cd", "candela", is.coherent = TRUE, gen.prefixes = TRUE, true.basis
         = "cd")

## Selected dimensions which don't have their own units in SI
## ----------------------------------------------------------

add_dimension("velocity", c(length = 1, time = -1)) # m/s
add_dimension("acceleration", c(velocity = 1, time = -1)) # (m/s)/s
add_dimension("area", c(length = 2)) # m^2
add_dimension("momentum", c(mass = 1, velocity = 1)) # kg (m/s)

## Dimensions with their own units
## -------------------------------

add_dimension("frequency", c(time = -1))
add_unit("frequency", "Hz", "hertz", "hertz",
         is.coherent = TRUE, gen.prefixes = TRUE)

add_dimension("angle", c(length = 1, length = -1)) # rad = m/m
add_unit("angle", "rad", "radian",
         is.coherent = TRUE, gen.prefixes = FALSE)

add_dimension("solid_angle", c(angle = 2)) # sr = rad^2
add_unit("solid_angle", "sr", "steradian",
         is.coherent = TRUE, gen.prefixes = FALSE)

add_dimension("force", c(mass = 1, acceleration = 1)) # N = kg (m/s)/s 
                                        # should perhaps be (momentum = 1, time = -1?)
add_unit("force", "N", "newton", is.coherent = TRUE, gen.prefixes = TRUE)

add_dimension("pressure", c(force = 1, area = -1)) # Pa = N/m^2
add_unit("pressure", "Pa", "pascal", is.coherent = TRUE, gen.prefixes = TRUE)

add_dimension("energy", c(force = 1, length = 1)) # J = N m
add_unit("energy", "J", "joule", is.coherent = TRUE, gen.prefixes = TRUE)

add_dimension("power", c(energy = 1, time = -1)) # W = J/s
add_unit("power", "W", "watt", is.coherent = TRUE, gen.prefixes = TRUE)

add_dimension("electric_charge", c(electric_current = 1, time = -1)) # C = A s
add_unit("electric_charge", "C", "coulomb", is.coherent = TRUE, gen.prefixes =
         TRUE)

add_dimension("voltage", c(energy = 1, electric_charge = -1)) # V = J / C.
add_unit("voltage", "V", "volt", is.coherent = TRUE, gen.prefixes = TRUE)

## Non-SI units
## ------------

add_unit("time", "h", "hour", multiple = 60 * 60)
add_unit("mass", "t", "tonne", multiple = 1000, gen.prefixes = TRUE)
add_unit(DIMENSIONLESS, "%", "per cent", "per cent", multiple = 0.01)

## "Small-c" calorie
## -----------------
## Definition: International Steam Table calorie (1956)
## See, eg, http://en.wikipedia.org/wiki/Calorie
## This also appears to be the definition used by DUKES
add_unit("energy", "cal", "calorie", multiple = 4.1868, gen.prefixes = TRUE)

## Tonne of oil equivalent
## -----------------------
## Definition: OECD/IEA definition, also the one used by DUKES
add_unit("energy", "toe", "tonne of oil equivalent", "tonnes of oil equivalent",
         multiple = as.Quantity(1e10, "cal"), gen.prefixes = TRUE)

## Therm
## -----
## Definition: The Units of Measurement Regulations, 1995
## http://www.legislation.gov.uk/uksi/1995/1804/schedule/made
add_unit("energy", "therm", "therm", multiple = as.Quantity(105.505585257348,
                                       "MJ"), gen.prefixes = TRUE)
