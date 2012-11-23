
## The SI prefixed versions of the base units
## ==========================================

add_unit("M", "g", "gram", is.coherent = TRUE, gen.prefixes = TRUE, true.basis = "kg")
add_unit("L", "m", "metre", is.coherent = TRUE, gen.prefixes = TRUE, true.basis = "m")
add_unit("T", "s", "second", is.coherent = TRUE, gen.prefixes = TRUE, true.basis = "s")
add_unit("I", "A", "ampere", is.coherent = TRUE, gen.prefixes = TRUE, true.basis = "A")
add_unit("Th", "K", "kelvin", is.coherent = TRUE, gen.prefixes = TRUE, true.basis =
         "K")
add_unit("N", "mol", "mole", is.coherent = TRUE, gen.prefixes = TRUE, true.basis =
         "mol") 
add_unit("J", "cd", "candela", is.coherent = TRUE, gen.prefixes = TRUE, true.basis
         = "cd")


## Selected dimensions which don't have their own units in SI
## ==========================================================

add_dimension("velocity", c(length = 1, time = -1)) # m/s
add_dimension("acceleration", c(velocity = 1, time = -1)) # (m/s)/s
add_dimension("area", c(length = 2)) # m^2
add_dimension("momentum", c(mass = 1, velocity = 1)) # kg (m/s)



## Dimensions with their own units
## ===============================

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
## ============================

add_unit("time", "h", "hour", multiple = 60 * 60)
add_unit("mass", "t", "tonne", multiple = 1000, gen.prefixes = TRUE)


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
         multiple = Quantity(1e10, "cal"), gen.prefixes = TRUE)

## Therm
## -----
## Definition: The Units of Measurement Regulations, 1995
## http://www.legislation.gov.uk/uksi/1995/1804/schedule/made
add_unit("energy", "therm", "therm", multiple = Quantity(105.505585257348,
                                       "MJ"), gen.prefixes = TRUE)
