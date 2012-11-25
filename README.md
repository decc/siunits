An experimental, extensible units package for R
===============================================

Most physical quantities carry units. This package defines a new class, `Quantity`, which allows numerical quantities in R to carry their physical units. Simple operations (`+`, `-`, `*`, `/`, and `^`) on quantities deal correctly with their units (and complain when the operation is not allowed by dimensional analysis). Conversion of units is supported. 

Presently undocument and little commented.



Examples
--------

### Defining simple quantities

```r
m1 <- as.Quantity(1, "kg")
m2 <- as.Quantity(2, "kg")
```

### Arithmetic with quantities

```r
2 * m1
```

```
## Units: kg 
## [1] 2
```

```r
m1 + m2
```

```
## Units: kg 
## [1] 3
```

Quantities must have compatible units to be added.

```r
l1 <- as.Quantity(2.54, "cm")
m1 + l1
```

```
## Error: arguments to '+' must be unit compatible
```


### Conversion of units
The full set of SI prefixes is supported.

```r
as.Quantity(m1, "µg")
```

```
## Units: µg 
## [1] 1e+09
```

Names are available in case you forget.

```r
name.unit("YJ")
```

```
## [1] "yottajoules"
```

Complex units are expressed in the usual notation. Multiplication is indicated with a space, powers by `^`. (The solidus (`/`) is not yet supported.) Parentheses group subnits. 

```r
e1 <- as.Quantity(10, "GW h")
as.Quantity(e1, "ktoe")
```

```
## Units: ktoe 
## [1] 0.8598
```

The dimension of a derived dimension is given by writing it in square brackets. 

```r
e2 <- as.Quantity(10, "(GW h)_[energy]")
```

The system distinguishes between the dimensions of `e1`, which are `power x time`, and the dimension of `e2`, which is `energy`. 

### Extensibility
Units can be added to the list of known units.

```r
## The unit 'tonnes of coal equivalent' is not defined in the siunits
## package.
e2 <- as.Quantity(1, "tce")
```

```
## Error: "tce" is not a known unit
```

The definition of tce is 7 gigacalories, and the calorie is already part of the siunits package. The argument `gen.prefixes = TRUE` causes the full set of SI-prefixed units to be added.

```r
add_unit("energy", "tce", "tonne of coal equivalent", "tonnes of coal equivalent", 
    multiple = as.Quantity(7, "Gcal"), gen.prefixes = TRUE)
as.Quantity(1, "tce")
```

```
## Units: tce 
## [1] 1
```

Not only `tce` but the full set of prefixed units will have been created.

```r
name.unit("Mtce")
```

```
## [1] "megatonnes of coal equivalent"
```


### More complex constructions
Emissions factors have dimensions of mass per unit energy.

```r
gas.CO2 <- as.Quantity(0.185, "kg (kW h)^-1")  # These are the units in DUKES
```

Conversions are straightforward:

```r
as.Quantity(gas.CO2, "kg therm^-1")
```

```
## Units: kg therm^-1 
## [1] 5.422
```

Emissions are the product of an emissions factor and an energy. The energy does not have to be in the same units as that in which the emissions are expressed.

```r
CO2 <- gas.CO2 * as.Quantity(1, "ktoe")
CO2
```

```
## Units: (kg (kW h)^-1) ktoe 
## [1] 0.185
```

The result keeps all the dimensions and units in the factors. Nonetheless, the result is a mass, so can be converted to any convenient unit with dimension `mass`.

```r
as.Quantity(CO2, "kt")
```

```
## Units: kt 
## [1] 2.152
```


