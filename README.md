An experimental, extensible units package for R
===============================================

Most physical quantities carry units. This package defines a new class, "quantity"", which allows numerical quantities in R to carry their phsyical units. Simple operations ("+", "-", "*", "/", "^") on quantities deal correctly with their units (and complain when the operation is not allowed by dimensional analysis). Conversion of units are supported. 

Presently undocument and little commented.




Examples
--------

### Defining simple quantities

```r
m1 <- as.Quantity(1, "kg")
m2 <- as.Quantity(2, "kg")
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


### Conversion of units

```r
as.Quantity(m1, "g")
```

```
## Units: g 
## [1] 1000
```

```r
e1 <- as.Quantity(10, "GW h")
as.Quantity(e1, "ktoe")
```

```
## Units: ktoe 
## [1] 0.8598
```


### Extensibility

```r
## The unit 'tonnes of coal equivalent' is not defined.
as.Quantity(e1, "tce")
```

```
## Error: "tce" is not a known unit
```

```r

add_unit("energy", "tce", "tonne of coal equivalent", "tonnes of coal equivalent", 
    multiple = as.Quantity(7, "Gcal"), gen.prefixes = TRUE)
as.Quantity(e1, "tce")
```

```
## Units: tce 
## [1] 1228
```


### More complex constructions

```r
gas.CO2 <- as.Quantity(0.185, "kg (kW h)_[energy]^-1")
as.Quantity(gas.CO2, "kg therm^-1")
```

```
## Units: kg therm^-1 
## [1] 5.422
```

```r
gas <- as.Quantity(1, "ktoe")
gas * gas.CO2
```

```
## Units: ktoe (kg (kW h)^-1) 
## [1] 0.185
```

```r
as.Quantity(gas * gas.CO2, "kt")
```

```
## Units: kt 
## [1] 2.152
```


