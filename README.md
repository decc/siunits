An experimental, extensible units package for R
===============================================

Most physical quantities carry units. This package allows one to include the
units along with the values of numerical quantities. Quantities may be
"converted" -- that is, expressed in different units, where the new units have
the same dimension as the originals.

Presently undocument and little commented.

Getting started
---------------


```r
library(devtools)
load_all(reset = TRUE)
```

```
## Loading siunits
```


Examples
--------


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

```r

add_unit("energy", "tce", "tonne of coal equivalent", "tonnes of coal\nequivalent", 
    multiple = as.Quantity(7, "Gcal"), gen.prefixes = TRUE)
```

