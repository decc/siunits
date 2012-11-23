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
install_github(repo = "decc/siunits")
```

Examples
--------

```r
m1 <- as.Quantity(1, "kg")
m2 <- as.Quantity(2, "kg")

2 * m1
m1 + m2

as.Quantity(m1, "g")

e1 <- as.Quantity(10, "GW h")
as.Quantity(e1, "ktoe")

add_unit("energy", "tce", "tonne of coal equivalent", "tonnes of coal
equivalent", multiple = as.Quantity(7, "Gcal"), gen.prefixes = TRUE)
```