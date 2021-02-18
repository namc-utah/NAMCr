# NAMCr
NAMC Database and Analysis API for R

## Install

Development version

```r
install.packages("remotes")
remotes::install_github("namc-utah/NAMCr")
```

## Example

Example via namespace

```r
requireNamespace("NAMCr")

data = NAMCr::query("sites")
View(data)
```

Example via package loading

```r
library("NAMCr")

data = query("sites")
View(data)
```