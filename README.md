# NAMCr
NAMC Database and Analysis API for R

## Install

Development version

```r
install.packages("remotes")
remotes::install_github("namc-utah/NAMCr")
```

## Update

Update to latest package version

```r
detach("NAMCr", unload=TRUE)
remotes::install_github("namc-utah/NAMCr", force=TRUE, upgrade="never", quiet=TRUE)
```

## Examples

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

data = query("projectOrganisms", projectIds=c(1,2))
View(data)

data = query("projectOrganisms", projectIds=c(1,2))
View(data)
```

Refresh API schema / Credentials

```r
requireNamespace("NAMCr")

# Refresh cached API schema to capture API updates
NAMCr::reinitiate()

# Refresh cached API schema and credentials
NAMCr::reinitiate(TRUE)

```

View API Documentation

```r
requireNamespace("NAMCr")

NAMCr::docs()

```

View Information on all Endpoint

```r
requireNamespace("NAMCr")

NAMCr::info()

```


View Information on an Individual Endpoint

```r
requireNamespace("NAMCr")

NAMCr::info("samples")

```

List Available Endpoints

```r
requireNamespace("NAMCr")

NAMCr::endpoints()

```

Connection Diagnostics

```r
requireNamespace("NAMCr")

NAMCr::diagnose_connection()

```

## CLI (command line interface)

Example CLI call

```r
requireNamespace("NAMCr")

data = NAMCr::cli()
```