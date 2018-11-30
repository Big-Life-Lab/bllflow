# About the pkgdown document
*pkgdown* builds website using R package documentation (the .RD files).

The documentation build is [here](https://big-life-lab.github.io/bllFlow/docs)
(not yet active until we merge this branch with master)

If you would like to contribute to and build the pkgdown document.

```
# Install release version from CRAN
install.packages("pkgdown")

# Install development version from GitHub
devtools::install_github("r-lib/pkgdown")
```

Run pkgdown from the package directory each time you release your package:
```
pkgdown::build_site()
```

For more information about [pkgdown](https://pkgdown.r-lib.org)