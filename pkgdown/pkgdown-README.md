# About the pkgdown document
*pkgdown* builds website using R package documentation (the .RD files).

The documentation website is [https://big-life-lab.github.io/bllFlow/](https://big-life-lab.github.io/bllFlow/). That site is rendered from the `master` branch and may not be UTD if `pkgdown` branch has not been merged.

You can view the most recent rendering from Rstudio local file at: bllFlow/docs/index.html

Contribute to and build the pkgdown document.
```
# Install release version
install.packages("pkgdown")
```

Run pkgdown from the package directory each time you release your package:
```
pkgdown::build_site()
```

For more information about [pkgdown](https://pkgdown.r-lib.org)