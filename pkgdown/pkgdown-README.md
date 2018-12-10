# About the pkgdown document
*pkgdown* builds website using R package documentation (the .RD files).

The documentation website is [https://big-life-lab.github.io/bllFlow/docs](https://big-life-lab.github.io/bllFlow/docs)

Or local at: bllFlow/docs/index.html

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