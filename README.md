
<!-- README.md is generated from README.Rmd. Please edit that file -->
bllFlow package <img src="man/figures/pbl-sticker.png" align="right" alt="" width="120" />
==========================================================================================

[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

bllFlow supports open and reproducible research for teams. There there are two main features: - The Model Specification Worksheet (MSW) - CSV files that contain key information (metadata) about the variables in your model. - Output files - CSV files that contain results of your model that adhere to reporting guidelines such as TRIPOD, STROBE, and RECORD.

"Flow" refers to the process of using the Model Specification Worksheet to perform rountine data cleaning and transformation, performance reporting, and model deployment. The imperitive for bllFlow is the development of predictive models but key parts help research using routinely-collected data.

Installation
------------

    # Install from GitHub
    # f not installed, install the devtools
    install.packages("devtools")

    # then, install the package
    devtools::install_github("Big-Life-Lab/bllFlow", 
      auth_token="75bdffc4a195fbc11ffccb546707030a8361d7de",
      ref="master")

Documentation
-------------

1.  Package documentation in pkgdown format is [here](https://big-life-lab.github.io/bllFlow/docs) (currently only accessible to BLL team). See the [pkgdown README](README-pkgdown.md) for instructions about how to build and contribute to the pkgdown document.

2.  You can see bllFlow in action [here](https://big-life-lab.github.io/bllFlow-bookdown/).

RStudio Plugin Installation
---------------------------

1.  Restart the IDE
2.  If the plugin was successfully installed there should be a new entry in the Addins menu at the top of the IDE called "R Project Builder"

<img src="man/figures/Rstudio-addin.png"/>

Plugin Usage
------------

1.  Open the project's 'Model Specification Worksheet (MSW)' (CSV file).
2.  Run the plugin.
3.  The generated project should be in a folder called "generated-project" within the folder containing the web specifications file.

The package requires a MSW CSV file that identifies:

1.  Which variable you would like transformed.
2.  Which transformation you would like performed.

`code example here to load and create a table output....`
