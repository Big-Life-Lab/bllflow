
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bllflow - an R package for efficient, transparent data prepartion and reporting <img src="./man/figures/pbl-sticker.png" align="right" alt="" width="120" />

### Is bllflow for you?

  - Do you shudder at the thought of trying to update the analyses for a
    previous study? (let alone imagine someone else trying to replicate
    your analyses?)

  - Are your data and statistical models becoming more complex,
    challenging to perform, and challenging to report?

  - Are you concerned about the
    [misuse](https://www.nature.com/articles/d41586-019-00857-9) of
    statistical findings? But not sure about reporting all results of
    all analyses?

  - Do you work in teams that span disciplines and institutions?

We answered ‘yes’ to all these questions and then created *bllflow*.

### The purpose of bllflow

**bllFlow** supports transparent, reproducible data analyses and model
development. The goal is to improved science quality with quicker and
more efficient data analyses.

### What does bllflow do?

The focus of *bllflow* is data cleaning and variable transformation –
the most time consuming and tedious analytic task – and analyses
reporting.

*bllflow* functions and workflow build from other packages including
**sjmisc**, **tableone**, **codebook**, and **Hmisc**.

There are three main features:

1)  **The Model Specification Workbook (MSW)** - Start your model
    development with worksheets (CSV files) that contain information
    about the variables in your model, data cleaning and transformation
    steps and how to create output tables.

2)  **Functions to perform routine data cleaning and transformation
    tasks** - use functions with or without the Model Specification
    Workbook. Functions with ‘BLL’ in the function name perform data
    cleaning and transformation using the Model Specification Workbook.

3)  **Formatted output files, tables** - results of your analyses in a
    consistent format following the concept of ‘one document, many
    uses’.

At any point of your analyses you have:

  - a log of data cleaning and transformed variables (how your data was
    cleaned and transformed).
  - a codebook to facilitate data transparency and provenance.

*bllflow* supports the use of metadata, including:

  - the Data Document Initiative ([DDI](https://ddialliance.org)).
  - Predictive Model Modelling Language for predictive algorithms
    ([PMML](http://dmg.org/pmml/v4-3/GeneralStructure.html)) files for
    transparent algorithm reporting and deployment.

*bllflow* workflow and functions support reporting guidelines such as
[TRIPOD](https://www.equator-network.org/2015/01/07/guidelines-for-reporting-multivariable-prediction-models-for-individual-prognosis-or-diagnosis-tripod-published/),
[STROBE](http://www.equator-network.org/reporting-guidelines/strobe/),
and
[RECORD](http://www.equator-network.org/reporting-guidelines/record/).

## Installation

    # If not installed, install the devtools
    install.packages("devtools")
    
    # then, install the package
    devtools::install_github("Big-Life-Lab/bllFlow")

There are plans to submit bllFlow to CRAN once we include all seven
steps of the *bllflow* workflow. Currently on step \#4.

## Contributing to the package

Please follow [this guide](CONTRIBUTING.md) if you like to contribute to
the *bllflow* package.
