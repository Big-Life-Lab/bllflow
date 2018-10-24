There are two reasons to use this R package.

1. To transform variables using a consistent approach.
2. To generate R code that can be converted (transpiled) into Predictive Modelling Markup Language (PMML).

Currently, four transformations are supported:

- Centering
- Interaction terms
- Dummying
- RCS Variables
  This project is also available as an RStudio plugin and it's the recommended
  way to use it

# RStudio Plugin Installation

1. Install the devtool in RStudio by running the command `install.packages("devtools")`
2. Install the plugin by running the command `devtools::install_github("Big-Life-Lab/pbl-r-project-builder")`
   - If you have 2 factor authentication on your github account then you will need
     to retreive your personal access token from github (https://help.github.com/articles/creating-a-personal-access-token-for-the-command-line/)
     and use it to install the plugin by running the command `devtools::install_github("Big-Life-Lab/pbl-r-project-builder", auth_token="<personal access token>")`
3. Restart the IDE
4. If the plugin was successfully installed there should be a new entry in the
   Addins menu at the top of the IDE called "R Project Builder"

![Rstudio add-in](Rstudio-addin.png)

# How the package works

The package requires a CSV file that identifies:

1. Which variable you would like transformed.
2. Which transformation you would like performed.

The CSV file (what we call the `Model Specification File (MSF`). Excute the following code to see how the MSF is organized.

`code example here to load and create a table output....`

In the same code, .....

# Plugin Usage

1. Open the web specifications CSV file for which you need to build an R project for
2. Run the plugin
3. The generated project should be in a folder called "generated-project" within the folder
   containing the web specifications file
