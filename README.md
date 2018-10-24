Builds out an R project based on a Web Specifications file. Currently
it creates code for:
* Centering
* Interaction terms
* Dummying
* RCS Variables
This project is also available as an RStudio plugin and it's the recommended
way to use it

# RStudio Plugin Installation

1. Install the devtool in RStudio by running the command `install.packages("devtools")`
2. Install the plugin by running the command `devtools::install_github("https://github.com/Big-Life-Lab/pbl-r-project-builder")`
    * If you have 2 factor authentication on your github account then you will need
    to retreive your personal access token from github (https://help.github.com/articles/creating-a-personal-access-token-for-the-command-line/)
    and use it to install the plugin by running the command `devtools::install_github("https://github.com/Big-Life-Lab/pbl-r-project-builder", auth_token="<personal access token>")`
3. Restart the IDE
4. If the plugin was successfully installed there should be a new entry in the
Addins menu at the top of the IDE called "R Project Builder"

# Plugin Usage

1. Open the web specifications CSV file for which you need to build an R project for
2. Run the plugin
3. The generated project should be in a folder called "generated-project" within the folder
containing the web specifications file