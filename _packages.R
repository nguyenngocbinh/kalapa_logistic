# Initial
options(repos = "http://cran.rstudio.org")
have.packages <- installed.packages()[, 1]
cran.packages <- c("tidyverse", "inspectdf", "data.table", "mlr3verse", "here", "paradox" )
to.install <- setdiff(cran.packages, have.packages)
if (length(to.install) > 0) install.packages(to.install)

#-----------------------------------------------------------------------------
# require packages
pkgs <- c("inspectdf", "readr", "dplyr", "data.table", "tidyr", "magrittr", "drake", "scorecard",
          "rsample", "broom", "R.utils", "pROC", "ROCR", "purrr", "tibble", "mice")
lapply(pkgs, function(pk) require(pk, character.only = TRUE))
