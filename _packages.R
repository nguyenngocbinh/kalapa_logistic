# Initial
options(repos = "http://cran.rstudio.org")
have.packages <- installed.packages()
cran.packages <- c("tidyverse", "inspectdf", "data.table", "mlr3verse", "here", "paradox" )
to.install <- setdiff(cran.packages, have.packages[, 1])
if (length(to.install) > 0) install.packages(to.install)

#-----------------------------------------------------------------------------
# require packages
pkgs <- c("inspectdf", "readr", "dplyr", "data.table", "tidyr", "magrittr", "drake", "scorecard",
          "rsample", "broom", "R.utils", "pROC", "ROCR", "purrr", "tibble")
lapply(pkgs, function(pk) require(pk, character.only = TRUE))

