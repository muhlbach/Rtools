options(warn = 0)

R.packages <- c(
"plyr digest ggplot2 colorspace stringr RColorBrewer reshape2 zoo proto scales car 
dichromat gtable munsell labeling Hmisc rJava mvtnorm bitops foreign XML 
lattice e1071 gtools sp gdata Rcpp MASS Matrix lmtest survival caTools multcomp 
RCurl knitr xtable xts rpart evaluate RODBC quadprog tseries DBI nlme lme4 reshape 
sandwich leaps gplots abind randomForest Rcmdr coda maps igraph formatR maptools 
RSQLite psych KernSmooth rgdal RcppArmadillo effects sem vcd XLConnect markdown 
timeSeries timeDate RJSONIO cluster scatterplot3d nnet fBasics forecast quantreg 
foreach chron plotrix matrixcalc aplpack strucchange iterators mgcv kernlab 
SparseM tree robustbase vegan devtools latticeExtra modeltools slam TTR quantmod 
relimp akima memoise caret glmnet ROCR gbm party arules klaR RWeka ipred lars 
earth CORElearn mboost grf ranger hdm anytime bizdays bsts doParallel ggpubr ggthemes margins 
Rfast AER aod corrplot xgboost RQuantLib pls 
here Cairo tidyverse tables wCorr Gmisc pacman 
readr dplyr lubridate gridExtra hablar purrr data.table rstudioapi profvis matrixStats 
DescTools kableExtra latex2exp pbmcapply doMC h2o automl")

# Currently these packages cannot be installed:
# None 

R.packages <- gsub("\n", "", R.packages)
R.packages <- strsplit(R.packages, "\\s+")[[1]]
R.packages <- unique(R.packages)

# Remove already-installed packages from list
R.packages <- R.packages[!(R.packages %in% installed.packages()[,"Package"])]

# Install
# install.packages(R.packages)

# cardichromat, XMLlattice

# Pre-allocate
packages_not_installed <- c()

# Install package one at a time
for (rpackage in R.packages) {
  
  # Status tracker
  cat("\n\n ########## INSTALLLING PACKAGE:", rpackage, " ########## \n")
  
  # Try-Catch Install
  tc <- tryCatch(utils::install.packages(rpackage, force = FALSE), warning = function(w) return(w))
  
  # Get message
  return_msg <- ifelse(test = is.null(tc), yes = TRUE, no = rpackage)
  if(return_msg == TRUE){return_msg <- NULL}
  
  # Extend list of packages not installed
  packages_not_installed <- c(packages_not_installed, return_msg)
  
}
