###############################################################################
#################### PURPOSE
###############################################################################
'
The purpose of this script is standardize functions that are being used routinely

Author: Nicolaj
Date: February 26, 2021
'

###############################################################################
#################### STANDARD UTILITY FUNCTIONS
###############################################################################
# "not.in" function
'%!in%' <- function(x,y)!('%in%'(x,y))

# Omit NA entries in list
na.omit.list <- function(y) { return(y[!sapply(y, function(x) all(is.na(x)))]) }

# Omit element if it contains "contains"
list.omit.if <- function(lst, contains) {lst[lapply(lst, function(x) length(grep(contains,x,value=FALSE))) == 0]}

# Get n'th element in list of lists
get.nth.element <- function(list.of.lists, nth.element){
  sapply(list.of.lists, `[`, nth.element)
}

# Multiply to numbers
multiply <- function(x, c){x * c}

# Convert knitr_kable to LaTex table by removing table enviroment and keeping only tabuylar enviroment
fromKabletoLatex <- function(tab){
  
  # Remove "\begin{table}[!h]"
  tab <- gsub("\\\\begin\\{table\\}\\[!h]", "", tab) #\\[!h]
  
  # Remove "\end{table}"
  tab <- gsub("\\\\end\\{table\\}", "", tab)
  
  # Remove centering
  tab <- gsub("\\\\centering", "", tab)
  
  return(tab)
  
}


# Capitalize first letter and leave the rest
firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  return(x)
}

# Round and format to exact digits
round.to.exact.digit <- function(x, digits){
  
  x <- format(round(x, digits = digits), nsmall = digits)
  
  return(x)
  
}


# Add % to elements
to.percent <- function(x, digits = 2){
  
  # Test if matrix or data.frame
  isMulti <- is.matrix(x) | is.data.frame(x)
  
  if (isMulti) {
    
    # Find index
    idx_num <- apply(X = as.matrix(x), MARGIN = 2, FUN = is.numeric)
    
    ## Change all indices
    # Multiply by 100
    x[,idx_num] <- multiply(x = x[,idx_num], c = 100)
    
    # Round
    x[,idx_num] <- round.to.exact.digit(x = x[,idx_num], digits = digits)
    
    # Paste %
    x[,idx_num] <- apply(X = x[,idx_num], MARGIN = 2, FUN = paste0, "\\%")
    
  } else {
    
    if (is.numeric(x)) {
      
      # Multiply by 100
      x <- multiply(x = x, c = 100)
      
      # Round
      x <- round.to.exact.digit(x = x, digits = digits)
      
      # Paste
      x <- paste0(x, "\\%")
      
    }
    
  }
  
  return(x)
  
}


# Get extremus stats
getExtrema <- function(x, extrema = "max", by = NULL){
  
  # Get numerical col idx
  idx.num <- sapply(X = x, FUN = is.numeric)
  
  # Subset
  x <- x[, idx.num]
  
  if (is.null(by)) {
    stats <- do.call(what = extrema, args = x)
  } else if(by == "col"){
    stats <- apply(X = x, MARGIN = 2, FUN = get(extrema))
  } else if(by == "row"){
    stats <- apply(X = x, MARGIN = 1, FUN = get(extrema))
  } else {
    stop("Please specify correct `by`")
  }
  
  return(stats)
}


# Aggregate by mulltiple columsn and give appropriate names
aggregate_by_multiple <- function(x, by, FUN) {
  
  # Compute outcomes by misclassiffied (TRUE/FALSE) and predicted outcome group (1,...,num_group)
  stats_temp <- aggregate(x = x,
                          by = by,
                          FUN = FUN,
                          na.rm = TRUE) 
  
  # Transform from long to wide
  stats_temp <- setDF(data.table::dcast(data = setDT(stats_temp),
                                        formula = paste0("1 ~ ", paste(names(by), collapse = " + ")),
                                        value.var = "x"))
  
  # Remove empty (as we have transformed into only one row)
  stats_temp$. <- NULL
  
  # Give appropriate names
  names(stats_temp) <- do.call(c,lapply(strsplit(x = names(stats_temp), split = "_"), FUN = function(x){paste0(paste0(names(by), x), collapse = "_")}))
  
  # Return
  return(stats_temp)
  
}



###############################################################################
#################### STATISTICAL FUNCTIONS
###############################################################################
# Get iid standard erros from design matrix and residuals
get.iid.se <- function(X, eps){
  
  # Find n and p
  if (is.matrix(X)) {
    n <- nrow(X)
    p <- ncol(X)  
  } else {
    n <- length(X)
    p <- 1  
  }
  
  # Estimate iid variance
  var_iid <- (t(eps) %*% eps)/(n-p) * solve(X %*% X)
  
  # Extract se
  se_idd <- sqrt(diag(var_iid))
  
  return(se_idd)
}

# Get beta coefficients from linear regression
get.beta <- function(Y, X, constant = FALSE){
  
  if (constant) {
    # Add ones
    X <- cbind(1,X)
  }
  
  # Estimate betaa
  beta <- solve(t(X) %*% X) %*% (t(X) %*% Y)
  
  return(beta)
  
}


# Function to compute expanding means
expanding.mean <- function(x, na.rm = TRUE){
  
  # Copy matrix
  x.ave <- x
  
  if (ncol(x) > 1) {
    # Compute expanding row mean  
    for (j in 2:ncol(x)) {
      x.ave[, j] <- rowMeans(x[, 1:j], na.rm)
    }
    
  }
  
  return(x.ave)
}


# Add CI
add.ci <- function(x, se, alpha = 0.05){
  
  # Compute bounds
  lower <- x - qnorm(p = 1-alpha/2) * se
  upper <- x + qnorm(p = 1-alpha/2) * se
  
  # Return obj
  retx <- data.frame("ci.lower" = lower, "ci.upper" = upper)
  
  # Return
  return(retx)
  
}


# Return quantile of empirical CDF
empirical_cdf <- function(x, value, numerical.output=TRUE) {
  
  if (numerical.output) {
    # Return numerical cdf
    cdf <- ecdf(x)(value)
    
  } else {
    # Return CDF as character
    cdf <- paste(100*round(ecdf(x)(value), 2), "%", sep = "")
  }
  
  return(cdf)
}


# Various machine learning methods in standardized syntax. Allow for cross-validation as well.
MLmodels <- function(Y, X, newdata = NULL, model = "randomforests", params = NULL, return.model = FALSE,
                     cv = FALSE, parallel = TRUE, ncorespct = 0.8){
  
  # --------------------
  # SETUP
  # --------------------

  # Libraries
  stopifnot(require(ranger))
  stopifnot(require(xgboost))
  stopifnot(require(glmnet))
  stopifnot(require(caret))
  stopifnot(require(hdm))
  stopifnot(require(doParallel))
  
  # Recast as matrices
  Y <- as.matrix(Y)
  X <- as.matrix(X)
  
  # Number of parameters
  p <- ncol(X)
  N <- nrow(X)
  
  # Preferred/critical parameters across all models
  params_preferred <- list(
    
    # ranger
    num.trees = 200,
    oob.error = FALSE,
    
    # boosting
    "nrounds" = 200,
    "verbose" = FALSE
  )
  
  # --------------------
  # SANITY CHECKS
  # --------------------
  # Make sure critical parameters are specified (when default is not available)
  if (is.null(params)) {
    # Overwrite completely
    params <- params_preferred
  } else {
    # Append missing parameters
    params <- c(params, params_preferred[!(params_preferred %in% params)])
  }
  
  if (!is.null(newdata)) {
    # Recast as matrix
    newdata <- as.matrix(newdata)
    
    # Get difference of colnames
    diff_colnamees <- setdiff(colnames(X), colnames(newdata))
    
    # Make sure X and newdata contain same colnames
    if ( !(is.null(diff_colnamees) | length(diff_colnamees) == 0) ) {
      stop("'newdata' does not contain all the columns of 'X'")
    }
    
  }
  
  # Check if data are comformable
  if (nrow(Y) != nrow(X)) {
    stop("Y and X have different lengths")
  }
  
  
  
  # --------------------
  # CROSS-VALIDATION
  # --------------------
  if (cv) {
    
    # Obtain number of cores (min = 1, max = cores-1)
    ncores <- min(max(1,floor((detectCores()-1)*ncorespct)),detectCores()-1)
    
    # Specify parameters used to control training in CV
    params_train_control <- trainControl(method = "repeatedcv",
                                         number = 5,
                                         repeats = 1,
                                         search = "grid",
                                         verboseIter = FALSE,
                                         allowParallel = TRUE,
                                         returnData = FALSE)
    
    # Specify default parameters used in cv
    params_default_cv <- list("y" = as.numeric(Y),
                              "x" = X,
                              "metric" = "RMSE",
                              "trControl" = params_train_control,
                              "preProcess" = NULL)
    
  }
  
  
  
  # --------------------
  # MODEL SETTINGS
  # --------------------
  # Specific settings that depend on the model
  
  if (model == "randomforests") {
    # ----------
    # RANDOM FORESTS
    # ----------
    if (cv) {
      
      ## CV
      # Specific CV parameters
      par_mtry <- floor(seq(from = 0.1, to = 0.9, length.out = 5) * p)
      par_mtry <- par_mtry[par_mtry > 1 & par_mtry <= 20]
      
      # Specify CV parameters
      params_cv <- expand.grid(mtry = par_mtry,                                   # default: sqrt(p)
                               splitrule  ="variance",                            # default: "variance", 
                               min.node.size = seq(from = 5, to = 25, length.out = 3)) # default: 5
      
      # Extra model-specific parameters
      params_extra_cv <- list(num.threads = 1, # fix num.threads=1 to avoid multithreadding
                              num.trees = NA, oob.error = NA)
      
      # Determine the function call used to CV (assuming the package is loaded)
      fnc_call_cv <- "ranger"
      
      # Define call used to make predictions
      predict_call_cv <- "predict(object = f, newdata)"
      
    } else {
      
      # Determine the function call (assuming the package is loaded)
      fnc_call <- "ranger"
      
      # Specify data parameters
      params_data <- list("formula" = "Y ~ .",
                          "data" = data.frame("Y" = Y, "X" = X))
      
      # Required parameters
      params_required <- list()
      
      # Define call used to make predictions
      predict_call <- "predict(object = f, data.frame('X' = newdata))$predictions"
      
    }
    
    
  } else if (model == "boosting") {
    # ----------
    # BOOSTING / BOOSTED TREES
    # ----------
    if (cv) {
      ## CV
      # Specific CV parameters
      
      # Specify CV parameters
      params_cv <- expand.grid(nrounds = c(200),         # no default
                               max_depth = c(3, 6, 9),           # default: 6
                               eta = c(0.001, 0.1, 0.3),                  # default: 0.3
                               gamma = c(0),                  # default: 0
                               colsample_bytree = c(2/3),     # default: 1
                               min_child_weight= c(1),        # default: 1
                               subsample = c(1))              # default: 1
      
      # Extra model-specific parameters
      params_extra_cv <- list("x" = X,
                              "objective" = "reg:squarederror",
                              nthread = 1)
      
      # Give colnames (caret needs x to have colnames!)
      colnames(params_extra_cv$x) <- paste0("X",1:p)
      
      
      # Determine the function call used to CV (assuming the package is loaded)
      fnc_call_cv <- "xgbTree"
      
      # Define call used to make predictions
      predict_call_cv <- "predict(object = f, newdata)"
      
    } else {
      # Determine the function call (assuming the package is loaded)
      fnc_call <- "xgboost"
      
      # Specify data parameters
      params_data <- list("data" = xgb.DMatrix(data = X, label = Y))
      
      # Required parameters
      params_required <- list(booster = "gbtree",
                              objective = "reg:squarederror")
      
      # Define call used to make predictions
      predict_call <- "predict(object = f, newdata)"
    }
    
    
  } else if (model == "lasso") {
    # ----------
    # LASSO
    # ----------
    if (cv) {
      
      ## CV
      # Specific CV parameters (only lasso)
      alpha_par <- c(1)
      
      # Initialize
      params_cv <- data.frame()
      
      for (a in alpha_par) {
        
        ## Compute sequence
        # lambda_max <- max(abs(colSums(sweep(x = X, MARGIN = 1, STATS = Y, FUN = "*")))) / (N*a)
        # lambda_min <- 0.001*lambda_max
        # lambda_seq <- exp(seq(log(lambda_max), log(lambda_min), length.out = 50))
        lambda_seq <- glmnet(X, Y, alpha = a, nlambda = 50)$lambda
        
        # Construct grid
        params_cv_temp <- data.frame("alpha" = a, "lambda" = lambda_seq)
        
        # Add to grid
        params_cv <- rbind(params_cv, params_cv_temp)
      }
      
      # Extra model-specific parameters
      params_extra_cv <- list("family" = "gaussian")
      
      # Give colnames (caret needs x to have colnames!)
      # colnames(params_extra_cv$x) <- paste0("X",1:p)
      
      # Determine the function call used to CV (assuming the package is loaded)
      fnc_call_cv <- "glmnet"
      
      # Define call used to make predictions
      predict_call_cv <- "predict(object = f, newdata)"
      
    } else {
      # Determine the function call (assuming the package is loaded)
      fnc_call <- "glmnet"
      
      # Specify data parameters
      params_data <- list("x" = X,
                          "y" = Y)
      
      # Required parameters 
      params_required <- list("family"="gaussian")
      
      # Define call used to make predictions
      predict_call <- "rowMeans(predict(object = f, newdata))"
    }
    
    
    
  } else if (model == "ols") {
    
    # No need to cross-valie OLS as it has an analytical solution, hence set cv = FALSE
    cv <- FALSE
    
    # Determine the function call (assuming the package is loaded)
    fnc_call <- "lm"
    
    # Specify data parameters
    params_data <- list("formula" = "Y ~ .",
                        "data" = data.frame("Y" = Y, "X" = X))
    
    # Required parameters
    params_required <- list()
    
    # Define call used to make predictions
    predict_call <- "predict(object = f, data.frame('X' = newdata))"
    
  } else {
    
    stop("Specify correct model")
    
  }
  
  
  
  
  
  
  # --------------------
  # ESTIMATE MODEL
  # --------------------
  if (cv) {
    
    # Update parameters (only those who are set to "NA" above)
    params_extra_cv <- modifyList(x = params_extra_cv,
                                  val = params[intersect(names(params_extra_cv)[is.na(params_extra_cv)], names(params))])
    
    # Add some default CV parameters after having speficied the model parameters
    params_model_cv <- c(list("method" = fnc_call_cv,
                              "tuneGrid" = params_cv),
                         params_extra_cv)
    
    # Merge
    params_final <- modifyList(x = params_default_cv, val = params_model_cv)
    
    if (parallel){
      # Start cluster
      clust <- parallel::makeCluster(spec = ncores, type = "FORK")
      doParallel::registerDoParallel(cl = clust)
      # showConnections()
    }
    
    # Estimate model, f (suppress warning because XGB and GLMNET give warnings)
    suppressWarnings(f <- DescTools::DoCall(what = caret::train, args = params_final))
    
    if (parallel){
      # Stop cluster
      stopCluster(clust)
      registerDoSEQ()
      rm(clust)
    }
    
    # Predict
    yhat <- NULL
    if (!is.null(newdata)) {
      yhat <- as.numeric(eval(parse(text = predict_call_cv)))
      # showConnections()
    }
    
  } else {
    
    # Get default parameters as list
    params_default <- as.list(rlang::fn_fmls(fn = get(fnc_call)))  
    
    # Remove types of "language"
    params_default[lapply(X = params_default, FUN = typeof) %in% c("language")] <- NULL  
    
    ## Update params
    # Get string of arguments to be updated
    args_update <- intersect(names(params_default), names(params))
    
    # Merge parameters
    params_default <- modifyList(x = params_default, val = params[args_update])
    
    # Remove types of "symbol"
    params_default[lapply(X = params_default, FUN = typeof) %in% c("symbol")] <- NULL  
    
    # Specific parameters
    params_specific <- modifyList(x = params_data, val = params_required)
    
    # Merge parameters
    params_final <- modifyList(x = params_default, val = params_specific) 
    
    # Estimate model, f
    # f <- Gmisc::fastDoCall(what = get(fnc_call), args = params_final)
    f <- DescTools::DoCall(what = get(fnc_call), args = params_final)
    
    # Predict
    yhat <- NULL
    if (!is.null(newdata)) {
      yhat <- as.numeric(eval(parse(text = predict_call)))
    }
  }
  
  # --------------------
  # RETURN
  # --------------------
  # Define object to be returned
  if (return.model) {
    
    ret_obj <- list("yhat" = yhat,
                    "trained.f" = f)
    
  } else {
    
    ret_obj <- yhat
    
  }
  
  # Return
  return(ret_obj)
  
  
  
} # END FUNCTION



# Bias-variance decomputation of parameter theta
bias.variance.decomposition <- function(theta.hat, theta0 = 0, tol = 0.0001){
  
  # Ensure conformity
  if (ncol(theta.hat) != length(theta0)) {
    theta0 <- rep(theta0, ncol(theta.hat))
  }
  
  # Averages of empirical estimates
  theta.bar <- colMeans(theta.hat)
  
  # Bias: E[theta.hat] - theta0
  theta.bias <- theta.bar - theta0
  
  # Variance: E[ (theta.hat - E[theta.hat])^2]
  theta.diff <- sweep(x = theta.hat, MARGIN = 2, STATS = theta.bar, FUN = "-")
  theta.diff.sq <- theta.diff^2
  theta.variance <- colMeans(theta.diff.sq)
  
  # MSE: E[ (theta.hat - theta0)^2]
  theta.mse <- colMeans(sweep(x = theta.hat, MARGIN = 2, STATS = theta0, FUN = "-")^2)
  
  # Sanity check: MSE = bias^2 + variance
  if (any(theta.mse - (theta.bias^2 + theta.variance) > tol)) {
    stop("MSE does not equal bias^2+variance")
  }
  
  # Construct return object
  ret.obj <- matrix(data = c(theta.bias, theta.variance, theta.mse), nrow = 3, byrow = TRUE)
  
  # Give names
  rownames(ret.obj) <- c("bias", "variance", "mse")
  colnames(ret.obj) <- colnames(theta.hat)
  
  return(ret.obj)
  
}


# Bias-variance decomposition of estimates of conditional means
mse.decomposition <- function(Y, Y.hat, tol = 0.0001, by.group = NULL){
  
  ## Compute loss for all
  # Compute prediction error: fhat(x_i) - f(x_i)
  eps <- Y - Y.hat
  
  # Bias: E[e]
  bias <- mean(eps, na.rm = TRUE)
  
  # Variance: E[(e - E[e])^2]
  variance <- mean((eps - mean(eps, na.rm = TRUE))^2, na.rm = TRUE)
  
  # MSE: E[e^2]
  mse <- mean(eps^2, na.rm = TRUE)
  
  # Sanity check: MSE = bias^2 + variance
  if (any(mse - (bias^2 + variance) > tol)) {
    stop("MSE does not equal bias^2+variance")
  }
  
  # Store results
  results <- list("bias" = bias, "variance" = variance, "mse" = mse)
  
  ## Compute loss by group
  if (!is.null(by.group)) {
    
    # Unique groups
    groups.unique <- unique(by.group)
    
    # Cumpute by group
    for (g in groups.unique) {
      
      # Get indices
      idx <- by.group == g
      
      # Compute prediction error: fhat(x_i) - f(x_i)
      eps_g <- Y[idx] - Y.hat[idx]
      
      # Bias: E[e]
      bias_g <- mean(eps_g, na.rm = TRUE)
      
      # Variance: E[(e - E[e])^2]
      variance_g <- mean((eps_g - mean(eps_g, na.rm = TRUE))^2, na.rm = TRUE)
      
      # MSE: E[e^2]
      mse_g <- mean(eps_g^2, na.rm = TRUE)
      
      # Sanity check: MSE = bias^2 + variance
      if (any(mse_g - (bias_g^2 + variance_g) > tol)) {
        stop("MSE does not equal bias^2+variance")
      }
      
      # Save results
      results_g <- list("bias" = bias_g, "variance" = variance_g, "mse" = mse_g)
      
      # Add names
      names(results_g) <- paste0(names(results_g), "_g", g)
      
      # Append results
      results <- c(results, results_g)
      
    } # FOR loop
    
  } # IF statement
  
  
  return(results)
  
}


# Function to compute the 0-1 loss
get.zero.loss.group <- function(Y.group=NULL, Y.hat, Y=NULL, num.groups=3, by.group = NULL){
  
  # Handle missing observations
  # idx.missing <- is.na(Y.hat)
  
  if (!is.null(Y.group)) {
    # Everything is great, continue
  } else if (!is.null(Y)) {
    Y.group <- as.numeric(Hmisc::cut2(rank(Y, ties.method = "average", na.last = "keep"), g = num.groups))
  } else {
    stop("Please provide either 'Y.group' or 'Y'")
  }
  
  ## Compute loss for all
  # Subset
  # Y.group <- Y.group[!idx.missing]
  # Y.hat <- Y.hat[!idx.missing]
  
  # Compute group based on predictions
  Y.hat.group <- as.numeric(Hmisc::cut2(rank(Y.hat, ties.method = "average", na.last = "keep"), g = num.groups))
  # NB! When we have missing, the ordering will change even for W1 where we do not have missing
  
  # Compute 0-1 loss
  loss.01 <- mean(Y.group != Y.hat.group, na.rm = TRUE)
  
  # Store results
  results <- list("loss01" = loss.01)
  
  ## Compute loss by group
  if (!is.null(by.group)) {
    
    # Unique groups
    groups.unique <- unique(by.group)
    
    # Cumpute by group
    for (g in groups.unique) {
      
      # Get indices
      idx <- by.group == g
      
      # Update indices
      # idx <- idx[!idx.missing]
      
      # Compute 0-1 loss
      loss.01_g <- mean(Y.group[idx] != Y.hat.group[idx], na.rm = TRUE)
      
      # Save results
      results_g <- list("loss01" = loss.01_g)
      
      # Add names
      names(results_g) <- paste0(names(results_g), "_g", g)
      
      # Append results
      results <- c(results, results_g)
      
    } # FOR loop
    
  } # IF statement
  
  
  return(results)
  
}


# Compute degree of overfitting
fitting.degree <- function(Y.observed, Y.true, Y.hat, by.group = NULL){
  
  ### OVERALL
  
  ## OVERFITTING
  # Compute absolute loss (true and noise)
  eps.true <- abs(Y.true - Y.hat)
  eps.observed <- abs(Y.observed - Y.hat)
  
  # Indicate overfitting
  overfiting.to.observed <- mean(eps.observed < eps.true, na.rm = TRUE)
  
  ## UNDERFITTING
  dist.to.mean <- abs(Y.hat - mean(Y.observed, na.rm = TRUE))
  
  # Indicate underfitting
  underfiting.to.signal <- mean(dist.to.mean < eps.true, na.rm = TRUE)
  
  # Store results
  results <- list("overfitting" = overfiting.to.observed,
                  "underfitting" = underfiting.to.signal)
  
  ### BY GROUP
  if (!is.null(by.group)) {
    
    # Unique groups
    groups.unique <- unique(by.group)
    
    # Compute by group
    for (g in groups.unique) {
      
      # Get indices
      idx <- by.group == g
      
      ## OVERFITTING

      # Indicate overfitting
      overfiting.to.observed_g <- mean(eps.observed[idx] < eps.true[idx], na.rm = TRUE)
      
      ## UNDERFITTING (keep the same mean)
      # Indicate underfitting
      underfiting.to.signal_g <- mean(dist.to.mean[idx] < eps.true[idx], na.rm = TRUE)
      
      # Store results
      results_g <- list("overfitting" = overfiting.to.observed_g,
                        "underfitting" = underfiting.to.signal_g)
      
      # Add names
      names(results_g) <- paste0(names(results_g), "_g", g)
      
      # Append results
      results <- c(results, results_g)
      
    } # FOR loop
    
    
  } # IF statement
  
  
  return(results)
  
}

# Check if observations are misclassified
is_misclassified <- function(Y.hat, Y.group=NULL, Y=NULL, num.groups=3){
  
  if (!is.null(Y.group)) {
    # Everything is great, continue
  } else if (!is.null(Y)) {
    Y.group <- as.numeric(Hmisc::cut2(rank(Y, ties.method = "average", na.last = "keep"), g = num.groups))
  } else {
    stop("Please provide either 'Y.group' or 'Y'")
  }
  
  # Compute group based on predictions
  Y.hat.group <- as.numeric(Hmisc::cut2(rank(Y.hat, ties.method = "average", na.last = "keep"), g = num.groups))
  
  # Check if misclassified
  is.misclassified <- as.numeric(Y.group != Y.hat.group)
  
  return(is.misclassified)
  
}



# Function to raise element to powers
raise.to.power <- function(x,p){`^`(x,p)}

# Evaluate performance
evaluate_performance <- function(observed, predicted, training.mean = NULL){
  
  # Compute squared error
  squared_error <- (observed-predicted)^2
  
  # MSE & RMSE
  mse <- mean(squared_error, na.rm = TRUE)
  rmse <- sqrt(mse)
  
  # Out-of-sample R2
  if (is.null(training.mean)) {
    oosr2 <- NULL
  } else {
    SSR = sum(squared_error)
    SST = sum((observed - training.mean)^2)
    oosr2 = 1 - SSR / SST
  }
  
  
  results <- list("MSE" = mse,
                  "RMSE" = rmse,
                  "OoS-R2" = oosr2)
  
  return(results)
  
}

#  Obtain density
get.density <- function(x, na.rm = TRUE){return(x = density(x, na.rm = na.rm)$y)}


get.yx.inputs <- function(x, outlier.pct = 0.025, by = NULL){
  
  # Get quantiles
  qnts <- apply(X = x, MARGIN = 2, FUN = quantile, probs = c(outlier.pct, 1-outlier.pct), na.rm = TRUE)
  
  # Limit x-axis
  limit.x.abs <- max(abs(qnts))
  x.limits <- c(-limit.x.abs,limit.x.abs)
  
  ## Get density to set limit on y-axis
  # Find outliers
  idx.outlier <- abs(x) > limit.x.abs
  
  # Set outliers to NA
  x[idx.outlier] <- NA
  
  # Get densities and maximum density (used to scale density plots)
  densities <- purrr::transpose(apply(X = x, MARGIN = 2, FUN = density, na.rm = TRUE))
  
  # Get maximum density overall
  df.max.density <- data.frame("group" = "overall",
                               "density" = max(unlist(densities$y)))  
  
  
  # By group
  if (!is.null(by)) {
    
    # Omit columns with less than `min_obs` observations
    min_obs <- 5
    
    # Split estimates by ID    
    data.by <- split(x = x, f = by)
    
    # Clean; Omit columns with less than `5` observations
    data.by.clean <- lapply(X = data.by, FUN = function(x){x[, colSums(!is.na(x)) > min_obs]})
    
    # Obtain densities by split
    density.by <- lapply(X = data.by.clean, FUN = function(x){return(apply(X = x, MARGIN = 2, FUN = get.density))})
    
    # Obtain maximum densities
    max.density.by <- lapply(X = lapply(X = density.by, FUN = colMax, as.df=TRUE), FUN = as.data.frame)
    
    # Bind
    max.density.by <- data.table::rbindlist(max.density.by, fill = TRUE,  use.names=TRUE)
    
    # Give names
    # colnames(max.density.by) <- sort(unique(by))
    
    # Add group
    df.max.density.by <- cbind.data.frame("group" = names(density.by), max.density.by)
    
  } else {
    
    df.max.density.by <- NULL
  }
  
  return(list("xlim" = x.limits,
              "density" = df.max.density,
              "densityby" = df.max.density.by))
  
  
}
###############################################################################
#################### COLORS
###############################################################################

# Define color palette
colors_palette <- c(
  # COLOR-BLIND FRIENDLY: scales::show_col(rcartocolor::carto_pal(12, "Safe"))
  "#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499",
  "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888",
  
  # OLD MASTER
  "#3C5488FF","#E64B35FF", "#00A087FF", "#00A1D5FF", "#FFCD00FF",
  "#8491B4FF", "#E377C2FF", "#FF7F0EFF", "#9467BDFF", "#F39B7FFF",
  "#79AF97FF", "#B24745FF", "#374E55FF", "#00FFFFFF", "#80796BFF",
  "#91D1C2FF", "#BCBD22FF", "#00FF00FF","#2CA02CFF", "#FFFF00FF")


# palette_OkabeIto <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", 
#                       "#0072B2", "#D55E00", "#CC79A7", "#999999")
# scales::show_col(palette_OkabeIto)
# scales::show_col(colors_palette)
# Show colors
# scales::show_col(colors_palette)
# scales::show_col(pal_npg()(20))
# scales::show_col(pal_aaas()(20))
# scales::show_col(pal_nejm()(20))
# scales::show_col(pal_lancet()(20))
# scales::show_col(pal_jama()(20))
# scales::show_col(pal_ucscgb()(20))
# scales::show_col(pal_d3()(20))
# scales::show_col(pal_locuszoom()(20))
# scales::show_col(pal_uchicago()(20))
# scales::show_col(pal_startrek()(20))

################################################################################
#################### GGPLOT
################################################################################

#extract legend
#https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
extract.legend.ggplot <- function(gg){
  tmp <- ggplot_gtable(ggplot_build(gg))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
  }




################################################################################
#################### GGPLOT SETTINGS
################################################################################

## GG settings
size_geom_point <- 3
size_geom_line <- 2
size_geom_line_helper <- 1.5
alpha_ribbon <- 0.2
size_of_legend_key <- 8
num_axis_breaks_y <- 5
num_axis_breaks_x <- 5  

# Edit text size
base_size_text <- 32

# Size of plots
px <- 500

# Height and width
height_opt <- 10
width_opt <- 10

# Saving format
save_format <- cairo_ps # Use "eps" (or cairo_ps for transparent figures)

# Manual settings
theme_manual_settings <- "theme(text=element_text(size=base_size_text, family = 'Times'),
                               legend.position = 'top',
                               legend.spacing.x = unit(0.25, 'cm'),
                               legend.text = element_text(margin = margin(r = 40, unit = 'pt')),
                               plot.margin = unit(c(1.5,1.5,1.5,1.5), 'cm'))"



################################################################################
#################### OLD FUNCTIONS
################################################################################
#' 
#' #' Replace all values with NA where a certain condition is met
#' #'
#' #' This function takes a dataframe and replaces all values that meet the
#' #'   condition specified as an NA value, following a special syntax.
#' #'
#' #' @param data A dataframe
#' #' @param condition A condition required to be TRUE to set NA. Here, the condition
#' #'   is specified with a formula, following the syntax: `~.x {condition}`.
#' #'   For example, writing `~.x < 20` would mean "where a variable value is less
#' #'   than 20, replace with NA".
#' #'
#' #' @examples
#' 
#' #' dat_ms <- tibble::tribble(~x,  ~y,    ~z,
#' #'                           1,   "A",   -100,
#' #'                           3,   "N/A", -99,
#' #'                           NA,  NA,    -98,
#' #'                           -99, "E",   -101,
#' #'                           -98, "F",   -1)
#' #'
#' #' dat_ms
#' 
#' #' #replace all instances of -99 with NA
#' #' replace_with_na_all(data = dat_ms,
#' #'                     condition = ~.x == -99)
#' #'
#' #' # replace all instances of -99 or -98, or "N/A" with NA
#' #' replace_with_na_all(dat_ms,
#' #'                     condition = ~.x %in% c(-99, -98, "N/A"))
#' 
#' #' # replace all instances of common na strings
#' #' replace_with_na_all(dat_ms,
#' #'                     condition = ~.x %in% common_na_strings)
#' #'
#' #' # where works with functions
#' #' replace_with_na_all(airquality, ~ sqrt(.x) < 5)
#' #'
#' #' @export
#' replace_with_na_all <- function(data, condition) {
#'   purrr::map_dfc(data, ~ na_set(.x, condition) )
#' }
#' 
#' # Future work
#' # replace_with_na_all(airquality, . < 20)
#' # replace_with_na_all(airquality, x < 20)
#' # replace_with_na_all(airquality, function(x) mean(x) < 20)
#' 
#' #' Replace specified variables with NA where a certain condition is met
#' #'
#' #' @param data dataframe
#' #' @param .vars A character string of variables to replace with NA values
#' #' @param condition A condition required to be TRUE to set NA. Here, the condition
#' #'   is specified with a formula, following the syntax: `~.x {condition}`.
#' #'   For example, writing `~.x < 20` would mean "where a variable value is less
#' #'   than 20, replace with NA".
#' #'
#' #' @return a dataframe
#' #'
#' #' @export
#' #'
#' #' @examples
#' #'
#' #' dat_ms <- tibble::tribble(~x,  ~y,    ~z,
#' #'                           1,   "A",   -100,
#' #'                           3,   "N/A", -99,
#' #'                           NA,  NA,    -98,
#' #'                           -99, "E",   -101,
#' #'                           -98, "F",   -1)
#' #'
#' #' dat_ms
#' #'
#' #' replace_with_na_at(data = dat_ms,
#' #'                  .vars = "x",
#' #'                  condition = ~.x == -99)
#' #'
#' #' replace_with_na_at(data = dat_ms,
#' #'                  .vars = c("x","z"),
#' #'                  condition = ~.x == -99)
#' #'
#' #' # replace using values in common_na_strings
#' #' replace_with_na_at(data = dat_ms,
#' #'                  .vars = c("x","z"),
#' #'                  condition = ~.x %in% common_na_strings)
#' #'
#' #'
#' replace_with_na_at <- function(data, .vars, condition) {
#'   test_if_dataframe(data)
#'   test_if_null(data)
#'   test_if_missing(data)
#'   purrr::modify_at(data, .vars, ~ na_set(.x, condition))
#' }
#' 
#' 
#' #' Replace values with NA based on some condition, for variables that meet some predicate
#' #'
#' #' @param data Dataframe
#' #' @param .predicate A predicate function to be applied to the columns or a
#' #'   logical vector.
#' #' @param condition A condition required to be TRUE to set NA. Here, the condition
#' #'   is specified with a formula, following the syntax: `~.x {condition}`.
#' #'   For example, writing `~.x < 20` would mean "where a variable value is less
#' #'   than 20, replace with NA".
#' #'
#' #' @return Dataframe
#' #' @export
#' #'
#' #' @examples
#' #'
#' #' dat_ms <- tibble::tribble(~x,  ~y,    ~z,
#' #'                           1,   "A",   -100,
#' #'                           3,   "N/A", -99,
#' #'                           NA,  NA,    -98,
#' #'                           -99, "E",   -101,
#' #'                           -98, "F",   -1)
#' #'
#' #' dat_ms
#' #'
#' #' replace_with_na_if(data = dat_ms,
#' #'                  .predicate = is.character,
#' #'                  condition = ~.x == "N/A")
#' 
#' #' replace_with_na_if(data = dat_ms,
#' #'                    .predicate = is.character,
#' #'                    condition = ~.x %in% common_na_strings)
#' #'
#' #' replace_with_na(dat_ms,
#' #'               to_na = list(x = c(-99, -98),
#' #'                            y = c("N/A"),
#' #'                            z = c(-101)))
#' #'
#' #'
#' replace_with_na_if <- function(data, .predicate, condition) {
#'   test_if_dataframe(data)
#'   test_if_null(data)
#'   test_if_missing(data)
#'   purrr::modify_if(data, .predicate, ~ na_set(.x, condition))
#' }
#' 
#' # utility funs for replace_with_na_*  ------------------------------------------
#' 
#' create_mapper_na <- function(condition){
#'   glue::glue("~ {rlang::f_text(condition)} & !is.na(.x)") %>%
#'     stats::as.formula() %>%
#'     purrr::as_mapper()
#' }
#' 
#' na_set <- function(vec, condition) {
#'   # modify this vector with this function, return NA
#'   purrr::modify_if(vec, create_mapper_na(condition) , ~ NA) %>%
#'     # flatten this out into a regular vector
#'     purrr::reduce(c)
#'   
#'   # na_set(aq_small$Ozone, ~ .x < 20)
#' }
#' 
#' 
#' # # Obtain legend
#' # get_legend<-function(myggplot){
#' #   tmp <- ggplot_gtable(ggplot_build(myggplot))
#' #   leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
#' #   legend <- tmp$grobs[[leg]]
#' #   return(legend)
#' # }
#' 
