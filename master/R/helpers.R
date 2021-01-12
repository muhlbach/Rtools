###############################################################################
#################### STANDARD FUNCTIONS
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

multiply <- function(x, c){x * c}

###############################################################################
#################### ML FUNCTIONS
###############################################################################
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


get.beta <- function(Y, X, constant = FALSE){
  
  if (constant) {
    # Add ones
    X <- cbind(1,X)
  }
  
  # Estimate betaa
  beta <- solve(t(X) %*% X) %*% (t(X) %*% Y)
  
  return(beta)
  
}



MLmodels <- function(Y, X, newdata = NULL, model = "randomforests", params = NULL, return.model = FALSE, cv = FALSE, parallel = TRUE){
  
  # --------------------
  # SETUP
  # --------------------
  # Set seed
  set.seed(1991)
  
  # Libraries
  stopifnot(require(ranger))
  stopifnot(require(xgboost))
  stopifnot(require(glmnet))
  stopifnot(require(caret))
  stopifnot(require(hdm))
  stopifnot(require(parallel))
  
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
    
    # Determine the function call (assuming the package is loaded)
    fnc_call <- "ranger"
    
    # Specify data parameters
    params_data <- list("formula" = "Y ~ .",
                        "data" = data.frame("Y" = Y, "X" = X))
    
    # Required parameters
    params_required <- list()
    
    # Define call used to make predictions
    predict_call <- "predict(object = f, data.frame('X' = newdata))$predictions"
    
    
    ## CV
    # Specific CV parameters
    par_mtry <- floor(seq(from = 0.1, to = 0.9, length.out = 5) * p)
    par_mtry <- par_mtry[par_mtry > 1 & par_mtry <= 20]
    
    # Specify CV parameters
    params_cv <- expand.grid(mtry = par_mtry,                                   # default: sqrt(p)
                             splitrule  ="variance",                            # default: "variance", 
                             min.node.size = seq(from = 5, to = 25, length.out = 3)) # default: 5
    
    # Extra model-specific parameters
    params_extra_cv <- list("x" = data.frame("X" = X),
                            num.trees = NA, oob.error = NA, num.threads = 1)
    
    # Determine the function call used to CV (assuming the package is loaded)
    fnc_call_cv <- "ranger"
    
    # Define call used to make predictions
    predict_call_cv <- "predict(object = f, data.frame('X' = newdata))"
    
  } else if (model == "boosting") {
    # ----------
    # BOOSTING / BOOSTED TREES
    # ----------
    
    # Determine the function call (assuming the package is loaded)
    fnc_call <- "xgboost"
    
    # Specify data parameters
    params_data <- list("data" = xgb.DMatrix(data = X, label = Y))
    
    # Required parameters
    params_required <- list(booster = "gbtree",
                            objective = "reg:squarederror")
    
    # Define call used to make predictions
    predict_call <- "predict(object = f, newdata)"
    
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
    params_extra_cv <- list("x" = xgb.DMatrix(as.matrix(X)),
                            "objective" = "reg:squarederror",
                            nthread = 1)
    
    # Give colnames (caret needs x to have colnames!)
    colnames(params_extra_cv$x) <- paste0("X",1:p)
    
    
    # Determine the function call used to CV (assuming the package is loaded)
    fnc_call_cv <- "xgbTree"
    
    # Define call used to make predictions
    predict_call_cv <- "predict(object = f, xgb.DMatrix(newdata))"
    
    
  } else if (model == "lasso") {
    # ----------
    # LASSO
    # ----------
    
    # Determine the function call (assuming the package is loaded)
    fnc_call <- "glmnet"
    
    # Specify data parameters
    params_data <- list("x" = X,
                        "y" = Y)
    
    # Required parameters 
    params_required <- list("family"="gaussian")
    
    # Define call used to make predictions
    predict_call <- "rowMeans(predict(object = f, newdata))"
    
    ## CV
    # Specific CV parameters
    alpha_par <- c(0.5, 1)
    
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
    params_extra_cv <- list("x" = X,
                            "family" = "gaussian")
    
    # Give colnames (caret needs x to have colnames!)
    colnames(params_extra_cv$x) <- paste0("X",1:p)
    
    # Determine the function call used to CV (assuming the package is loaded)
    fnc_call_cv <- "glmnet"
    
    # Define call used to make predictions
    predict_call_cv <- "predict(object = f, data.frame(newdata))"
    
    
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
    
    # Update parameters
    params_extra_cv <- modifyList(x = params_extra_cv, val = params[intersect(names(params_extra_cv), names(params))])
    
    # Add some default CV parameters after having speficied the model parameters
    params_model_cv <- c(list("method" = fnc_call_cv,
                              "tuneGrid" = params_cv),
                         params_extra_cv)
    
    # Merge
    params_final <- modifyList(x = params_default_cv, val = params_model_cv)
    
    # Set seed
    set.seed(1991)
    
    if(parallel){
      # Start cluster
      cl <- makeForkCluster(nnodes = detectCores())
      registerDoParallel(cl)  
    }
    
    # Estimate model, f (suppress warning because XGB and GLMNET give warnings)
    suppressWarnings(f <- DescTools::DoCall(what = caret::train, args = params_final))
    
    if(parallel){
      # Stop cluster
      stopCluster(cl)
    }
    
    # Predict
    yhat <- NULL
    if (!is.null(newdata)) {
      yhat <- as.numeric(eval(parse(text = predict_call_cv)))
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


###############################################################################
#################### COMPUTATION
###############################################################################

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

# Function to raise element to powers
raise.to.power <- function(x,p){`^`(x,p)}

# Function to compute the 0-1 loss
get.zero.loss.group <- function(Y.group=NULL, Y.hat, Y=NULL, num.groups=3, by.group = NULL){
  
  # Handle missing observations
  idx.missing <- is.na(Y.hat)
  
  if (!is.null(Y.group)) {
    # Everything is great, continue
  } else if (!is.null(Y)) {
    Y.group <- as.numeric(Hmisc::cut2(Y, g=num.groups)) 
  } else {
    stop("Please provide either 'Y.group' or 'Y'")
  }
  
  ## Compute loss for all
  # Subset
  Y.group <- Y.group[!idx.missing]
  Y.hat <- Y.hat[!idx.missing]
  
  # Compute group based on predictions
  Y.hat.group <- as.numeric(Hmisc::cut2(Y.hat, g=num.groups)) 
  
  # Compute 0-1 loss
  loss.01 <- mean(Y.group != Y.hat.group)
  
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
      idx <- idx[!idx.missing]
      
      # Compute 0-1 loss
      loss.01_g <- mean(Y.group[idx] != Y.hat.group[idx])
      
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


# Evaluate performance
evaluate_performance <- function(observed, predicted){
  
  # Compute squared error
  squared_error <- (observed-predicted)^2
  
  # MSE
  mse <- mean(squared_error, na.rm = TRUE)
  rmse <- sqrt(mse)
  
  results <- list("MSE" = mse,
                  "RMSE" = rmse)
  
  return(results)
  
}


# Capitalize first letter and leave the rest
firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  return(x)
}


# Add % to elements
to.percent <- function(x, digits = 2){
  
  x <- x %>%
    # Multiply by 100
    mutate_if(is.numeric, multiply, c = 100) %>%
    
    # Round off
    mutate_if(is.numeric, round, digits = digits) %>%
    
    # Insert %
    mutate_if(is.numeric, paste0, "\\%")
  
  return(x)
  
}










#  Obtain density
get.density <- function(x, na.rm = TRUE){return(x = density(x, na.rm = na.rm)$y)}

#' Replace all values with NA where a certain condition is met
#'
#' This function takes a dataframe and replaces all values that meet the
#'   condition specified as an NA value, following a special syntax.
#'
#' @param data A dataframe
#' @param condition A condition required to be TRUE to set NA. Here, the condition
#'   is specified with a formula, following the syntax: `~.x {condition}`.
#'   For example, writing `~.x < 20` would mean "where a variable value is less
#'   than 20, replace with NA".
#'
#' @examples

#' dat_ms <- tibble::tribble(~x,  ~y,    ~z,
#'                           1,   "A",   -100,
#'                           3,   "N/A", -99,
#'                           NA,  NA,    -98,
#'                           -99, "E",   -101,
#'                           -98, "F",   -1)
#'
#' dat_ms

#' #replace all instances of -99 with NA
#' replace_with_na_all(data = dat_ms,
#'                     condition = ~.x == -99)
#'
#' # replace all instances of -99 or -98, or "N/A" with NA
#' replace_with_na_all(dat_ms,
#'                     condition = ~.x %in% c(-99, -98, "N/A"))

#' # replace all instances of common na strings
#' replace_with_na_all(dat_ms,
#'                     condition = ~.x %in% common_na_strings)
#'
#' # where works with functions
#' replace_with_na_all(airquality, ~ sqrt(.x) < 5)
#'
#' @export
replace_with_na_all <- function(data, condition) {
  purrr::map_dfc(data, ~ na_set(.x, condition) )
}

# Future work
# replace_with_na_all(airquality, . < 20)
# replace_with_na_all(airquality, x < 20)
# replace_with_na_all(airquality, function(x) mean(x) < 20)

#' Replace specified variables with NA where a certain condition is met
#'
#' @param data dataframe
#' @param .vars A character string of variables to replace with NA values
#' @param condition A condition required to be TRUE to set NA. Here, the condition
#'   is specified with a formula, following the syntax: `~.x {condition}`.
#'   For example, writing `~.x < 20` would mean "where a variable value is less
#'   than 20, replace with NA".
#'
#' @return a dataframe
#'
#' @export
#'
#' @examples
#'
#' dat_ms <- tibble::tribble(~x,  ~y,    ~z,
#'                           1,   "A",   -100,
#'                           3,   "N/A", -99,
#'                           NA,  NA,    -98,
#'                           -99, "E",   -101,
#'                           -98, "F",   -1)
#'
#' dat_ms
#'
#' replace_with_na_at(data = dat_ms,
#'                  .vars = "x",
#'                  condition = ~.x == -99)
#'
#' replace_with_na_at(data = dat_ms,
#'                  .vars = c("x","z"),
#'                  condition = ~.x == -99)
#'
#' # replace using values in common_na_strings
#' replace_with_na_at(data = dat_ms,
#'                  .vars = c("x","z"),
#'                  condition = ~.x %in% common_na_strings)
#'
#'
replace_with_na_at <- function(data, .vars, condition) {
  test_if_dataframe(data)
  test_if_null(data)
  test_if_missing(data)
  purrr::modify_at(data, .vars, ~ na_set(.x, condition))
}


#' Replace values with NA based on some condition, for variables that meet some predicate
#'
#' @param data Dataframe
#' @param .predicate A predicate function to be applied to the columns or a
#'   logical vector.
#' @param condition A condition required to be TRUE to set NA. Here, the condition
#'   is specified with a formula, following the syntax: `~.x {condition}`.
#'   For example, writing `~.x < 20` would mean "where a variable value is less
#'   than 20, replace with NA".
#'
#' @return Dataframe
#' @export
#'
#' @examples
#'
#' dat_ms <- tibble::tribble(~x,  ~y,    ~z,
#'                           1,   "A",   -100,
#'                           3,   "N/A", -99,
#'                           NA,  NA,    -98,
#'                           -99, "E",   -101,
#'                           -98, "F",   -1)
#'
#' dat_ms
#'
#' replace_with_na_if(data = dat_ms,
#'                  .predicate = is.character,
#'                  condition = ~.x == "N/A")

#' replace_with_na_if(data = dat_ms,
#'                    .predicate = is.character,
#'                    condition = ~.x %in% common_na_strings)
#'
#' replace_with_na(dat_ms,
#'               to_na = list(x = c(-99, -98),
#'                            y = c("N/A"),
#'                            z = c(-101)))
#'
#'
replace_with_na_if <- function(data, .predicate, condition) {
  test_if_dataframe(data)
  test_if_null(data)
  test_if_missing(data)
  purrr::modify_if(data, .predicate, ~ na_set(.x, condition))
}

# utility funs for replace_with_na_*  ------------------------------------------

create_mapper_na <- function(condition){
  glue::glue("~ {rlang::f_text(condition)} & !is.na(.x)") %>%
    stats::as.formula() %>%
    purrr::as_mapper()
}

na_set <- function(vec, condition) {
  # modify this vector with this function, return NA
  purrr::modify_if(vec, create_mapper_na(condition) , ~ NA) %>%
    # flatten this out into a regular vector
    purrr::reduce(c)
  
  # na_set(aq_small$Ozone, ~ .x < 20)
}




###############################################################################
#################### COLORS
###############################################################################

# Define color palette
colors_palette <- c(
  # MASTER
  "#3C5488FF","#E64B35FF", "#00A087FF", "#00A1D5FF", "#FFCD00FF",
  "#8491B4FF", "#E377C2FF", "#FF7F0EFF", "#9467BDFF", "#F39B7FFF",
  "#79AF97FF", "#B24745FF", "#374E55FF", "#00FFFFFF", "#80796BFF",
  "#91D1C2FF", "#BCBD22FF", "#00FF00FF","#2CA02CFF", "#FFFF00FF")

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

###############################################################################
#################### GGPLOT
###############################################################################

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
theme_manual_settings <- "theme(text=element_text(size=base_size_text, family = 'Times New Roman'),
                               legend.position = 'top',
                               legend.spacing.x = unit(0.25, 'cm'),
                               legend.text = element_text(margin = margin(r = 40, unit = 'pt')),
                               plot.margin = unit(c(1.5,1.5,1.5,1.5), 'cm'))"


