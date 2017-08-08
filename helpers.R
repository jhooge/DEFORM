library(caret)
# library(Rtsne) # Barnes-Hut implementation of t-Distributed Stochastic Neighbor Embedding


pcaTransform <- function(df) {
  preProc      <- preProcess(df, method=c("pca", "center", "scale"))
  transformed  <- predict(preProc, df)
  return(transformed) 
}

icaTransform <- function(df, n.comp=10) {
  preProc      <- preProcess(df, method=c("ica", "center", "scale"), n.comp=n.comp)
  transformed  <- predict(preProc, df)
  return(transformed) 
}

# tsneTransform <- function(df, perplexity=30) {
#   preProc      <- preProcess(df, method=c("knnImpute"))
#   imputed      <- unique(predict(preProc, df)) ## Impute missing values and remove duplicates
#   transformed  <- Rtsne(imputed, k=3, initial_dims=ncol(imputed), perplexity)
#   
#   return(transformed) 
# }

#' Copy arguments into env and re-bind any function's lexical scope to bindTargetEnv .
#' 
#' See http://winvector.github.io/Parallel/PExample.html for example use.
#' 
#' 
#' Used to send data along with a function in situations such as parallel execution 
#' (when the global environment would not be available).  Typically called within 
#' a function that constructs the worker function to pass to the parallel processes
#' (so we have a nice lexical closure to work with).
#' 
#' @param bindTargetEnv environment to bind to
#' @param objNames additional names to lookup in parent environment and bind
#' @param names of functions to NOT rebind the lexical environments of
bindToEnv <- function(bindTargetEnv=parent.frame(), objNames, doNotRebind=c()) {
  # Bind the values into environment
  # and switch any functions to this environment!
  for(var in objNames) {
    val <- get(var, envir=parent.frame())
    if(is.function(val) && (!(var %in% doNotRebind))) {
      # replace function's lexical environment with our target (DANGEROUS)
      environment(val) <- bindTargetEnv
    }
    # assign object to target environment, only after any possible alteration
    assign(var, val, envir=bindTargetEnv)
  }
}

startCluster <- function(cores=detectCores()) {
  cluster <- makeCluster(cores)
  return(cluster)
}

shutDownCluster <- function(cluster) {
  if(!is.null(cluster)) {
    stopCluster(cluster)
    cluster <- c()
  }
}
