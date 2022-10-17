#' Title impute
#' This functions contains different imputation methods and imputes the data with all
#' the different imputation methods 
#' @param data data matrix with simulated data
#' @param methods vector containing the method names 
#'
#' @return the matrix containing the imputed data
#' @export
#'
#' @examples 
#' imputed_data <- impute(data=miss_data, methods=imputation_methods)
#' #'####################################################

impute <- function(data, methods) {
  
  require(missForest)
  require(pcaMethods)
  require(impute)
  # require(PEMM)
  
  if (length(methods) != 1 & length(methods) != ncol(data)) {
    stop("Methods needs to be either one value or of the same length as number of columns in data.")
  }
  imputed_data <- matrix(NA,nrow = nrow(data),ncol = ncol(data))
  results_data <- data
  
  if (length(methods) == 1) {
    methods <- rep(methods, times=ncol(data))
  }
  
  if ("RF" %in% methods) {
    imputed_data <- missForest::missForest(xmis = data,maxiter = 10,verbose = TRUE)$ximp
    index <- which(methods == "RF")
    results_data[,index] <- imputed_data[,index]
    
  }
  
  if ("PPCA" %in% methods) {
    # Do cross validation with ppca for component 2:10
    esti <- kEstimate(Matrix= data, method = "ppca", evalPcs = 2:10, nruncv=1, em="nrmsep")
    # The best result was obtained for this number of PCs:esti$bestNPcs
    pc <- pcaMethods::pca(object = data,nPcs=esti$bestNPcs, method="ppca")
    #index <- which(methods == "PPCA")
    imputed_data <- completeObs(pc)
    
    index <- which(methods == "PPCA")
    results_data[,index] <- imputed_data[,index]
    
  }
  if("LLS" %in% methods ){
    # local least squares imputation
    lls_esti <- pcaMethods::llsImpute(Matrix = data, k = 10, center = TRUE, completeObs = TRUE, correlation = "pearson", allVariables = TRUE, maxSteps = 100)
    imputed_data <- completeObs(lls_esti)
    
    index <- which(methods == "LLS")
    results_data[,index] <- imputed_data[,index]
    
    
  } 
  
  if("svdImpute" %in% methods ){
    # Singular Value Decomposition
    svd_esti <- pcaMethods::pca(object = data, method="svdImpute", nPcs=10, center = FALSE)
    imputed_data <- completeObs(svd_esti)
    
    index <- which(methods == "svdImpute")
    results_data[,index] <- imputed_data[,index]
    
  } 
  
  if("KNNImpute" %in% methods ){
    # K-Nearest neighboors
    KNN_esti <- impute::impute.knn(data = data)
    imputed_data <- KNN_esti$data
    
    index <- which(methods == "KNNImpute")
    results_data[,index] <- imputed_data[,index]
    
  } 
  # if ("EM" %in% methods){
  #   # expectation minimazation algorithm
  #   EM_esti = PEMM::PEMM_fun(X= data, phi=1)
  #   imputed_data <- EM_esti$Xhat
  #   
  #   index <- which(methods == "EM")
  #   results_data[,index] <- imputed_data[,index]
  #   
  #   
  # }
  if ("BPCA" %in% methods){
    # bayesian principal component analysis
    pc <-pcaMethods:: pca(object = data, method="bpca", nPcs=10)
    
    ## Get the estimated complete observations
    imputed_data <- completeObs(pc)
    
    index <- which(methods == "BPCA")
    results_data[,index] <- imputed_data[,index]
    
  }
  if ("min" %in% methods) {
    imputed_data <- data
    foreach (data_column=which(methods == "min")) %do% {
      method <- methods[data_column]
      impu_value <- min(data[,data_column], na.rm=TRUE)
      results_data[is.na(imputed_data[,data_column]), data_column] <- impu_value
    }
    
  } 
  if ("halfmin" %in% methods) {
    imputed_data <- data
    foreach (data_column=which(methods == "halfmin")) %do% {
      method <- methods[data_column]
      impu_value <- (min(data[,data_column], na.rm=TRUE)/2)
      results_data[is.na(imputed_data[,data_column]), data_column] <- impu_value
    }
    
  } 
  
  if ("mean" %in% methods){
    imputed_data <- data
    foreach (data_column=which(methods == "mean")) %do% {
      method <- methods[data_column]
      impu_value <- round(mean(data[,data_column], na.rm=TRUE),digits = 2)
      results_data[is.na(imputed_data[,data_column]), data_column] <- impu_value
    }
    
  }
  if ("zero" %in% methods){
    imputed_data <- data
    foreach (data_column=which(methods == "zero")) %do% {
      method <- methods[data_column]
      impu_value <- 0
      results_data[is.na(imputed_data[,data_column]), data_column] <- impu_value
    }
    
  }
  if ("EX" %in% methods){
    
    
    index <- which(methods == "EX")
    results_data[,index] <-  data[,index]    
  }
  
  if ("NONE" %in% methods){
    
    
    index <- which(methods == "NONE")
    results_data[,index] <-  data[,index]
    
  }
  
  
  results_data
}
