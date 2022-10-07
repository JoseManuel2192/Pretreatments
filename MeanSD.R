# Return mean ± SD of a X matrix in function of Y 

MeanSD = function(X, Y, decimal, excel){
  require(memisc)
  factors = levels(as.factor(Y))
  resultsMean = resultsSD = results = matrix(ncol = ncol(X), nrow = length(factors))
  #' Calculate mean and SD
  for (i in 1:length(factors)) {
    XFactor = X[which(Y == factors[i]),]
    resultsMean[i,] = apply(XFactor, 2, function(X){
      mean(X)
    })
    resultsSD[i,] = apply(XFactor, 2, function(X){
      sd(X)
    })
  }
  
  #' Integrate mean ± SD
  rownames(results) = factors; colnames(results) = colnames(X)
  require(memisc)
  
  for (j in 1:ncol(results)) {
    absX = min(abs(resultsMean[,j]))
    for (i in 1:nrow(results)) {
      if (is.numeric(decimal)) {
        decimals = decimal
      }else{
        decimals = memisc::cases(
          absX <= 0.0001  -> 5,
          absX <= 0.001 & absX > 0.0001  -> 4,
          absX <= 0.08 & absX > 0.001 -> 3, 
          absX <= 2 & absX > 0.01 -> 2, 
          absX <= 15 & absX > 2 -> 1, 
          absX >= 15  -> 0 
        )
      }
      results[i,j] = paste(format(round(resultsMean[i,j],decimals), nsmall = decimals),
                           format( round(resultsSD[i,j],decimals), nsmall = decimals), 
                           sep = " ± ")
    }
  }
  if (exists("excel") & excel == TRUE) {
    require(xlsx)
    write.xlsx(results, "MeanSD.xlsx")
  }
  return(results)
}