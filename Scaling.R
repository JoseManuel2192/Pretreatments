scaleMethod = function(X, method = "autoscaling"){
  if (method == "autoscaling") {
    Xscaled = scale(X)}
  
  if (method == "paretoscaling") {
      Xcentered = apply(X, 2, function(X) X - mean(X))
      Xscaled = apply(Xcentered, 2, function(X) X/sqrt(sd(X)))
  }
  
  if (method == "vastscaling") {
      Xcentered = apply(X, 2, function(X) X - mean(X))
      Xscaled = apply(Xcentered, 2, function(X) X*mean(X)/(sd(X))^2)
  }
  return(Xscaled)
}






