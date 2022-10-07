#' Automatic Box-Cox transformations

#############################
boxcoxAndCorr = function(X, Y, step.second, perfboxcox, min.lambda, max.lambda){
  library(flowClust)
  library(car)
  if (perfboxcox == T) {
    Y_homoc_trans = as.factor(Y)
    final.levene = c()
    final.lambda = c()
    X.transf = matrix(ncol = ncol(X), nrow = nrow(X))
    for (var.iter in 1:ncol(X)){
      var.select = X[,var.iter]
      #Valores iniciales para comenzar la iteracion
      min.lambda = min.lambda
      max.lambda = max.lambda
      step = 0.212
      
      for(optimize.lambda in 1:2){
        lambda = seq(min.lambda,max.lambda,step) #EVITAR QUE lambda = 0
        
        #Chekeo homocedasticidad
        Levene = c()
        for(i in lambda){
          boxcox = flowClust::box(var.select,i)
          X_homoc_trans = as.matrix(boxcox)
          Y_levene_trans = as.factor(Y_homoc_trans) #Tengo que convertirla en factor porque si no, da error
          
          ## Kolmogorov-Smirnoff (p-value > 0.05 para descartar no-normalidad)
          if (length(levels(as.factor(X_homoc_trans))) < c(length(Y)/4)) {
            X_homoc_trans = X_homoc_trans * rnorm(length(Y), mean = 1, sd = 0.0001)
          }
          Levene = c(Levene, apply(X_homoc_trans, 2, function(X_homoc_trans){
            all_ks = ks.test(X_homoc_trans,pnorm,mean(X_homoc_trans), sd(X_homoc_trans))
            # print(all_ks)
            p_value = all_ks$p.value
          }))
          # 
          # Levene = c(Levene, apply(X_homoc_trans, 2, function(X_homoc_trans){
          #   test.levene.trans = leveneTest(X_homoc_trans ~ Y_levene_trans)
          #   # print(test.levene)
          #   p_value_Bartlett = test.levene.trans$`Pr(>F)`[1]
          # }))
        }
        # plot(Levene~lambda)
        # title(c("p-Levene vs lambda"))
        
        Levene[is.na(Levene)] = 0  #Cambio NAs por 0
        max.position = which(Levene == max(Levene))
        max.position = max.position[1]
        min.lambda = lambda[max.position] - abs(lambda[max.position])*0.2
        max.lambda = lambda[max.position] + abs(lambda[max.position])*0.2
        step = step.second
        if(optimize.lambda == 2){
          final.levene[var.iter] = max(Levene)
          final.lambda[var.iter] = lambda[max.position]
          X.transf[,var.iter] = as.matrix(flowClust::box(var.select,final.lambda[var.iter]))
          print(c(var.iter,"/", ncol(X)))
        }
      }
    }
  }else{
    X.transf = X
  }
  
  ## Shapiro Wilk test (p-value > 0.05 para descartar no-normalidad)
  shapiro_test = apply(X.transf, 2, function(X.transf){
    shapiro = shapiro.test(X.transf)
    # print(shapiro)
    round(shapiro$p.value, 6)
  })
  shapiro_test
  
  ## Kolmogorov-Smirnoff (p-value > 0.05 para descartar no-normalidad)
  ks_test = apply(X.transf, 2, function(X.transf){
    all_ks = ks.test(X.transf,pnorm,mean(X.transf), sd(X.transf))
    # print(all_ks)
    p_value = all_ks$p.value
  })
  round(ks_test,3)
  
  Xnew = X
  Xnew[,] = X.transf
  Pearson = cor(Xnew, method = "pearson")
  Pearson = format(round(Pearson,3),3)
  for (i in 1:c(ncol(Pearson)-1)) {
    Pearson[i, c((i+1):ncol(Pearson))] = rep("",c(ncol(Pearson)-(i)))
  }
  
  
  Spearman = cor(Xnew, method = "spearman")
  Spearman = format(round(Spearman,3),3)
  for (i in 1:c(ncol(Spearman)-1)) {
    Spearman[i, c((i+1):ncol(Spearman))] = rep("",c(ncol(Spearman)-(i)))
  }
  
  if (perfboxcox == T) {
    results = list("X.transf" = X.transf, "final.lambda" = final.lambda,
                   "Kolmogorov" = ks_test, "Shapiro" = shapiro_test,
                   "Spearman" = Spearman, "Pearson" = Pearson)
    return(results)
  }else{
    results = list("Spearman" = Spearman, "Pearson" = Pearson)
    return(results)
  }
  
}

