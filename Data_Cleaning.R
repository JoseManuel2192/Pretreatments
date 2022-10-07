############################################################################
### Metabolite and sample filtering
############################################################################
#' Elimina variables o muestras que tienen mas de un X% de NAs. Si deja una clase entera con todo 0, le asigna el valor minimo/2
#' ya que consideramos que esos datos son 0 porque estan por debajo del LOD y no porque se han integrado mal

Data.cleaning = function(X, Y, data, missingValueFeat, missingValueSamp){


  # Elimino datos negativos y convierto en 0 los NAs
  # Elimino datos negativos y convierto en 0 los NAs
  MV = function(X){
    X[which(is.na(X))] = 0
    X[which(X<0)] = 0
    X
  }
  X = MV(X)
  data.samples.removed = data
  removenumber = 0
  missingFeaturesa = 0
  removeFeatures = c()
  for (j in 1:ncol(X)){
    
    # % missing values in features
    missingFeatures = length(which(X[,j] == 0))/length(Y)*100
    if (missingFeatures > missingValueFeat){
      removeFeatures = c(removeFeatures,j)
    }
  }
  # missingFeaturesa # El % de missing mas alto. Descomentar para ver
  if (is.null(removeFeatures)){
    NremoveFeatures = 0
  } else{
    NremoveFeatures = length(removeFeatures)
    removed.features = colnames(X)[removeFeatures]
    X = X[,-removeFeatures]
    data.samples.removed = data.samples.removed[,-c(removeFeatures + ncategorical)]
  }
  removeFeatures = c() # Por seguridad elimino este vector para no volver a quitar features

  # Missing values in samples
  removed.features = 0
  removed.samples = 0
  # SAMPLES con un % de missing value mayor que el especificado
  removenumber = 0
  missingSamplea = 0
  removeSamples = c()
  for (j in 1:nrow(X)){
    missingSamples = length(which(X[j,] == 0))/ncol(X)*100
    if (missingSamples > missingValueSamp){
      removeSamples = c(removeSamples,j)
    }
  }
  
  if (is.null(removeSamples)){
    NremoveSamples = 0
  } else{
    removed.samples = Y[removeSamples]
    NremoveSamples = length(removeSamples)
    X = X[-removeSamples,]; drop(X)
    Y = Y[-removeSamples]; drop(Y)
    data.samples.removed = data[-removeSamples,]
  }
  removeSamples = c() #Elimino por seguridad
  
  #Asigno valor a las FEATURES que son todos 0 en una clase: mitad del minimo valor de la matriz
  a = 0
  positionmissing = 0
  if (NremoveSamples == 0){removeSamples = 5^5555} #Para que no me elimine ninguna
  Yfactors = Y
  levels=levels(Yfactors)
  minvalue = min(X[X>0])/2
  for (i in 1:length(levels)){
    for (j in 1:dim(X)[2]){
      
      missing = X[which(Yfactors==levels[i]),j]
      if (sum(missing) == 0){
        X[which(Yfactors==levels[i]),j] =  missing + minvalue
        a = a + 1
        positionmissing = c(positionmissing, j)
      }else{}
    }
  }
  
  print(c("NremoveFeatures",NremoveFeatures, "NremoveSamples", NremoveSamples))
  ifelse(NremoveFeatures > 0, print(removed.features),"Any feature removed")
  ifelse(NremoveSamples > 0, print(removed.samples),"Any sample removed")
  results = list("X" = X, "Y" = Y, "new.data" = data.samples.removed)
  return(invisible(results))
}
