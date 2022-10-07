##################################################################################################
# PQN normalization
##################################################################################################
# Funcion para normalizar
pqn <- function(X, n = "median", QC) { #La mediana es mas robusta porque no es sensible a los outliers
  X.norm <- matrix(nrow = nrow(X), ncol = ncol(X))
  colnames(X.norm) <- colnames(X)
  rownames(X.norm) <- rownames(X)
  
  if (exists("QC")) {
    # Si solo hay 1 QC
    if (length(QC) == 1) {
      mX <- as.numeric(X[QC, ])
    } else {
      if (n == "mean") {
        mX <- as.numeric(colMeans(X[QC, ]))
      }
      if (n == "median") {
        mX <- as.numeric(apply(X[QC, ], 2, stats::median))
      }
    }
  } else {
    # Si no, mean o median de todos los QC
    if (n == "mean") {
      mX <- as.numeric(colMeans(X))
    }
    if (n == "median") {
      mX <- as.numeric(apply(X, 2, stats::median))
    }
  }
  # Normalizacion
  for (a in 1:nrow(X)) {
    X.norm[a,] <- as.numeric(X[a, ] / stats::median(as.numeric(X[a, ] / mX)))
  }
  return(X.norm)
}

##################################################################################################