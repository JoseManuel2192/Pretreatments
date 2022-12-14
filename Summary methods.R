##################################################################################################
# PQN normalization
##################################################################################################

#' @title Perform Probabilistic Quotient Normalization
#' 
#' @description Perform Probabilistic Quotient Normalization
#'
#' @param X matrix to normalize samples * variables (rows * columns)
#' @param n normalization reference: "mean" for using the overall average of variables as reference 
#' or "median" (default) for using the overall median of variables as reference
#' @param QC vector of number(s) to specify samples which average to use as reference 
#' (e.g. QC samples)
#'
#' @return Normalized table samples * variables (rows * columns)
#' 
#' @details First a total area normalization should be done before PQN is applied.
#' 
#' @export
#' @importFrom stats median
#'
#' @author E. Nevedomskaya
#' @author Rico Derks
#' 
#' @references Dieterle, F., Ross, A., Schlotterbeck, G. & Senn, H. Probabilistic Quotient 
#' Normalization as Robust Method to Account for Dilution of Complex Biological Mixtures. 
#' Application in H1 NMR Metabonomics. Anal. Chem. 78, 4281-4290 (2006).

pqn = function(X, n = "median", QC) { #La mediana es mas robusta porque no es sensible a los outliers
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