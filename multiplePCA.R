
multiplePCA = function(X, Y, mfrow, width = 6000, height = 5000, pointsize = 12){
  source("/Users/josem/Google Drive/Functions/col_pch.R")
  col.pch = col.pch(Y)
  
  tiff(width = width, height = height, res = 600, compression = "lzw", pointsize = pointsize)
  for (i in 1:length(X)) {
    pcaFit = mixOmics::pca(X[[i]], ncomp = 2, center = TRUE, scale = TRUE) 
    plotIndiv(pcaFit, ind.names = F, group = Y, legend = TRUE, cex = 1.5, style = "graphics",
                abline = TRUE, title = "", size.legend = 1.2, size.xlabel = 1.7, comp = c(1,2), size.axis = 1.2,
                size.title = 2, star = F, col = col.pch$col, pch = col.pch$pch)
    
  }
  dev.off()
  
}




