require(statTarget)

require(xlsx); require(readxl); require(mixOmics); rm(list = ls()); wd = getwd()
data.read = read_excel(path = "./Áreas metabolitos Edu.xlsx", sheet = 1)
data = data.read[,-c(1)]
ncategorical = 4

metaData = data[,c(1:ncategorical)]
X = data[,-c(1:ncategorical)]; X = as.matrix(X)

#' Changing metaData
levels = levels(as.factor(metaData$class))
metaData$class[metaData$class == levels[1]] = 1
metaData$class[metaData$class == levels[2]] = 2
metaData$class[metaData$class == levels[3]] = 3
metaData$class[metaData$class == levels[4]] = 4
metaData$class[metaData$class == levels[5]] = "NA"

#' Saving it as .csv in the folder
# dir.create("Drift-correction")
# setwd("./Drift-correction")
# write.csv(metaData, "metaData.csv", row.names = F)
# write.csv(X, "X.csv", row.names = F)


metaData = as.matrix(metaData); mode(metaData)
X = as.matrix(X)
shiftCor(metaData,X, MLmethod = 'QCRFSC', imputeM = 'KNN',coCV = 30)







setwd(wd)

















