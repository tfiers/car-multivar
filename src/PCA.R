source('classification_init.R')

library(rrcov)

XTrain_quant = XTrain[continuous_cols]
k = 6
pca = PcaClassic(XTrain_quant, scale=TRUE, k=k)
Z = pca@loadings
pca@scores[1,]
as.numeric(XTrain_quant[1,]) %*% Z
