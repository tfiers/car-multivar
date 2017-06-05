# Find, save, and remove all outliers

library(robustbase)

data = XTrain[,continuous_cols]
mcd = covMcd(data)
d = sqrt(mahalanobis(data, mcd$center, mcd$cov))
outlier_indexes = which(d > 20)

outliers = XTrain[outlier_indexes,]
XTrain = XTrain[-outlier_indexes,]
