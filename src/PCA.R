source('classification_init.R')

library(rrcov)

XTrain_0 = scale(XTrain[continuous_cols])
XTest_0 = scale(XTest[continuous_cols])

find_PCA_k_test_error_rate = function(k) {
  pca = PcaClassic(XTrain_0, k=k)
  Z = pca@loadings
  # Todo: outliers? MD-OD plot
  XTrain_PCA_cols = XTrain_0 %*% Z
  XTest_PCA_cols = XTest_0 %*% Z
  XTrain_PCA_and_factors = cbind(XTrain_PCA_cols, XTrain[factor_cols])
  XTest_PCA_and_factors = cbind(XTest_PCA_cols, XTest[factor_cols])
  
  # Construct full logistic model
  fullmod = glm(euro_standard~., XTrain_PCA_and_factors, family=binomial)
  # Prune model
  redmod = stepAIC(fullmod, list(lower=~1,upper=~.), direction='both')
  
  return(get_error_rate(redmod, XTest_PCA_and_factors))
}

# We bepalen k zodat error rate op test set minimaal is.
EAERs = c(1:8)
for (k in 1:8) {
  EAERs[k] = find_PCA_k_test_error_rate(k)
}
EAERs
plot(EAERs)
lines(EAERs, xlab='k')

pca = PcaClassic(XTrain_0, k=8)
screeplot(pca, main='')
pca@eigenvalues

# Decision: retain k PC
k = 6
pca = PcaClassic(XTrain_0, k=k)
Z = pca@loadings
XTrain_PCA = XTrain_0 %*% Z
XTest_PCA = XTest_0 %*% Z
XTrain_PCA_and_factors = cbind(XTrain_PCA, XTrain[factor_cols])
XTest_PCA_and_factors = cbind(XTest_PCA, XTest[factor_cols])
XTrain_PCA_and_response = cbind(XTrain_PCA, XTrain['euro_standard'])
XTest_PCA_and_response = cbind(XTest_PCA, XTest['euro_standard'])

