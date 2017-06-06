# Logistic model

source('PCA.R')

# Construct full logistic model
fullmod = glm(euro_standard~., XTrain_PCA_and_factors, family=binomial)
# Prune model
lrmod = stepAIC(fullmod, list(lower=~1,upper=~.), direction='both')

print(get_error_rate(lrmod, XTrain_PCA_and_factors))
print(get_error_rate(lrmod, XTest_PCA_and_factors))


# Analyse found model

# Iterativa algo convergance
print(lrmod$converged)
print(lrmod$iter)

summary(lrmod)
# Residual deviance: 776 on 987 dof
qchisq(0.95,987) # = 1061 > 776 --> do not reject model
# Deviance of fit: 1324.28 - 775.97 = 548.3 on 4 degrees of freedom
# So this fit explains a lot more than a constant probability.
qchisq(0.95,4) # = 10

# Deviance residuals
plot(residuals(lrmod, 'deviance'), ylab='Deviance residuals')
plot_stdres_bounds()

# Plot logistic fit.
eta = predict(lrmod, XTrain_PCA_and_factors, type="link")
fitted_probs = predict(lrmod, XTrain_PCA_and_factors, type="response")
y = as.numeric(XTrain_PCA_and_factors$euro_standard) - 1

plot(eta, y, pch=4, xlab="Fitted eta", ylab="Euro-standard, Fitted probs")
points(eta, fitted_probs, pch=3, col='blue')
abline(h=cutoff, lty=2)
abline(v=-0.5, lty=2)


# Transform model coefficients back to input variable space
#
# Get PC coefficients only
PC_coeffs = lrmod$coefficients[2:(2+k-1)]
input_coeffs = Z %*% PC_coeffs
print(input_coeffs)

