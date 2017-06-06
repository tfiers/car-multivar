# Evaluate the found linear model

source('lm.R')

model = lm1

print(summary(model))
print(model$anova)

standardised_residuals = stdres(model)

for (col in c(factor_cols,continuous_cols)) {
  plot(XT[[col]], standardised_residuals, xlab=names(X)[col])
  plot_stdres_bounds()
}

plot(standardised_residuals)
plot_stdres_bounds()

plot(fitted(model), standardised_residuals)
plot_stdres_bounds()

b = boxcox(model)
lambda = b$x[which.max(b$y)]

qqnorm(standardised_residuals)
qqline(standardised_residuals)
shapiro.test(standardised_residuals)

# Compare with robust linear model
# lmrob = ltsreg(log(co2)~., data)
# ltsPlot(lmrob, which=c('rfit'))

# Diagnostic plot
if (!remove_stdres_outliers) {
  plot(rd, stdres(lm1))
  plot_stdres_bounds()
}

# Find strong stdres outliers
which(abs(stdres(lm1)) > 5)

print(sum(model$residuals^2))
print(lambda, digits=5)
print(stepAIC_convergence)


# Test the model on the test set
# Important: needs to use same transform for unbiased predictions.
XTT = expand_factors(transform(XTest, XSource=XTrain))
y = XTT$co2^-0.3
yhat = predict(lm1, XTT)

plot(y,yhat)
abline(0,1)

e = (y - yhat)
e_rel = e/y
qqnorm(e_rel)
qqline(e_rel)
plot(e_rel)
plot(yhat, e_rel)

n = 500
p = 11
sigma = sum(e^2)/(n-p)
print(sigma)

msre = sum(e_rel^2)/(n-p)
print(msre, digits=5)

mare = sum(abs(e_rel))/(n-p)
print(mare, digits=5)

# Manual prediction
# x = c(1, as.numeric(XTT[1,-c(5,11,14)]))
# w = as.numeric(summary(lm1)$coeff[,1])
# y[1]
# yhat[1]
# w %*% x
# 
# plot(XT_encoded$combined_metric, XT_encoded$co2^-0.3)
# points(XTT$combined_metric, XTT$co2^-0.3, col='blue')
