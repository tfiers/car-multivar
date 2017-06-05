# Model selection by bidirectional stepwise regression using AIC.

source('init.R')
source('remove_outliers.R')

library(MASS)
library(car)

remove_stdres_outliers = FALSE
transform_predictors = TRUE
transform_response = FALSE

# (Extra plot: MD-RD)
cont_cols = XTrain[,continuous_cols]
mcd = covMcd(cont_cols)
rd = sqrt(mahalanobis(cont_cols, mcd$center, mcd$cov))
md = sqrt(mahalanobis(cont_cols, colMeans(cont_cols), cov(cont_cols)))
plot(md, rd)

# Define, save and remove lm outliers
# (These were found with 'which(abs(stdres(lm1)) > 5))')
if (remove_stdres_outliers) {
  lm_outliers_idx = c(176, 317, 635)
  lm_outliers = XTrain[lm_outliers_idx,]
  XTrain = XTrain[-lm_outliers_idx,]
}

# Remember: data got shuffled in init.R.
# First/top number is original row;
# Second/bottom number is current row / indexing number.

# ML power transform of continuous variables
if (transform_predictors) {
  for (col in continuous_cols) {
    name = names(X)[col]
    #
    qqnorm(XTrain[,col], main=name)
    qqline(XTrain[,col])
    p_before = shapiro.test(XTrain[,col])$p
    #
    pt = powerTransform(XTrain[,col])
    pt$lambda = pt$lambda
    transf_coldata = bcPower(XTrain[,col], pt$lambda)
    #
    qqnorm(transf_coldata, main=name)
    qqline(transf_coldata)
    p_after = shapiro.test(transf_coldata)$p
    #
    if (p_after > 0.05) {
      XTrain[,col] = transf_coldata  
    }
    #
    print(name)
    print(as.numeric(pt$lambda), digits=3)
    print(p_before, digits=3)
    print(p_after, digits=3)
  }
}

data = XTrain[,c(continuous_cols, factor_cols)]

# Expand factor columns into a set of 'one hot' columns
# eg: fuel_type -> (fuel_typeHybrid, fuel_typePetrol)
# (fuel_type=Diesel is then encoded as (0,0))
design_matrix = model.matrix(lm(co2~., data))
data_encoded= cbind(design_matrix[,-1], data[,'co2',drop=FALSE])

vars = colnames(design_matrix)
vars = vars[-1] # Remove '(Intercept)'
full_formula = as.formula(paste("~",paste(vars,collapse="+")))

if (transform_response) {
  response = 'log(co2)'
} else {
  response = 'co2'
}

# Start with a full linear model
full_lm = lm(as.formula(paste(response,'~.')), data_encoded)
lm1 = stepAIC(full_lm, list(lower=~1, upper=full_formula), direction='both')

# Start with an empty model
empty_model = lm(as.formula(paste(response,'~1')), data_encoded)
lm2 = stepAIC(empty_model, list(lower=~1, upper=full_formula), direction='both')

# Check whether the two seeds converged to the same model
same_vars = setequal(names(lm1$coefficients), names(lm2$coefficients))
diff = lm1$coefficients[names(data_encoded)] - lm2$coefficients[names(data_encoded)]
same_coeffs = all((diff < 1e-12) | (is.na(diff)))
print(same_vars & same_coeffs)

print(lm1$anova)

plot_stdres_bounds = function() {
  abline(-2.5, 0, lty=2)
  abline(+2.5, 0, lty=2)
}

evaluate = function(model) {
  print(summary(model))
  standardised_residuals = stdres(model)
  
  for (col in c(factor_cols,continuous_cols)) {
    plot(XTrain[[col]], standardised_residuals, xlab=names(X)[col])
    plot_stdres_bounds()
  }
  
  plot(standardised_residuals)
  plot_stdres_bounds()
  
  plot(fitted(model), standardised_residuals)
  plot_stdres_bounds()
  
  boxcox(model)
  
  qqnorm(standardised_residuals)
  qqline(standardised_residuals)
  shapiro.test(standardised_residuals)
  
  print(sum(model$residuals^2))
}

evaluate(lm1)
# plot(lm1)

# Compare with robust linear model
# lmrob = ltsreg(log(co2)~., data)
# ltsPlot(lmrob, which=c('rfit'))

# Find stdres outliers
which(abs(stdres(lm1)) > 5)

# Diagnostic plot
if (!remove_stdres_outliers) {
  plot(rd, stdres(lm1))
  plot_stdres_bounds()
}

