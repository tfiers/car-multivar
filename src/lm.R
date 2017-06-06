# Model selection by bidirectional stepwise regression using AIC.

source('init.R')
source('remove_outliers.R')
source('transformer.R')

library(MASS)
library(car)

transform_predictors = TRUE
remove_stdres_outliers = TRUE
transform_response = TRUE

# Copy training set
XT = XTrain

# (Extra plot: MD-RD)
cont_cols = XT[,continuous_cols]
mcd = covMcd(cont_cols)
rd = sqrt(mahalanobis(cont_cols, mcd$center, mcd$cov))
md = sqrt(mahalanobis(cont_cols, colMeans(cont_cols), cov(cont_cols)))

# Remember: data got shuffled in init.R.
# First/top number is original row;
# Second/bottom number is current row / indexing number.

# ML power transform of continuous variables
if (transform_predictors) {
  XT = transform(XT, verbose=TRUE)
}

# Define, save and remove lm outliers
# (These were found with 'which(abs(stdres(lm1)) > 5))')
if (remove_stdres_outliers) {
  lm_outliers_idx = c(176, 317, 635)
  lm_outliers = XT[lm_outliers_idx,]
  XT = XT[-lm_outliers_idx,]
}

expand_factors = function(full_X) {
  data = full_X[,c(continuous_cols, factor_cols)]
  # Expand factor columns into a set of 'one hot' columns
  # eg: fuel_type -> (fuel_typeHybrid, fuel_typePetrol)
  # (fuel_type=Diesel is then encoded as (0,0))
  design_matrix = model.matrix(lm(co2~., data))
  X_encoded = cbind(design_matrix[,-1], data[,'co2',drop=FALSE])
  return(X_encoded)
}

XT_encoded = expand_factors(XT)

vars = colnames(XT_encoded)
vars = vars[-length(vars)] # Remove 'co2'
full_formula = as.formula(paste("~",paste(vars,collapse="+")))

if (transform_response) {
  # The power is determined by running:
  # b = boxcox(model_without_transformed_respons)
  # lambda = b$x[which.max(b$y)]
  response = 'co2^-0.3'
} else {
  response = 'co2'
}

# Start with a full linear model
full_lm = lm(as.formula(paste(response,'~.')), XT_encoded)
lm1 = stepAIC(full_lm, list(lower=~1, upper=full_formula), direction='both')

# Start with an empty model
empty_model = lm(as.formula(paste(response,'~1')), XT_encoded)
lm2 = stepAIC(empty_model, list(lower=~1, upper=full_formula), direction='both')

# (Extra: model with interaction terms)
# full2_lm = lm(as.formula(paste(response,'~.^2')), XT_encoded)
# lm1 = stepAIC(full2_lm, list(lower=~1, upper=~.^2), direction='both')

# Check whether the two seeds converged to the same model
same_vars = setequal(names(lm1$coefficients), names(lm2$coefficients))
diff = lm1$coefficients[names(XT_encoded)] - lm2$coefficients[names(XT_encoded)]
same_coeffs = all((diff < 1e-12) | (is.na(diff)))
stepAIC_convergence = same_vars & same_coeffs
