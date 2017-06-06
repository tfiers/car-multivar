source('init.R')
source('remove_RD_outliers.R')

library(MASS)

make_eurostandard_binary = function(X_in) {
  # euro_standard had been made a factor -- we must undo this.
  f = as.numeric(levels(X_in$euro_standard))[X_in$euro_standard]
  f = as.factor(f > 4)
  f = factor(f, labels=c('old', 'new'))
  X_in$euro_standard = f
  return(X_in)
}

XTrain = make_eurostandard_binary(XTrain)
XTest = make_eurostandard_binary(XTest)


# Proportion of new cars
cutoff = mean(as.numeric(XTrain$euro_standard) - 1)

get_table_glm = function(model, testing_set) {
  # Find estimated error rate on test set
  # Find predicted probabilities using reduced model
  pred.prob = predict(model, testing_set, type='response')
  # Bin predicted probabilities according to found proportion
  yhat = ifelse(pred.prob > cutoff, 1, 0)
  # Find real responses
  y = as.numeric(testing_set$euro_standard) - 1
  # Compare in table
  return(prop.table(table(y, yhat)))
}

get_table_DA = function(model, testing_set) {
  p = predict(model, testing_set)
  return(table(p$class, testing_set$euro_standard))
}

get_error_rate = function(model, testing_set, type='glm') {
  if (type=='glm') {
    table = get_table_glm(model, testing_set)
  } else {
    table = prop.table(get_table_DA(model, testing_set))
  }
  return(1 - sum(diag(table)))
}

