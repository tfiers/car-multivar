source('init.R')
source('remove_RD_outliers.R')

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
