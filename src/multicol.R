source('init.R')
source('remove_RD_outliers.R')

c = cor(XTrain[continuous_cols])
diag(solve(c))
ev = eigen(c)$values
sqrt(max(ev)/ev)
