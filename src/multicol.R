source('init.R')
source('remove_RD_outliers.R')

c = cor(XTrain[continuous_cols])
I = diag(nrow=dim(c)[1])
ridge = 0
diag(solve(c+ridge*I))
ev = eigen(c)$values
sqrt(max(ev)/ev)
