# Find, save, and remove all extreme univariate outliers
outlier_indexes = c()
for (col in continuous_cols) {
  col_data = XTrain[,col]
  Q1 = quantile(col_data, c(0.25))
  Q3 = quantile(col_data, c(0.75))
  IQR = Q3 - Q1
  new_outliers = which(col_data < Q1 - 15*IQR |
                         col_data > Q3 + 15*IQR)
  outlier_indexes = c(outlier_indexes, new_outliers)
}
outliers = XTrain[outlier_indexes,]
XTrain = XTrain[-outlier_indexes,]
