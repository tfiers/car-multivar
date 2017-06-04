rm(list = ls()) # Clear environment
options(digits = 2) # Set output display precision
cat("\014") # Clear console
# Hit Ctrl+Shift+L to clear plots

regenerate_plots = FALSE

library(ggplot2)
library(GGally)

X = read.table('data.txt', header=TRUE)
X$euro_standard = factor(X$euro_standard)

set.seed(0380267)
selVec <- c(sample(1:dim(X)[1],1000))
XTrain <- X[selVec,]
XTest <- X[-selVec,]

# These identifying variables will not be included in any models
id_col_names = c('manufacturer',
                 'model',
                 'description')
id_cols = which(names(X) %in% id_col_names)

factor_col_names = c('euro_standard',
                     'transmission_type',
                     'fuel_type')
factor_cols = which(names(X) %in% factor_col_names)

continuous_cols = which(!(names(X) %in% id_col_names)
                        & !(names(X) %in% factor_col_names))

# One-dimensional jittered points plots of variables with extreme outliers
if (regenerate_plots) {
  outlier_col_names = c('noise_level', 'nox_emissions')
  for (col_name in outlier_col_names) {
    col = which(names(X) == col_name)
    df = data.frame(vec=XTrain[,col])
    ggplot(df, aes(x=1, y=vec)) +
      # geom_boxplot(colour='grey60') +
      geom_jitter(width=0.4) +
      coord_flip() +
      ggtitle(col_name) +
      theme(axis.title.y=element_blank(),
            axis.title.x=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            legend.position='none')
    filename = paste('jitterbox_', col_name, '.pdf', sep='')
    ggsave(filename, width=15, height=2)
  }
}

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

# Pairwise plots
if (regenerate_plots) {
  ggpairs(XTrain[,c(continuous_cols, factor_cols)], aes(alpha=0.4), labeller='label_parsed')
  ggsave('pairs.pdf', width=15, height=13)
}

