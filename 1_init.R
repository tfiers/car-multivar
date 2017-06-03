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

if (regenerate_plots) {
  ggpairs(XTest[,c(continuous_cols, factor_cols)], aes(alpha=0.4), labeller='label_parsed')
  ggsave('pairs.pdf', width=15, height=10, scale=1)
}

