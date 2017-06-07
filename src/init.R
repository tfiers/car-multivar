rm(list = ls()) # Clear environment
options(digits = 4) # Set output display precision
cat("\014") # Clear console
# Hit Ctrl+Shift+L to clear plots

X = read.table('data.txt', header=TRUE)
X$euro_standard = factor(X$euro_standard)

set.seed(0380267)
selVec = c(sample(1:dim(X)[1],1000))
XTrain = X[selVec,]
XTest = X[-selVec,]

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

# Plotting init
library(ggplot2)
univariate_plot_theme = theme(axis.title.y=element_blank(),
                              axis.title.x=element_blank(),
                              axis.text.y=element_blank(),
                              axis.ticks.y=element_blank(),
                              panel.grid.major.y=element_blank(),
                              panel.grid.minor.y=element_blank(),
                              legend.position='none')

plot_stdres_bounds = function() {
  abline(-2.5, 0, lty=2)
  abline(+2.5, 0, lty=2)
}

