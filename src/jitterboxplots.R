# Univariate jittered points plots of the variables

source('init.R')

library(ggplot2)

# Define plotting function
make_jitterbox_plots = function(col_names, filename_prefix='jitterbox_', box=TRUE) {
  for (col_name in col_names) {
    col = which(names(X) == col_name)
    df = data.frame(vec=XTrain[,col])

    p = ggplot(df, aes(x=1, y=vec, alpha=0.5))
    if (box) {
      p = p + geom_boxplot(outlier.shape=NA) + xlim(0.5,1.5)
    } else {
      p = p + xlim(0.7,1.3)
    }
    p +
    geom_jitter(width=0.2) +
    coord_flip() +
    ggtitle(col_name) +
    theme(axis.title.y=element_blank(),
          axis.title.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          legend.position='none')

    filename = paste('../', filename_prefix, col_name, '.pdf', sep='')
    ggsave(filename, width=15, height=1.5)
  }
}

# Variables with extreme outliers
make_jitterbox_plots(c('noise_level', 'nox_emissions'), 'extreme_outliers_', box=FALSE)

# Remove outliers
source('remove_outliers.R')

# Make plots for all continuous variables
make_jitterbox_plots(names(X)[continuous_cols])
