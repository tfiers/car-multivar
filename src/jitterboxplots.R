source('init.R')

library(ggplot2)

# One-dimensional jittered points plots of variables with extreme outliers
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
