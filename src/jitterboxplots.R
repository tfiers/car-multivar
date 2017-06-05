# Univariate jittered point plots of all continuous variables

source('init.R')
source('remove_outliers.R')

for (col in continuous_cols) {
  col_name = names(X)[col]
  df = data.frame(vec=XTrain[,col])

  p = ggplot(df, aes(x=1, y=vec, alpha=0.2)) +
  geom_boxplot(outlier.shape=NA) +
  geom_jitter(width=0.2) +
  scale_x_continuous(expand=c(0.1,0.1)) +
  coord_flip() +
  ggtitle(col_name) +
  univariate_plot_theme
  
  filename = paste('../jitterbox_', col_name, '.pdf', sep='')
  ggsave(filename, width=8, height=1.1)
}
