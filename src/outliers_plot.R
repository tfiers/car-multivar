source('init.R')
source('remove_RD_outliers.R')

save = function(affix) { 
  filename = paste('../multivar_outlier_', affix, '.pdf', sep='')
  ggsave(filename, width=8, height=1.25)
}

df = data.frame(robust_distance=d)

p = ggplot(df, aes(x=seq_along(robust_distance), y=robust_distance, alpha=0.2)) +
  scale_x_continuous(expand=c(0.3,0.3)) +
  geom_point() +
  coord_flip() +
  ggtitle('Robust distance') +
  univariate_plot_theme
print(p)
save('all')

p + 
  coord_flip(ylim=c(0,40)) +
  ggtitle('(zoomed in)')
save('zoom')
