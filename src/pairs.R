source('init.R')
source('remove_RD_outliers.R')

library(GGally)

# Pairwise plots
ggpairs(XT, columns=c(continuous_cols, factor_cols),
        aes(alpha=0.01),
        labeller='label_parsed')
ggsave('../pairs.pdf', width=15, height=13)
