source('init.R')
source('remove_RD_outliers.R')

library(GGally)

# Pairwise plots
ggpairs(XTrain[,c(continuous_cols, factor_cols)],
        aes(alpha=0.4, color=fuel_type),
        labeller='label_parsed')
ggsave('../pairs.pdf', width=15, height=13)
