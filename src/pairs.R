source('init.R')
source('remove_RD_outliers.R')

library(GGally)

f = as.numeric(levels(XTrain$euro_standard))[XTrain$euro_standard]
f = as.factor(f > 4)
f = factor(f, labels=c('old', 'new'))
XT = cbind(XTrain, f)
names(XT)[15] = 'euro_standard_binary'

# Pairwise plots
ggpairs(XT, columns=c(continuous_cols, factor_cols),
        aes(alpha=0.01, color=euro_standard_binary),
        labeller='label_parsed')
ggsave('../pairs.pdf', width=15, height=13)
