rm(list = ls()) # Clear environment
options(digits = 2) # Set output display precision
cat("\014") # Clear console
# Hit Ctrl+Shift+L to clear plots

X = read.table('data.txt', header=TRUE)

set.seed(0380267)
selVec <- c(sample(1:dim(X)[1],1000))
XTrain <- X[selVec,]
XTest <- X[-selVec,]

