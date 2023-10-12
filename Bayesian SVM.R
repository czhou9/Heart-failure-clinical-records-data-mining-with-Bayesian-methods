library(rjags)
data0 <- read.csv("C:/Users/Chengyu Zhou/Desktop/ST540 Final Project/540 dataset shuffled.csv")
data0 <- data0[1:240, 2:14]
head(data0)

### input X, Y matrix
age <- data0[,1]
anaemia <- data0[,2] # binary
creatinine <- data0[,3]
diabetes <- data0[,4] # binary
ejection <- data0[,5]
high_bp <- data0[,6] # binary
platelets <- data0[,7]
serum_c <- data0[,8]
serum_s <- data0[,9]
sex <- data0[,10] # binary
smoking <- data0[,11] # binary
#time <- data0[,12]
death <- data0[,13]