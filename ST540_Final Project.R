library(rjags)
data0 <- read.csv("C:/Users/Chengyu Zhou/Desktop/ST540 Final Project/data_shuffled_new.csv")
data0 <- data0[1:160,2:14]
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

X <- cbind(1,scale(age),anaemia,scale(creatinine),diabetes,scale(ejection),
           high_bp, scale(platelets),scale(serum_c),scale(serum_s),sex,smoking)
Y <- death

n <- length(Y)
p <- ncol(X)
data   <- list(Y=Y,X=X,n=n,p=p)

burn <- 2000
n.iters <- 10000
#thin     <- 10
n.chains <- 2

###### Model 1
model_string <- textConnection("model{
 # likelihood
 for(i in 1:n){
 Y[i] ~ dbern(prob[i])
 logit(prob[i]) = inprod(X[i,],beta[])
 }
 
 # Priors
 for(j in 1:p){beta[j] ~ dnorm(0, 0.01)}

 
 
 # WAIC calculations
  for(i in 1:n){
     like[i]    <- dbin(Y[i],prob[i],1)
   }
 }"
)
model <-jags.model(model_string, data=data, 
                   n.chains=n.chains, quiet=TRUE)
update(model, burn, progress.bar="none")
samples <-coda.samples(model, variable.names=c("beta"), n.iter=n.iters,
                       progress.bar="none")
summary(samples)
plot(samples)
# Compute DIC
DIC_1    <- dic.samples(model,n.iter=n.iters,n.thin = 5, progress.bar="none")

# Compute WAIC
waic1   <- coda.samples(model, 
                        variable.names=c("like"), 
                        n.iter=n.iters, progress.bar="none")
like1   <- waic1[[1]]
fbar1   <- colMeans(like1)
P1      <- sum(apply(log(like1),2,var))
WAIC_1   <- -2*sum(log(fbar1))+2*P1


###### Model 2
model_string <- textConnection("model{
 # likelihood
 for(i in 1:n){
 Y[i] ~ dbern(prob[i])
 logit(prob[i]) = inprod(X[i,],beta[])
 }
 
 # Priors
 for(j in 1:p){beta[j] ~ dnorm(0,taub)}
 taub  ~ dgamma(0.1, 0.1)

 
 
 # WAIC calculations
  for(i in 1:n){
     like[i]    <- dbin(Y[i],prob[i],1)
   }
 }"
)
model <-jags.model(model_string, data=data, 
                   n.chains=n.chains, quiet=TRUE)
update(model, burn, progress.bar="none")
samples <-coda.samples(model, variable.names=c("beta"), n.iter=n.iters,
                       progress.bar="none")
summary(samples)
par(mar = c(2, 2, 2, 2)) 
plot(samples)
# Compute DIC
DIC_2    <- dic.samples(model,n.iter=n.iters,n.thin = 5, progress.bar="none")

# Compute WAIC
waic1   <- coda.samples(model, 
                        variable.names=c("like"), 
                        n.iter=n.iters, progress.bar="none")
like1   <- waic1[[1]]
fbar1   <- colMeans(like1)
P1      <- sum(apply(log(like1),2,var))
WAIC_2   <- -2*sum(log(fbar1))+2*P1


###### Model 3 
model_string <- textConnection("model{
 # likelihood
 for(i in 1:n){
 Y[i] ~ dbern(prob[i])
 logit(prob[i]) = inprod(X[i,],beta[])
 }
 
 # Priors
 for(j in 1:p){beta[j] ~ ddexp(0,taub)}
 taub  ~ dgamma(0.1, 0.1)

 
 
 # WAIC calculations
  for(i in 1:n){
     like[i]    <- dbin(Y[i],prob[i],1)
   }
 }"
)
model <-jags.model(model_string, data=data, 
                   n.chains=n.chains, quiet=TRUE)
update(model, burn, progress.bar="none")
samples <-coda.samples(model, variable.names=c("beta"), n.iter=n.iters,
                       progress.bar="none")
summary(samples)
# Compute DIC
DIC_3    <- dic.samples(model,n.iter=n.iters,n.thin = 5, progress.bar="none")

# Compute WAIC
waic1   <- coda.samples(model, 
                        variable.names=c("like"), 
                        n.iter=n.iters, progress.bar="none")
like1   <- waic1[[1]]
fbar1   <- colMeans(like1)
P1      <- sum(apply(log(like1),2,var))
WAIC_3   <- -2*sum(log(fbar1))+2*P1

summary(samples)
par(mar = c(2, 2, 2, 2)) 
plot(samples)


######## Conclude DIC, WAIC
DIC_1
DIC_2
DIC_3
WAIC_1
WAIC_2
WAIC_3