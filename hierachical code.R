library(rjags)
data1 <- read.csv("C:/Users/Chengyu Zhou/Desktop/ST540 Final Project/540 dataset shuffled (version 1).csv")
head(data1)

ageh <- data1[,16]
dea <- data1[,14]
eje <- data1[,6]

y       <- dea
x       <- log(eje)
n       <- length(y)
sp      <- as.numeric(ageh)
names   <- c("1","2","3","4")
data    <- list(y=y,x=x,sp=sp,n=n,N=4)
#data

model_string3 <- textConnection("model{
  for(i in 1:n){
    y[i]     ~ dnorm(muY[i],tau[sp[i]])
    muY[i]  <- log(a[sp[i]] + b[sp[i]]/(1+exp(-part[i]))) - 0.5/tau[sp[i]]
    part[i] <- (x[i]-c[sp[i]])/d[sp[i]]
  }

  for(j in 1:N){
    a[j]   <- exp(alpha[j,1])
    b[j]   <- exp(alpha[j,2])
    c[j]   <- alpha[j,3]
    d[j]   <- exp(alpha[j,4])

    for(k in 1:4){alpha[j,k]   ~ dnorm(0.0,0.1)}
    tau[j] ~ dgamma(0.1,0.1)
  }

  for(eje in 1:80){for(j in 1:4){
    PART[eje,j]   <- (log(eje)-c[j])/d[j] 
    fitted[eje,j] <- a[j] + b[j]/(1+exp(-PART[eje,j]))
  }}


}")

model3   <- jags.model(model_string3,data = data,quiet=TRUE, n.chains=2)
update(model3, burn, progress.bar="none")

samples3 <- coda.samples(model3, variable.names=c("a","b","c","d","fitted","tau"), n.iter=iters, thin=thin, progress.bar="none")

ESS <- effectiveSize(samples3) 
ESS[which.min(ESS)]