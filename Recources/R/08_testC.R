#                                      #
# Test based on recursive estimation   #
#                                      #
########################################

setwd("C:/Users/Pali/Desktop/dAPM/teaching/ekonometria/2026/R/8")

#nacitame data
enrollment <- read.table("08_enrollment.txt",header=TRUE)
head(enrollment)
attach(enrollment)
Y <- ROLL

k <- 4
n <- length(Y)
X <- cbind(rep(1,n),UNEM,HGRAD,INC)
detach(enrollment)

betaHAT0 <- betaHAT1 <- betaHAT2 <- betaHAT3 <- NULL
is0 <- is1 <- is2 <- is3 <- NULL
m <- k+1

for(t in m:n){

#odhad beta z t dat
   betaHAT <- solve(t(X[1:t,])%*%X[1:t,])%*%t(X[1:t,])%*%Y[1:t]

   betaHAT0 <- c(betaHAT0,betaHAT[1])
   betaHAT1 <- c(betaHAT1,betaHAT[2])
   betaHAT2 <- c(betaHAT2,betaHAT[3])
   betaHAT3 <- c(betaHAT3,betaHAT[4])

   #odhad sigma^2 z t dat
   s2 <- t(Y[1:t] - X[1:t,] %*% betaHAT) %*% (Y[1:t] - X[1:t,] %*% betaHAT) / (t-k)

   #hranice IS z t dat
   is0 <- c(is0, sqrt(s2*solve(t(X[1:t,])%*%X[1:t,])[1,1])*qt(0.975,df=(t-k)))
   is1 <- c(is1, sqrt(s2*solve(t(X[1:t,])%*%X[1:t,])[2,2])*qt(0.975,df=(t-k)))
   is2 <- c(is2, sqrt(s2*solve(t(X[1:t,])%*%X[1:t,])[3,3])*qt(0.975,df=(t-k)))
   is3 <- c(is3, sqrt(s2*solve(t(X[1:t,])%*%X[1:t,])[4,4])*qt(0.975,df=(t-k)))
}



########################
#obrazok pre beta0
windows()
plot(c(m:n), betaHAT0,xlim=c(m,n), ylim=c(min(betaHAT0-is0), max(betaHAT0+is0)), 
   main="Test based on recursive estimation", xlab="t", ylab="betaHAT0")
points(c(m:n), betaHAT0+is0, col="red", type="l")
points(c(m:n), betaHAT0-is0, col="red", type="l")
lines(c(m,n), c(max(betaHAT0-is0), max(betaHAT0-is0)), col="blue")

########################
#obrazok pre beta1
windows()
plot(c(m:n), betaHAT1, xlim=c(m,n), ylim=c(min(betaHAT1-is1), max(betaHAT1+is1)),
   main="Test based on recursive estimation", xlab="t", ylab="betaHAT1")
points(c(m:n), betaHAT1+is1, col="red", type="l")
points(c(m:n), betaHAT1-is1, col="red", type="l")
lines(c(m,n), c(max(betaHAT1-is1), max(betaHAT1-is1)), col="blue")

########################
#obrazok pre beta2
windows()
plot(c(m:n), betaHAT2, xlim=c(m,n), ylim=c(min(betaHAT2-is2), max(betaHAT2+is2)),
   main="Test based on recursive estimation", xlab="t", ylab="betaHAT2")
points(c(m:n), betaHAT2+is2, col="red", type="l")
points(c(m:n), betaHAT2-is2, col="red", type="l")
lines(c(m,n), c(max(betaHAT2-is2), max(betaHAT2-is2)), col="blue")

########################
#obrazok pre beta3
windows()
plot(c(m:n), betaHAT3, xlim=c(m,n), ylim=c(min(betaHAT3-is3), max(betaHAT3+is3)),
   main="Test based on recursive estimation",xlab="t",ylab="betaHAT3")
points(c(m:n), betaHAT3+is3, col="red", type="l")
points(c(m:n), betaHAT3-is3, col="red", type="l")
lines(c(m,n), c(max(betaHAT3-is3), max(betaHAT3-is3)), col="blue")

