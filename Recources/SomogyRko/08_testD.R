#                                                 #
# Test based on one-step ahead prediction error   #
#                                                 #
###################################################

setwd("C:/Users/Pali/Desktop/dAPM/teaching/ekonometria/2026/R/8/")

#nacitame data
enrollment <- read.table("08_enrollment.txt", header=TRUE)
head(enrollment)
attach(enrollment)
Y <- ROLL

k <- 4
n <- length(Y)
X <- cbind(rep(1,n), UNEM, HGRAD, INC)
detach(enrollment)

m <- k+2

# One step ahead

OSAPE <- NULL
pi95 <- NULL

for(t in m:n){
   # odhad beta z t-1 dat
   betaHAT <- solve(t(X[1:(t-1), ]) %*% X[1:(t-1), ]) %*% t(X[1:(t-1), ]) %*%Y [1:(t-1)]

   #predikcia Y_t
   a <- (as.vector(X[t, ]))
   OSAPE <- c(OSAPE, Y[t] - t(a) %*% betaHAT)

   #odhad sigma^2 z t-1 dat
   s2 <- t(Y[1:(t-1)] - X[1:(t-1),] %*% betaHAT) %*% (Y[1:(t-1)] - X[1:(t-1),] %*% betaHAT) / ((t-1)-k)

   #hranice PI pre Y_t z (t-1) dat
   pi95 <- c(pi95, sqrt(s2*(1+t(a) %*% solve(t(X[1:(t-1),]) %*% X[1:(t-1),]) %*% a))*qt(0.975,df=((t-1)-k)))
}

#vykreslime (H0 zamietame ak je OSAPE mimo intervalu)
plot(c(m:n), OSAPE, xlim=c(m,t), ylim=c(min(OSAPE-pi95), max(OSAPE+pi95)),
   main="Test based on one-step ahead prediction error", xlab="t", ylab="OSAPE")
points(c(m:n), 0+pi95, col="red", type="l")
points(c(m:n), 0-pi95, col="red", type="l")
lines(c(m,n), c(0,0), col="blue")

#alternativne: H0 zamietame ak sa ciara y=0 nezmesti do intervalu
windows()
plot(c(m:n), OSAPE, xlim=c(m,t), ylim=c(min(OSAPE-pi95), max(OSAPE+pi95)),
   main="Test based on one-step ahead prediction error", xlab="t", ylab="OSAPE")
points(c(m:n), OSAPE+pi95, col="red", type="l")
points(c(m:n), OSAPE-pi95, col="red", type="l")
lines(c(m,n), c(0,0), col="blue")
