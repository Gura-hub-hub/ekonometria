#                       #
# Cusum a Cusumsq test  #
#                       #
#########################

setwd("C:/Users/Pali/Desktop/dAPM/teaching/ekonometria/2026/R/8/")

#nacitame data
enrollment <- read.table("08_enrollment.txt",header=TRUE)
attach(enrollment)
Y <- ROLL

k <- 4
n <- length(Y)
X <- cbind(rep(1,n),UNEM,HGRAD,INC)
detach(enrollment)


# vypocet s^2 z celej vzorky
Y_fitted <- X %*% solve(t(X) %*% X) %*% t(X) %*% Y
RSS <- t(Y-Y_fitted) %*% (Y-Y_fitted)
s2 <- RSS/(n-k)

#postupne pocitanie odhadu pre beta, fitovanych hodnot, statistiky w, cusum, cusumsq
YHAT <- rep(0,k)
w <- rep(0,k)
cusum <- rep(0,k)
cusumsq <- rep(0,k)
for(t in (k+1):n){
	betaHAT <- solve(t(X[1:t-1,]) %*% X[1:t-1,]) %*% t(X[1:t-1,]) %*% Y[1:t-1]
	YHAT <- c(YHAT, X[t,] %*% betaHAT)
	w <- c(w, (Y[t]-YHAT[t])/sqrt(s2 + s2*t(X[t,]) %*% solve(t(X[1:t-1,]) %*% X[1:t-1,]) %*% X[t,]))
	cusum <- c(cusum,sum(w))
	cusumsq <- c(cusumsq,sum(w^2))
}
cusumsq <- cusumsq/sum(w^2)

#vykreslenie cusum
windows()
plot(c(k+1:n), cusum[k+1:n], ylab="CUSUM", xlab="t", 
   xlim=c(k,n),ylim=c(-30,30),main="Cusum test")
lines(c(k,n), c(1*0.948*sqrt(n-k),3*0.948*sqrt(n-k)), col="red")		#alpha=5%
lines(c(k,n), c(-1*0.948*sqrt(n-k),-3*0.948*sqrt(n-k)), col="red")
lines(c(k,n), c(1*1.143*sqrt(n-k),3*1.143*sqrt(n-k)), col="orange")		#alpha=1%
lines(c(k,n), c(-1*1.143*sqrt(n-k),-3*1.143*sqrt(n-k)), col="orange")

#vykreslenie cusumsq
windows()
plot(c(k+1:n), cusumsq[k+1:n], ylab="CUSUMSQ", xlab="t", 
   xlim=c(k,n), ylim=c(0,1), main="Cusumsq test")
lines(c(k,n), c(0,1), col="blue") #pribli?n? stredn? hodnota pre cusumsq
lines(c(k,n), c(0.323815,1+0.323815), col="red")	#alpha=5%
lines(c(k,n), c(-0.323815,1-0.323815), col="red")
lines(c(k,n), c(0.39489,1+0.39489), col="orange")	#alpha=1%
lines(c(k,n), c(-0.39489,1-0.39489), col="orange")
