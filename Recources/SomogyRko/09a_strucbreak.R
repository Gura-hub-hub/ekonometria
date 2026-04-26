
# Chow breakpoint test

setwd("C:/Users/Pali/Desktop/dAPM/teaching/ekonometria/2026/R/9/")

prijem <- read.table("09_prijem.txt", header=TRUE)
attach(prijem)
head(prijem)
prijem

k <- 4
n <- length(income)
n1 <- 22
n2 <- n-n1

X1 <- cbind(rep(1,n1),age[1:n1], education[1:n1], coworkers[1:n1])
X2 <- cbind(rep(1,n2),age[(n1+1):n], education[(n1+1):n], coworkers[(n1+1):n])
Y <- income

betaHAT1 <- lm(income[1:n1]~age[1:n1]+education[1:n1]+coworkers[1:n1])$coeff
betaHAT2 <- lm(income[(n1+1):n]~age[(n1+1):n]+education[(n1+1):n]+coworkers[(n1+1):n])$coeff
detach(prijem)

betaHAT <- c(betaHAT1,betaHAT2)
O1 <- matrix(0,ncol=k,nrow=n1)
O2 <- matrix(0,ncol=k,nrow=n2)
X <- rbind(cbind(X1,O1),cbind(O2,X2))
#lm(Y~X-1) (da nam to iste betaHAT)

####### H0: beta(1)=beta(2)

I <- diag(k)
R <- cbind(I,-I)
r <- rep(0,k)
q <- nrow(R)

RSS <- t(Y-X%*%betaHAT)%*%(Y-X%*%betaHAT)
#RSS <- t(Y[1:n1]-X1%*%betaHAT1)%*%(Y[1:n1]-X1%*%betaHAT1) + t(Y[(n1+1):n]-X2%*%betaHAT2)%*%(Y[(n1+1):n]-X2%*%betaHAT2)

s2 <- RSS/(n-2*k)

F <- (1/q)*(t(R%*%betaHAT-r) %*% solve(R%*%solve(t(X)%*%X)%*%t(R)) %*% (R%*%betaHAT-r))/s2
F

#kvantil
qf(0.95,df1=k,df2=(n-2*k))
#p-value
1-pf(F,df1=k,df2=(n-2*k))

#H0 zamietame


####### testujeme rovnost iba prvych dvoch zloziek H0: beta(1)[1:2]=beta(2)[1:2]
# ci sa rovnaju intercepty a koeficienty pri age

R <- R[1:2,]
q <- nrow(R)
r <- c(0,0)
F <- (1/q)*(t(R%*%betaHAT-r) %*% solve(R%*%solve(t(X)%*%X)%*%t(R)) %*% (R%*%betaHAT-r))/s2
F

#kvantil
qf(0.95,df1=q,df2=(n-2*k))
#p-value
1-pf(F,df1=q,df2=(n-2*k))

#H0 zamietame
##################################################################################################################################
realitka<-read.table("09_realitka.txt",header=TRUE)
attach(realitka)
realitka
head(realitka)

n1 <- 50
n2 <- 50
n3 <- 50
n <- n1+n2+n3
k <- 2

color<-c(rep("black",n1), rep("red",n2), rep("blue",n3))
plot(rozloha, cena, col=color)
plot(rozloha, cena)

betaHAT1 <- lm(cena[1:n1]~rozloha[1:n1])$coeff
betaHAT2 <- lm(cena[(n1+1):(n1+n2)]~rozloha[(n1+1):(n1+n2)])$coeff
betaHAT3 <- lm(cena[(n1+n2+1):n]~rozloha[(n1+n2+1):n])$coeff

X1 <- cbind(rep(1,n1),rozloha[1:n1])
X2 <- cbind(rep(1,n2),rozloha[(n1+1):(n1+n2)])
X3 <- cbind(rep(1,n3),rozloha[(n1+n2+1):n])

betaHAT <- c(betaHAT1,betaHAT2,betaHAT3)

O1 <- matrix(0,ncol=k,nrow=n1)
O2 <- matrix(0,ncol=k,nrow=n2)
O3 <- matrix(0,ncol=k,nrow=n3)
X <- rbind(cbind(X1,O1,O1),cbind(O2,X2,O2),cbind(O3,O3,X3))
Y <- cena

detach(realitka)

####### H0: beta(1)=beta(2)=beta(3)

I <- diag(k)
Ok <- matrix(0, ncol=k, nrow=k)
R <- rbind(cbind(I,-I,Ok), cbind(I,Ok,-I))
q <- nrow(R)
r <- rep(0,q)


RSS <- t(Y-X%*%betaHAT) %*% (Y-X%*%betaHAT)
s2 <- RSS/(n-3*k)

F <- (1/q)*(t(R%*%betaHAT-r) %*% solve(R%*%solve(t(X)%*%X)%*%t(R)) %*% (R%*%betaHAT-r))/s2
F

#kvantil
qf(0.95,df1=q,df2=(n-3*k))
#p-value
1-pf(F,df1=q,df2=(n-3*k))

#H0 zamietame
