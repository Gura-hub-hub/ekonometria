setwd("C:/Users/Pali/Desktop/dAPM/teaching/ekonometria/2025/R/10/")

investicie<-read.table("10_investicie.txt", header=TRUE)
attach(investicie)
investicie
head(investicie)

model <- lm(Kt~t+K0)
betaHAT <- model$coeff

n <- length(Kt)
k <- 3

Y <- Kt
X <- cbind(rep(1,n), t, K0)
YHAT <- X %*% betaHAT

library(sandwich)
################ a)  bez heteroskedasticity   ##############
sandwichC <- vcovHC(model, type="const", sandwich=TRUE)
sandwichC

s2 <- sum((Y-YHAT)^2)/(n-k)
OmegaHAT <- diag(n)*s2
solve(t(X)%*%X) %*% t(X) %*% OmegaHAT %*% X %*% solve(t(X)%*%X)
s2*solve(t(X)%*%X)

################ b) Whiteovo HCE  ###############
sandwich0 <- vcovHC(model, type="HC0", sandwich=TRUE)
sandwich0

OmegaHAT<-diag(as.vector((Y-YHAT)^2))
solve(t(X)%*%X) %*% t(X) %*% OmegaHAT %*% X %*% solve(t(X)%*%X)

################ c) Whiteovo HCE korekcia1 ###############
sandwich1 <- vcovHC(model, type="HC1", sandwich=TRUE)
sandwich1

OmegaHAT <- diag(as.vector((Y-YHAT)^2))*(n/(n-k))
solve(t(X)%*%X) %*% t(X) %*% OmegaHAT %*% X %*% solve(t(X)%*%X)

################ d) Whiteovo HCE korekcia2 ###############
sandwich2 <- vcovHC(model, type="HC2", sandwich=TRUE)
sandwich2

H <- X %*% solve(t(X)%*%X) %*% t(X)
h <- diag(H)
OmegaHAT <- diag(as.vector((Y-YHAT)^2))*(1/(1-h))
solve(t(X)%*%X) %*% t(X) %*% OmegaHAT %*% X %*% solve(t(X)%*%X)


########## Testovanie strudlovych hypotez ############

#####################################
# test vyznamnosti regresie
#####################################
R <- rbind(c(0,1,0),c(0,0,1))
r <- c(0,0)
q <- 2

#a)
F <- (1/q)*t(R%*%betaHAT-r)%*%solve(R%*%sandwichC%*%t(R))%*%(R%*%betaHAT-r)
#  (1/q)*t(R%*%betaHAT-r)%*%solve(R%*%solve(t(X)%*%X)%*%t(R))%*%(R%*%betaHAT-r)/s2

F
1-pf(F,q,(n-k))  #p-value

#b)
W0 <- t(R%*%betaHAT-r)%*%solve(R%*%sandwich0%*%t(R))%*%(R%*%betaHAT-r)
W0
1-pchisq(W0,q)    #p-value

#c)
W1 <- t(R%*%betaHAT-r)%*%solve(R%*%sandwich1%*%t(R))%*%(R%*%betaHAT-r)
W1
1-pchisq(W1,q)    #p-value

#d)
W2 <- t(R%*%betaHAT-r)%*%solve(R%*%sandwich2%*%t(R))%*%(R%*%betaHAT-r)
W2
1-pchisq(W2,q)    #p-value


#hypoteza sa zamieta

#####################################
# H0 beta1=0 a beta2=1
#####################################
R <- rbind(c(0,1,0),c(0,0,1))
r <- c(0,1)
q <- 2

#a)
F <- (1/q)*t(R%*%betaHAT-r)%*%solve(R%*%sandwichC%*%t(R))%*%(R%*%betaHAT-r)
F
1-pf(F,q,(n-k))  #p-value

#b)
W0 <- t(R%*%betaHAT-r)%*%solve(R%*%sandwich0%*%t(R))%*%(R%*%betaHAT-r)
W0
1-pchisq(W0,q)    #p-value

#c)
W1 <- t(R%*%betaHAT-r)%*%solve(R%*%sandwich1%*%t(R))%*%(R%*%betaHAT-r)
W1
1-pchisq(W1,q)    #p-value

#d)
W2 <- t(R%*%betaHAT-r)%*%solve(R%*%sandwich2%*%t(R))%*%(R%*%betaHAT-r)
W2
1-pchisq(W2,q)    #p-value

#hypoteza sa nezamieta iba v pripade a) ked neuvazujeme heteroskedasticitu (co nie je dobre)

#################################
#H0 beta2=1
#################################
R <- rbind(c(0,0,1))
r <- 1
q <- 1

#a)
F <- (1/q)*t(R%*%betaHAT-r)%*%solve(R%*%sandwichC%*%t(R))%*%(R%*%betaHAT-r)
F
1-pf(F,q,(n-k))  #p-value

#b)
W0 <- t(R%*%betaHAT-r)%*%solve(R%*%sandwich0%*%t(R))%*%(R%*%betaHAT-r)
W0
1-pchisq(W0,q)    #p-value

#c)
W1 <- t(R%*%betaHAT-r)%*%solve(R%*%sandwich1%*%t(R))%*%(R%*%betaHAT-r)
W1
1-pchisq(W1,q)    #p-value

#d)
W2 <- t(R%*%betaHAT-r)%*%solve(R%*%sandwich2%*%t(R))%*%(R%*%betaHAT-r)
W2
1-pchisq(W2,q)    #p-value

#H0 sa nezamieta

#####################################
# to iste ako hypoteza o kontraste
#####################################

a <- c(0,0,1)
r <- 1

#a)
T <- (t(a)%*%betaHAT-r)/sqrt(t(a)%*%sandwichC%*%a)
#  (t(a)%*%betaHAT-r)/sqrt(s2*t(a)%*%solve(t(X)%*%X)%*%a)
T
T^2 #=F
(1-pt(T,df=(n-k)))*2     #p-value

#b)
U0 <- (t(a)%*%betaHAT-r)/sqrt(t(a)%*%sandwich0%*%a)
U0
U0^2 #=W0
(1-pnorm(U0))*2  #p-value

#c)
U1 <- (t(a)%*%betaHAT-r)/sqrt(t(a)%*%sandwich1%*%a)
U1
U1^2 #=W1
(1-pnorm(U1))*2  #p-value

#d)
U2 <- (t(a)%*%betaHAT-r)/sqrt(t(a)%*%sandwich2%*%a)
U2 
U2^2 #=W2
(1-pnorm(U2))*2  #p-value


#H0 sa nezamieta

detach(investicie)

