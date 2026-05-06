
###################### Whiteov test #########################################################################

setwd("C:/Users/Pali/Desktop/dAPM/teaching/ekonometria/2026/R/10")
investicie <- read.table("10_investicie2.txt",header=TRUE)
attach(investicie)
investicie

model <- lm(Kt ~ t + K0)
betaHAT <- model$coeff
n <- length(Kt)
k <- 3
Y <- Kt
X <- cbind(rep(1,n),t,K0)
YHAT <- X%*%betaHAT
epsilonHAT <- Y-YHAT

#test pomocou R^2 (asymptoticky)
artificial <- lm(epsilonHAT^2 ~ I(t^2) + I(K0^2) + t + K0 + I(t*K0))
cosi <- k*(k+1)/2
R2 <- summary(artificial)$r.squared
n*R2
kvantil <- qchisq(0.95,df=cosi-1)
kvantil

x <- seq(0,30,by=0.1)
y <- dchisq(x,df=cosi-1)
plot(x,y,type="l",xlab="",ylab="")
points(n*R2,0,cex=2,pch=20,col="blue")
points(kvantil,0,cex=2,pch=20)#,col="red")

#exaktny test
RSS <- summary(artificial)$sigma^2*(n-cosi)
artificialsub <- lm(epsilonHAT^2 ~ 1)
RSSsub <- summary(artificialsub)$sigma^2*(n-1)
Fprime <- ( (RSSsub-RSS)/(cosi-1) ) / ( RSS/(n-cosi) )
Fprime
qf(0.95,df1=cosi-1,df2=(n-cosi))

###################### duplicity v artificial modeli ####################################################

model <- lm(Kt ~ t + I(t^2) + K0)
betaHAT <- model$coeff
n <- length(Kt)
k <- 4
Y <- Kt
X <- cbind(rep(1,n),t,t^2,K0)
YHAT <- X%*%betaHAT
epsilonHAT <- Y-YHAT

#test pomocou R^2 (asymptoticky)
artificial <- lm(epsilonHAT^2 ~ I(t^2) + I(t^4) + I(K0^2) + t + K0 + I(t^3) + I(t*K0) + I(t^2*K0))
cosi <- k*(k+1)/2
cosi <- cosi-1
R2 <- summary(artificial)$r.squared
n*R2
kvantil <- qchisq(0.95,df=cosi-1)
kvantil

x <- seq(0,40,by=0.1)
y <- dchisq(x,df=cosi-1)
plot(x,y,type="l",xlab="",ylab="")
points(n*R2,0,cex=2,pch=20,col="blue")
points(kvantil,0,cex=2,pch=20)#,col="red")

#exaktny test
RSS <- summary(artificial)$sigma^2*(n-cosi)
artificialsub <- lm(epsilonHAT^2 ~ 1)
RSSsub <- summary(artificialsub)$sigma^2*(n-1)
Fprime <- ( (RSSsub-RSS)/(cosi-1) ) / ( RSS/(n-cosi) )
Fprime
qf(0.95,df1=cosi-1,df2=(n-cosi))


detach(investicie)

#######################################################################################################

letenky <- read.table("07_letenky.txt",header=TRUE)
attach(letenky)
letenky

model <- lm(cena ~ vzdialenost + predchadzajucilet)
betaHAT <- model$coeff
n <- length(cena)
k <- 3
Y <- cena
X <- cbind(rep(1,n),vzdialenost,predchadzajucilet)
YHAT <- X%*%betaHAT
epsilonHAT <- Y-YHAT

#test pomocou R^2 (asymptoticky)
artificial <- lm(epsilonHAT^2 ~ I(vzdialenost^2) + I(predchadzajucilet^2) + vzdialenost + predchadzajucilet + I(vzdialenost*predchadzajucilet))
cosi <- k*(k+1)/2
R2 <- summary(artificial)$r.squared
n*R2
kvantil <- qchisq(0.95,df=cosi-1)
kvantil

x <- seq(0,30,by=0.1)
y <- dchisq(x,df=cosi-1)
plot(x,y,type="l",xlab="",ylab="")
points(n*R2,0,cex=2,pch=20,col="blue")
points(kvantil,0,cex=2,pch=20)#,col="red")

#exaktny test
RSS <- summary(artificial)$sigma^2*(n-cosi)
artificialsub <- lm(epsilonHAT^2 ~ 1)
RSSsub <- summary(artificialsub)$sigma^2*(n-1)
Fprime <- ( (RSSsub-RSS)/(cosi-1) ) / ( RSS/(n-cosi) )
Fprime
qf(0.95,df1=cosi-1,df2=(n-cosi))

detach(letenky)
