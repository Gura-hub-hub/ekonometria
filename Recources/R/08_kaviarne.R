
#######################################################################################################
#
#				Chow forecast test
#
#######################################################################################################

setwd("C:/Users/Pali/Desktop/dAPM/teaching/ekonometria/2025/R/8")

kaviarne <- read.table("08_kaviarne.txt",header=TRUE)
head(kaviarne)
kaviarne <- kaviarne[sort.list(kaviarne[,2]), ]			# v druhom kole nacitat data a toto nespustit
head(kaviarne)

par <- 0.85 # 85% trenovacich dat a 15% testovacich dat

chow.test <- function(par){
	Y <- kaviarne[,4]
	stolicky <- kaviarne[,1]
	presso <- kaviarne[,2]
	konkurencia <- kaviarne[,3]
	n <- length(Y)
	X <- cbind(rep(1,n),stolicky,presso,konkurencia)
	
	model <- lm(Y~stolicky+presso+konkurencia)

	n1 <- ceiling(n*par)
	X1 <- X[1:n1,]
	Y1 <- Y[1:n1]
	model1 <- lm(Y1~stolicky[1:n1]+presso[1:n1]+konkurencia[1:n1])
	betaHAT1 <- model1$coeff
	k <- length(betaHAT1)

	X2 <- X[(n1+1):n,]
	Y2 <- Y[(n1+1):n]
	YHAT2 <- X2%*%betaHAT1
	n2 <- n-n1

	I <- diag(rep(1,n2))
	s1 <- (summary(model1)$sigma)^2
	F <- ( t(Y2-YHAT2) %*% solve( I+ X2 %*% solve(t(X1)%*%X1) %*% t(X2) ) %*% (Y2-YHAT2) ) / (n2*s1)
	p <- 1-pf(F,n2,n1-k)
	list(n=n, n1=n1, n2=n2, k=k, betaHAT=model$coeff, betaHAT1=betaHAT1, F=F, p=p)
}

CHOW <- chow.test(0.85)		# 85% trenovacich dat a 15% testovacich dat
CHOW
CHOW <- chow.test(0.90)		# 90% trenovacich dat a 10% testovacich dat
CHOW
CHOW <- chow.test(0.95)		# 95% trenovacich dat a 5% testovacich dat
CHOW


#obrazok: hustota F, test. statistika
x <- seq(0,6,by=0.01)
plot(x,df(x,df1=CHOW$n2,df2=(CHOW$n1-CHOW$k)),type="l", main="hustota F-rozdelenia (n2, n1-k)", ylab="", xlab="")
points(CHOW$F,0,pch=19, cex=1, col="red")
text(4,0.6,labels=paste("p-hodnota je ",round(CHOW$p,6)))


