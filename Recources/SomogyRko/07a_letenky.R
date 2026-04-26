################# Nacitame data a spravime zakladne odhady ####################
setwd("C:/Users/Pali/Desktop/dAPM/teaching/ekonometria/2026/R/7")
letenky <- read.table("07_letenky.txt", header=TRUE)
head(letenky)
cena <- letenky[,1]
vzdial <- letenky[,2]
predch <- letenky[,3]

n <- length(cena)
k <- 3

MODEL <- lm(cena~vzdial+predch)

betaHAT <- MODEL$coeff
summary(MODEL)
s2 <- (summary(MODEL)$sigma)^2

X <- cbind(rep(1,n),vzdial,predch)


###################### Test normality ##############################

#rezidua
epsilonHAT <- cena - X%*%betaHAT

ks.test(epsilonHAT,"pnorm", mean=0,sd=sqrt(var(epsilonHAT)))

qqnorm(epsilonHAT)
qqline(epsilonHAT)

##################  Elipsoid spolahlivosti pre beta z R^3 ##########
#install.packages("rgl")
library(rgl)

# cislo z predpisu pre elipsoid
K95 <- k*s2*qf(0.95,df1=k,df2=(n-k)) # prava strana zapisu

# delenie parametrickeho priestoru
r1 <- betaHAT[1]/10
r2 <- betaHAT[2]/20
r3 <- betaHAT[3]*5
del <- 20

beta0seq <- seq(from=betaHAT[1]-r1, to=betaHAT[1]+r1, by=(r1/del))
beta1seq <- seq(from=betaHAT[2]-r2, to=betaHAT[2]+r2, by=(r2/del))
beta2seq <- seq(from=betaHAT[3]-r3, to=betaHAT[3]+r3, by=(r3/del))


# ktore bety patria do elipsoidu?
x<-y<-z<-NULL
for(i in 1:length(beta0seq)){
  for(j in 1:length(beta1seq)){
     for(l in 1:length(beta2seq)){
         if( t((betaHAT - c(beta0seq[i],beta1seq[j],beta2seq[l])))%*%t(X)%*%X%*%(betaHAT - c(beta0seq[i],beta1seq[j],beta2seq[l])) < K95 ){
            x <- c(x,beta0seq[i])
            y <- c(y,beta1seq[j])
            z <- c(z,beta2seq[l])
   }}}}

# obrazok
open3d()
plot3d(cbind(x,y,z),col = "blue",xlab="beta_0",ylab="beta_1",zlab="beta_2",axes=TRUE,
       xlim=c(betaHAT[1]-r1,betaHAT[1]+r1), ylim=c(betaHAT[2]-r2,betaHAT[2]+r2), zlim=c(betaHAT[3]-r3,betaHAT[3]+r3))
plot3d(betaHAT[1],betaHAT[2],betaHAT[3],add=TRUE,col="red",size=10)


###################### Test vyznamnosti regresie ############

# statistika F
R <- rbind(c(0,1,0),c(0,0,1))
R
r <- c(0,0)
q <- 2

F <- (t(R%*%betaHAT-r)%*% solve(R%*% solve(t(X)%*%X)%*%t(R)) %*%(R%*%betaHAT-r))/(q*s2)
F

# kvantil
qf(0.95,df1=q,df2=(n-k))
# p-value
1 - pf(F,df1=q,df2=(n-k))

# priamo v Rku
summary(MODEL)

######################  H0: beta2=0 ########################

# statistika F
R <- t(c(0,0,1))
R
r <- 0
q <- 1

F <- (1/q)*(t(R%*%betaHAT-r)%*% solve(R%*% solve(t(X)%*%X)%*%t(R)) %*%(R%*%betaHAT-r))/s2
F

# statistika F'
subMODEL <- lm(cena~vzdial)
s2sub <- (summary(subMODEL)$sigma)^2

RSS <- (n-k)*s2
RSSsub <- (n-2)*s2sub

Fprime <- (1/q)*(RSSsub-RSS)/(RSS/(n-k))
Fprime

# statistika F''
R2 <- summary(MODEL)$r.squared
R2sub <- summary(subMODEL)$r.squared

Fprimeprime <- (1/q)*(R2-R2sub)/((1-R2)/(n-k))
Fprimeprime


# H0 nezamietame ak F je menej ako krit. hodnota (kvantil) Fisherovho rozdelenia s q a n-k stupnami volnosti
qf(0.95,q,n-k)

# p-value
1 - pf(F,df1=q,df2=(n-k))

#=> H0 nezamietame, parameter beta2 NIE je signifikantny

# statistika t: test hypotezy o kontraste
a <- c(0,0,1)
r <- 0

t<-(t(a)%*%betaHAT-r)/sqrt(t(a)%*%solve(t(X)%*%X)%*%a*s2)
t
t^2
# H0 zamietame ak T je prilis kladna alebo prilis zaporna
qt(0.975,df=(n-k))
qt(0.025,df=(n-k))

x <- seq(-5,5,by=0.01)
plot(x,dt(x,df=(n-k)),type="l", main="hustota t-rozdelenia (n-k)", ylab="", xlab="")

# p-value
2*(1 - pt(t,df=(n-k)))
1 - pf(t^2,df1=q,df2=(n-k))

# priamo v Rku
summary(MODEL)

# hypotezu H0 sme nezamietli, dalej staci subMODEL

###################### elipsa spolahlivosti pre beta z R^2 v subMODELI  ########################
betaHATsub <- subMODEL$coeff
X <- cbind(rep(1,n),vzdial)

# cisla z predpisu pre elipsoid pre alfa=0.05 a alfa=0.01
K95 <- 2*s2sub*qf(0.95,df1=2,df2=(n-2))
K99 <- 2*s2sub*qf(0.99,df1=2,df2=(n-2))

# delenie parametrickeho priestoru
r1 <- betaHATsub[1]/20
r2 <- betaHATsub[2]/20
del <- 150

beta0seq <- seq(from=betaHATsub[1]-r1, to=betaHATsub[1]+r1,by=(r1/del))
beta1seq <- seq(from=betaHATsub[2]-r2, to=betaHATsub[2]+r2,by=(r2/del))

x95<-y95<-x99<-y99<-NULL
for(i in 1:length(beta0seq)){
  for(j in 1:length(beta1seq)){
     if( t((betaHATsub - c(beta0seq[i],beta1seq[j])))%*%t(X)%*%X%*%((betaHATsub - c(beta0seq[i],beta1seq[j]))) < K95 ){
           x95 <- c(x95,beta0seq[i])
           y95 <- c(y95,beta1seq[j])}
     if( t((betaHATsub - c(beta0seq[i],beta1seq[j])))%*%t(X)%*%X%*%((betaHATsub - c(beta0seq[i],beta1seq[j]))) < K99 ){
           x99 <- c(x99,beta0seq[i])
           y99 <- c(y99,beta1seq[j])}
}}

# obrazok: 2 oblasti spolahlivosti, ktora je vacsia?

plot(x99,y99,pch=19,col="grey", xlab="beta_0",ylab="beta_1",
     xlim=c(betaHATsub[1]-r1,betaHATsub[1]+r1), ylim=c(betaHATsub[2]-r2,betaHATsub[2]+r2))
points(x95,y95,col="gray55",pch=19)
points(betaHATsub[1],betaHATsub[2],pch=19,col="red")
points(betaHAT[1],betaHAT[2],pch=19,col="blue")


########################### dalsie intervalove odhady ######################################

alpha <- 0.05
### (este raz) elipsoid pre alfa=0.05
plot(x95,y95,pch=16,col="grey", xlab="beta_0",ylab="beta_1")
points(betaHATsub[1],betaHATsub[2],pch=19,col="red")

# oramujeme elipsoid
points(c(min(x95),max(x95)),c(min(y95),min(y95)),lwd=2,type="l")
points(c(min(x95),max(x95)),c(max(y95),max(y95)),lwd=2,type="l")
points(c(min(x95),min(x95)),c(min(y95),max(y95)),lwd=2,type="l")
points(c(max(x95),max(x95)),c(min(y95),max(y95)),lwd=2,type="l")
   # pp. aspon 95%

### IS pre beta_0 a beta_1 zvlast (IS pre kontrast a=(1,0)^T resp a=(0,1)^T)

# cisla z predpisu
K0 <- qt(1-alpha/2,df=(n-2))*sqrt(s2sub*solve(t(X)%*%X)[1,1])
K1 <- qt(1-alpha/2,df=(n-2))*sqrt(s2sub*solve(t(X)%*%X)[2,2])

Lbeta0 <- betaHATsub[1]-K0
Ubeta0 <- betaHATsub[1]+K0

Lbeta1 <- betaHATsub[2]-K1
Ubeta1 <- betaHATsub[2]+K1

points(c(Lbeta0,Ubeta0),c(betaHATsub[2],betaHATsub[2]),type="l",lwd=2,col="red")
points(c(betaHATsub[1],betaHATsub[1]),c(Lbeta1,Ubeta1),type="l",lwd=2,col="red")

# obdlznik na zaklade IS
points(c(Lbeta0,Ubeta0),c(Lbeta1,Lbeta1),type="l",lty="dashed",lwd=2,col="blue")
points(c(Lbeta0,Ubeta0),c(Ubeta1,Ubeta1),type="l",lty="dashed",lwd=2,col="blue")
points(c(Lbeta0,Lbeta0),c(Lbeta1,Ubeta1),type="l",lty="dashed",lwd=2,col="blue")
points(c(Ubeta0,Ubeta0),c(Lbeta1,Ubeta1),type="l",lty="dashed",lwd=2,col="blue")
   # nema pp. 95%

### Bonferroniho metoda pre simultanne IS

B0 <- qt((1-alpha/(2*2)),df=(n-2))*sqrt(s2sub*solve(t(X)%*%X)[1,1])
B1 <- qt((1-alpha/(2*2)),df=(n-2))*sqrt(s2sub*solve(t(X)%*%X)[2,2])

Lbeta0_Bonf <- betaHATsub[1]-B0
Ubeta0_Bonf <- betaHATsub[1]+B0

Lbeta1_Bonf <- betaHATsub[2]-B1
Ubeta1_Bonf <- betaHATsub[2]+B1

points(c(Lbeta0_Bonf,Ubeta0_Bonf),c(Lbeta1_Bonf,Lbeta1_Bonf),type="l",lwd=2,col="green")
points(c(Lbeta0_Bonf,Ubeta0_Bonf),c(Ubeta1_Bonf,Ubeta1_Bonf),type="l",lwd=2,col="green")
points(c(Lbeta0_Bonf,Lbeta0_Bonf),c(Lbeta1_Bonf,Ubeta1_Bonf),type="l",lwd=2,col="green")
points(c(Ubeta0_Bonf,Ubeta0_Bonf),c(Lbeta1_Bonf,Ubeta1_Bonf),type="l",lwd=2,col="green")
    
# pp. aspon 95%

############### Predikcny interval ####################

vzdialenost <- 19.5768
a <- c(1,vzdialenost)
Yhat <- t(a)%*%betaHATsub
Yhat

LPI <- t(a)%*%betaHATsub-sqrt(s2sub)*qt(1-alpha/2,df=n-2)*sqrt(1+t(a)%*%solve(t(X)%*%X)%*%a)
UPI <- t(a)%*%betaHATsub+sqrt(s2sub)*qt(1-alpha/2,df=n-2)*sqrt(1+t(a)%*%solve(t(X)%*%X)%*%a)
LPI
UPI

LPI <- t(a)%*%betaHATsub-sqrt(s2sub)*qt(1-alpha/2,df=n-2)*sqrt(t(a)%*%solve(t(X)%*%X)%*%a)
UPI <- t(a)%*%betaHATsub+sqrt(s2sub)*qt(1-alpha/2,df=n-2)*sqrt(t(a)%*%solve(t(X)%*%X)%*%a)
LPI
UPI
