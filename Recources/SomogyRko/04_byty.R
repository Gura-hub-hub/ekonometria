setwd("C:/Users/Pali/Desktop/dAPM/teaching/ekonometria/2026/R/4")

#nacitame data
byty<-read.table("04_byty.txt",header=TRUE)
head(byty)

attach(byty)

#nakreslime data
plot(rozloha, cena)

### Model 1
### Manualne

X <- cbind(1, log(rozloha))
n <- nrow(X)
k <- ncol(X)
Y <- cena

# MNS
beta_hat <- solve(t(X)%*%X)%*%t(X)%*%Y

# Vykreslenie
#------------

# regresna krivka
osX <- seq(from=min(rozloha), to=max(rozloha), by=0.1)
osY <- beta_hat[1] + beta_hat[2]*log(osX)
lines(osX,osY,col="red",lwd=2)

# fitted values
Y_hat <- X %*% beta_hat
points(rozloha,Y_hat,col="blue",pch=19)

# vykreslenie v zavislosti od log(rozloha) => priamka
plot(log(rozloha), cena)
abline(beta_hat, col="red", lwd=2)
points(log(rozloha), Y_hat,col="blue", pch=19)

# da sa aj tradicne vykreslit
plot(log(rozloha), cena)
osX <- seq(from=min(log(rozloha)), to=max(log(rozloha)), by=0.1)
osY <- beta_hat[1] + beta_hat[2]*osX
lines(osX,osY, col="red", lwd=2)
points(log(rozloha), Y_hat,col="blue", pch=19)

# Vypocty
#--------

# rezidua
epsilon_hat <- Y - Y_hat
sum(epsilon_hat)

# residual sum of squares
RSS <- sum(epsilon_hat^2)
RSS

#odhad pre sigma^2
s2 <- RSS/(n-k)
s2

#explained sum of squares
ESS <- sum((Y_hat - mean(Y_hat))^2)
ESS

#total sum of squares
TSS <- sum((Y - mean(Y))^2)

TSS
RSS + ESS # TSS = RSS + ESS

#coefficient of determination
R2 <- ESS/TSS
R2
R2 <- 1 - RSS/TSS
R2

#adjusted coefficient of detemination
adjusted_R2 <- 1 - (RSS/(n-k))/(TSS/(n-1))
adjusted_R2


### Model 1
### Pomocou funkcii (na par riadkov)

m1 <- lm(cena~log(rozloha), x=TRUE)
   # x=TRUE ... ziskame maticu X
m1

summary(m1)

beta_hat    #... Estimate
sqrt(s2)    #... Residual standard error
R2          #... Multiple R-squared
adjusted_R2 #... Adjusted R-squared

# Ak to potrebujeme z funkcii vytiahnut:

X <- m1$x            # matica X
n <- nrow(X)
k <- ncol(X)
beta_hat <- m1$coef  # MNS odhad

attributes(summary(m1))  # co je v summary ulozene
?summary.lm              # help

R2 <- summary(m1)$r.squared
adjusted_R2 <- summary(m1)$adj.r.squared
s2 <- summary(m1)$sigma^2
RSS <- (n-k)*s2
epsilon_hat2 <- summary(m1)$residuals
epsilon_hat2 - epsilon_hat  # kontrola

Y_hat2 <- fitted(m1)
   # to iste: Y_hat2 <- fitted.values(m1)
Y_hat - Y_hat2
ESS <- sum((Y_hat2 - mean(Y_hat2))^2)
TSS <- sum((Y - mean(Y))^2)

################ Model 2

# na pripomenutie:
summary(m1)

m2 <- lm(cena~log(rozloha)+rozloha)
summary(m2)

# porovnanie s Modelom 1
R2            # v Modeli 2 vzrastlo
adjusted_R2   # v Modeli 2 kleslo

# RSS (kvoli AIC)
summary(m2)$sigma
s_m2 <- summary(m2)$sigma
RSS_m2 <- s_m2^2*(n-3)
RSS_m2

# Model 2b
m2b <- lm(cena~rozloha)
summary(m2b)
   # R2 aj R2adj klesli: suverenne horsi model

# interpretacie koeficientov

### Centrovanie regresorov => lepsia interpretacia
# Model 2c
rozloham <- rozloha - mean(rozloha)
m2c <- lm(cena~rozloham)

m2b
m2c # rovnake bety1

# vykreslenia
plot(rozloha, cena)
abline(m2b$coef,col="red",lwd=2)
windows()
plot(rozloham, cena)
abline(m2c$coef,col="red",lwd=2)
graphics.off()

summary(m2b)
summary(m2c)  # rovnake vsetko okrem interceptu

################ Model 3

#uz aj vzdialenost

m3 <- lm(cena~log(rozloha)+vzdialenost)
summary(m3)
s_m3 <- summary(m3)$sigma
RSS_m3 <- s_m3^2*(n-3)

############### Model 4

m4 <- lm(cena~vzdialenost)
summary(m4)
s_m4 <- summary(m4)$sigma
RSS_m4 <- s_m4^2*(n-2)

############### Akaike information criterion

AIC1<-2*(2/n)+log(RSS/n)
AIC2<-2*(3/n)+log(RSS_m2/n)
AIC3<-2*(3/n)+log(RSS_m3/n)
AIC4<-2*(2/n)+log(RSS_m4/n)

Akaike <- c(AIC1,AIC2,AIC3,AIC4)
Akaike

minAIC<-min(Akaike)
which(Akaike == min(Akaike))

# Model 4 je z pohladu AIC najhorsi - vylucime
# skusme ho vykreslit:
plot(vzdialenost, cena)
abline(m4, col="red", lwd=2)

# ostatne su porovnatelne
# aj ked tesne najlepsi je prvy

############### Zmena mierky
# az po 4.3

m2b <- lm(cena~rozloha)
summary(m2b)

rozlohad <- rozloha*100 # zmena mierky: m^2 -> dm^2

m2d <- lm(cena~rozlohad)
summary(m2d)
# beta1_hat sa zmensila na 1/100

############### Nezabudnut detach
detach(byty)


############### Logaritmy

### Cobb-Douglas produkcna funkcia

cd <- read.table("04_cd.csv", header=TRUE, sep=",", row.names=1)

head(cd)
plot(cd)
m1 <- lm(Y ~ K+L, data=cd)
m2 <- lm(log(Y) ~ log(K)+log(L), data=cd)  # logaritmovane data

summary(m1)
summary(m2)
  # pri log-log modeli je velmi mierne lepsie R2
  # 1% zmena v K sa prejavi v 0.23% zmene v produkcii


