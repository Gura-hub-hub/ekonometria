# set working directory (nastavit pracovny priecinok)
setwd("C:/Users/Pali/Desktop/dAPM/teaching/ekonometria/2026/R/2")
# vsimnite si lomitka typu "/", nie "\"

# nacitame data
mzdy <- read.table("02_mzdy.txt")
# zdroj: R balicek "Ecdat" - Wages data

# pozrime sa na data
mzdy
head(mzdy)

# vykreslenie
plot(mzdy$ed, mzdy$wage)
plot(mzdy$ed, mzdy$wage, cex=0.8)
   # cex zvacsuje/zmensuje velkosti bodiek (default=1)

# ked sa vam nechce stale pisat mzdy$, mozete pouzit attach()
attach(mzdy)

ed
plot(ed, wage)

# na konci prace s attachnutymi datami treba dat detach
detach(mzdy)

# teraz to uz nefunguje
ed              # error
plot(ed, wage)  # error

# praca s attachnutymi datami moze vytvarat zmatky, takze to nepouzijeme
# Napr.
# attach(mzdy)
# ed <- exp(ed)
# co je teraz ed a co je mzdy$ed?
# ed
# mzdy$ed
# detach(mzdy)

### MNS odhad - manualne

Y <- mzdy$wage
n <- length(Y)
n

X <- cbind(rep(1,n), mzdy$ed) # column bind, existuje aj rbind: row bind
# to iste: X <- cbind(1,mzdy$ed)
X
x # case-sensitivity!
k <- dim(X)[2]
# to iste: k <- ncol(X)
k

# MNS odhad
beta_hat <- solve(t(X) %*% X) %*% t(X) %*% Y  # solve() vypluje inverznu maticu
beta_hat

# vykreslenie regresnej krivky
plot(mzdy$ed, mzdy$wage, cex=0.8)
osX <- seq(from=min(mzdy$ed), to=max(mzdy$ed), by=0.1)
osY <- beta_hat[1] + beta_hat[2]*osX
lines(osX, osY, col="red", lwd=2)

# alternativne
plot(mzdy$ed, mzdy$wage, cex=0.8)
abline(beta_hat, col="red", lwd=2) # y = a + bx

# vykreslenie fitted values
Y_hat <- X %*% beta_hat
points(mzdy$ed, Y_hat, col="green", pch=19)

# rezidua
epsilon_hat <- Y-Y_hat
sum(epsilon_hat) # na prvom cviceni (pozn. 2) sme ukazovali, ze ked v modeli je intercept, toto je nula

# odhad pre sigma^2
s2 <- t(epsilon_hat) %*% (epsilon_hat) / (n-k)
s2
# inak: sum(epsilon_hat^2)/(n-k)
# este inak: crossprod(epsilon_hat)/(n-k)

microbenchmark::microbenchmark(t(epsilon_hat) %*% (epsilon_hat) / (n-k))
microbenchmark::microbenchmark(sum(epsilon_hat^2)/(n-k))
microbenchmark::microbenchmark(crossprod(epsilon_hat)/(n-k))

### MNS vypocet na dva riadky
model <- lm(mzdy$wage ~ mzdy$ed)
model
summary(model)

beta_hat    #... Coefficients
sqrt(s2)    #... Residual standard error

# alternativne (namiesto attach())
model <- lm(wage ~ ed, data=mzdy)

# vykreslenie
model$coef
plot(mzdy$ed, mzdy$wage)
abline(model$coef, lwd=2, col="red")

### Viac premennych
model2 <- lm(wage ~ ed + exp, data=mzdy)
summary(model2)

y_hat <- fitted(model2)
   # fitted values

# odhadovane vs skutocne ypsilony
plot(y_hat, mzdy$wage, cex=0.8, asp=1)  
abline(c(0,1), lwd=2, col="red")  # priamka, na ktorej nastava rovnost
points(fitted(model), mzdy$wage, col="blue", pch=2, cex=0.8)  # pre povodny model

model3 <- lm(wage ~ ed + I(ed^2) + exp, data=mzdy)
   # aj to je linearna regresia
   # I(): "as is" - aby si to Rko neinterpretovalo po svojom
summary(model3)

# model2 vs model3
plot(y_hat, mzdy$wage, cex=0.8, asp=1)  
abline(c(0,1), lwd=2, col="red")
points(fitted(model3), mzdy$wage, col="green", pch=0, cex=0.8)

