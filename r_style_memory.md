# R Code Style Memory — Ekonometria LS 2025/2026
# Zdroj: Recources/02_mzdy.R, 04_byty.R, 07a_letenky.R, 07b_domy.R

---

## 1. ŠTRUKTÚRA SÚBORU

```r
# Vzdy nastavit pracovny priecinok na zaciatku
setwd("C:/Users/.../ekonometria/R/X")   # lomitka "/" nie "\"

# Nacitanie dat
data <- read.table("subor.txt", header=TRUE)
# alebo CSV:
data <- read.table("subor.csv", header=TRUE, sep=";")
data <- read.table("subor.csv", header=TRUE, sep=",", row.names=1)

# Pozriet sa na data
head(data)
data
```

---

## 2. MANUÁLNY OLS ODHAD (maticovo)

```r
Y <- data$zavislaPremenna
n <- length(Y)

# Matica X — vzdy zacina stlpcom jednotiek (intercept)
X <- cbind(rep(1, n), data$x1, data$x2)
# kratsia verzia:
X <- cbind(1, data$x1, data$x2)
k <- ncol(X)   # pocet parametrov (vratane interceptu)

# MNS odhad
beta_hat <- solve(t(X) %*% X) %*% t(X) %*% Y
# alternativa (rychlejsia):
beta_hat <- solve(crossprod(X)) %*% t(X) %*% Y

# Fitted values
Y_hat <- X %*% beta_hat

# Rezidua
epsilon_hat <- Y - Y_hat
sum(epsilon_hat)   # kontrola: musi byt ~0 (ked je intercept)

# Odhad sigma^2
s2 <- sum(epsilon_hat^2) / (n - k)
# ekvivalentne:
s2 <- t(epsilon_hat) %*% epsilon_hat / (n - k)
s2 <- crossprod(epsilon_hat) / (n - k)
```

---

## 3. OLS CEZ lm() — SKRÁTENÁ CESTA

```r
# Zakladny model
model <- lm(wage ~ ed, data=mzdy)
model <- lm(wage ~ ed + exp, data=mzdy)

# Logaritmicka transformacia
m1 <- lm(cena ~ log(rozloha), data=byty, x=TRUE)
   # x=TRUE ... ulozi maticu X do m1$x

# Kvadraticke cleny (pouzit I())
m3 <- lm(wage ~ ed + I(ed^2) + exp, data=mzdy)

# Faktorove premenne — Rko automaticky vytvori dummy premenne
m1 <- lm(cena ~ dealer + izby + rozloha + resale, data=domy, x=TRUE)
# Prinudena konverzia na faktor:
m1b <- lm(cena ~ as.factor(izby), data=domy)

# Interakcie
m2 <- lm(cena ~ dealer + izby + rozloha + resale + dealer:resale, data=domy)
# x*y = x + y + x:y
m2b <- lm(cena ~ izby + dealer*resale + resale*rozloha, data=domy)

summary(model)   # hlavny vystup: koeficienty, std chyby, t-test, R2
```

---

## 4. EXTRAKCIA HODNÔT Z lm() A summary()

```r
# Z objektu model:
beta_hat   <- model$coef           # vektor koeficientov
Y_hat      <- fitted(model)        # fitted values (= fitted.values())
epsilon_hat <- residuals(model)    # rezidua
X          <- model$x              # matica X (len ak x=TRUE v lm())

# Z summary():
attributes(summary(model))        # co vsetko je ulozene

R2          <- summary(model)$r.squared
adj_R2      <- summary(model)$adj.r.squared
s2          <- summary(model)$sigma^2   # odhad sigma^2
sqrt(s2)    # = Residual standard error zo summary

RSS <- (n - k) * s2
ESS <- sum((Y_hat - mean(Y_hat))^2)
TSS <- sum((Y - mean(Y))^2)
# kontrola: RSS + ESS == TSS
```

---

## 5. RSS, ESS, TSS, R²

```r
RSS <- sum(epsilon_hat^2)
ESS <- sum((Y_hat - mean(Y_hat))^2)
TSS <- sum((Y - mean(Y))^2)
RSS + ESS   # = TSS (kontrola)

R2 <- ESS / TSS
R2 <- 1 - RSS / TSS   # ekvivalentne

adjusted_R2 <- 1 - (RSS / (n - k)) / (TSS / (n - 1))
```

---

## 6. TESTOVANIE HYPOTÉZ — F-TEST (cez R maticu)

```r
# Vseobecny tvar: H0: R*beta = r
# Test vyznamnosti regresie: H0: beta1=beta2=0
R <- rbind(c(0,1,0), c(0,0,1))
r <- c(0, 0)
q <- 2   # pocet obmedzeni (= nrow(R))

F <- (t(R %*% betaHAT - r) %*%
      solve(R %*% solve(t(X) %*% X) %*% t(R)) %*%
      (R %*% betaHAT - r)) / (q * s2)
F

# Kritcka hodnota a p-value
qf(0.95, df1=q, df2=(n-k))
1 - pf(F, df1=q, df2=(n-k))
```

---

## 7. TESTOVANIE HYPOTÉZ — F-TEST (cez RSS submodelu)

```r
# Submodel (obmedzeny model)
subMODEL <- lm(cena ~ vzdial, data=data)   # bez predch
s2sub  <- summary(subMODEL)$sigma^2
RSSsub <- (n - 2) * s2sub

# Uplny model
RSS <- (n - k) * s2

# F' statistika
Fprime <- (1/q) * (RSSsub - RSS) / (RSS / (n - k))
Fprime

# Ekvivalentne cez R2
R2    <- summary(MODEL)$r.squared
R2sub <- summary(subMODEL)$r.squared
Fprimeprime <- (1/q) * (R2 - R2sub) / ((1 - R2) / (n - k))

# Rychla alternativa v Rku:
anova(subMODEL, MODEL)   # priamo da F a p-value

# Kritcka hodnota
qf(0.95, q, n-k)
1 - pf(Fprime, df1=q, df2=(n-k))
```

---

## 8. T-TEST (test hypotézy o kontraste)

```r
# H0: a^T * beta = r
a <- c(0, 0, 1)   # kontrast (napr. beta2=0)
r <- 0

t_stat <- (t(a) %*% betaHAT - r) /
           sqrt(t(a) %*% solve(t(X) %*% X) %*% a * s2)
t_stat
t_stat^2   # = F pre q=1

# Kriticke hodnoty
qt(0.975, df=(n-k))
qt(0.025, df=(n-k))

# p-value (obojstranny test)
2 * (1 - pt(t_stat, df=(n-k)))
1 - pf(t_stat^2, df1=1, df2=(n-k))   # ekvivalentne

# Priamo v Rku: stlpec "t value" a "Pr(>|t|)" v summary(model)
```

---

## 9. INTERVALY SPOĽAHLIVOSTI

```r
alpha <- 0.05

# IS pre kontrast a^T*beta (napr. pre beta_j: a=(0,...,1,...,0))
# Pre beta_0: a=(1,0)^T => solve(t(X)%*%X)[1,1]
K0 <- qt(1-alpha/2, df=(n-k)) * sqrt(s2 * solve(t(X)%*%X)[1,1])
K1 <- qt(1-alpha/2, df=(n-k)) * sqrt(s2 * solve(t(X)%*%X)[2,2])

Lbeta0 <- betaHAT[1] - K0;   Ubeta0 <- betaHAT[1] + K0
Lbeta1 <- betaHAT[2] - K1;   Ubeta1 <- betaHAT[2] + K1

# Bonferroniho simultanne IS (pre m parametrov naraz)
m <- 2   # pocet parametrov
B0 <- qt(1 - alpha/(2*m), df=(n-k)) * sqrt(s2 * solve(t(X)%*%X)[1,1])
B1 <- qt(1 - alpha/(2*m), df=(n-k)) * sqrt(s2 * solve(t(X)%*%X)[2,2])
# => siroky kvantil: 1 - alpha/(2*m) namiesto 1 - alpha/2
```

---

## 10. PREDIKČNÉ INTERVALY

```r
# Novy bod pozorovania
a <- c(1, 19.5768)   # (1, x_new)
Yhat <- t(a) %*% betaHAT   # bodova predikcna hodnota

# Predikcny interval (obsahuje +1 pod odmocninou)
LPI <- t(a)%*%betaHAT - sqrt(s2)*qt(1-alpha/2, df=n-k)*sqrt(1 + t(a)%*%solve(t(X)%*%X)%*%a)
UPI <- t(a)%*%betaHAT + sqrt(s2)*qt(1-alpha/2, df=n-k)*sqrt(1 + t(a)%*%solve(t(X)%*%X)%*%a)

# IS pre strednu hodnotu E[Y|x] (bez +1)
LCI <- t(a)%*%betaHAT - sqrt(s2)*qt(1-alpha/2, df=n-k)*sqrt(t(a)%*%solve(t(X)%*%X)%*%a)
UCI <- t(a)%*%betaHAT + sqrt(s2)*qt(1-alpha/2, df=n-k)*sqrt(t(a)%*%solve(t(X)%*%X)%*%a)
# PI je vzdy sirsi ako CI (o ten +1 pod odmocninou)
```

---

## 11. ELIPSOID SPOĽAHLIVOSTI (pre beta)

```r
library(rgl)   # 3D vizualizacia

# Hranicna hodnota elipsoidu
K95 <- k * s2 * qf(0.95, df1=k, df2=(n-k))
K99 <- k * s2 * qf(0.99, df1=k, df2=(n-k))

# Grid hodnot okolo betaHAT
r1 <- betaHAT[1]/20;  r2 <- betaHAT[2]/20
del <- 150
beta0seq <- seq(betaHAT[1]-r1, betaHAT[1]+r1, by=r1/del)
beta1seq <- seq(betaHAT[2]-r2, betaHAT[2]+r2, by=r2/del)

# Vypocet: ktore body patria do elipsoidu?
x95 <- y95 <- NULL
for(i in 1:length(beta0seq)){
  for(j in 1:length(beta1seq)){
    b <- c(beta0seq[i], beta1seq[j])
    if(t(betaHAT-b) %*% t(X)%*%X %*% (betaHAT-b) < K95){
      x95 <- c(x95, beta0seq[i])
      y95 <- c(y95, beta1seq[j])
    }
  }
}

# Obrazok (2D elipsa)
plot(x95, y95, pch=19, col="grey", xlab="beta_0", ylab="beta_1")
points(betaHAT[1], betaHAT[2], pch=19, col="red")
```

---

## 12. TEST NORMALITY REZIDUÍ

```r
# Kolmogorov-Smirnov test
epsilonHAT <- Y - X %*% betaHAT
ks.test(epsilonHAT, "pnorm", mean=0, sd=sqrt(var(epsilonHAT)))
# H0: rezidua maju normalne rozdelenie N(0, sigma^2)
# p >= 0.05 => H0 nezamietame

# QQ plot (vizualna kontrola normality)
qqnorm(epsilonHAT)
qqline(epsilonHAT)
```

---

## 13. AIC

```r
# Akaike Information Criterion (manualne)
AIC1 <- 2*(2/n) + log(RSS_m1/n)
AIC2 <- 2*(3/n) + log(RSS_m2/n)
# mensie AIC = lepsi model
which(c(AIC1, AIC2) == min(c(AIC1, AIC2)))
```

---

## 14. VIZUALIZÁCIA

```r
# Zakladny scatter plot
plot(data$x, data$y, cex=0.8)   # cex: velkost bodiek

# Regresna priamka
abline(model$coef, col="red", lwd=2)
abline(c(intercept, slope), col="red", lwd=2)

# Regresna krivka (pri transformacii)
osX <- seq(min(x), max(x), by=0.1)
osY <- beta_hat[1] + beta_hat[2]*log(osX)
lines(osX, osY, col="red", lwd=2)

# Pridanie bodov do existujuceho grafu
points(x, Y_hat, col="blue", pch=19)    # plne kruhy
points(x, Y_hat, col="green", pch=0)   # prazdne stvorce
points(x, Y_hat, col="blue", pch=2)    # trojuholniky

# Osa x vs osa y pri porovnani fitted vs skutocne
plot(Y_hat, Y, cex=0.8, asp=1)
abline(c(0,1), lwd=2, col="red")   # idealna priamka y=x

# Hustota t-rozdelenia
x <- seq(-5, 5, by=0.01)
plot(x, dt(x, df=(n-k)), type="l", main="hustota t-rozdelenia", ylab="", xlab="")

# Viacero okien
windows()        # novy graficky okno
graphics.off()   # zatvorenie vsetkych okien
```

---

## 15. KONVENCIE NÁZVOV PREMENNÝCH

| Premenná | Popis |
|----------|-------|
| `beta_hat` / `betaHAT` | OLS odhad vektora beta |
| `epsilon_hat` / `epsilonHAT` | vektor reziduí |
| `Y_hat` / `Yhat` | fitted values |
| `s2` | odhad sigma^2 |
| `n` | počet pozorovaní |
| `k` | počet parametrov (vrátane interceptu) |
| `q` | počet obmedzení pri testovaní |
| `RSS`, `ESS`, `TSS` | súčty štvorcov |
| `R2`, `adjusted_R2` | koeficienty determinácie |
| `LPI`, `UPI` | dolná/horná hranica predikčného intervalu |
| `Lbeta0`, `Ubeta0` | dolná/horná hranica IS pre beta_0 |
| `subMODEL`, `mSub` | obmedzený (submodel) |

---

## 16. ČASTÉ VZORY

```r
# Nacitanie dat roznymi sposobmi
read.table("file.txt")                          # bez hlavicky
read.table("file.txt", header=TRUE)             # s hlavickou
read.table("file.csv", header=TRUE, sep=";")    # CSV s ;
read.table("file.csv", header=TRUE, sep=",", row.names=1)

# Porovnanie dvoch modelov
anova(subMODEL, model)   # priamo F-test a p-value

# Matice/vektory
solve(A)          # inverzna matica
t(A)              # transponovana matica
A %*% B           # maticove nasobenie
crossprod(A)      # = t(A) %*% A (rychlejsie)
cbind(1, x1, x2)  # spajanie stlpcov
rbind(r1, r2)     # spajanie riadkov
dim(X)[2]         # = ncol(X)

# Kvanily rozdeleni
qt(0.975, df=n-k)          # t-rozdelenie
qf(0.95, df1=q, df2=n-k)   # F-rozdelenie

# p-values
2*(1 - pt(t_stat, df=n-k))          # obojstranny t-test
1 - pf(F_stat, df1=q, df2=n-k)     # F-test
```

---

## 17. BALÍČKY POUŽÍVANÉ V KURZE

```r
library(rgl)                      # 3D vizualizacia elipsoidu
microbenchmark::microbenchmark()  # porovnanie rychlosti vypoctov
# install.packages("rgl")
```

---

## 18. ŠTÝL KOMENTÁROV

- Komentáre v **slovenčine** alebo angličtine
- Sekcie oddelené: `###` alebo `####...####`
- Inline komentáre za `#` s medzerou
- Výstupy zo `summary()` komentované napr.: `#... Estimate`, `#... Residual standard error`
- Pri dôležitých výsledkoch: `# => H0 nezamietame` / `# => H0 zamietame`
