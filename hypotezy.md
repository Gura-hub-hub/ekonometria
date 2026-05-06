# Ekonometria – projekt LS 2025/2026

Termín odovzdania: **10.5.2026 23:59**  
Email: pal.somogyi@fmph.uniba.sk | predmet: `[EM PROJEKT] Priezvisko1, Priezvisko2, Priezvisko3`  
Súbory: dáta (.txt/.csv), R skript (.r/.txt), dokument (.pdf, max 4 strany)

---

## Povinná úloha

Odhady β a σ², interpretácie, R², RSS, ESS, TSS.

```r
model <- lm(Y ~ x1 + x2, data = data)
summary(model)

n <- nrow(data); k <- length(model$coef)
betaHAT <- model$coef
YHAT    <- fitted(model)
epsilonHAT <- resid(model)

RSS <- sum(epsilonHAT^2)
ESS <- sum((YHAT - mean(Y))^2)
TSS <- sum((Y - mean(Y))^2)        # TSS = ESS + RSS
R2  <- summary(model)$r.squared    # R2 = ESS/TSS
s2  <- summary(model)$sigma^2      # s2 = RSS/(n-k)
```

---

## Kategória A — zvolená: test hypotézy o kontraste

### Test hypotézy o kontraste (t-test)

**H0:** `a'β = r`  (napr. H0: β₂ = 0, alebo H0: β₂ = 1)  
Jeden parameter alebo lineárna kombinácia parametrov.

```r
a <- c(0, 0, 1)   # testujeme beta_2
r <- 0

X <- model.matrix(model)
T <- (t(a) %*% betaHAT - r) / sqrt(s2 * t(a) %*% solve(t(X)%*%X) %*% a)
T

qt(0.975, df = n-k)           # kritická hodnota (dvojstranný test)
2 * (1 - pt(abs(T), df = n-k)) # p-hodnota
```

Zamietame H0 ak `|T| > qt(0.975, n-k)`.  
T² = F (ekvivalentné s F-testom pre q=1).  
`summary(model)` dáva t-štatistiky pre každý parameter priamo.

## Kategória B — zvolená: Bonferroniho simultánne IS

### Bonferroniho simultánne intervaly spoľahlivosti

Chceme IS pre viacero parametrov naraz s celkovou spoľahlivosťou ≥ 1-α.  
Bonferroni: každý jednotlivý IS má hladinu `1 - α/(2·p)`, kde p = počet IS.

```r
alpha <- 0.05
p <- 2   # počet parametrov (napr. beta_0 a beta_1)

covMAT <- s2 * solve(t(X) %*% X)

# pre beta_j: sqrt(covMAT[j,j]) je štandardná chyba
B0 <- qt(1 - alpha/(2*p), df = n-k) * sqrt(covMAT[1,1])
B1 <- qt(1 - alpha/(2*p), df = n-k) * sqrt(covMAT[2,2])

c(betaHAT[1] - B0, betaHAT[1] + B0)   # Bonferroni IS pre beta_0
c(betaHAT[2] - B1, betaHAT[2] + B1)   # Bonferroni IS pre beta_1
```

Rozdiel oproti obyčajným IS: používa kvantil `qt(1 - α/(2p))` namiesto `qt(1 - α/2)` → intervaly sú širšie, ale spoľahlivosť dvojice IS je ≥ 95%.


## Kategória C — zvolená: test heteroskedasticity

### Whiteov test heteroskedasticity

**H0:** chyby merania sú homoskedastické (Var(εᵢ) = σ² pre všetky i)

**Postup:**
1. Odhadni model, spočítaj `εhat`
2. Regreduj `εhat²` na regressory, ich štvorce a krížové členy (artificial model)
3. Štatistika `n·R²` ~ χ²(cosi−1) asymptoticky

```r
epsilonHAT <- resid(model)

# pre model Y ~ x1 + x2 (k=3 vrátane interceptu):
artificial <- lm(epsilonHAT^2 ~ I(x1^2) + I(x2^2) + x1 + x2 + I(x1*x2))
cosi <- k*(k+1)/2   # = 6 pre k=3

# asymptotický test (chi²):
stat <- n * summary(artificial)$r.squared
kvantil <- qchisq(0.95, df = cosi - 1)
stat; kvantil   # zamietame H0 ak stat > kvantil

# exaktný test (F'):
RSS_art    <- summary(artificial)$sigma^2 * (n - cosi)
RSS_art0   <- summary(lm(epsilonHAT^2 ~ 1))$sigma^2 * (n - 1)
Fprime <- ((RSS_art0 - RSS_art) / (cosi-1)) / (RSS_art / (n-cosi))
qf(0.95, df1 = cosi-1, df2 = n-cosi)
```

---

## Rýchly prehľad

| Úloha                          | Štatistika | Rozdelenie    |
|-------------------------------|------------|---------------|
| 1 parameter (H0: aβ = r)      | T          | t(n−k)        |
| viac parametrov (H0: Rβ = r)  | F          | F(q, n−k)     |
| porovnanie modelov             | F'         | F(q, n−k)     |
| normalita rezíduí              | KS test    | —             |
| IS pre kontrast                | t kvantil  | t(n−k)        |
| Bonferroni IS (p parametrov)   | t kvantil  | t(n−k), α/2p  |
| predikčný interval             | t kvantil  | t(n−k), +1    |
| homoskedasticita (White)       | n·R²       | χ²(cosi−1)    |
| 1 param., heterosked.          | U          | N(0,1)        |
| viac param., heterosked.       | W          | χ²(q)         |
| Chow forecast                  | F          | F(n2, n1−k)   |
| Chow breakpoint (2 skupiny)    | F          | F(k, n−2k)    |
| CUSUM (α=5%)                   | —          | ±0.948√(n−k)  |
| CUSUMSQ (α=5%)                 | —          | ±0.3238       |
