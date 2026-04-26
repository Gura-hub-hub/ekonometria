setwd("C:/Users/Pali/Desktop/dAPM/teaching/ekonometria/2026/R/7")

domy <- read.table("07_houses.csv", header=TRUE, sep=";")

head(domy)

# faktorove/kvalitativne premenne: dealer, resale
table(domy$dealer)
table(domy$resale)

# model
m1 <- lm(cena ~ dealer+izby+rozloha+resale, data = domy, x = TRUE)
summary(m1)
   # Rko spoznalo faktorove premenne a vytvorilo dummy premenne
   # do interceptu skrylo "Builder", "No"

# prislusna X
X <- m1$x
head(domy)
head(X)

# ak chcem, aby Rko bralo nejaku premennu ako faktorovu:
# as.factor()
m1b <- lm(cena ~ dealer+as.factor(izby)+rozloha+resale, data = domy, x = TRUE)
summary(m1b)
   # kazda namerana uroven izieb (okrem prvej) je teraz samostatna dummy prem.
   # (to samozrejme standardne nechceme)

###

# H0: "dealer" nema signifikantny vplyv

# Pomocou F'

q <- 2

n <- nrow(X)
k <- ncol(X)

s2 <- (summary(m1)$sigma)^2
RSS <- (n-k)*s2

# submodel
mSub <- lm(cena ~ izby+rozloha+resale, data = domy)
ksub <- length(mSub$coef)
s2sub <- (summary(mSub)$sigma)^2
RSSsub <- (n-ksub)*s2sub

Fprime <- (1/q)*(RSSsub-RSS)/(RSS/(n-k))
Fprime

# krit. hodnota
qf(0.95, q, n-k)

# p-value
1 - pf(Fprime, df1=q, df2=(n-k))

# priamo v Rku: funkcia anova()
anova(mSub, m1)

n - k    # Res.Df
q        # Df
Fprime   # F

###

# Interakcie

m2 <- lm(cena ~ dealer + izby + rozloha + resale + dealer:resale + resale:rozloha, 
         data = domy, x = TRUE)
summary(m2)
   # dealer:resale: ak chceme vyjadrit, ze vplyv dealera je rozny podla toho, 
   #    ci dom je opakovane predany (resp. naopak)
   # resale:rozloha: ... vplyv rozlohy je rozny podla toho, ci je dom opakovane predany

# to iste:
m2b <- lm(cena ~  izby + dealer*resale + resale*rozloha, 
         data = domy, x = TRUE)
summary(m2b)
   # x*y zahrnie x, y aj interakciu

# su interakcie dealer:resale signifikantne? (H0: prislusne bety su nulove)
m2sub <- lm(cena ~ dealer + izby + rozloha + resale + resale:rozloha, 
         data = domy, x = TRUE)
anova(m2sub, m2)

