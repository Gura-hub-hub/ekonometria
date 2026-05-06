#nacitanie dat
setwd("C:/Users/Pali/Desktop/dAPM/teaching/ekonometria/2026/R/10")
uvery <- read.table("10_uvery.txt",header=TRUE)
attach(uvery)
head(uvery)

#vytvorenie modelu 
model <- lm(Hypoteka~Nehnutelnost+Prijem)		 #vytvorime model ako doteraz a pozrieme do summary
summary(model)								 #Rko zistilo dummy premenne, ale vsimnime si, ze v intercepte je Byt
Nehnutelnost <- relevel(as.factor(Nehnutelnost), ref = "D") #do interceptu pojde Dom namiesto Bytu
model <- lm(Hypoteka~Nehnutelnost+Prijem)				
summary(model)								 #uz je to tak ako chceme

#priprava na artificial model
betaHAT <- model$coeff
k <- length(betaHAT)
n <- length(Hypoteka)
Y <- Hypoteka		
Byt <- 1*(Nehnutelnost=="B")				#1* urobi z logickych hodnot nuly a jednotky
Pozemok <- 1*(Nehnutelnost=="P")			
X <- cbind(rep(1,n),Byt,Pozemok,Prijem)		#Dom nedame, lebo je schovany v intercepte			
YHAT <- X%*%betaHAT
epsilonHAT <- Y-YHAT

#Whiteov asymptoticky test H0: chyby merani su homoskedasticke 
artificial<-lm(epsilonHAT^2 ~ Byt + Pozemok + I(Prijem^2)		
			+ I(Prijem) 						
			+ I(Byt*Prijem) 						
			+ I(Pozemok*Prijem) )					
summary(artificial)
cosi<-k*(k+1)/2									
cosi<-cosi-2-1									
R2<-summary(artificial)$r.squared
n*R2											#testovacia statistika
kvantil<-qchisq(0.95,df=cosi-1)
kvantil										#H0 nezamietame

#vykreslenie situacie pri testovani hypotezy o homoskedasticite
x<-seq(0,30,by=0.1)		
y<-dchisq(x,df=cosi-1)
plot(x,y,type="l",xlab="",ylab="")
points(n*R2,0,cex=2,pch=20,col="blue")
points(kvantil,0,cex=2,pch=20)

detach(uvery)

