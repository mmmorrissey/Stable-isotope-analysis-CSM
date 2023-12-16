#Zooplankton
getwd()
setwd("C:\\Users\\mmorr\\OneDrive\\Desktop\\Grad School\\Thesis\\Zooplankton\\SIA Results Zoop\\")
Zoop_SIA<-read.csv("Zooplankton SIA.csv")
head(Zoop_SIA)
colnames(Zoop_SIA)
colnames(Zoop_SIA)[5]<-"Distance"
Zoop_SIA["Distance"]
Zoop_SIA$Order[Zoop_SIA$Order == "Calanoid"] <- "Calanoida"


library(ggplot2)
#Wunderbar. Subset the orders
Calanoid_SIA<-subset(Zoop_SIA,Order=="Calanoid")
Cladocera_SIA<-subset(Zoop_SIA,Order=="Cladocera")
#perhaps we should add distance from colony
#ok added. now see if there's relationship between 
#distance from colony & d15N
#-----------------------------------
#d15N
#create LM model, check residuals & fit
##Calanoid lm
Calanoid_lm<-lm(d15N~Distance,data=Calanoid_SIA)
plot(Calanoid_lm) #Pond 1 high leverage point
resNoid<-resid(Calanoid_mod)
fitNoid<-fitted(Calanoid_mod)
plot(fitNoid,resNoid,ylab="Residuals",
     xlab="Fitted values")
hist(resNoid,breaks=5)
lag.plot(resNoid, main="Lag plot, Calanoid d15N residuals",
         diag=FALSE,do.lines=FALSE)
qqnorm(resNoid)
summary.lm(Calanoid_mod)
#0.05844
library(ggplot2)
ggplot(data=Calanoid_SIA,aes(x=Distance,y=d15N)) + 
  geom_point() + 
  theme_bw() + 
  labs(title="Calanoid δ15N",x="Distance from colony (km)",
      y="δ15N (‰)")

##Cladocera lm
Cladocera_lm<-lm(d15N~Distance,data=Cladocera_SIA)
plot(Cladocera_lm) #all points good
resCera<-resid(Cladocera_mod)
fitCera<-fitted(Cladocera_mod)
plot(fitCera,resCera,ylab="Residuals",
     xlab="Fitted values")
hist(resCera,breaks=5)
lag.plot(resCera, main="Lag plot, Cladocera d15N residuals",
         diag=FALSE,do.lines=FALSE)
qqnorm(resCera)
summary.lm(Cladocera_mod)
#0.037901*
ggplot(data=Cladocera_SIA,aes(x=Distance,y=d15N))+
  geom_point() +
  geom_smooth(method="lm") +  
  theme_bw() + 
  labs(title="Cladocera δ15N",x="Distance from colony (km)",
       y="δ15N (‰)")

##both Clad and Cala look like they could be exponential fits
exp.noid<-lm(formula = log(d15N)~Distance,data=Calanoid_SIA)
exp.cera<-lm(formula = log(d15N) ~ Distance,data=Cladocera_SIA)
summary(exp.noid)
#r2 = 0.600, p = 0.02397
plot(exp.noid) #all good
plot(exp.cera)
summary(exp.cera) #pond 12 high leverage point
summary(exp.cera)
#r2 = 0.3857, p = 0.0414
ggplot(data=Calanoid_SIA,aes(x=Distance,y=d15N)) + 
  geom_point() + 
  geom_smooth(method="nls",
              formula=y ~ a*exp(-b *x),
              method.args=list(start=c(a=9.4811,b=0.5856)),
              se=FALSE,
              colour="red") + 
  labs(x="Distance from colony (km)") +
  ylab(bquote(δ^15*"N(‰)"))
#changing the 'a' and 'b' values doesn't change the 
#line at all, weird..
#y-intercept should be different :(
exp(9.4811)

#maybe logarithmic is better?
exp.noid.2<-lm(formula=d15N~log(Distance),data=Calanoid_SIA)
plot(exp.noid.2) #Pond 1- high leverage point
summary(exp.noid.2) #r2 = 0.8446, p = 0.0012
exp.cera.2<-lm(formula=d15N~log(Distance),data=Cladocera_SIA)
plot(exp.cera.2) ##still issues- Pond 1 high leverage point
summary(exp.cera.2) #r2 = 0.6758, p = 0.0010
#but they look great!

ggplot(Cladocera_SIA,aes(x=Distance,y=d15N)) + 
  geom_point()+ 
  theme_bw() +
  geom_smooth(method="lm",
              formula=y~log(x),
              colour="cyan4") +
  labs(x="Distance from colony (km)") +
  ylab(bquote(δ^15*"N(‰)"))
#smooth formula is: y = 4.1382 -2.5275ln(x)
library(SciViews)
4.1382-2.5275*ln(0.226)
#correct answer!
ggplot(data=Calanoid_SIA,aes(x=Distance,y=d15N)) + 
  geom_point() + 
  theme_bw() +
  geom_smooth(method="lm",
              formula=y~log(x),
              colour="red") +
  labs(x="Distance from colony (km)") +
  ylab(bquote(δ^15*"N(‰)"))
#formula: y = 8.8421-3.1038*ln(x)
8.8421-3.1038*ln(0.226)
#yay!
#cladocera and calanoid
ggplot(Zoop_SIA,aes(x=Distance,y=d15N,colour=Order)) + 
  geom_point() + 
  theme_bw() +
  labs(x="Distance from colony (km)") +
  ylab(bquote(δ^15*"N(‰)"))
#correlation between two
cor.test(Calanoid_SIA$d15N,Cladocera_SIA$d15N)
#problem because there are different numbers of 
#samples in the two orders.
#so let's make a df with only the 8 ponds with values
head(Zoop_SIA)
head(Cladocera_SIA)
tail(Cladocera_SIA)
head(Calanoid_SIA)
Calanoid_SIA_2<-Calanoid_SIA
colnames(Calanoid_SIA_2)[4]<-"d15N.Calanoid"
head(Calanoid_SIA_2)
Cladocera_SIA$d15N
Cladocera_d15N<-Cladocera_SIA$d15N
Cladocera_d15N
#removing ponds 2,3,4,7
Cladocera_d15N_remove<-Cladocera_d15N[-c(2,3,4,7)]
Cladocera_d15N_remove
#yee
Zoop_d15N_all<-cbind(Calanoid_SIA_2,Cladocera_d15N_remove)
head(Zoop_d15N_all)
colnames(Zoop_d15N_all)[6]<-"d15N.Cladocera"
colnames(Zoop_d15N_all)
cor.test(Zoop_d15N_all$d15N.Calanoid,Zoop_d15N_all$d15N.Cladocera)
#---------------------------------------------------
#d13C
#visualize first
ggplot(Zoop_SIA,aes(x=Distance,y=d13C,color=Order)) + 
  geom_point() + 
  geom_smooth(method="lm")
#looks maybe parabolic?
#let's try lm first
Calanoid_C_lm<-lm(d13C~Distance,data=Calanoid_SIA)
plot(Calanoid_C_lm) #no leverage points
summary(Calanoid_C_lm) #p = 0.459, r2 = 0.09
Cladocera_C_lm<-lm(d13C~Distance,data=Cladocera_SIA)
plot(Cladocera_C_lm) #no strong leverage points
summary(Cladocera_C_lm) #p = 0.629, r2 = 0.024
#let's try parabolic
ggplot(Cladocera_SIA,aes(x=Distance,y=d13C)) + 
  geom_point() + 
  geom_smooth(method="lm")
#polynomial models
Calanoid_poly_C<-lm(d13C~poly(Distance,degree=2,raw=T),data=Calanoid_SIA)
summary(Calanoid_poly_C) #r2 = 0.401, p=0.278
ggplot(Calanoid_SIA,aes(x=Distance,y=d13C)) + 
  geom_point() + 
  theme_bw() +
  geom_smooth(method="lm",
              formula = y~poly(x,2),color="red") + 
  labs(x="Distance from colony (km)") +
  ylab(bquote(δ^13*"C(‰)"))

Cladocera_poly_C<-lm(d13C~poly(Distance,degree=2,raw=T),data=Cladocera_SIA)
summary(Cladocera_poly_C) #r2 = 0.372, p = 0.123
ggplot(Cladocera_SIA,aes(x=Distance,y=d13C)) + 
  geom_point() + 
  geom_smooth(method='lm',
             formula = y~poly(x,2)) + 
  labs(x="Distance from colony (km)") +
  ylab(bquote(δ^13*"C(‰)"))

#correlation between N and C?
cor.test(Cladocera_SIA$d13C,Cladocera_SIA$d15N)#p=0.41,r=0.26
cor.test(Calanoid_SIA$d13C,Calanoid_SIA$d15N) #p=0.12,r=0.59
#not really

#---------------------------------------------------
#let's add the lake data to the zoop data
setwd("C:/Users/mmorr/OneDrive/Desktop/Grad School/Thesis/Waterrrr/Results")
Pond_data<-read.csv("Water chemistry.csv")
head(Pond_data)
library(vctrs)
library(ade4)
library(gclus)
library(ape)
library(cluster)
CSM_total<-cbind(Zoop_SIA,Pond_data)
#too many rows in the Zoop SIA df, because Calanoid and
#Cladocera separated. Make them separate
Cladocera_Pond<-cbind(Pond_data,Cladocera_SIA)
head(Cladocera_Pond)
ggplot(Cladocera_Pond,aes(x=Distance)) + 
  geom_point(aes(y=d15N,color = 'red'))

+ 
  geom_point(aes(y=Sediment.d15N,color="cyan4"))

ggplot(Zoop_SIA,aes(x=Distance,y=d15N,color=Order)) + 
  geom_point() + 
  theme_bw() +
  xlab("Distance from colony (km)") + 
  ylab(bquote(δ^15~"N(‰)"))

Algae_lm<-lm(Algae.d15N~Distance,data=Cladocera_Pond)

ggplot(Zoop_SIA,aes(x=Distance,y=d13C,color=Order)) + 
  geom_point()
Cladocera_d13C_lm<-lm(d13C~Distance,data=Cladocera_SIA)
summary(Cladocera_d13C_lm)
Calanoid_d13C_lm<-lm(d13C~Distance,data=Calanoid_SIA)
summary(Calanoid_d13C_lm)


#---------------------------------------------------------
#Removing "high leverage" points for d15N
Cladocera_remove<-Cladocera_SIA[-c(1),]
Calanoid_remove<-Calanoid_SIA[-c(1),]
Cladocera_remove_log<-lm(formula=d15N~log(Distance),data=Cladocera_remove)
Calanoid_remove_log<-lm(formula=d15N~log(Distance),data=Calanoid_remove)
plot(Cladocera_remove_log)
plot(Calanoid_remove_log)
#so we got rid of the high leverage points.
summary(Calanoid_remove_log)
#no longer significant, although r2 is 0.5063
summary(Cladocera_remove_log)
#still significant, but not as much

ggplot(Calanoid_remove,aes(x=Distance,y=d15N)) + 
  geom_point() + 
  geom_smooth(method="lm",
              formula=y~log(x),
              colour="red")
ggplot(Cladocera_remove,aes(x=Distance,y=d15N)) + 
  geom_point() + 
  geom_smooth(method="lm",
              formula=y~log(x))
