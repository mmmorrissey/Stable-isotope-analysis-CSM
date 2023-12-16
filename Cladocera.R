##Cladocera
shapiro.test(Cladocera_SIA$d15N)
setwd("C:\\Users\\mmorr\\OneDrive\\Desktop\\Grad School\\Thesis\\Zooplankton\\SIA Results Zoop\\")
Zoop_SIA<-read.csv("Zooplankton SIA.csv")
colnames(Zoop_SIA)
colnames(Zoop_SIA)[5]<-"Distance"
colnames(Zoop_SIA)[6]<-"percent.N"
colnames(Zoop_SIA)[7]<-"percent.C"
Cladocera_SIA<-subset(Zoop_SIA,Order=="Cladocera")
Calanoida_SIA<-subset(Zoop_SIA,Order=="Calanoida")
##exponential
#only gonna do NLS, compare using AIC
#y = α*e(βx)
nls1<-nls(d15N~ a*exp(b*Distance), 
          start=list(a=10,b=-1),
          data=Cladocera_SIA)
plot(nls1)
qqnorm(nls1)
summary(nls1)
res1<-resid(nls1)
hist(res1)

ggplot(Cladocera_SIA,aes(x=Distance,y=d15N)) + 
  geom_point() + 
  theme_bw() +
  geom_smooth(method="nls",
              formula=y ~ a*exp(b *x),
              method.args=list(start=c(a=10.78655,b=1.0095)),
              se=FALSE) + 
  labs(x="Distance from colony (km") + 
  ylab(bquote(δ^15*"N(‰)"))

##inverse square law
#y= α + β*(x^-2)
nls2<-nls(d15N~a+b*I(1/(Distance^2)),
          start=list(a=13,b=-1),data=Cladocera_SIA)
summary(nls2)
#resid & fit
res2<-resid(nls2)
fit_Clad<-fitted(nls2)
plot(fit_Clad,res2,
     cex.axis=1.5,cex.lab=1.5,
     xlab='Fitted Values',
     ylab='Residuals',
     pch=19)
hist(res2,breaks=9,
     cex.axis=1.5,cex.lab=1.5,cex.main=1.5,
     main="Histogram of Cladocera residuals, inverse model",
     xlab='Residuals')
ggplot(Cladocera_SIA,aes(x=Distance,y=d15N)) + 
  geom_point() + 
  theme_bw() +
  geom_smooth(method="nls",
              formula=y~a+b*I(1/x^2),
              se=FALSE,
              method.args=list(start=c(a=0.6,b=1))) + 
  labs(x="Distance from colony (km") + 
  ylab(bquote(δ^15*"N(‰)"))
#power law
#y = α * x^β
nls3<-nls(d15N~a*Distance^b,
       start=list(a=3,b=1),data=Cladocera_SIA)
summary(nls3)
res3<-resid(nls3)
hist(res3)
plot(nls3)
ggplot(Cladocera_SIA,aes(x=Distance,y=d15N)) + 
  geom_point() + 
  theme_bw() +
      geom_smooth(method = "nls",
              formula = y~a*x^b,
              se=FALSE,
              method.args=list(start=c(a=3,b=-1))) + 
  labs(x="Distance from colony (km") + 
  ylab(bquote(δ^15*"N(‰)"))
#null model
nullmod_Clad_N <- lm(d15N ~ 1,data=Cladocera_SIA)
summary(nullmod_Clad_N)
AIC(nls1,nls2,nls3,nullmod_Clad_N)
#evaluate models
library(AICcmodavg)

ggplot(Zoop_SIA,aes(x=Distance,y=d15N,colour=Order)) + 
  geom_point() + 
  theme_bw() + 
  geom_smooth(method='nls',
              formula=y~a+b*I(1/x^2),
              se=FALSE,
              method.args=list(start=c(a=13,b=-1)),
              data=Cladocera_SIA) + 
  geom_smooth(method='nls',
              formula = y~a*x^b,
              se=FALSE,
              method.args=list(start=c(a=8,b=-0.4)),
              data=Calanoida_SIA) + 
  labs(x='Distance from colony (km)') +
  ylab(bquote(δ^15*'N(‰)')) + 
  theme(text=element_text(size=16))
              
#--------------------------------
#d13C
Cladocera_poly_C<-glm(d13C~poly(Distance,degree=2,raw=T),
                      family='gaussian',data=Cladocera_SIA)
plot(Cladocera_poly_C) #fine
summary(Cladocera_poly_C)
#use lm to report p, r2
Cladocera_poly_lm<-lm(d13C~poly(Distance,degree=2,
                               raw=T),data=Cladocera_SIA)
plot(Cladocera_poly_lm)
summary(Cladocera_poly_lm)
#LR = (1+F(dfnum/dfdenom))^(n/2)
2.665*(2/9)
1.5922^6
#without pond 5
Cladocera_5<-Cladocera_SIA[-c(5),]
Cladocera_5$d13C
Cladocera_SIA$d13C
Cladocera_5_C_poly<-lm(formula=d13C~poly(Distance,degree=2,raw=T),data=Cladocera_5)
plot(Cladocera_5_C_poly)
summary(Cladocera_5_C_poly)
#LR = (1+F(dfnum/dfdenom))^(n/2)
6.112*(2/8)
2.528^(11/2)
ggplot(Zoop_SIA,aes(x=Distance,y=d13C,colour=Order)) + 
  geom_point()+
  theme_bw() +
  geom_smooth(method="glm",
              formula = y~poly(x,2),
              method.args = list(family = 'gaussian')) +
  labs(x="Distance from colony (km)") +
  ylab(bquote(δ^13*"C(‰)"))
coef(Calanoid_poly_C)
coef(Cladocera_poly_C)
par(mfrow=c(1,2))
ggplot(Calanoid_SIA,aes(x=Distance,y=d13C)) + 
  geom_point() + 
  theme_bw() +
  geom_smooth(method="glm",
              formula = y~poly(x,2),
              method.args = list(family = 'gaussian')) +
  labs(x="Distance from colony (km)") +
  ylab(bquote(δ^13*"C(‰)"))
ggplot(Cladocera_SIA,aes(x=Distance,y=d13C)) + 
  geom_point() + 
  theme_bw() + 
  geom_smooth(method="glm",
              formula = y~poly(x,2),
              method.args = list(family = 'gaussian')) +
  labs(x="Distance from colony (km)") +
  ylab(bquote(δ^13*"C(‰)"))

ggplot(Cladocera_SIA,aes(x=Distance,y=d13C)) + 
  geom_point() + 
  theme_bw() + 
  labs(x="Distance from colony (km)") +
  ylab(bquote(δ^13*"C(‰)")) + 
  theme(text=element_text(size=16))

lm1<-lm(d13C~Distance,data=Cladocera_SIA)
summary(lm1)
Cladocera_2<-Cladocera_SIA[-c(11,12),]
lm2<-lm(d13C~Distance,data=Cladocera_2)
summary(lm2)


#-------
#EA
#N
Clad_N<-lm(percent.N~Distance, data=Cladocera_SIA)
plot(Clad_N)
summary(Clad_N)
Clad_N_log<-lm(percent.N~log(Distance),data=Cladocera_SIA)
plot(Clad_N_log)
summary(Clad_N_log)
ggplot(Cladocera_SIA,aes(x=Distance,y=percent.N)) + 
  geom_point()
#LR = (1+F(dfnum/dfdenom))^(n/2)
2.631*(1/10)
1.2631^6
#4.0609
library(ggplot2)
ggplot(Cladocera_SIA,aes(x=Distance,y=percent.C)) + 
  geom_point() + 
  geom_smooth(method='lm',
              formula=y~log(x))
#C
Clad_C_log<-lm(percent.C~log(Distance),data=Cladocera_SIA)
par(mfrow=c(2,2))
plot(Clad_C_log)#sort of bowl
summary(Clad_C_log)#0.047
Clad_C<-lm(percent.C~Distance,data=Cladocera_SIA)
plot(Clad_C)#sort of bowl, residuals fan to left
summary(Clad_C) #0.1128
ggplot(Cladocera_SIA,aes(x=Distance,y=percent.C)) + 
  geom_point() + 
  geom_smooth(method='glm',
              formula=y~x,
              method.args = list(family =gaussian(link='log')))
Clad_C_GLM<-glm(percent.C~Distance,family=gaussian(link='log'),
                data=Cladocera_SIA)
plot(Clad_C_GLM)
summary(Clad_C_GLM)
Clad_C_models<-list(Clad_C,Clad_C_log)
Clad_C_names<-c('LM','log')
library(AICcmodavg)
aictab(cand.set=Clad_C_models,modnames=Clad_C_names)
#log is better
#LR = (1+F(dfnum/dfdenom))^(n/2)
3.021*(1/10)
1.3021^6

#I wonder if leaving out ponds 11 and 12 change
#carbon analysis..

ggplot(Zoop_SIA,aes(x=Distance,y=percent.N,colour=Order)) +
  geom_point() + 
  theme_bw() +
  geom_smooth(method='lm',
              se=FALSE) + 
  labs(x='Distance from colony (km)',y='%N') + 
  theme(text=element_text(size=20))
  
