##Calanoida
Zoop_SIA<-read.csv("Zooplankton stable isotope and percent N and C.csv")
colnames(Zoop_SIA)
colnames(Zoop_SIA)[5]<-"Distance"
colnames(Zoop_SIA)[6]<-"percent.N"
colnames(Zoop_SIA)[7]<-"percent.C"
Calanoida_SIA<-subset(Zoop_SIA,Order=="Calanoida")
##exponential
#y = α*e(βx)
#only gonna do NLS, compare using AIC
Cal_1<-nls(d15N~a*exp(b*Distance), 
          start=list(a=13,b=-1),
          data=Calanoida_SIA)
summary(Cal_1)
plot(Cal_1)
res_Cal_1<-resid(Cal_1)
hist(res_Cal_1)
ggplot(Calanoida_SIA,aes(x=Distance,y=d15N)) + 
  geom_point() + 
  theme_bw() +
  geom_smooth(method="nls",
              aes(colour="NLS"),
              formula=y ~ a*exp(b *x),
              method.args=list(start=c(a=10.78655,b=1.0095)),
              se=FALSE) + 
  labs(x="Distance from colony (km") + 
  ylab(bquote(δ^15*"N(‰)"))

##inverse square law
#y= α + β*(x^-2)
Cal_2<-nls(d15N~a+b*I(1/(Distance^2)),
          start=list(a=5,b=0.5),data=Calanoida_SIA)
summary(Cal_2)
plot(Cal_2)
#resid & fit
res_Cal_2<-resid(Cal_2)
hist(res_Cal_2)
ggplot(Calanoida_SIA,aes(x=Distance,y=d15N)) + 
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
Cal_3<-nls(d15N~a*Distance^b,
          start=list(a=8,b=-.1),data=Calanoida_SIA)
AIC(Cal_3)
summary(Cal_3)
plot(Cal_3)
fit_Cal<-fitted(Cal_3)
res_Cal_3<-resid(Cal_3)
plot(fit_Cal,res_Cal_3,
     cex.axis=1.5,cex.lab=1.5,
     xlab='Fitted Values',
     ylab='Standardized Residuals',
     pch=19)
hist(res_Cal_3,
     cex.axis=1.5,cex.lab=1.5,cex.main=1.5,
     xlab="Residuals",
     main='Histogram of Calanoida residuals, power model')
ggplot(Calanoida_SIA,aes(x=Distance,y=d15N)) + 
  geom_point() + 
  theme_bw() +
  geom_smooth(method = "nls",
              formula = y~a*x^b,
              se=FALSE,
              method.args=list(start=c(a=3,b=-1))) + 
  labs(x="Distance from colony (km") + 
  ylab(bquote(δ^15*"N(‰)"))
#null model
nullmod_Cal_N <- lm(d15N ~ 1,data=Calanoida_SIA)
summary(nullmod_Cal_N)
AIC(Cal_3)

#evaluate models
library(AICcmodavg)
models.nls.Cal<-list(nls_Cal_N,Cal_2,Cal_3)
model.names<-c('exponential','inverse','power')
AIC(Cal_1,Cal_2,Cal_3,nullmod_Cal_N)
aictab(cand.set=models.nls.Cal,modnames=model.names)
#power best
anova(Cal_3,nullmod_Cal_N)
AIC(nls1)
AIC(nls_Cal_N,Cal_2,Cal_3)

ggplot(Zoop_SIA,aes(x=Distance,y=d15N,colour=Order)) + 
  geom_point() + 
  theme_bw() +
  geom_smooth(method="glm",
              formula=y~log(x),
              method.args = list(family = "Gamma"(link='log')),
              data=Zoop_SIA[Zoop_SIA$d15N >0, ]) + 
  labs(x="Distance from colony (km)") +
  ylab(bquote(δ^15*"N(‰)"))
summary(glm3) #p-value = 0.000898,t-value = -6.082
#Likelyhood Ratio = (1+t^2/(n-2))^(n/2)
(-6.082)^2
1+(36.99072/6)
7.16512^4
#LR = 6946847
#likehood ratio
#LR = (1+t^2/(n-2))^(n/2)
(-4.443)^2
1+(19.74025/10)
4.290042^(6)
summary(Cal_glm3)
summary(Cal_glm2)
summary(Cal_glm1)
#----------------------------------
#d13C
Cal_lm_C<-lm(d13C~Distance,data=Calanoida_SIA)
plot(Cal_lm_C) #bowl
summary(Cal_lm_C)#p = 0.459, r2 = 0.09
ggplot(Calanoida_SIA,aes(x=Distance,y=d13C)) + 
  geom_point() + 
  geom_smooth(method="nls",
              formula=y~a*x^b,
              se=FALSE,
              method.args=list(start=c(a=-30,b=0.01)))
Calanoida_nls1<-nls(d13C~a*Distance^b,
start=list(a=30,b=.1),data=Calanoida_SIA)
plot(Calanoida_nls1)
summary(Calanoida_nls1)
plot(Calanoida_glm_1) #bowl, pond 12
summary(Calanoida_glm_1)#no good
ggplot(Calanoida_SIA,aes(x=Distance,y=d13C)) + 
  geom_point() + 
  geom_smooth(method = "nls",
              formula = y~a*x^b,
              se=FALSE,
              method.args=list(start=c(a=30,b=0.01)))
Cal_lm_3<-lm(log|(d13C)|~log(Distance),data=Calanoida_SIA)
plot(Cal_lm_3)
Cal_lm_2<-lm(d13C~log(Distance),data=Calanoida_SIA)
plot(Cal_lm_2)#bowl, pond 1
ggplot(Calanoida_SIA,aes(x=Distance,y=d13C)) + 
  geom_point() + 
  geom_smooth(method='lm',
              formula=y~log(x))
summary(Cal_lm_2)
Calanoida_poly_C<-glm(d13C~poly(Distance,degree=2,raw=T),
                     family='gaussian',data=Calanoida_SIA)
plot(Calanoida_poly_C) #1 strong leverage
summary(Calanoida_poly_C)
Calanoida_poly_lm<-lm(d13C~poly(Distance,degree=2,
                               raw=T),data=Calanoida_SIA)
par(mfrow = c(2,2))
plot(Calanoida_poly_lm)
summary(Calanoida_poly_lm)
#LR = (1+F(dfnum/dfdenom))^(n/2)
1.673*(2/5)
1.6692^(4)
#LR = 7.763

ggplot(Calanoida_SIA,aes(x=Distance,y=d13C)) + 
  geom_point() + 
  geom_smooth(method="lm",
              formula = y~poly(x,2)) +
  labs(x="Distance from colony (km)") +
  ylab(bquote(δ^13*"C(‰)"))

              
#--------------
#EA
#N
Cal_N<-lm(percent.N~Distance,data=Calanoida_SIA)
plot(Cal_N)
summary(Cal_N)
#LR = (1+F(dfnum/dfdenom))^(n/2)
6.425*(1/6)
2.07^4
ggplot(Calanoida_SIA,aes(x=Distance,y=percent.N)) + 
  geom_point() + 
  theme_bw() +
  geom_smooth(method='lm') + 
  labs(x='Distance from colony (km)', y='Calanoida %N') + 
  theme(text=element_text(size=14))

Cal_C<-lm(percent.C~Distance,data=Calanoida_SIA)
plot(Cal_C)
ggplot(Calanoida_SIA,aes(x=Distance,y=percent.C)) + 
  geom_point() + 
  geom_smooth(method='lm')
  geom_smooth(method="glm",
              formula = y~x,
              method.args = list(family =gaussian(link='log')))
Cal_C_glm<-glm(percent.C~Distance,family=gaussian(link='log'),
              data=Calanoida_SIA)
plot(Cal_C_glm)
summary(Cal_C_glm)
