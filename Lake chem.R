#Playing with CSM Pond Data 
library(vegan)
library(vctrs)
library(ggplot2)
library(ade4)
library(gclus)
library(ape)
library(cluster)
library(tidyr)
library(egg)
library(patchwork)
Pond_data<-read.csv("Water chemistry.csv")
colnames(Pond_data)
#Rename column names
colnames(Pond_data)<-c("Pond_ID","Date","Distance","pH","Cond.","Depth","UTM_Zone",
                       "Easting","Northing","TP","TN","DOC","TIC","Ca",
                       "Mg","K","Na","Notes","Algae_d13C","Algae_d15N",
                       "Sed_d13C","Sed_d15N","X")
colnames(Pond_data)

#TN
ggplot(Pond_data,aes(x=Distance,y=TN)) + 
  geom_point()
TN_exp<-nls(TN~a*exp(b*Distance),
            start=list(a=3,b=-1),
            data=Pond_data)
plot(TN_exp)
res_TN_1<-resid(TN_exp)
hist(res_TN_1)
TN_inverse<-nls(TN~a+b*I(1/(Distance^2)),
                start=list(a=3,b=-1),data=Pond_data)
plot(TN_inverse)
res_TN_2<-resid(TN_inverse)
hist(res_TN_2,breaks=11)
TN_power<-nls(TN~a*Distance^b,
                start=list(a=3,b=-1),data=Pond_data)
plot(TN_power)
plot(fit_TN,res_TN_3,
     cex.axis=1.5,cex.lab=1.5,cex.main=1.5,cex.sub=1.5,
     xlab='Fitted Values',
     ylab='Standardized Residuals',
     pch=19)
)
fit_TN<-fitted(TN_power)
res_TN_3<-resid(TN_power)
hist(res_TN_3,
     cex.axis=1.5,cex.lab=1.5,cex.main=1.5,
     xlab='Residuals',
     main='Histogram of total nitrogen residuals, power model')
AIC(TN_exp,TN_inverse,TN_power,TN_null)
TN_null<-lm(TN~1,data=Pond_data)
anova(TN_inverse,TN_null)
ggplot(Pond_data,aes(x=Distance,y=TN)) + 
  geom_point() + 
  theme_bw() +
  geom_smooth(method="nls",
              formula=y~a+b*I(1/x^2),
              se=FALSE,
              method.args=list(start=c(a=1,b=-1))) + 
  labs(x="Distance from colony (km)",y="Total Nitrogen (mg/L)") + 
  theme(text=element_text(size=20))
ggplot(Pond_data,aes(x=Distance,y=TN)) + 
  geom_point() + 
  theme_bw() +
  geom_smooth(method="nls",
              formula=y~a*x^b,
              se=FALSE,
              method.args=list(start=c(a=1,b=-1))) + 
  labs(x="Distance from colony (km)",y="Total Nitrogen (mg/L)") + 
  theme(text=element_text(size=20))
ggplot(Pond_data,aes(x=Distance,y=Na)) + 
  geom_point() + 
  theme_bw() +
  geom_smooth(method="nls",
              formula = y~a*x^b,
              se=FALSE,
              method.args=list(start=c(a=1,b=-1))) + 
  labs(x="Distance from colony (km)",y="Total Nitrogen (mg/L)") + 
  theme(text=element_text(size=20))

#Na
Na_exp<-nls(Na~a*exp(b*Distance),
            start=list(a=200,b=-1),
            data=Pond_data)
plot(Na_exp)
res_Na_1<-resid(Na_exp)
hist(res_Na_1)
Na_inverse<-nls(Na~a+b*I(1/(Distance^2)),
                start=list(a=200,b=-1),data=Pond_data)
plot(Na_inverse)
fit_Na<-fitted(Na_inverse)
res_Na_2<-resid(Na_inverse)
plot(fit_Na,res_Na_2,
     cex.axis=1.5,cex.lab=1.5,cex.main=1.5,
     xlab='Fitted Values',
     ylab='Standardized Residuals',
     pch=19)
hist(res_Na_2,
     cex.axis=1.5,cex.lab=1.5,cex.main=1.5,
     xlab='Residuals',
     main='Histogram of sodium residuals, inverse model')
Na_power<-nls(Na~a*Distance^b,
              start=list(a=200,b=-1),data=Pond_data)
plot(Na_power)
res_Na_3<-resid(Na_power)
hist(res_Na_3)
Na_null<-lm(Na~1,data=Pond_data)
AIC(Na_exp,Na_inverse,Na_power,Na_null)
ggplot(Pond_data,aes(x=Distance,y=Na)) + 
  geom_point() + 
  theme_bw() + 
  geom_smooth(method='nls',
              formula=y~a+b*I(1/x^2),
              method.args=list(start=c(a=200,b=-1)),
              se=FALSE) + 
  labs(x="Distance from colony (km)") + 
  ylab(bquote(Na^'+'*' (mg/L)')) + 
  theme(text=element_text(size=20))

summary(model_NA)
#Create a dataframe with only environmental variables
Pond_env<-Pond_data[,c("TP","TN","DOC",
                       "TIC","Ca","Mg","K","Na")]
#need distance included, all variables in one column
#make in excel
Water_chem_transformed<-read.csv("Water chem transformed.csv")
head(Water_chem_transformed)
ggplot(Water_chem_transformed,aes(x=Distance,y=Amount)) + 
  geom_point() + 
  facet_wrap("Variable")
#Sodium effs the whole thing up
par(mfrow=c(3,3))
#TN
ggplot(Pond_data,aes(x=Distance,y=TN)) + 
  geom_point() + 
  geom_line() + 
  theme_bw() + 
  labs(x='Distance from colony (km)',y='Total Nitrogen (mg/mL)') + 
  theme(text=element_text(size=20))
#TP
TP_exp<-nls(TP~a*exp(b*Distance),
            start=list(a=1,b=-1),
            data=Pond_data)
plot(TP_exp)
res_TP_1<-resid(TP_exp)
hist(res_TP_1)
TP_inverse<-nls(TP~a+b*I(1/(Distance^2)),
                start=list(a=1,b=-1),data=Pond_data)
plot(TP_inverse)
res_TP_2<-resid(TP_inverse)
hist(res_TP_2)
TP_power<-nls(TP~a*Distance^b,
              start=list(a=.1,b=-.2),data=Pond_data)
summary(TP_power)
res_TP_3<-resid(TP_power)
hist(res_TP_3)
ggplot(Pond_data,aes(x=Distance,y=TP)) + 
  geom_point() +
  geom_smooth(method = "gam", 
              formula = y~s(x,k=6)) 
  
ggplot(Pond_data,aes(x=Distance,y=TP)) + 
  geom_point() + 
  geom_line() + 
  geom_smooth(method='nls',
              formula=y~a+b*I(1/x^2),
              se=FALSE,
              method.args=list(start=c(a=1,b=-1))) +
  theme_bw() + 
  labs(x='Distance from colony (km)',y='Total Phosphorus (mg/mL)') +
  theme(text=element_text(size=20))
ggplot(Pond_data,aes(x=Distance,y=TP)) + 
  geom_point() + 
  geom_line() + 
  geom_smooth(method='nls',
              formula=y~a*exp(b*x),
              se=FALSE,
              method.args=list(start=c(a=1,b=-1))) +
  theme_bw() + 
  labs(x='Distance from colony (km)',y='Total Phosphorus (mg/mL)') +
  theme(text=element_text(size=20))

#bad

TP_null<-lm(TP~1,data=Pond_data)
models_TP<-list(TP_exp,TP_inverse,TP_power)
AIC(TP_exp,TP_inverse,TP_power,TP_null)
aictab(cand.set=models_TP)
Pond_data
#DOC
ggplot(Pond_data,aes(x=Distance,y=DOC)) + 
  geom_point() + 
  theme_bw() + 
  geom_smooth(method='lm') +
  labs(x='Distance from colony (km)',y='DOC (mg/mL)') +
  theme(text=element_text(size=20))
DOC_lm<-lm(DOC~Distance,data=Pond_data)
plot(DOC_lm)
summary(DOC_lm)
DOC_exp<-nls(DOC~a*exp(b*Distance),
               start=list(a=10,b=.1),
               data=Pond_data)
summary(DOC_exp)
plot(DOC_exp)
DOC_fit<-fitted(DOC_exp)
plot(DOC_fit,res_DOC_1,
     cex.axis=1.5,cex.lab=1.5,cex.main=1.5,
     pch=19,
     xlab='Fitted Values',
     ylab='Standardized Residuals')
res_DOC_1<-resid(DOC_exp)
hist(res_DOC_1,
     cex.axis=1.5,cex.lab=1.5,cex.main=1.5,
     xlab='Residuals',
     main='Histogram of DOC residuals, exponential model')
DOC_inv<-nls(DOC~a+b*I(1/(Distance^2)),
              start=list(a=10,b=0.1),data=Pond_data)
plot(DOC_inv)
res_DOC_2<-resid(DOC_inv)
hist(res_DOC_2)
DOC_power<-nls(DOC~a*Distance^b,
             start=list(a=10,b=-0.1),data=Pond_data)
plot(DOC_power)
res_DOC_3<-resid(DOC_power)
hist(res_DOC_3)
ggplot(Pond_data,aes(x=Distance,y=DOC)) + 
  geom_point() + 
  theme_bw() + 
  geom_smooth(method='nls',
              formula=y~a*exp(b*x),
              se=FALSE,
              method.args=list(start=c(a=10,b=.1))) + 
  labs(x="Distance from colony (km)",y="DOC (mg/L)") + 
  theme(text=element_text(size=20))
DOC_null<-lm(DOC~1,data=Pond_data)
AIC(DOC_exp,DOC_inv,DOC_power,DOC_null,DOC_lm)
#TIC
ggplot(Pond_data,aes(x=Distance,y=TIC)) + 
  geom_point() + 
  geom_line() + 
  theme_bw() + 
  labs(x='Distance from colony (km)',y='TIC (mg/mL)') + 
  theme(text=element_text(size=20))

#Ca
ggplot(Pond_data,aes(x=Distance,y=Ca)) + 
  geom_point()
Ca_exp<-nls(Ca~a*exp(b*Distance),
            start=list(a=8,b=-1),
            data=Pond_data)
summary(Ca_exp)
plot(Ca_exp)
res_Ca_1<-resid(Ca_exp)
hist(res_Ca_1)
Ca_inverse<-nls(Ca~a+b*I(1/(Distance^2)),
                start=list(a=8,b=-1),data=Pond_data)
plot(Ca_inverse)
res_Ca_2<-resid(Ca_inverse)
hist(res_Ca_2)
Ca_power<-nls(Ca~a*Distance^b,
              start=list(a=8,b=-1),data=Pond_data)
plot(Ca_power)
fit_Ca<-fitted(Ca_power)
res_Ca_3<-resid(Ca_power)
plot(fit_Ca,res_Ca_3,
     cex.lab=1.5,cex.main=1.5,cex.axis=1.5,
     xlab='Fitted Values',
     ylab='Standardized Residuals',
     pch=19)
hist(res_Ca_3,
     cex.lab=1.5,cex.main=1.5,cex.axis=1.5,
     main='Histogram of calcium residuals, power model',
     xlab='Residuals')
Ca_null<-lm(Ca~1,data=Pond_data)
AIC(Ca_exp,Ca_inverse,Ca_power,Ca_null)
ggplot(Pond_data,aes(x=Distance,y=Ca)) + 
  geom_point() + 
  theme_bw() + 
  geom_smooth(method='nls',
              formula=y~a+b*I(1/x^2),
              method.args=list(start=c(a=8,b=-1)),
              se=FALSE) + 
  labs(x="Distance from colony (km)") + 
  ylab(bquote(Ca^'2+'*' (mg/L)')) + 
  theme(text=element_text(size=20))
ggplot(Pond_data,aes(x=Distance,y=Ca)) + 
  geom_point() + 
  theme_bw() + 
  geom_smooth(method='nls',
              formula=y~a*x^b,
              method.args=list(start=c(a=8,b=-1)),
              se=FALSE) + 
  labs(x="Distance from colony (km)") + 
  ylab(bquote(Ca^'2+'*' (mg/L)')) + 
  theme(text=element_text(size=20))


ggplot(Pond_data,aes(x=Distance,y=Ca)) + 
  geom_point() + 
  geom_line() + 
  theme_bw() + 
  labs(x='Distance from colony (km)') + 
  ylab(bquote(Ca^'2+')) + 
  theme(text=element_text(size=20))
#Na
ggplot(Pond_data,aes(x=Distance,y=Na)) + 
  geom_point() + 
  geom_line() + 
  theme_bw() + 
  labs(x='Distance from colony (km)') + 
  ylab(bquote(Na^'+'))
#---------------------------------------
#Mg
ggplot(Pond_data,aes(x=Distance,y=Mg)) + 
  geom_point()
Mg_exp<-nls(Mg~a*exp(b*Distance),
            start=list(a=18,b=-1),
            data=Pond_data)
summary(Mg_exp)
plot(Mg_exp)
res_Mg_1<-resid(Mg_exp)
hist(res_Mg_1)
Mg_inverse<-nls(Mg~a+b*I(1/(Distance^2)),
                start=list(a=18,b=-1),data=Pond_data)
plot(Mg_inverse)
fit_Mg<-fitted(Mg_inverse)
res_Mg_2<-resid(Mg_inverse)
plot(fit_Mg,res_Mg_2,
     cex.axis=1.5,cex.lab=1.5,cex.main=1.5,
     pch=19,
     xlab='Fitted Values',
     ylab='Standardized Residuals')
hist(res_Mg_2,
     cex.axis=1.5,cex.lab=1.5,cex.main=1.5,
     xlab='Residuals',
     main='Histogram of magnesium residuals, inverse model')
Mg_power<-nls(Mg~a*Distance^b,
              start=list(a=18,b=-1),data=Pond_data)
plot(Mg_power)
res_Mg_3<-resid(Mg_power)
hist(res_Mg_3)
Mg_null<-lm(Mg~1,data=Pond_data)
AIC(Mg_exp,Mg_inverse,Mg_power,Mg_null)
ggplot(Pond_data,aes(x=Distance,y=Mg)) + 
  geom_point() + 
  theme_bw() + 
  geom_smooth(method='nls',
              formula=y~a+b*I(1/x^2),
              method.args=list(start=c(a=200,b=-1)),
              se=FALSE) + 
  labs(x="Distance from colony (km)") + 
  ylab(bquote(Mg^'2+'*' (mg/L)')) + 
  theme(text=element_text(size=20))
#power
#K
K_exp<-nls(K~a*exp(b*Distance),
            start=list(a=6.5,b=-1),
            data=Pond_data)
plot(K_exp)
res_K_1<-resid(K_exp)
hist(res_K_1)
K_inverse<-nls(K~a+b*I(1/(Distance^2)),
                start=list(a=6.5,b=-1),data=Pond_data)

summary(K_inverse)
plot(K_inverse)
fit_K<-fitted(K_inverse)
res_K_2<-resid(K_inverse)
plot(fit_K,res_K_2,
     cex.axis=1.5,cex.lab=1.5,cex.main=1.5,
     pch=19,
     xlab='Fitted Values',
     ylab='Standardized Residuals')
hist(res_K_2,
     cex.axis=1.5,cex.lab=1.5,cex.main=1.5,
     xlab='Residuals',
     main='Histogram of potassium residuals, inverse model')
K_power<-nls(K~a*Distance^b,
              start=list(a=6.5,b=-1),data=Pond_data)
summary(K_power)
res_K_3<-resid(K_power)
hist(res_K_3)
K_null<-lm(K~1,data=Pond_data)
AIC(K_exp,K_inverse,K_power,K_null)
#inverse
ggplot(Pond_data,aes(x=Distance,y=K)) + 
  geom_point() + 
  geom_smooth(method='nls',
              formula=y~a+b*I(1/x^2),
              se=FALSE,
              method.args=list(start=c(a=6.5,b=-1))) +
  theme_bw() + 
  labs(x='Distance from colony (km)') + 
  ylab(bquote(K^'+'*'(mg/L)')) +
  theme(text=element_text(size=20))
ggplot(Pond_data,aes(x=Distance,y=K)) + 
  geom_point() + 
  geom_line() + 
  geom_smooth(method="glm",
              formula=y~I(1/x^2),
              se=TRUE,
              method.args=list(family="Gamma"(link='log'))) +
  theme_bw() + 
  labs(x='Distance from colony (km)') + 
  ylab(bquote(K^'+'*'(mg/L)')) +
  theme(text=element_text(size=20))

Na_inverse<-glm(Na~I(1/Distance^2),family=gaussian,
                data=Pond_data)
#inverse
ggplot(Pond_data,aes(x=Distance,y=K)) + 
  geom_point() + 
  theme_bw() +
  labs(x='Distance from colony (km)') +
  ylab(bquote(K^'+'*'(mg/mL)')) + 
  theme(text=element_text(size=20))

#pH
ggplot(Pond_data,aes(x=Distance,y=pH)) + 
  geom_point() + 
  geom_smooth(method='lm')
pH_lm<-lm(pH~Distance,data=Pond_data)
plot(pH_lm)
summary(pH_lm)
pH_exp<-nls(pH~a*exp(b*Distance),
            start=list(a=7,b=-1),
            data=Pond_data)
plot(pH_exp)
res_pH_1<-resid(pH_exp)
hist(res_pH_1)
pH_inverse<-nls(pH~a+b*I(1/(Distance^2)),
                start=list(a=7,b=-1),data=Pond_data)
plot(pH_inverse)
res_pH_2<-resid(pH_inverse)
hist(res_pH_2)
pH_power<-nls(pH~a*Distance^b,
              start=list(a=7,b=-1),data=Pond_data)
plot(pH_power,cex.lab=1.5)
fit_pH<-fitted(pH_power)
plot(pH_power,cex.axis=1.5)
plot(fit_pH,res_pH_3,
     cex.axis=1.5,cex.lab=1.5,cex.main=1.5,cex.sub=1.5,
     xlab='Fitted Values',
     ylab='Standardized Residuals',
     pch=19)
par(mfrow=c(1,2))
pdf(pointsize = 20)
hist(res_pH_3,
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
     xlab='Residuals',
     main="Histogram of pH residuals, power model")
ggplot(Pond_data,aes(x=Distance,y=pH)) + 
  geom_point() + 
  geom_smooth(method='nls',
              formula = y~a*x^b,
              se=FALSE,
              method.args=list(start=c(a=7,b=-1))) +
  theme_bw() + 
  labs(x='Distance from colony (km)',y='pH') +
  theme(text=element_text(size=20))
pH_null<-lm(pH~1,data=Pond_data)
AIC(pH_exp,pH_inverse,pH_power,pH_null,pH_lm)
#power
#cond
Pond_data$Cond.
Pond_data$Cond.<-(Pond_data$Cond.)*1000
ggplot(Pond_data,aes(x=Distance,y=Cond.)) + 
  geom_point()
cond_exp<-nls(Cond.~a*exp(b*Distance),
            start=list(a=1500,b=-2),
            data=Pond_data)
summary(cond_exp)
plot(cond_exp)
res_cond_1<-resid(cond_exp)
hist(res_cond_1)
cond_inverse<-nls(Cond.~a+b*I(1/(Distance^2)),
                start=list(a=1800,b=-2),data=Pond_data)
plot(cond_inverse)
fit_cond<-fitted(cond_inverse)
res_cond_2<-resid(cond_inverse)
plot(fit_cond,res_cond_2,
     cex.axis=1.5,cex.lab=1.5,cex.main=1.5,cex.sub=1.5,
     xlab='Fitted Values',
     ylab='Standardized Residuals',
     pch=19)
hist(res_cond_2,cex.axis=1.5,cex.lab=1.5,cex.main=1.5,cex.sub=1.5,
     main='Histogram of conductivity residuals, inverse model',
     xlab='Residuals')
cond_power<-nls(Cond.~a*Distance^b,
              start=list(a=1800,b=-2),data=Pond_data)
plot(cond_power)
res_cond_3<-resid(cond_power)
hist(res_cond_3)
ggplot(Pond_data,aes(x=Distance,y=Cond.)) + 
  geom_point() + 
  geom_smooth(method='nls',
              formula=y~a+b*I(1/x^2),
              se=FALSE,
              method.args=list(start=c(a=1800,b=-2))) +
  theme_bw() + 
  labs(x='Distance from colony (km)') +
  ylab(bquote('Conductivity (µS/cm)')) + 
  theme(text=element_text(size=20))
cond_null<-lm(Cond.~1,data=Pond_data)
AIC(cond_exp,cond_inverse,cond_power,cond_null)
#inverse
