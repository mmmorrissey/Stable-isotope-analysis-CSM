#sediment, phytoplankton (algae), zooplankton, chlorophyll a
install.packages("grid")
library(ggplot2)
Zoop_algae_sediment<-read.csv("Zooplankton, algae, sediment.csv")
#chlorophyll a
chla<-read.csv("CSM_MOLLY_chla_data")
#chlA
#add distance to chla
chla<-CSM_MOLLY_chla_data
#only need first twelve distances
Distance<-subset(Zoop_algae_sediment[1:12,])
Distance<-Distance$Distance
Distance<-Distance(c[1:12,])
chla<-cbind(chla,Distance)
head(Zoop_algae_sediment)
#rename column names
colnames(Zoop_algae_sediment)
colnames(Zoop_algae_sediment)[6]<-"percent.N"
colnames(Zoop_algae_sediment)[7]<-"percent.C"
#subset elements
Sediment<-subset(Zoop_algae_sediment,Variable == "Sediment")
Phyto<-subset(Zoop_algae_sediment,Variable == "Phytoplankton")
Cladocera<-subset(Zoop_algae_sediment,Variable == "Cladocera")
Calanoida<-subset(Zoop_algae_sediment,Variable == "Calanoida")
#make df with both zooplankton orders
Zoop<-subset(Zoop_algae_sediment,Variable == "Cladocera"|Variable == "Calanoida")
#make df with algae and sediment
Sed_algae<-subset(Zoop_algae_sediment,Variable == "Sediment"|Variable=="Phytoplankton")
#rename columns
colnames(Zoop)[colnames(Zoop)=="Variable"] <- "Order"
colnames(Cladocera)[colnames(Cladocera)=="Variable"] <-"Order"
colnames(Calanoida)[colnames(Calanoida)=="Variable"] <-"Order"

#let's figure out how d15N changes with distance from colony
#simple linear regression doesn't capture data as well as I would like, we'll try creating different models
#view the suitablility of the models using "summary(model)" or making plots of residuals and fitted data
#compare models with AIC
#start with sediment
#first model: exponential
#y = α*e(βx)
Sed_1<-nls(d15N~ a*exp(b*Distance), 
            start=list(a=16,b=-3),
            data=Sediment)
summary(Sed_1) #summary of how well the model fits the data
#second model: inverse
#y = α+β*(x^-2)
Sed_2<-nls(d15N~a+b*I(1/Distance^2),
           start=list(a=1,b=.3),
           data=Sediment)
summary(Sed_2)
res_sed<-resid(Sed_2) #residuals of model
fit_sed<-fitted(Sed_2) #fit of model
plot(fit_sed,res_sed,
     cex.axis=1.5,cex.lab=1.5,
     pch=19,
     xlab='Fitted Values',
     ylab='Standardized Residuals') #visualize fit vs res, do the points fan out?
hist(res_sed,
     cex.axis=1.5,cex.lab=1.5,cex.main=1.5,
     main='Histogram of sediment residuals, inverse model',
     xlab='Residuals') #is the histogram normal? skewed?
#third model: power
#y = α*x^β
Sed_3<-nls(d15N~a*Distance^b,
            start=list(a=1,b=-1),data=Sediment)
summary(Sed_3)           
#create a null model to compare the other three to
Sed_null<-lm(d15N~1,data=Sediment)
AIC(Sed_1,Sed_2,Sed_3,Sed_null)

#phytoplankton data
#exponential model
Phyto_1<-nls(d15N~a*exp(b*Distance),
             start=list(a=7,b=-0.1),
             data=Phyto)
summary(Phyto_1)
#inverse model
Phyto_2<-nls(d15N~a+b*I(1/Distance^2),
             start=list(a=4,b=0.3),
             data=Phyto)
summary(Phyto_2)
#power model
Phyto_3<-nls(d15N~a*Distance^b,
             start=list(a=5,b=-0.3),data=Phyto)
summary(Phyto_3)
fit_phyto<-fitted(Phyto_3)
res_phyto<-resid(Phyto_3)
plot(fit_phyto,res_phyto,
     cex.axis=1.5,cex.lab=1.5,cex.main=1.5,
     xlab='Fitted Values',ylab='Standardized Residuals',
     pch=19)
hist(res_phyto,
     cex.axis=1.5,cex.lab=1.5,cex.main=1.5,
     main="Histogram of phytoplankton residuals, power model",
     xlab='Residuals')
#null model
Phyto_null<-lm(d15N~1,data=Phyto)
AIC(Phyto_1,Phyto_2,Phyto_3,Phyto_null)

#plots
ggplot(Zoop,aes(x=Distance,y=d15N,color=Order)) + 
  geom_point() + 
  theme_bw() +
  labs(x="Distance from colony (km)") + 
  ylab(bquote(δ^15*"N(‰)")) + 
  theme(text=element_text(size=12))+ 
  geom_smooth(method="nls",
              formula=y~a+b*I(1/x^2),
              method.args=list(start=c(a=9.4811,b=0.8586)),
              se=FALSE,
              data=Cladocera)+ 
  geom_smooth(method="nls",
              formula=y~a*x^b,
              method.args=list(start=c(a=15,b=-1)),
              data=Calanoida,
              se=FALSE) +
  theme(plot.margin = margin(1, 1, 2, 2, "cm"))

ggplot(Sed_algae,aes(x=Distance,y=d15N,colour=Variable)) + 
  geom_point() + 
  theme_bw() +
  geom_smooth(method="nls",
              formula=y~a+b*I(1/x^2),
              method.args=list(start=c(a=0.6,b=0.3)),
              se=FALSE,
              data=Sediment) + 
  geom_smooth(method="nls",
              formula=y~a*x^b,
              method.args=list(start=c(a=5,b=-0.3)),
              data=Phyto,
              se=FALSE) + 
  labs(x="Distance from colony (km)") + 
  ylab(bquote(δ^15*"N(‰)")) + 
  theme(text=element_text(size=12)) + 
  scale_y_continuous(breaks=c(0,4,8,12,16)) + 
  theme(plot.margin = margin(1, 1, 2, 2, "cm"))

#
#d13C
ggplot(Sed_algae,aes(x=Distance,y=d13C,colour=Variable)) + 
  geom_point()
Sed_C<-lm(d13C~Distance,data=Sediment)  
plot(Sed_C)
summary(Sed_C)
#Likelyhood Ratio = (1+F(dfnum/dfdenom))^(n/2)
8.488*(1/10)
1.8488^6
#LR = 39.933

#------------------
#visualize d13C
ggplot(Sed_algae,aes(x=Distance,y=d13C,colour=Variable)) + 
  geom_point() + 
  theme_bw() +
  geom_smooth(method='lm',
              se=FALSE) + 
  labs(x="Distance from colony (km)") + 
  ylab(bquote(δ^13*"C(‰)")) + 
  theme(text=element_text(size=14)) 

ggplot(Phyto,aes(x=Distance,y=d13C)) + 
  geom_point()
Phyto_d13C<-lm(d13C~Distance,data=Phyto)
plot(Phyto_d13C) #fine
summary(Phyto_d13C) #p=0.667, n.s.
#Pearson's correlation
with(Sed_algae,cor.test(d13C,Variable,method='pearson'))
cor.test(Pond_data$Algae_d13C,Pond_data$Sed_d13C,method='pearson')
#---------------------
#%C and %N
ggplot(Sediment, aes(x=Distance,y=percent.N)) + 
  geom_point()
Sed_N<-lm(percent.N~Distance,data=Sediment)
plot(Sed_N)
summary(Sed_N)
 
#-------------------
#C:N
p3<-ggplot(Sediment,aes(x=Distance,y=C.N)) + 
  geom_point() + 
  geom_smooth(method="lm") + 
  theme_bw() + 
  theme(text=element_text(size=14)) + 
  labs(x='Distance from colony (km)',y='Sediment C:N')
CN_C<-lm(C.N~d13C,data=Sediment)
par(mfrow=c(2,2))
plot(CN_C)#bowl, pond 3 leverage
summary(CN_C) #p=0.0006
cor.test(Sediment$C.N,Sediment$d13C)
p4<-ggplot(Sediment,aes(x=d13C,y=C.N)) + 
  geom_point(size=2) + 
  geom_smooth(method='lm') + 
  theme_bw() +
  geom_text(aes(label = Pond),nudge_y=5.5) +
  labs(y='Sediment C:N') + 
  xlab(bquote(δ^13*"C(‰)")) + 
  theme(text=element_text(size=14))
multiplot(p3,p4,cols=2)
cor.test(Sediment$C.N,Sediment$d13C)

plot(Sediment$Distance,Sediment$d13C)
  
CN_lm_sed<-lm(C.N~Distance,data=Sediment)
res11<-resid(CN_lm_sed)
hist(res11)#guuuud
par(mfrow=c(2,2))
summary(CN_lm_sed)
plot(CN_lm_sed) #bowl shaped, no leverage
summary(CN_lm_sed)#p=0.0001, r2=0.79
CN_exp_sed<-glm(C.N~Distance,family="gaussian"(link=log),
                data=Sediment)
CN_log_sed<-glm(C.N~log(Distance),family='gaussian',
                data=Sediment)
plot(CN_log_sed)
summary(CN_log_sed)
summary(CN_lm_sed)
#LR = (1+F(dfnum/dfdenom))^(n/2)
#LR = 12823.16
plot(CN_exp_sed) #bowl-ish
summary(CN_exp_sed)
ggplot(Sediment,aes(x=Distance,y=C.N)) + 
  geom_point() +
  theme_bw() +
  geom_smooth(method="lm") +
  labs(x='Distance from colony (km)',y='Sediment C:N')
#LR = (1+F(dfnum/dfdenom))^(n/2)
p6<-ggplot(Sediment,aes(x=Distance,y=percent.C)) + 
  theme_bw() + 
  geom_point() + 
  geom_smooth(method='lm',
              se=FALSE) +
  theme(text=element_text(size=12)) + 
  labs(x="Distance from colony (km)",y="Sediment %C")

multiplot(p5,p6,cols=2)
#------------------------------------
#chlA
#add distance to chla
chla<-CSM_MOLLY_chla_data
#only need first twelve distances
Distance<-subset(Zoop_algae_sediment[1:12,])
Distance<-Distance$Distance
Distance<-Distance(c[1:12,])
chla<-cbind(chla,Distance)
#hooray
#exponential
chla_1<-nls(chla~a*exp(b*Distance),
            start=list(a=.1,b=-1),
            data=chla)
summary(chla_1)
#inverse
chla_2<-nls(chla~a+b*I(1/Distance^2),
           start=list(a=.01,b=.004),
           data=chla)
summary(chla_2)
res_chla<-resid(chla_2)
fit_chla<-fitted(chla_2)
plot(fit_chla,res_chla,
     cex.axis=1.5,cex.lab=1.5,
     pch=19,
     xlab='Fitted Values',
     ylab='Standardized Residuals')
par(mfrow=c(1,1))
hist(res_chla,
     cex.axis=1.5,cex.lab=1.5,cex.main=1.5,
     main='Histogram of chlorophyll a residuals, inverse model',
     xlab='Residuals')
#power
chla_3<-nls(chla~a*Distance^b,
           start=list(a=0.02,b=-1),data=chla)
summary(chla_3)           
#null
chla_null<-lm(chla~1,data=chla)
AIC(chla_1,chla_2,chla_3,chla_null)

ggplot(chla,aes(x=Distance,y=chla)) + 
  geom_point() + 
  theme_bw() + 
  labs(x='Distance from colony (km)')+ 
  ylab(bquote("Chlorophyll  "*alpha))
ggplot(chla,aes(x=Distance,y=chla)) + 
  geom_point() + 
  theme_bw() + 
  labs(x='Distance from colony (km)')+ 
  ylab(bquote("Chlorophyll  "*alpha)) + 
  geom_smooth(method="nls",
              formula=y~a+b*I(1/x^2),
              method.args=list(start=c(a=.01,b=0.004)),
              se=FALSE,
              data=chla) + 
  theme(text=element_text(size=14))
