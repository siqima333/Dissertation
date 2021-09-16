library(ggplot2)
library(gridExtra)
library(mgcv)
library(reshape2)
library(RColorBrewer)
library(scales)
UKdata<-read.csv("/Users/masiqi/Desktop/postgraduate-info/class/Dissertation/GAM model/UK.csv",header=TRUE)
# substitute unobserved data in column hospital and deaths into 0
UKdata$Hospital[1:52]<-0
UKdata$Deaths[1:31]<-0
# translate data into double type
UKdata$Hospital<-as.numeric(UKdata$Hospital)
UKdata$Deaths<-as.numeric(UKdata$Deaths)
UKdata$Cases<-as.numeric(UKdata$Cases)
# set start and end label
start<-2
lag<-0
# For consistence, I use the data when hospital admitted cases began to be recorded.
UKdata<-UKdata[52:(dim(UKdata)[1]-lag),]
# calculate time interval
timev<-UKdata$Day[start:(dim(UKdata)[1]-lag)]
Tv <- timev
# extract data about death, confirmed cases and hospitalization
death<-diff(UKdata[(start-1):(dim(UKdata)[1]-lag),5])
cases<-diff(UKdata[(start-1):(dim(UKdata)[1]-lag),3])
hospital<-diff(UKdata[(start-1):(dim(UKdata)[1]-lag),4])
#-----------------------------------------------------------------------------------#
# 1. Determine periodic effect ➡️ determine spline ➡️ determine k
# 2. For death data: Wesnesday-effect, B-spline, k=40
# 3. For confirmed cases data: Day-of-week effect excluding Saturday, Cubic regression, k=40
# 4. For hospitalization: weekends-effect, B-spline, k=30
f<-function(case,DW,k,spline){
  gam(case~s(Tv,bs=spline,k=k)+DW, family=nb(link="log"),method="REML")
}

## Death data
DW<-weekdays(as.Date(Tv,origin="2020-01-09"))
DW <- ifelse(DW=='Wednesday','Y',"N")

# process of choosing k
m1<-f(death,DW,k=20,spline="bs")
m2<-f(death,DW,k=30,spline="bs")
m3<-f(death,DW,k=40,spline="bs") # the best one
m4<-f(death,DW,k=60,spline="bs")
m5<-f(death,DW,k=80,spline="bs")
m6<-f(death,DW,k=120,spline="bs")
summary(m6)
AIC(m1,m2,m3,m4,m5,m6)
rsd <- residuals(m1)
gam(rsd~s(Tv,k=40,bs="cs"),gamma=1.4,data=UKdata) 
rsd <- residuals(m2)
gam(rsd~s(Tv,k=60,bs="cs"),gamma=1.4,data=UKdata) 
rsd <- residuals(m3)
gam(rsd~s(Tv,k=80,bs="cs"),gamma=1.4,data=UKdata) 
rsd <- residuals(m4)
gam(rsd~s(Tv,k=120,bs="cs"),gamma=1.4,data=UKdata) 
rsd <- residuals(m5)
gam(rsd~s(Tv,k=160,bs="cs"),gamma=1.4,data=UKdata) 
rsd <- residuals(m6)
gam(rsd~s(Tv,k=240,bs="cs"),gamma=1.4,data=UKdata) 

# plot
k1<-rep("k=20",length(fitted(m1)))
k2<-rep("k=30",length(fitted(m2)))
k3<-rep("k=40",length(fitted(m3)))
k4<-rep("k=60",length(fitted(m4)))
k5<-rep("k=80",length(fitted(m5)))
k6<-rep("k=120",length(fitted(m6)))
data_death<-data.frame(Death=c(fitted(m1),fitted(m2),fitted(m3),fitted(m4),fitted(m5),fitted(m6)),k=c(k1,k2,k3,k4,k5,k6))
colnames(data_death)<-c("Death","K")
realdeath<-data.frame(x=as.Date(Tv, origin ="2020-01-09"),y=death)
p1<-ggplot(data=realdeath,aes(x=as.Date(Tv, origin = "2020-01-09"),y=death),size=0.5)+
  geom_point(colour="#6BAED6")+
  geom_line(data=data_death[which(data_death$K=="k=20"),],aes(y=Death),size=1)+
  labs(subtitle = "k=20",x="",y="")+
  scale_x_continuous(breaks=c(realdeath$x[200],realdeath$x[250],realdeath$x[300],realdeath$x[17]),labels=factor(c("27-Aug","5-Nov","25-Dec","17-Mar")))+
  theme(axis.text.x=element_text(size=7))
p2<-ggplot(data=realdeath,aes(x=as.Date(Tv, origin = "2020-01-09"),y=death),size=0.5)+
  geom_point(colour="#6BAED6")+
  geom_line(data=data_death[which(data_death$K=="k=30"),],aes(y=Death),size=1)+
  labs(subtitle = "k=30",x="",y="")+
  scale_x_continuous(breaks=c(realdeath$x[200],realdeath$x[250],realdeath$x[300],realdeath$x[17]),labels=factor(c("27-Aug","5-Nov","25-Dec","17-Mar")))+
  theme(axis.text.x=element_text(size=7))
p3<-ggplot(data=realdeath,aes(x=as.Date(Tv, origin = "2020-01-09"),y=death),size=0.5)+
  geom_point(colour="#6BAED6")+
  geom_line(data=data_death[which(data_death$K=="k=40"),],aes(y=Death),size=1)+
  labs(subtitle = "k=40",x="",y="")+
  scale_x_continuous(breaks=c(realdeath$x[200],realdeath$x[250],realdeath$x[300],realdeath$x[17]),labels=factor(c("27-Aug","5-Nov","25-Dec","17-Mar")))+
  theme(axis.text.x=element_text(size=7))
p4<-ggplot(data=realdeath,aes(x=as.Date(Tv, origin = "2020-01-09"),y=death),size=0.5)+
  geom_point(colour="#6BAED6")+
  geom_line(data=data_death[which(data_death$K=="k=60"),],aes(y=Death),size=1)+
  labs(subtitle = "k=60",x="",y="")+
  scale_x_continuous(breaks=c(realdeath$x[200],realdeath$x[250],realdeath$x[300],realdeath$x[17]),labels=factor(c("27-Aug","5-Nov","25-Dec","17-Mar")))+
  theme(axis.text.x=element_text(size=7))
p5<-ggplot(data=realdeath,aes(x=as.Date(Tv, origin = "2020-01-09"),y=death),size=0.5)+
  geom_point(colour="#6BAED6")+
  geom_line(data=data_death[which(data_death$K=="k=80"),],aes(y=Death),size=1)+
  labs(subtitle = "k=80",x="",y="")+
  scale_x_continuous(breaks=c(realdeath$x[200],realdeath$x[250],realdeath$x[300],realdeath$x[17]),labels=factor(c("27-Aug","5-Nov","25-Dec","17-Mar")))+
  theme(axis.text.x=element_text(size=7))
p6<-ggplot(data=realdeath,aes(x=as.Date(Tv, origin = "2020-01-09"),y=death),size=0.5)+
  geom_point(colour="#6BAED6")+
  geom_line(data=data_death[which(data_death$K=="k=120"),],aes(y=Death),size=1)+
  labs(subtitle = "k=120",x="",y="")+
  scale_x_continuous(breaks=c(realdeath$x[200],realdeath$x[250],realdeath$x[300],realdeath$x[17]),labels=factor(c("27-Aug","5-Nov","25-Dec","17-Mar")))+
  theme(axis.text.x=element_text(size=7))
grid.arrange(p1,p2,p3,p4,p5,p6,ncol=3)

## Confirmed cases data
DW<-weekdays(as.Date(Tv,origin="2020-01-09"))
DW <- ifelse(DW=='Saturday','Y',"N")
c1<-f(cases,DW,k=20,spline="cr")
c2<-f(cases,DW,k=30,spline="cr")
c3<-f(cases,DW,k=40,spline="cr") # the best one
c4<-f(cases,DW,k=60,spline="cr")
c5<-f(cases,DW,k=80,spline="cr")
c6<-f(cases,DW,k=120,spline="cr")
rsd <- residuals(c1)
gam(rsd~s(Tv,k=40,bs="cs"),gamma=1.4,data=UKdata) 
rsd <- residuals(c2)
gam(rsd~s(Tv,k=60,bs="cs"),gamma=1.4,data=UKdata) 
rsd <- residuals(c3)
gam(rsd~s(Tv,k=80,bs="cs"),gamma=1.4,data=UKdata) 
rsd <- residuals(c4)
gam(rsd~s(Tv,k=120,bs="cs"),gamma=1.4,data=UKdata) 
rsd <- residuals(c5)
gam(rsd~s(Tv,k=160,bs="cs"),gamma=1.4,data=UKdata) 
rsd <- residuals(c6)
gam(rsd~s(Tv,k=240,bs="cs"),gamma=1.4,data=UKdata)
## Hospitalization
DW<-weekdays(as.Date(Tv,origin="2020-01-09"))
DW <- ifelse(DW=='Saturday','Y',ifelse(DW=="Sunday","Y","N"))
h1<-f(hospital,DW,k=20,spline="bs")
h2<-f(hospital,DW,k=30,spline="bs") # the best one
h3<-f(hospital,DW,k=40,spline="bs")
h4<-f(hospital,DW,k=60,spline="bs")
h5<-f(hospital,DW,k=80,spline="bs")
h6<-f(hospital,DW,k=120,spline="bs")
#-----------------------------------------------------------------------------------#
# find the peak point
c_max<-Tv[which.max(c3$fitted.values)] # case 339
d_max<-Tv[which.max(m3$fitted.values)] # death 356
h_max<-Tv[which.max(h2$fitted)]  # hospital 347
#-----------------------------------------------------------------------------------#
## use bootstrap to find CI
# Method 1
# step1: generate  \epsilon from gam fitted to real-world data
# step2: bootstrap B's \epsilon*
# step3: find y*, y*= gam fitted + \epsilon*
# step3: find 0.01 quantile and 0.99 quantile of y*
B<-1000
fittedvalue<-function(B,x,xfitted,k){
  res<-x-xfitted
  n<-length(res)
  yfitted<-bootfit<-matrix(0,nrow=B,ncol=n) # store fitted value from bootstrap sample
  for(i in 1:B){
    ind<-sample(res,n,replace = TRUE)
    for(q in 1:n){   
      bootfit[i,q]<-data.frame(ind)[q,1]
      yfitted[i,q]<-xfitted[q]+bootfit[i,q]
    }
  }
  yfitted
}
quantilef<-function(data,tup,tdown){
  n<-length(Tv)
  nintyfiveq<-zerozerofiveq<-0
  for (i in 1:n) {
    nintyfiveq[i]<-quantile(data[,i],tup)
    zerozerofiveq[i]<-quantile(data[,i],tdown)
  }
  data.frame(nintyfiveq,zerozerofiveq)
}

deathboot<-fittedvalue(B,death,fitted(m3),k=40)
caseboot<-fittedvalue(B,cases,fitted(c3),k=40)
hospitalboot<-fittedvalue(B,hospital,fitted(h2),k=30)

deathup<-quantilef(deathboot,0.99,0.01)[1]
deathdown<-quantilef(deathboot,0.99,0.01)[2]

caseup<-quantilef(caseboot,0.99,0.01)[1]
casedown<-quantilef(caseboot,0.99,0.01)[2]
which.max(deathup$nintyfiveq)-which.max(c3$fitted.values)
which.max(deathdown$zerozerofiveq)-which.max(c3$fitted.values)
hospitalup<-quantilef(hospitalboot,0.99,0.01)[1]
hospitalsown<-quantilef(hospitalboot,0.99,0.01)[2]
casedata<-data.frame(Tv,c3$fitted.values,caseup,casedown)
colnames(casedata)<-c("Tv","Fitted","Up","Down")
casedata2<-melt(casedata,id.vars ="Tv")
#-----------------------------------------------------------------------------------#
# plot to point out 
fitteddata<-data.frame(Tv, UKdata$Date[1:length(Tv)], c3$fitted.values, m3$fitted.values, h2$fitted.values)
colnames(fitteddata)<-c("Tv", "Date", "Cases", "Deaths", "Hospitalizations")
data_max<-c(which.max(c3$fitted.values),which.max(m3$fitted.values),which.max(h2$fitted.values))
fitteddata$Date[data_max]
cols<-c(brewer.pal(n=9,name="Blues"))
a<-data.frame(round(quantile(Tv,probs=c(0,1)),0))
colnames(a)<-c("quantile")
pos<-fitteddata$Date[fitteddata$Tv==a$quantile]
redcol<-c(brewer.pal(n=9,name="Reds"))

c<-ggplot(data=fitteddata,aes(x=Tv,y=Cases))+
  geom_rect(aes(xmin=Tv[Tv==53],xmax=Tv[Tv==102],ymin=-Inf, ymax=Inf),fill="#D9D9D9",alpha=0.4)+
  geom_rect(aes(xmin=Tv[Tv==259],xmax=Tv[Tv==290],ymin=-Inf, ymax=Inf),fill="#D9D9D9",alpha=0.4)+
  geom_rect(aes(xmin=Tv[Tv==333],xmax=Tv[Tv==536],ymin=-Inf, ymax=Inf),fill=cols[2],alpha=0.4)+
  geom_rect(aes(xmin=Tv[Tv==341],xmax=Tv[Tv==402],ymin=-Inf, ymax=Inf),fill="#F0F0F0",alpha=0.4)+
  geom_rect(aes(xmin=Tv[Tv==312],xmax=Tv[Tv==333],ymin=-Inf, ymax=Inf),fill=cols[1])+
  geom_rect(aes(xmin=Tv[Tv==536],xmax=Inf,ymin=-Inf, ymax=Inf),fill=cols[3],alpha=0.4)+
  geom_line(colour="#6BAED6",size=0.8)+
  geom_point(data=fitteddata, aes(x=Tv[data_max[1]],y=max(c3$fitted.values)))+
  scale_x_continuous(breaks = Tv[data_max[1]],labels = fitteddata$Date[data_max][1] )+
  geom_vline(xintercept =Tv[data_max[1]],colour="#EF3B2C",linetype="dotted")+
  labs(x="")+
  geom_text(data=data.frame(Tv=data_max[1],Cases=round(max(fitteddata$Cases),0)),aes(label=Cases),size=3)

d<-ggplot(data=fitteddata,aes(x=Tv,y=Deaths))+
  geom_rect(aes(xmin=Tv[Tv==53],xmax=Tv[Tv==102],ymin=-Inf, ymax=Inf),fill="#D9D9D9",alpha=0.4)+
  geom_rect(aes(xmin=Tv[Tv==259],xmax=Tv[Tv==290],ymin=-Inf, ymax=Inf),fill="#D9D9D9",alpha=0.4)+
  geom_rect(aes(xmin=Tv[Tv==333],xmax=Tv[Tv==536],ymin=-Inf, ymax=Inf),fill=cols[2],alpha=0.4)+
  geom_rect(aes(xmin=Tv[Tv==341],xmax=Tv[Tv==402],ymin=-Inf, ymax=Inf),fill="#F0F0F0",alpha=0.4)+
  geom_rect(aes(xmin=Tv[Tv==312],xmax=Tv[Tv==333],ymin=-Inf, ymax=Inf),fill=cols[1])+
  geom_rect(aes(xmin=Tv[Tv==536],xmax=Inf,ymin=-Inf, ymax=Inf),fill=cols[3],alpha=0.4)+
  geom_line(colour="#6BAED6",size=0.8)+
  geom_point(data=fitteddata, aes(x=Tv[data_max[2]],y=max(m3$fitted.values)))+
  scale_x_continuous(breaks = Tv[data_max[2]],labels = fitteddata$Date[data_max][2])+
  geom_vline(xintercept =Tv[data_max[2]],colour="#EF3B2C",linetype="dotted")+
  labs(x="")+
  geom_text(data=data.frame(Tv=data_max[2],Deaths=round(max(fitteddata$Deaths),0)),aes(label=Deaths),size=3)

h<-ggplot(data=fitteddata,aes(x=Tv,y=Hospitalizations))+
  geom_rect(aes(xmin=Tv[Tv==53],xmax=Tv[Tv==102],ymin=-Inf, ymax=Inf),fill="#D9D9D9",alpha=0.4)+
  geom_rect(aes(xmin=Tv[Tv==259],xmax=Tv[Tv==290],ymin=-Inf, ymax=Inf),fill="#D9D9D9",alpha=0.4)+
  geom_rect(aes(xmin=Tv[Tv==333],xmax=Tv[Tv==536],ymin=-Inf, ymax=Inf),fill=cols[2],alpha=0.4)+
  geom_rect(aes(xmin=Tv[Tv==341],xmax=Tv[Tv==402],ymin=-Inf, ymax=Inf),fill="#F0F0F0",alpha=0.4)+
  geom_rect(aes(xmin=Tv[Tv==312],xmax=Tv[Tv==333],ymin=-Inf, ymax=Inf),fill=cols[1])+
  geom_rect(aes(xmin=Tv[Tv==536],xmax=Inf,ymin=-Inf, ymax=Inf),fill=cols[3],alpha=0.4)+
  geom_line(colour="#6BAED6",size=0.8)+
  geom_point(data=fitteddata, aes(x=Tv[data_max[3]],y=max(h2$fitted.values)))+
  scale_x_continuous(breaks = Tv[c(1,520,data_max[3])],labels =c(pos,fitteddata$Date[data_max][3]))+
  #scale_x_continuous(breaks = Tv[data_max[3]],labels = c(1:length(Tv))[data_max[3]] )+
  geom_vline(xintercept =Tv[data_max[3]],colour="#EF3B2C",linetype="dotted")+
  labs(x="Date")+
  geom_text(data=data.frame(Tv=data_max[3],Hospitalizations=round(max(fitteddata$Hospitalizations),0)),aes(label=Hospitalizations),size=3)

grid.arrange(c,d,h,ncol=1)

#-----------------------------------------------------------------------------------#
# plot s'(t)
# DWtype=c("death","case","hospital")
smoothderivative<-function(DWtype,fit,k){
  DW<-weekdays(as.Date(Tv,origin="2020-01-09"))
  eps<-1e-7
  xv<-seq(min(Tv),max(Tv), length=520)+eps
  dow<-weekdays(as.Date(xv, origin = "2020-01-09")) 
  if(DWtype=="death"){
    DW <- ifelse(DW=='Wednesday','Y',"N")
    dow<-ifelse(dow=='Wednesday','Y',"N")
    off<-2
    }
  if(DWtype=="case"){
    DW <- ifelse(DW=='Saturday','Y',"N")
    dow<- ifelse(dow=='Saturday','Y',"N")
    off<-2
  }
  if(DWtype=="hospital"){
    DW <- ifelse(DW=='Saturday','Y',ifelse(DW=="Sunday","Y","N"))
    dow <- ifelse(dow=='Saturday','Y',ifelse(dow=="Sunday","Y","N"))
    off<-2
  }
  newd<-data.frame(Tv,DW)
  X0 <- predict(fit, newd,type="lpmatrix")
  newd1<-data.frame(Tv=xv,DW=dow)
  X1 <- predict(fit, newd1,type="lpmatrix")
  Xp <- (X1-X0)/eps
  Xi <- Xp*0 
  Xi[,1:(k-1)+off] <- Xp[,1:(k-1)+off]
  df <- Xi%*%coef(fit)              ## ith smooth derivative 
  df.sd <- rowSums(Xi%*%fit$Vp*Xi)^.5
  sdt<-df
  sdtup<-df+2*df.sd
  sdtlow<-df-2*df.sd
  doub <- ifelse(sdt < 0, 100, log(2)/sdt)
  doubup <- ifelse(sdtup < 0, 100, log(2)/sdtup)
  doublow <- ifelse(sdtlow < 0, 100, log(2)/sdtlow)
  res<-data.frame(Tv,UKdata$Date[2:521],sdt,sdtup,sdtlow,doub,doubup,doublow)
}
# death
deathst<-smoothderivative("death",m3,k=40)
colnames(deathst)<-c("Tv","Date","d-sdt","d-sdtup","d-sdtlow","d-doub","d-doubup","d-doublow")
casest<-smoothderivative("case",c3,k=40)
colnames(casest)<-c("Tv","Date","c-sdt","c-sdtup","c-sdtlow","c-doub","c-doubup","c-doublow")
hospitalst<-smoothderivative("hospital",h2,k=30)
colnames(hospitalst)<-c("Tv","Date","h-sdt","h-sdtup","h-sdtlow","h-doub","h-doubup","h-doublow")
datast<-data.frame(casest,deathst[,3:6],hospitalst[3:6])
datast2<-melt(datast[,-2],id.vars = "Tv")

ggplot(datast,aes(x=Tv))+ylim(c(-0.08,0.1))+
  geom_rect(aes(xmin=Tv[Tv==53],xmax=Tv[Tv==102],ymin=-Inf, ymax=Inf),fill="#D9D9D9",alpha=0.4)+
  geom_rect(aes(xmin=Tv[Tv==259],xmax=Tv[Tv==290],ymin=-Inf, ymax=Inf),fill="#D9D9D9",alpha=0.4)+
  geom_rect(aes(xmin=Tv[Tv==333],xmax=Tv[Tv==536],ymin=-Inf, ymax=Inf),fill=cols[2],alpha=0.4)+
  geom_rect(aes(xmin=Tv[Tv==341],xmax=Tv[Tv==402],ymin=-Inf, ymax=Inf),fill="#F0F0F0",alpha=0.4)+
  geom_rect(aes(xmin=Tv[Tv==312],xmax=Tv[Tv==333],ymin=-Inf, ymax=Inf),fill=cols[1])+
  geom_rect(aes(xmin=Tv[Tv==536],xmax=Inf,ymin=-Inf, ymax=Inf),fill=cols[3],alpha=0.4)+
  geom_line(aes(y=c.sdt,colour="Confirmed cases"))+
  #geom_ribbon(aes(ymin =c.sdtlow ,ymax = c.sdtup,fill ="#4292C6"),alpha=0.3)+
  geom_line(aes(y=d.sdt,colour="Death"),alpha=0.8)+
  #geom_ribbon(aes(ymin = d.sdtlow,ymax =d.sdtup ),fill ="#08519C",alpha=0.3)+
  geom_line(aes(y=h.sdt,colour="Hospitalization"),alpha=0.8)+
  #geom_ribbon(aes(ymin = h.sdtlow,ymax = h.sdtup ),fill ="#74C476",alpha=0.3)
  scale_colour_manual(values=c("#4292C6","#41AB5D","#737373"))+  
  scale_x_continuous(breaks = c(Tv[Tv==312],Tv[Tv==334]),labels =c("8-Dec","30-Dec"))+
  scale_fill_discrete(breaks=c("Confirmed cases","Death","Hospitalization"))+
  theme(legend.position = "top",legend.title = element_blank(),axis.text.x = element_text(size=7))+
  labs(x="",y="Instantaneous growth rate")+
  geom_hline(yintercept = 0,colour="red",alpha=0.3)

#-----------------------------------------------------------------------------------------------#
# calculate structerd people data
vaccine<-read.csv("/Users/masiqi/Desktop/postgraduate-info/class/Dissertation/GAM model/vaccination rate.csv",header=TRUE)
as.numeric(vaccine$under65)
as.numeric(vaccine$over65)
# death
structured<-read.csv("/Users/masiqi/Desktop/postgraduate-info/class/Dissertation/GAM model/structured.csv",header=TRUE)
as.numeric(structured$Death.under65)
as.numeric(structured$Death.over65)
start<-2
lag<-0
# calculate week interval
week<-structured$Day
Wv<-week
M<-c(rep("Feb",3),rep("Mar",4),rep("Apr",4),rep("May",4),rep("Jun",5),rep("Jul",5),rep("Aug",3))
length(Wv)
fdeath_under65<-gam(structured$Death.under65~s(Wv,bs="bs",k=10), family=nb(link="log"),method="REML")
fdeath_over65<-gam(structured$Death.over65~s(Wv,bs="bs",k=10), family=nb(link="log"),method="REML")
data<-data.frame(Wv,structured$Date,fdeath_under65$fitted.values,fdeath_over65$fitted.values)
colnames(data)<-c("Wv","Date","under65","over65")

# plot death number
deathsnumber<-ggplot(data=data,aes(x=Wv))+
  geom_rect(aes(xmin=Wv[1],xmax=Wv[23.5],ymin=-Inf, ymax=Inf),fill=cols[2],alpha=0.4)+
  geom_rect(aes(xmin=Wv[1],xmax=Wv[4.8],ymin=-Inf, ymax=Inf),fill="#F0F0F0",alpha=0.4)+
  geom_rect(aes(xmin=Wv[23.5],xmax=Wv[28],ymin=-Inf, ymax=Inf),fill=cols[3],alpha=0.4)+
  geom_line(aes(y=over65,colour="65 and over"))+
  geom_line(aes(y=under65,colour="Under 65"))+
  scale_x_continuous(breaks=Wv[c(1,4,7,10,13,16,19,22,25,28)],labels =data$Date[c(1,4,7,10,13,16,19,22,25,28)] )+
  geom_hline(yintercept = 0,colour="black",alpha=0.5)+
  theme(legend.position = "right",legend.title = element_blank(),axis.text.x = element_text(size=7),
        legend.text = element_text(size=5),title = element_text(size=8))+
  labs(subtitle = "Death",y="Daily increase number",x="")

fsdf<-function(data,k,Wv){
  newd<-data.frame(Wv)
  fit<-data
  X0 <- predict(fit, newd,type="lpmatrix")
  eps<-1e-7
  wv<-seq(min(Wv),max(Wv), length=length(Wv))+eps
  newd1<-data.frame(Wv=wv)
  X1 <- predict(fit, newd1,type="lpmatrix")
  Xp <- (X1-X0)/eps
  Xi <- Xp*0 
  off<-1
  Xi[,1:(k-1)+off] <- Xp[,1:(k-1)+off]
  df <- Xi%*%coef(fit)              ## ith smooth derivative 
  df.sd <- rowSums(Xi%*%fit$Vp*Xi)^.5
  sdt<-df
  sdtup<-df+2*df.sd
  sdtlow<-df-2*df.sd
  doub <- ifelse(sdt < 0, 100, log(2)/sdt)
  doubup <- ifelse(sdtup < 0, 100, log(2)/sdtup)
  doublow <- ifelse(sdtlow < 0, 100, log(2)/sdtlow)
  res<-data.frame(sdt,sdtup,sdtlow,doub,doublow,doublow)
}
sdeathunder65<-fsdf(fdeath_under65,10,Wv)
sdeathover65<-fsdf(fdeath_over65,10,Wv)
datadeath<-data.frame(Wv,structured$Date,sdeathunder65$sdt,sdeathunder65$sdtup,sdeathunder65$sdtlow,
                      sdeathover65$sdt,sdeathover65$sdtup,sdeathover65$sdtlow)
names(datadeath)<-c("Wv","Date","under65","under65up","under65low","over65","over65up","over65low")

#datadeath<-melt(datadeath[,-2],id.vars ="Wv")
deaths<-ggplot(data=datadeath,aes(x=Wv))+
  geom_rect(aes(xmin=Wv[1],xmax=Wv[23.5],ymin=-Inf, ymax=Inf),fill=cols[2],alpha=0.4)+
  geom_rect(aes(xmin=Wv[1],xmax=Wv[4.8],ymin=-Inf, ymax=Inf),fill="#F0F0F0",alpha=0.4)+
  geom_rect(aes(xmin=Wv[23.5],xmax=Wv[28],ymin=-Inf, ymax=Inf),fill=cols[3],alpha=0.4)+
  geom_line(aes(y=over65,colour="65 and over"))+
  geom_line(aes(y=under65,colour="Under 65"))+
  geom_ribbon(aes(ymin =over65low ,ymax = over65up, fill="65 and over: 95% CI"),alpha=0.3)+
  geom_ribbon(aes(ymin =under65low ,ymax = under65up,fill="Under 65: 95% CI"),alpha=0.3)+
  scale_x_continuous(breaks=Wv[c(1,4,7,10,13,16,19,22,25,28)],labels =datadeath$Date[c(1,4,7,10,13,16,19,22,25,28)] )+
  geom_hline(yintercept = 0,colour="black",alpha=0.5)+
  theme(legend.position = "right",legend.title = element_blank(),axis.text.x = element_text(size=7),
        legend.text = element_text(size=5),title = element_text(size=8))+
  labs(subtitle = "Death",y="Instantaneous growth rate",x="")


# hospital
hospitalstructure<-read.csv("/Users/masiqi/Desktop/postgraduate-info/class/Dissertation/GAM model/structured-hospital.csv",header=TRUE)
as.numeric(hospitalstructure$Hospital.under65)
as.numeric(hospitalstructure$Hospital.over65)
as.numeric(hospitalstructure$Day)
week<-hospitalstructure$Day
Wv<-week
fhospital_under65<-gam(data=hospitalstructure,hospitalstructure$Hospital.under65~s(Wv,bs="bs",k=10), family=nb(link="log"),method="REML")
fhospital_over65<-gam(data=hospitalstructure,hospitalstructure$Hospital.over65~s(Wv,bs="bs",k=10), family=nb(link="log"),method="REML")
# plot hospital number
data<-data.frame(Wv,hospitalstructure$Date,fhospital_under65$fitted.values,fhospital_over65$fitted.values)
colnames(data)<-c("Wv","Date","under65","over65")
hospitalnumber<-ggplot(data=data,aes(x=Wv))+
  geom_rect(aes(xmin=Wv[1],xmax=Wv[23.5],ymin=-Inf, ymax=Inf),fill=cols[2],alpha=0.4)+
  geom_rect(aes(xmin=Wv[1],xmax=Wv[4.8],ymin=-Inf, ymax=Inf),fill="#F0F0F0",alpha=0.4)+
  geom_rect(aes(xmin=Wv[23.5],xmax=Wv[29],ymin=-Inf, ymax=Inf),fill=cols[3],alpha=0.4)+
  geom_line(aes(y=over65,colour="65 and over"))+
  geom_line(aes(y=under65,colour="Under 65"))+
  scale_x_continuous(breaks=Wv[c(1,4,7,10,13,16,19,22,25,28)],labels =data$Date[c(1,4,7,10,13,16,19,22,25,28)] )+
  geom_hline(yintercept = 0,colour="black",alpha=0.5)+
  theme(legend.position = "right",legend.title = element_blank(),axis.text.x = element_text(size=7),
        legend.text = element_text(size=5),title = element_text(size=8))+
  labs(subtitle = "Hospitalization",y="Daily increase number",x="")

shospitalunder65<-fsdf(fhospital_under65,10,Wv)
shospitalover65<-fsdf(fhospital_over65,10,Wv)
datahospital<-data.frame(Wv,hospitalstructure$Date,shospitalunder65$sdt,shospitalunder65$sdtup,shospitalunder65$sdtlow,
                         shospitalover65$sdt,shospitalover65$sdtup,shospitalover65$sdtlow)
names(datahospital)<-c("Wv","Date","under65","under65up","under65low","over65","over65up","over65low")
hospitalization<-ggplot(data=datahospital,aes(x=Wv))+
  geom_rect(aes(xmin=Wv[1],xmax=Wv[23.5],ymin=-Inf, ymax=Inf),fill=cols[2],alpha=0.4)+
  geom_rect(aes(xmin=Wv[1],xmax=Wv[4.8],ymin=-Inf, ymax=Inf),fill="#F0F0F0",alpha=0.4)+
  geom_rect(aes(xmin=Wv[23.5],xmax=Wv[29],ymin=-Inf, ymax=Inf),fill=cols[3],alpha=0.4)+
  geom_line(aes(y=over65,colour="65 and over"))+
  geom_line(aes(y=under65,colour="Under 65"))+
  geom_ribbon(aes(ymin =over65low ,ymax = over65up, fill="65 and over: 95% CI"),alpha=0.3)+
  geom_ribbon(aes(ymin =under65low ,ymax = under65up,fill="Under 65: 95% CI"),alpha=0.3)+
  scale_x_continuous(breaks=Wv[c(1,4,7,10,13,16,19,22,25,28)],labels =datahospital$Date[c(1,4,7,10,13,16,19,22,25,28)] )+
  geom_hline(yintercept = 0,colour="black",alpha=0.5)+
  theme(legend.position = "right",legend.title = element_blank(),axis.text.x = element_text(size=7),
        legend.text = element_text(size=5),title = element_text(size=8))+
  labs(subtitle = "Hospitalization",y="Instantaneous growth rate",x="")
# positive
positivestructure<-read.csv("/Users/masiqi/Desktop/postgraduate-info/class/Dissertation/GAM model/structure-positive.csv",header=TRUE)
as.numeric(positivestructure$Positive.under70)
as.numeric(positivestructure$Positive.over70)
as.numeric(positivestructure$Day)
week<-positivestructure$Day
Wv<-week
fpositive_under70<-gam(data=positivestructure,Positive.under70~s(Wv,bs="cr",k=10), family=nb(link="log"),method="REML")
fpositive_over70<-gam(data=positivestructure,Positive.over70~s(Wv,bs="cr",k=10), family=nb(link="log"),method="REML")
# plot positiven number
data<-data.frame(Wv,positivestructure$Date,fpositive_under70$fitted.values,fpositive_over70$fitted.values)
data1<-data.frame(Wv,positivestructure$Date,fpositive_under70$fitted.values/57790115,fpositive_over70$fitted.values/9006692)
colnames(data1)<-c("Wv","Date","under70","over70")
positiverate<-ggplot(data1,aes(x=Wv))+
  geom_rect(aes(xmin=Wv[1],xmax=Wv[23.5],ymin=-Inf, ymax=Inf),fill=cols[2],alpha=0.4)+
  geom_rect(aes(xmin=Wv[1],xmax=Wv[4.8],ymin=-Inf, ymax=Inf),fill="#F0F0F0",alpha=0.4)+
  geom_rect(aes(xmin=Wv[23.5],xmax=Wv[29],ymin=-Inf, ymax=Inf),fill=cols[3],alpha=0.4)+
  geom_line(aes(y=over70,colour="70 and over"))+
  geom_line(aes(y=under70,colour="Under 70"))+ 
  scale_x_continuous(breaks=Wv[c(1,4,7,10,13,16,19,22,25,28)],labels =data$Date[c(1,4,7,10,13,16,19,22,25,28)] )+
  geom_hline(yintercept = 0,colour="black",alpha=0.5)+
  theme(legend.position = "right",legend.title = element_blank(),axis.text.x = element_text(size=7),
        legend.text = element_text(size=5),title = element_text(size=8))+
  labs(subtitle = "Confirmed cases",y="Daily increase rate",x="")

grid.arrange(positivenumber, positiverate,ncol=1)  


colnames(data)<-c("Wv","Date","under70","over70")
positivenumber<-ggplot(data=data,aes(x=Wv))+
  geom_rect(aes(xmin=Wv[1],xmax=Wv[23.5],ymin=-Inf, ymax=Inf),fill=cols[2],alpha=0.4)+
  geom_rect(aes(xmin=Wv[1],xmax=Wv[4.8],ymin=-Inf, ymax=Inf),fill="#F0F0F0",alpha=0.4)+
  geom_rect(aes(xmin=Wv[23.5],xmax=Wv[29],ymin=-Inf, ymax=Inf),fill=cols[3],alpha=0.4)+
  geom_line(aes(y=over70,colour="70 and over"))+
  geom_line(aes(y=under70,colour="Under 70"))+
  scale_x_continuous(breaks=Wv[c(1,4,7,10,13,16,19,22,25,28)],labels =data$Date[c(1,4,7,10,13,16,19,22,25,28)] )+
  geom_hline(yintercept = 0,colour="black",alpha=0.5)+
  theme(legend.position = "right",legend.title = element_blank(),axis.text.x = element_text(size=7),
        legend.text = element_text(size=5),title = element_text(size=8))+
  labs(subtitle = "Confirmed cases",y="Daily increase number",x="")
grid.arrange( deathsnumber, hospitalnumber,ncol=1)


spositiveuner70<-fsdf(fpositive_under70,10,Wv)
spositiveover70<-fsdf(fpositive_over70,10,Wv)
datapositive<-data.frame(Wv,positivestructure$Date,spositiveuner70$sdt,spositiveuner70$sdtup,spositiveuner70$sdtlow,
                         spositiveover70$sdt,spositiveover70$sdtup,spositiveover70$sdtlow)
names(datapositive)<-c("Wv","Date","under70","under70up","under70low","over70","over70up","over70low")
positive<-ggplot(data=datapositive,aes(x=Wv))+
  geom_rect(aes(xmin=Wv[1],xmax=Wv[23.5],ymin=-Inf, ymax=Inf),fill=cols[2],alpha=0.4)+
  geom_rect(aes(xmin=Wv[1],xmax=Wv[4.8],ymin=-Inf, ymax=Inf),fill="#F0F0F0",alpha=0.4)+
  geom_rect(aes(xmin=Wv[23.5],xmax=Wv[29],ymin=-Inf, ymax=Inf),fill=cols[3],alpha=0.4)+
  geom_line(aes(y=over70,colour="70 and over"))+
  geom_line(aes(y=under70,colour="Under 70"))+
  geom_ribbon(aes(ymin =over70low ,ymax = over70up, fill="70 and over: 95% CI"),alpha=0.3)+
  geom_ribbon(aes(ymin =under70low ,ymax = under70up,fill="Under 70: 95% CI"),alpha=0.3)+
  scale_x_continuous(breaks=Wv[c(1,4,7,10,13,16,19,22,25,28)],labels =positivestructure$Date[c(1,4,7,10,13,16,19,22,25,28)] )+
  geom_hline(yintercept = 0,colour="black",alpha=0.5)+
  theme(legend.position = "right",legend.title = element_blank(),axis.text.x = element_text(size=7),
        legend.text = element_text(size=5),title = element_text(size=8))+
  labs(subtitle = "Confirmed cases",y="Instantaneous growth rate",x="")
grid.arrange(deaths, hospitalization,ncol=1)


