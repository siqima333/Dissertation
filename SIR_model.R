# (1) Let nH, nL denote proportion of high- and low-risk group population.
# (2) Let hH, hH denote vaccination rate of high- and low-risk group population.
# (3) Let vH, vL denote vaccine effectiveness for high- and low-risk group population.
## Assume b1=1, nH==nL=0.5
R0 <- 2.5 # 4,3,2.5,2,1.4

par(mfrow=c(1,2))
# case 1: hH==hL 

# for one dose
vH <- 0.55
vL <- 0.65
hH <- seq(0,1,0.01)
A<-1-(vH+vL)*hH/2
B<-(vH+vL)^2*hH^2/4
C<-(1-vH*hH)*(1-vL*hH)
AA<-1-C*R0^2
BB<-2-2*R0*A
CC<-B*R0^2+2*R0*A-R0^2*A^2-1
b2_1<-(-BB)+sqrt((BB)^2+4*(A)*(C))/2/(A) # It seems b2_1 is a reasonable solution.
#b2_2<-(-BB)-sqrt((BB)^2+4*(A)*(C))/2/(A)
b2<-b2_1
Rv <- R0/(1+b2)*(1-(vH+vL)/2*hH+sqrt((vH-vL)^2/4*hH^2+b2^2*(1-vH*hH)*(1-vL*hH)))
plot(hH, Rv, type='l', ylim=c(0,5))
abline(h=1)

# for two dose
vH <- 0.9
vL <- 0.8 
hH <- seq(0,1,0.01)
A<-1-(vH+vL)*hH/2
B<-(vH+vL)^2*hH^2/4
C<-(1-vH*hH)*(1-vL*hH)
AA<-1-C*R0^2
BB<-2-2*R0*A
CC<-B*R0^2+2*R0*A-R0^2*A^2-1
b2_1<-(-BB)+sqrt((BB)^2+4*(A)*(C))/2/(A) # It seems b2_1 is a reasonable solution.
#b2_2<-(-BB)-sqrt((BB)^2+4*(A)*(C))/2/(A)
b2<-b2_1
Rv <- R0/(1+b2)*(1-(vH+vL)/2*hH+sqrt((vH-vL)^2/4*hH^2+b2^2*(1-vH*hH)*(1-vL*hH)))
plot(hH, Rv, type='l', ylim=c(0,5))
abline(h=1)
# calculate vaccination rate that achieves herd immunity
VRdata<-data.frame(hH=hH,Rv)
VR<-VRdata[which(VRdata$Rv<1),] # hH==hL==0.96, the herd immunity will be achieved

# When R0=5,4,3,2.5, for one-dose VE, herd immunity will never be achieved
# When R0=2, for one-dose VE, herd immunity will be achieved at hH==hL==0.84
# When R0=1.4, for one-dose VE, herd immunity will be achieved at hH==hL==0.48
# When R0=5, for two-dose VE, herd immunity will be achieved at hH==hL==0.96
# When R0=4, for two-dose VE, herd immunity will be achieved at hH==hL==0.89
# When R0=3, for two-dose VE, herd immunity will be achieved at hH==hL==0.79
# When R0=2.5, for two-dose VE, herd immunity will be achieved at hH==hL==0.71
# When R0=2, for two-dose VE, herd immunity will be achieved at hH==hL==0.59
# When R0=1.4, for two-dose VE, herd immunity will be achieved at hH==hL==0.34

#--------------------------------------------------------------------------------------------------#
# case 2: fixed hL=0.5, 0.6, 0.7, 0.8, 0.9
R0=1.4
hH <- seq(0,1,0.01)
hL<-c(0.5,0.6,0.7,0.8,0.9)

# For one dose
vH <- 0.55
vL <- 0.65 
par(mfrow=c(2,3))
for(i in 1:length(hL)){
  A<-1-(vH*hH+vL*hL[i])/2
  B<-(vH*hH-vL*hL[i])^2/4
  C<-(1-vH*hH)*(1-vL*hL[i])
  AA<-1-C*R0^2
  BB<-2-2*R0*A
  CC<-B*R0^2+2*R0*A-R0^2*A^2-1
  b2_1<-(-BB)+sqrt((BB)^2+4*(A)*(C))/2/(A)
 #b2_2<-(-BB)-sqrt((BB)^2+4*(A)*(C))/2/(A)
  b2<-b2_1
  Rv <- R0/(1+b2)*(1-(vH*hH+vL*hL[i])/2+sqrt((vH*hH-vL*hL[i])^2/4+b2^2*(1-vH*hH)*(1-vL*hL[i])))
  plot(hH, Rv, type='l', ylim=c(0,5), main =paste("hL=",hL[i]))
  abline(h=1)
}
j=4
Rv <- R0/(1+b2)*(1-(vH*hH+vL*hL[j])/2+sqrt((vH*hH-vL*hL[j])^2/4+b2^2*(1-vH*hH)*(1-vL*hL[j])))
VRdata<-data.frame(hH=hH,Rv)
VR<-VRdata[which(VRdata$Rv<1),] 

# For two dose
R0=1.4
vH <- 0.8
vL <- 0.9 
par(mfrow=c(2,3))
for(i in 1:length(hL)){
  A<-1-(vH*hH+vL*hL[i])/2
  B<-(vH*hH-vL*hL[i])^2/4
  C<-(1-vH*hH)*(1-vL*hL[i])
  AA<-1-C*R0^2
  BB<-2-2*R0*A
  CC<-B*R0^2+2*R0*A-R0^2*A^2-1
  b2_1<-(-BB)+sqrt((BB)^2+4*(A)*(C))/2/(A)
  #b2_2<-(-BB)-sqrt((BB)^2+4*(A)*(C))/2/(A)
  b2<-b2_1
  Rv <- R0/(1+b2)*(1-(vH*hH+vL*hL[i])/2+sqrt((vH*hH-vL*hL[i])^2/4+b2^2*(1-vH*hH)*(1-vL*hL[i])))
  plot(hH, Rv, type='l', ylim=c(0,5), main =paste("hL=",hL[i]))
  abline(h=1)
}
j=5
Rv <- R0/(1+b2)*(1-(vH*hH+vL*hL[j])/2+sqrt((vH*hH-vL*hL[j])^2/4+b2^2*(1-vH*hH)*(1-vL*hL[j])))
VRdata<-data.frame(hH=hH,Rv)
VR<-VRdata[which(VRdata$Rv<1),] # hL==0.96 and hH=0.99, the herd immunity will be achieved when R0=5
# plot: two dose vaccination 
RV5<-Rv
data3<-data.frame(hH,RV1,RV2,RV3,RV4) #R0=2.5
colnames(data3)<-c("hH","hL=0.6","hL=0.7","hL=0.8","hL=0.9")
data3<-melt(data3,id.vars = "hH")
r1<-ggplot(data=data3,aes(x=hH,y=value))+
  geom_hline(yintercept = 1,alpha=0.5)+
  geom_line(aes(colour=variable))+
  scale_colour_manual(values =cols[c(7,6,5,4)])+
  ylim(c(0.9,1.75))+
  labs(subtitle="R0=2.5, Fixed hL",x="hH",y="Rv")+
  scale_x_continuous(limits = c(0.5,0.9))+
  theme(legend.title=element_blank(),legend.position = "top",title = element_text(size=10))+
  theme(axis.text.x=element_text(size=7),panel.grid.minor=element_blank(),legend.text = element_text(size=7))

data1.4<-data.frame(hH,RV1,RV2,RV3,RV4,RV5)#R0=1.4
colnames(data1.4)<-c("hH","hL=0.5","hL=0.6","hL=0.7","hL=0.8","hL=0.9")
data1.4<-melt(data1.4,id.vars = "hH")
r2<-ggplot(data=data1.4,aes(x=hH,y=value))+
  geom_hline(yintercept = 1,alpha=0.5)+
  geom_line(aes(colour=variable))+
  scale_colour_manual(values =cols[c(8,7,6,5,4)])+
  ylim(c(0.9,1.3))+
  labs(subtitle="R0=1.4, Fixed hL",x="hH",y="Rv")+
  scale_x_continuous(limits = c(0.1,0.3))+
  theme(legend.title=element_blank(),legend.position = "top")+
  theme(title = element_text(size=10),legend.text = element_text(size=7))+
  theme(axis.text.x=element_text(size=7),panel.grid.minor=element_blank())
grid.arrange(r1,r2,ncol=2)
