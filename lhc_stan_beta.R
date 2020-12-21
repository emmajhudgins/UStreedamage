### Theoretic analysis of STAN models for estimating pest severity and cost ## 
#1=gamma, 2=weibull, 3=lnorm, 4=pareto
rm(list=ls()) 
supercomputer=F

if (supercomputer==F)
{
i=1
nm="beta"
thread=0
setwd('~/Desktop/OneDrive - McGill University/Turtles/')
}

if (supercomputer==TRUE)
{
  args <- commandArgs(trailingOnly = TRUE)
  nm<-args[1]
  thread<-args[2]
  control_path=system("cat sc_full_path",intern=TRUE)
  i<-as.numeric(system(paste("echo `ssh snowi '~/get_rep 1",nm,thread,control_path,"'`",sep=" "), intern=TRUE))
  errorlog <- file(paste("errorlog_",nm,"_",i, ".txt", sep=""), open="at")
  sink(errorlog, type="message", append=T)
  msglog <- file(paste("msglog_",nm,"_",i, ".txt", sep=""), open="at")
  sink(msglog, type="output", append=T)
}
lhc<-read.csv('lhc_beta_data.csv')
pars<-read.csv('lhc_draws_beta.csv')

while(i<=1000 && i!=-99)
{
  library(rstan)
  rstan_options(auto_write = TRUE)
  y<-as.numeric(lhc[i,])
  m_beta<-stan(file="beta_mort_lhc.stan",data = list(y=y),pars=c("shape", "scale", "log_lik"),iter=10000, control=list(adapt_delta=0.999999),cores=1)
  pp_par1<-length(which(extract(m_beta)$shape<=pars[i,1]))/20000
  pp_par2<-length(which(extract(m_beta)$scale<=pars[i,2]))/20000
  
  write.table(cbind(i,pp_par1, pp_par2), file=paste("lhc_sim",nm,"csv", sep='.'), row.names=F, append=T, sep=",", col.names=F)
  if (supercomputer==F)
  {
  i=i+1
  }
  if (supercomputer==TRUE)
  {
    i<-as.numeric(system(paste("echo `ssh snowi '~/get_rep 0",nm,thread,control_path,"'`",sep=" "), intern=TRUE))
  }
}




library(rstan)
rstan_options(auto_write = TRUE)

oneT=0.01
threeT=0.10
fiveT=0.25
eightT=0.95


subsubone_spp=357 #aukema
subone_spp=38 # rating of 1
one_spp=14
three_spp=10
five_spp=5
eight_spp=4
nine_spp=1
ten_spp=3

# 
one_spp<-80
three_spp<-110
five_spp<-14
eight_spp<-19
nine_spp<-6
ten_spp<-7
subone_spp<-33
subsubone_spp<-418-60
# 
inv_dat_reduced<-read.csv("each_spp_pesthost_patho.csv")
lengths<-aggregate(inv_dat_reduced$genus~inv_dat_reduced$pest,FUN=length) 
colnames(lengths)[1]<-"pest"
 lengths_each<-merge(inv_dat_reduced,lengths, by="pest")
 inv_dat_reduced$severity<-as.numeric(inv_dat_reduced$severity)
 one_spp<-sum((1*inv_dat_reduced$severity==1)/lengths_each$`inv_dat_reduced$genus`)
 three_spp<-sum((1*inv_dat_reduced$severity==3)/lengths_each$`inv_dat_reduced$genus`)
 five_spp<-sum((1*inv_dat_reduced$severity==5)/lengths_each$`inv_dat_reduced$genus`)
 eight_spp<-sum((1*inv_dat_reduced$severity==8)/lengths_each$`inv_dat_reduced$genus`)
 nine_spp<-sum((1*inv_dat_reduced$severity==9)/lengths_each$`inv_dat_reduced$genus`)
 ten_spp<-sum((1*inv_dat_reduced$severity==10)/lengths_each$`inv_dat_reduced$genus`)
subone_spp<-33+sum((1*inv_dat_reduced$severity==0)/lengths_each$`inv_dat_reduced$genus`)
 subsubone_spp<-418-60
 
  m_beta<-stan(file="beta_mort.stan",data = list(oneT=oneT, threeT=threeT, fiveT=fiveT, eightT=eightT, subsubone_spp=subsubone_spp, subone_spp=subone_spp, one_spp=one_spp, three_spp=three_spp, five_spp=five_spp, eight_spp=eight_spp, nine_spp=nine_spp, ten_spp=ten_spp),pars=c("shape", "scale", "log_lik", "nineT", "suboneT", "subsuboneT"),iter=10000, control=list(adapt_delta=0.999999),cores=4)
# 
# 
# 
# 
# 



# pests2<-read.csv('datanorm.csv')
# pests2$LATIN_NAME<-gsub("Lepidosaphes ulmi.*","Lepidosaphes ulmi",pests2$LATIN_NAME)
# pests2$med_sev<-dat$`inv_dat$severity`[match(pests2$LATIN_NAME, dat$`inv_dat$clean_pest`, )]
# pests2$med_sev[which(is.na(pests2$med_sev)==T)]<-0
# length(which(pests2$med_sev==0))
#
#
# assess_bet<-extract(m_beta)
# hist(assess_bet$shape)
# hist(assess_bet$scale)
# hist(assess_bet$nineT)
# hist(assess_bet$suboneT)
# hist(assess_bet$subsuboneT)
 
 beta_samps<-extract(m_beta)
 subsubone<-subone<-one<-three<-five<-eight<-nine<-ten<-rep(0,20000)
 for (i in 1:20000)
 {
   rand_beta<-rbeta(100000,shape1=beta_samps$shape[i], shape2=beta_samps$scale[i])
   subsubone[i]<-sample(rand_beta[which(rand_beta<beta_samps$subsuboneT[i])],1)
   subone[i]<-sample(rand_beta[which(rand_beta>=beta_samps$subsuboneT[i] & rand_beta<beta_samps$suboneT[i]+beta_samps$subsuboneT[i])],1)
   one[i]<-sample(rand_beta[which(rand_beta>=beta_samps$suboneT[i]+beta_samps$subsuboneT[i] & rand_beta<oneT)],1)
   three[i]<-sample(rand_beta[which(rand_beta>=oneT& rand_beta<threeT)],1)
   five[i]<-sample(rand_beta[which(rand_beta>=threeT& rand_beta<fiveT)],1)
  eight[i]<-sample(rand_beta[which(rand_beta>=fiveT& rand_beta<eightT)],1)
  if (length(which(rand_beta>=eightT& rand_beta<beta_samps$nineT[i])>0))
  {
  nine[i]<-sample(rand_beta[which(rand_beta>=eightT& rand_beta<beta_samps$nineT[i])],1)
  }
  if (length(which(rand_beta>=beta_samps$nineT[i])))
  {
  ten[i]<-sample(rand_beta[which(rand_beta>=beta_samps$nineT[i])],1)
  }
 }
 for( j in which(nine==0))
 {
   while(nine[j]==0)
   {
   rand_beta<-rbeta(100000,shape1=beta_samps$shape[j], shape2=beta_samps$scale[j])
   if (length(which(rand_beta>=eightT& rand_beta<beta_samps$nineT[i])>0))
   {
   nine[j]<-sample(rand_beta[which(rand_beta>=eightT& rand_beta<beta_samps$nineT[i])],1)}
   }
 }
 for( j in which(ten==0))
 {
   while(ten[j]==0)
   {
     rand_beta<-rbeta(100000,shape1=beta_samps$shape[j], shape2=beta_samps$scale[j])
     if (length(which(rand_beta>=beta_samps$nineT[i])))
     {
     ten[j]<-sample(rand_beta[which(rand_beta>=beta_samps$nineT[i])],1)}
   }
 }


  saveRDS(beta_samps, file="beta_samps_new2.rds")
  beta_new<-readRDS('beta_samps_new.rds')
  old_stan<-read.csv('potter_mort.csv')
  
  write.csv(cbind((subsubone),(subone),(one),(three),(five), (eight), (nine), (ten)), file="potter_mort_all3.csv", row.names=F)
 # 
 # inv_dat_reduced$est_sev<-rep(0, nrow)
 # subsubone<-qbeta(0.5*pbeta(beta_samps$subsuboneT, shape1=beta_samps$shape, shape2=beta_samps$scale, lower.tail=T), shape1=beta_samps$shape, shape2=beta_samps$scale)
 # subone<-qbeta(pbeta(beta_samps$subsuboneT+0.5*beta_samps$suboneT, shape1=beta_samps$shape, shape2=beta_samps$scale, lower.tail=T), shape1=beta_samps$shape, shape2=beta_samps$scale)
 # one<-qbeta(pbeta(beta_samps$subsuboneT+beta_samps$suboneT+0.5*oneT, shape1=beta_samps$shape, shape2=beta_samps$scale, lower.tail=T), shape1=beta_samps$shape, shape2=beta_samps$scale)
 # three<-qbeta(pbeta(oneT+0.5*threeT, shape1=beta_samps$shape, shape2=beta_samps$scale, lower.tail=T), shape1=beta_samps$shape, shape2=beta_samps$scale)
 # five<-qbeta(pbeta(threeT+0.5*fiveT, shape1=beta_samps$shape, shape2=beta_samps$scale, lower.tail=T), shape1=beta_samps$shape, shape2=beta_samps$scale)
 # eight<-qbeta(pbeta(fiveT+0.5*eightT, shape1=beta_samps$shape, shape2=beta_samps$scale, lower.tail=T), shape1=beta_samps$shape, shape2=beta_samps$scale)
 # nine<-qbeta(pbeta(eightT+0.5*beta_samps$nineT, shape1=beta_samps$shape, shape2=beta_samps$scale, lower.tail=T), shape1=beta_samps$shape, shape2=beta_samps$scale)
 # ten<-qbeta(pbeta(beta_samps$nineT+0.5, shape1=beta_samps$shape, shape2=beta_samps$scale, lower.tail=T), shape1=beta_samps$shape, shape2=beta_samps$scale)
beta_samps<-readRDS('~/Desktop/OneDrive - McGill University/Turtles/beta_samps_new.rds')
hist(beta_samps$shape)
hist(beta_samps$scale)




x <- seq(0.01, 0.99, length=99)
hx<-matrix(0,20000,99)
for (i in 1:20000)
{
hx[i,] <-pbeta(x,beta_samps$shape[i],beta_samps$scale[i], lower.tail = F)
}
colmax<-colmin<-colmid<-0
for (i in 1:99)
{
  colmax[i]<-quantile(hx[,i], 0.975)
  colmin[i]<-quantile(hx[,i],0.025)
  colmid[i]<-quantile(hx[,i], 0.5)
}
library(ggplot2)
data<-data.frame(cbind(x,colmax, colmin, colmid))
ggplot(data,  aes(y=colmid,x=x)) + 
  geom_line(data=data,aes(y=colmid,x=x))+
  geom_ribbon(data=data, aes(x=x, ymin=colmin, ymax=colmax),fill="gray30", alpha=0.8)+
  scale_x_continuous(name="fraction killed")+scale_y_continuous(name="probability density")+theme_classic()


breaks<-c(median(beta_samps$subsuboneT), median(beta_samps$suboneT), 0.01,0.10,0.25,0.95,median(beta_samps$nineT))
widths<-0
widths[1]<-breaks[1]
for (i in 2:7)
{
  widths[i]<-breaks[i]-breaks[i-1]
}
widths[8]<-1-breaks[7]

midpoints<-0
breaks<-c(0,breaks,1)
for(i in 1:8)
{
  midpoints[i]<-(breaks[i]+breaks[i+1])/2
}

#widths[which(widths<0.01)]<-0.01


x <- c(1e-30,0.000001,0.00001,0.0001,0.001,seq(0.01, 1, length=100))
hx<-matrix(0,20000,105)
for (i in 1:20000)
{
  hx[i,] <-pbeta(x,beta_samps$shape[i],beta_samps$scale[i], lower.tail = F)
}
colmax<-colmin<-colmid<-0
for (i in 1:105)
{
  colmax[i]<-quantile(hx[,i], 0.975)
  colmin[i]<-quantile(hx[,i],0.025)
  colmid[i]<-quantile(hx[,i], 0.5)
}
library(ggplot2)
library(viridis)
pal<-colorRampPalette(c("royalblue","springgreen","yellow","red"))
pal<-viridis(8)
data<-data.frame(cbind(x,colmax, colmin, colmid))
ggplot(data,  aes(y=colmid,x=x)) +geom_col(data=data.frame(cbind(fracs, widths)),width=widths,inherit.aes = F, aes(y=fracs, x=midpoints, fill=c("<<1%", "<<1%-<1%", "<1-1%", "1-10%","10-25%","25-95%","95->95%",">>95%")), colour="black", fill=pal)+ scale_fill_manual(values=c("<<1"=pal[1], "<1"=pal[2], "1"=pal[3], "3"=pal[4], "5"=pal[5], "8"=pal[6], "9"=pal[7],"10"=pal[8]),name="Severity", breaks=c("<<1%", "<<1%-<1%", "<1-1%", "1-10%","10-25%","25-95%","95->95%",">>95%"))+ scale_x_continuous(name="Fraction Killed")+scale_y_continuous(name="Probability Density")+theme_classic()+theme(legend.text = element_text(size=11), axis.text=element_text(size=11))


+geom_ribbon(data=data, aes(x=x, ymin=colmin, ymax=colmax),fill="gray30", alpha=0.75)

subsubone_spp+subone_spp+one_spp+three_spp+five_spp+eight_spp+nine_spp+ten_spp
fracs<-c(subsubone_spp/627, subone_spp/627, one_spp/627, three_spp/627, five_spp/627, eight_spp/627, nine_spp/627, ten_spp/627)


subsubone_est<-beta_samps$subsuboneT
subone_est<-beta_samps$suboneT
nine_est<-beta_samps$nineT
boxplot(subsubone_est, subone_est, nine_est)

ggplot(data=setNames(data.frame(subsubone_est),"X1"))+geom_density(aes(x=X1, y=..scaled..), fill=pal(1)[1])+theme_classic()+scale_x_continuous(name="<<1 Upper Threshold")+scale_y_continuous(name="Posterior Probability (kernel density)")+theme(axis.text=element_text(size=11))

ggplot(data=setNames(data.frame(beta_samps$shape),"X1"))+geom_density(aes(x=X1, y=..scaled..), fill=pal(1)[1])+theme_classic()+scale_x_continuous(name="Beta a")+scale_y_continuous(name="Posterior Probability (kernel density)")+theme(axis.text=element_text(size=11))

ggplot(data=setNames(data.frame(beta_samps$scale),"X1"))+geom_density(aes(x=X1, y=..scaled..), fill=pal(1)[1])+theme_classic()+scale_x_continuous(name="Beta b")+scale_y_continuous(name="Posterior Probability (kernel density)")+theme(axis.text=element_text(size=11))

ggplot(data=setNames(data.frame(subone_est),"X1"))+geom_density(aes(x=X1, y=..scaled..), fill=pal(2)[2])+theme_classic()+scale_x_continuous(name="<1 Lower Threshold")+scale_y_continuous(name="Posterior Probability (kernel density)")+theme(axis.text=element_text(size=11))

ggplot(data=setNames(data.frame(nine_est),"X1"))+geom_density(aes(x=X1, y=..scaled..), fill=pal(3)[2])+theme_classic()+scale_x_continuous(name="9 Upper Threshold")+scale_y_continuous(name="Posterior Probability (kernel density)")+theme(axis.text=element_text(size=11))

medianrisk<-pbeta(0.5,beta_samps$shape,beta_samps$scale, lower.tail=F)

pottermort<-read.csv('potter_mort_all3.csv')
ggplot(data=pottermort)+geom_density(aes(x=V8, y=..scaled..), fill=pal(3)[2])+theme_classic()+scale_x_continuous(name="H")+scale_y_continuous(name="Posterior Probability (kernel density)")+theme(axis.text=element_text(size=11))
ggplot(data=pottermort)+geom_density(aes(x=V7, y=..scaled..), fill=pal(3)[2])+theme_classic()+scale_x_continuous(name="G")+scale_y_continuous(name="Posterior Probability (kernel density)")+theme(axis.text=element_text(size=11))
ggplot(data=pottermort)+geom_density(aes(x=V6, y=..scaled..), fill=pal(3)[2])+theme_classic()+scale_x_continuous(name="F")+scale_y_continuous(name="Posterior Probability (kernel density)")+theme(axis.text=element_text(size=11))
ggplot(data=pottermort)+geom_density(aes(x=V5, y=..scaled..), fill=pal(3)[2])+theme_classic()+scale_x_continuous(name="E")+scale_y_continuous(name="Posterior Probability (kernel density)")+theme(axis.text=element_text(size=11))
ggplot(data=pottermort)+geom_density(aes(x=V4, y=..scaled..), fill=pal(3)[2])+theme_classic()+scale_x_continuous(name="D")+scale_y_continuous(name="Posterior Probability (kernel density)")+theme(axis.text=element_text(size=11))
ggplot(data=pottermort)+geom_density(aes(x=V3, y=..scaled..), fill=pal(3)[2])+theme_classic()+scale_x_continuous(name="C")+scale_y_continuous(name="Posterior Probability (kernel density)")+theme(axis.text=element_text(size=11))
ggplot(data=pottermort)+geom_density(aes(x=V2, y=..scaled..), fill=pal(3)[2])+theme_classic()+scale_x_continuous(name="B")+scale_y_continuous(name="Posterior Probability (kernel density)")+theme(axis.text=element_text(size=11))
ggplot(data=pottermort)+geom_density(aes(x=V1, y=..scaled..), fill=pal(3)[2])+theme_classic()+scale_x_continuous(name="A")+scale_y_continuous(name="Posterior Probability (kernel density)")+theme(axis.text=element_text(size=11))





ggplot(data=setNames(data.frame(medianrisk),"X1"))+geom_density(aes(x=X1, y=..scaled..), fill=pal(5)[4])+theme_classic()+scale_x_continuous(name="% Mortality")+scale_y_continuous(name="Posterior Probability", labels=function(x)x/20000)+theme(axis.text=element_text(size=11))
points<-colmid[c(1,4,5,8,20,54,101,103)]  
stanest<-read.csv('~/Desktop/OneDrive - McGill University/Turtles/potter_mort3.csv')
ggplot(data) +geom_col(data=data.frame(cbind(fracs, widths)),width=widths,inherit.aes = F, aes(y=rep(0.6,8), x=midpoints, fill=c("<<1%", "<<1-<1%", "<1-1%", "1-10%","10-25%","25-95%","95->95%",">>95%")), alpha=0.5)+scale_fill_manual(values=c("<<1%"=pal(8)[1], "<<1-<1%"=pal(8)[2], "<1-1%"=pal(8)[3], "1-10%"=pal(8)[4], "10-25%"=pal(8)[5], "25-95%"=pal(8)[6], "95->95%"=pal(8)[7],">>95%"=pal(8)[8]),name="Severity", breaks=c("<<1%", "<<1-<1%", "<1-1%", "1-10%","10-25%","25-95%","95->95%",">>95%"))+scale_x_continuous(name="Fraction Killed")+scale_y_continuous(name="Probability Density", limits=c(0,0.6))+theme_classic()+theme(legend.text = element_text(size=11), axis.text=element_text(size=11))

+ geom_line(data=data,aes(y=colmid,x=x),size=1)+geom_ribbon(data=data, aes(x=x, ymin=colmin, ymax=colmax),fill="gray30", alpha=0.75)

+geom_point(data=data.frame(t(stanest)),aes(x=t.stanest., y=points),  colour='gray30', size=2)




ggplot(data[2:6,])+geom_col(data=data.frame(cbind(fracs[2:3], widths[2:3])),width=widths[2:3],inherit.aes = F, aes(y=rep(0.6,2), x=midpoints[2:3], fill=c("<<1-<1%", "<1-1%" )), alpha=0.5)+scale_fill_manual(values=c("<<1%"=pal(8)[1], "<<1-<1%"=pal(8)[2], "<1-1%"=pal(8)[3], "1-10%"=pal(8)[4], "10-25%"=pal(8)[5], "25-95%"=pal(8)[6], "95->95%"=pal(8)[7],">>95%"=pal(8)[8]),name="Severity", breaks=c("<<1%", "<<1-<1%", "<1-1%"))+scale_x_continuous(name="Fraction Killed")+scale_y_continuous(name="Probability Density")+theme_classic()+theme(legend.text = element_text(size=11), axis.text=element_text(size=11))+ geom_line(data=data[2:6,],aes(y=colmid,x=x),size=1)+geom_ribbon(data=data[2:6,], aes(x=x, ymin=colmin, ymax=colmax),fill="gray30", alpha=0.75)


ggplot(data[100:105,])+geom_col(data=data.frame(cbind(fracs[7:8], widths[7:8])),width=widths[7:8],inherit.aes = F, aes(y=rep(0.01,2), x=midpoints[7:8], fill=c("95->95%", ">>95%" )), alpha=0.5)+scale_fill_manual(values=c("<<1%"=pal(8)[1], "<<1-<1%"=pal(8)[2], "<1-1%"=pal(8)[3], "1-10%"=pal(8)[4], "10-25%"=pal(8)[5], "25-95%"=pal(8)[6], "95->95%"=pal(8)[7],">>95%"=pal(8)[8]),name="Severity", breaks=c( "95->95%",">>95%"))+scale_x_continuous(name="Fraction Killed")+scale_y_continuous(name="Probability Density")+theme_classic()+theme(legend.text = element_text(size=11), axis.text=element_text(size=11))+ geom_line(data=data[100:105,],aes(y=colmid,x=x),size=1)+geom_ribbon(data=data[100:105,], aes(x=x, ymin=colmin, ymax=colmax),fill="gray30", alpha=0.75)


ggplot(data[1:3,])+geom_col(data=data.frame(cbind(fracs[1], widths[1])),width=widths[1],inherit.aes = F, aes(y=rep(0.8,1), x=midpoints[1], fill=c("<<1%" )), alpha=0.5)+scale_fill_manual(values=c("<<1%"=pal(8)[1]),name="Severity", breaks=c( "<<1%"))+scale_x_continuous(name="Fraction Killed", limits=c(0,1e-5))+scale_y_continuous(name="Probability Density")+theme_classic()+theme(legend.text = element_text(size=11), axis.text=element_text(size=11))+ geom_line(data=data[1:3,],aes(y=colmid,x=x),size=1)+geom_ribbon(data=data[1:3,], aes(x=x, ymin=colmin, ymax=colmax),fill="gray30", alpha=0.75)


write.csv(cbind(mean(subsubone),mean(subone), mean(one), mean(three), mean(five), mean(eight), mean(nine), mean(ten)), file="potter_mort3.csv", row.names=F)
