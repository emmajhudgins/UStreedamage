library(viridis)
library(here)
library(ggplot2)
library(sp)
library(maptools)
library(maps)

setwd(here())

long_st<-readRDS('./output/mortality_longlag.RDS')
short_st<-readRDS('/output/mortality_shortlag.RDS')
mid_st<-readRDS('./outputmortality_midlag.RDS')


data2<-read.csv('./data/datanorm.csv', stringsAsFactors = FALSE)
data2<-data2[-c(25,69),]
data2<-data2[-4,]

bore<-which(data2$Guild=="Borers")
suck<-which(data2$Guild=="Suckers")
def<-which(data2$Guild=="Defoliators")



sum(apply(short_st,1,sum)[which(data2$Guild=="Borers")])*0.02/(1-(1+0.02)^-30)
sum(apply(mid_st,1,sum)[which(data2$Guild=="Borers")])*0.02/(1-(1+0.02)^-30)
sum(apply(long_st,1,sum)[which(data2$Guild=="Borers")])*0.02/(1-(1+0.02)^-30)

sum(apply(short_st,1,sum)[which(data2$Guild=="Defoliators")])*0.02/(1-(1+0.02)^-30)
sum(apply(mid_st,1,sum)[which(data2$Guild=="Defoliators")])*0.02/(1-(1+0.02)^-30)
sum(apply(long_st,1,sum)[which(data2$Guild=="Defoliators")])*0.02/(1-(1+0.02)^-30)

sum(apply(short_st,1,sum)[which(data2$Guild=="Suckers")])*0.02/(1-(1+0.02)^-30)
sum(apply(mid_st,1,sum)[which(data2$Guild=="Suckers")])*0.02/(1-(1+0.02)^-30)
sum(apply(long_st,1,sum)[which(data2$Guild=="Suckers")])*0.02/(1-(1+0.02)^-30)


data2$COMMON_NAM[which(data2$Guild=="Borers")][order(apply(vshort[,,4:10]*ave_discount[1:7],1,sum)[which(data2$Guild=="Borers")], decreasing = T)][1:3]
data2$COMMON_NAM[which(data2$Guild=="Borers")][order(apply(vmid[,,4:10]*ave_discount[1:7],1,sum)[which(data2$Guild=="Borers")], decreasing = T)][1:3]
data2$COMMON_NAM[which(data2$Guild=="Borers")][order(apply(vlong[,,4:10]*ave_discount[1:7],1,sum)[which(data2$Guild=="Borers")], decreasing = T)][1:3]


data2$COMMON_NAM[which(data2$Guild=="Suckers")][order(apply(short[,,4:10]*ave_discount[1:7],1,sum)[which(data2$Guild=="Suckers")], decreasing = T)][1:3]
data2$COMMON_NAM[which(data2$Guild=="Suckers")][order(apply(mid[,,4:10]*ave_discount[1:7],1,sum)[which(data2$Guild=="Suckers")], decreasing = T)][1:3]
data2$COMMON_NAM[which(data2$Guild=="Suckers")][order(apply(long[,,4:10]*ave_discount[1:7],1,sum)[which(data2$Guild=="Suckers")], decreasing = T)][1:3]


data2$COMMON_NAM[which(data2$Guild=="Defoliators")][order(colSums(short)[which(data2$Guild=="Defoliators")], decreasing = T)][1:3]
data2$COMMON_NAM[which(data2$Guild=="Defoliators")][order(apply(mid*ave_discount[1:7],1,sum)[which(data2$Guild=="Defoliators")], decreasing = T)][1:3]
data2$COMMON_NAM[which(data2$Guild=="Defoliators")][order(apply(long[,,4:10]*ave_discount[1:7],1,sum)[which(data2$Guild=="Defoliators")], decreasing = T)][1:3]


time_vl<-apply(long_st,3,sum)

time_vm<-apply(mid_st,3,sum)

time_vs<-apply(short_st,3,sum)

data<-data.frame(cbind(time_vl, time_vm,  time_vs))

pal<-viridis

ggplot(data[4:10,])+geom_col(aes(x=1:7, y=time_vl*ave_discount[1:7], fill="100 Year"))+geom_col(aes(x=1:7, y=time_vm*ave_discount[1:7] ,fill="50 Year"))+geom_col(aes(x=1:7, y=time_vs*ave_discount[1:7], fill="10 Year"))+scale_x_continuous(name="Time", breaks=c(1,3,5,7), labels=c(2020,2030,2040,2050))+scale_y_continuous(name="Cost (2020 USD)")+theme_classic()+theme(axis.text=element_text(size=11))+scale_fill_manual(values=c("100 Year"=pal(3)[1],"50 Year"=pal(3)[2], "10 Year"=pal(3)[3]), name="Mortality Debt", breaks=c("10 Year", "50 Year", "100 Year"))

scens<-c("short", "mid", "long")
scenarios<-matrix(0,10000,12)

#best guess scenario - bore=short, suck=mid, path=mid, def=long
bestguess<-apply(readRDS('./output/by_site_st_costs.RDS'),1,sum)
  
i=1
for (j in 1:3)
{
  scenarios[,i]<-rowSums(get(scens[3])[,def])+rowSums(get(scens[2])[,suck])+rowSums(get(scens[j])[,bore])
  i=i+1
}
for (k in 1:3)
{
  scenarios[,i]<-rowSums(get(scens[3])[,def])+rowSums(get(scens[k])[,suck])+rowSums(get(scens[1])[,bore])
  i=i+1
}
 for (l in 1:3)
   {
   scenarios[,i]<-rowSums(get(scens[l])[,def])+rowSums(get(scens[2])[,suck])+rowSums(get(scens[1])[,bore])
   i=i+1
 }
 for (m in 1:3)
      {
     scenarios[,i]<-rowSums(get(scens[3])[,def])+rowSums(get(scens[2])[,suck])+rowSums(get(scens[1])[,bore])
   i=i+1
      }



scenarios<-0.02*scenarios/(1-(1+0.02)^-30)
quantile(scenarios, 0.025)
quantile(scenarios, 0.975)

ggplot(data=data.frame(unlist(c(scenarios))))+geom_density(aes(x=unlist(c(scenarios))), fill=viridis(1)[1])+theme_classic()+scale_x_continuous(name="Annualized Street Tree Costs Across All Scenarios (20219 USD)",limits=c(1e+07,1e+09))+scale_y_continuous(name="Posterior Probability Density")+theme(axis.text=element_text(size=11))+geom_vline(xintercept=(quantile(scenarios, 0.025)), linetype="dotted", size=1.5, color="yellow")+ geom_vline(xintercept=(quantile(scenarios, 0.975)), linetype="dotted", size=1.5, color="yellow")+ geom_vline(xintercept=(mean(unlist(c(scenarios)))), linetype="dotted", size=1.5, color="red")



bestguess<-readRDS('output/costquantiles_bestguess.RDS')[[1]]

ggplot(data=data.frame(bestguess))+geom_density(aes(x=unlist(c(bestguess))), fill=viridis(1)[1])+theme_classic()+scale_x_continuous(name="Best Guess Scenario Annualized Street Tree Costs (2019 USD)",limits=c(3e+06,8e+07))+scale_y_continuous(name="Posterior Probability Density")+theme(axis.text=element_text(size=11))+geom_vline(xintercept=(quantile(bestguess, 0.025)), linetype="dotted", size=1.5, color="yellow")+ geom_vline(xintercept=(quantile(bestguess, 0.975)), linetype="dotted", size=1.5, color="yellow")+ geom_vline(xintercept=(mean(unlist(c(bestguess)))), linetype="dotted", size=1.5, color="red")


quantile(scenarios[,1:3], 0.025)
quantile(scenarios[,1:3], 0.975)

quantile(scenarios[,4:6], 0.025)
quantile(scenarios[,4:6], 0.975)

quantile(scenarios[,7:9], 0.025)
quantile(scenarios[,7:9], 0.975)

quantile(scenarios[,10:12], 0.025)
quantile(scenarios[,10:12], 0.975)


short<-readRDS('mortality_short.RDS')[[3]]
mid<-readRDS('mortality_mid.RDS')[[3]]
long<-readRDS('mortality_long.RDS')[[3]]
scens<-c('short', 'mid', 'long')

#best guess - bore=short, suck=mid, path=mid, def=long
bestguess<-rowSums(get(scens[2])[,def])+rowSums(get(scens[3])[,suck])+rowSums(get(scens[1])[,bore])
i=1
for (j in 1:3)
{
  scenarios[,i]<-rowSums(get(scens[2])[,path])+rowSums(get(scens[3])[,def])+rowSums(get(scens[2])[,suck])+rowSums(get(scens[j])[,bore])
  i=i+1
}
for (k in 1:3)
{
  scenarios[,i]<-rowSums(get(scens[2])[,path])+rowSums(get(scens[3])[,def])+rowSums(get(scens[k])[,suck])+rowSums(get(scens[1])[,bore])
  i=i+1
}
for (l in 1:3)
{
  scenarios[,i]<-rowSums(get(scens[2])[,path])+rowSums(get(scens[l])[,def])+rowSums(get(scens[2])[,suck])+rowSums(get(scens[1])[,bore])
  i=i+1
}
for (m in 1:3)
{
  scenarios[,i]<-rowSums(get(scens[m])[,path])+rowSums(get(scens[3])[,def])+rowSums(get(scens[2])[,suck])+rowSums(get(scens[1])[,bore])
  i=i+1
}


quantile(scenarios[,1:3], 0.025)
#/1.4E+09
quantile(scenarios[,1:3], 0.975)
#/1.4E+09

quantile(scenarios[,4:6], 0.025)
#/1.4E+09
quantile(scenarios[,4:6], 0.975)
#/1.4E+09

quantile(scenarios[,7:9], 0.025)
#/1.4E+09
quantile(scenarios[,7:9], 0.975)
#/1.4E+09

quantile(scenarios[,10:12], 0.025)
#/1.4E+09
quantile(scenarios[,10:12], 0.975)
#/1.4E+09

##newpest

total_maple_damage<-readRDS('./output/total_maple_damage.rds')
total_oak_damage<-readRDS('./output/total_oak_damage.rds')
m<-SpatialPointsDataFrame(coords=cbind(grid$X_coord, grid$Y_coord), data=as.data.frame(total_maple_damage))
port<-c(1233,2622,3066)
portname<-c("NY", "CA", "LA")
USA_merged<-map('usa', fill=TRUE, plot=F)
IDs <- sapply(strsplit(USA_merged$names, ":"), function(x) x[1])
sp_map_usa <- map2SpatialPolygons(USA_merged, IDs=IDs, proj4string=CRS("+proj=longlat +datum=WGS84"))
# Create two panels side by side
pal<-viridis
par(mar=c(0.5,0.5,0.5,0.5))
par(oma=c(0,0,0,4))
layout(t(1:2), widths=c(6,1))
bins<-seq(0,6,length.out=50)
bins<-10^bins
m$Col<-pal(50)[findInterval(m$V1, bins)+1]
transform_usa<-spTransform(sp_map_usa, CRS("+proj=eqdc +lat_0=39 +lon_0=-96 +lat_1=33 +lat_2=45 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")) # simulation grid is in United States Equidistant Conic projection


##maple tree results
plot(transform_usa, lwd=0.2, main=NULL)
points(cbind(grid$X_coord, grid$Y_coord), pch=15, cex=0.5, col=m$Col)
plot(transform_usa,add=TRUE, lwd=2)
points(cbind(grid$X_coord[port[1]], grid$Y_coord[port[1]]), pch=23, cex=1, bg="darkred",col="darkred")
image(1,1:50,t(1:50), col=pal(50), axes=FALSE)
par(las=1)
axis(4,labels=c("0",expression(10^{2}), expression(10^{4}),expression(10^{6})), at=c(seq(1,50,length.out=4)), cex.axis=1)
par(las=0)
mtext(side=4, "Exposed maple street trees", line=3)

m$Col<-pal(50)[findInterval(m$V2, bins)+1]
plot(transform_usa, lwd=0.2, main=NULL)
points(cbind(grid$X_coord, grid$Y_coord), pch=15, cex=0.5, col=m$Col)
plot(transform_usa,add=TRUE, lwd=2)
points(cbind(grid$X_coord[port[2]], grid$Y_coord[port[2]]), pch=23, cex=1, bg="darkred", col="darkred")
image(1,1:50,t(1:50), col=pal(50), axes=FALSE)
par(las=1)
axis(4,labels=c("0",expression(10^{2}), expression(10^{4}),expression(10^{6})), at=c(seq(1,50,length.out=4)), cex.axis=1)
par(las=0)
mtext(side=4, "Exposed maple street trees", line=3)


m$Col<-pal(50)[findInterval(m$V3, bins)+1]
plot(transform_usa, lwd=0.2, main=NULL)
points(cbind(grid$X_coord, grid$Y_coord), pch=15, cex=0.5, col=m$Col)
plot(transform_usa,add=TRUE, lwd=2)
points(cbind(grid$X_coord[port[3]], grid$Y_coord[port[3]]), pch=23, cex=1, bg="darkred", col="darkred")
image(1,1:50,t(1:50), col=pal(50), axes=FALSE)
par(las=1)
axis(4,labels=c("0",expression(10^{2}), expression(10^{4}),expression(10^{6})), at=c(seq(1,50,length.out=4)), cex.axis=1)
par(las=0)
mtext(side=4, "Exposed maple street trees", line=3)

##oak trees
m<-SpatialPointsDataFrame(coords=cbind(grid$X_coord, grid$Y_coord), data=as.data.frame(total_oak_damage))
m$Col<-pal(50)[findInterval(m$V1, bins)+1]
plot(transform_usa, lwd=0.2, main=NULL)
points(cbind(grid$X_coord, grid$Y_coord), pch=15, cex=0.5, col=m$Col)
plot(transform_usa,add=TRUE, lwd=2)
points(cbind(grid$X_coord[port[1]], grid$Y_coord[port[1]]), pch=23, cex=1, bg="darkred",col="darkred")
image(1,1:50,t(1:50), col=pal(50), axes=FALSE)
par(las=1)
axis(4,labels=c("0",expression(10^{2}), expression(10^{4}),expression(10^{6})), at=c(seq(1,50,length.out=4)), cex.axis=1)
par(las=0)
mtext(side=4, "Exposed oak street trees", line=3)

m$Col<-pal(50)[findInterval(m$V2, bins)+1]
plot(transform_usa, lwd=0.2, main=NULL)
points(cbind(grid$X_coord, grid$Y_coord), pch=15, cex=0.5, col=m$Col)
plot(transform_usa,add=TRUE, lwd=2)
points(cbind(grid$X_coord[port[2]], grid$Y_coord[port[2]]), pch=23, cex=1, bg="darkred", col="darkred")
image(1,1:50,t(1:50), col=pal(50), axes=FALSE)
par(las=1)
axis(4,labels=c("0",expression(10^{2}), expression(10^{4}),expression(10^{6})), at=c(seq(1,50,length.out=4)), cex.axis=1)
par(las=0)
mtext(side=4, "Exposed oak street trees", line=3)


m$Col<-pal(50)[findInterval(m$V3, bins)+1]
plot(transform_usa, lwd=0.2, main=NULL)
points(cbind(grid$X_coord, grid$Y_coord), pch=15, cex=0.5, col=m$Col)
plot(transform_usa,add=TRUE, lwd=2)
points(cbind(grid$X_coord[port[3]], grid$Y_coord[port[3]]), pch=23, cex=1, bg="darkred", col="darkred")
image(1,1:50,t(1:50), col=pal(50), axes=FALSE)
par(las=1)
axis(4,labels=c("0",expression(10^{2}), expression(10^{4}),expression(10^{6})), at=c(seq(1,50,length.out=4)), cex.axis=1)
par(las=0)
mtext(side=4, "Exposed oak street trees", line=3)
