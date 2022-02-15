library(viridis)
library(here)
library(ggplot2)
library(sp)
library(maptools)
library(maps)
library(geosphere)
library(rgdal)
library(dplyr)

setwd(here())

#scenarios of damage
long_st<-readRDS('./output/costquantiles_long.RDS')[[1]]
short_st<-readRDS('./output/costquantiles_short.RDS')[[1]]
mid_st<-readRDS('./output/costquantiles_mid.RDS')[[1]]

#trees
pred_trees_huge<-readRDS('./output/pred_trees_huge_public.RDS') #private data removed
#pred_trees_huge<-readRDS('~/Desktop/OneDrive - McGill University/Grad/scripts/pred_trees_huge.RDS') #private data


# 50x50km grid spatial information
grid<-read.csv('./data/countydatanorm_march.csv')

#pest data
data2<-read.csv('./data/datanorm.csv', stringsAsFactors = FALSE)
data2<-data2[-c(25,69),]
data2<-data2[-4,]

bore<-which(data2$Guild=="Borers")
suck<-which(data2$Guild=="Suckers")
def<-which(data2$Guild=="Defoliators")

#average predictions
bestguess_genus<-readRDS('./output/mean_mort.RDS')[,c(def,bore,suck),]
new_presences<-readRDS('./output/new_presences.RDS')

##mean cost by guild across scenarios

sum(apply(short_st,2,mean)[which(data2$Guild=="Borers")])*0.02/(1-(1+0.02)^-30)
sum(apply(mid_st,2,mean)[which(data2$Guild=="Borers")])*0.02/(1-(1+0.02)^-30)
sum(apply(long_st,2,mean)[which(data2$Guild=="Borers")])*0.02/(1-(1+0.02)^-30)

sum(apply(short_st,2,mean)[which(data2$Guild=="Defoliators")])*0.02/(1-(1+0.02)^-30)
sum(apply(mid_st,2,mean)[which(data2$Guild=="Defoliators")])*0.02/(1-(1+0.02)^-30)
sum(apply(long_st,2,mean)[which(data2$Guild=="Defoliators")])*0.02/(1-(1+0.02)^-30)

sum(apply(short_st,2,mean)[which(data2$Guild=="Suckers")])*0.02/(1-(1+0.02)^-30)
sum(apply(mid_st,2,mean)[which(data2$Guild=="Suckers")])*0.02/(1-(1+0.02)^-30)
sum(apply(long_st,2,mean)[which(data2$Guild=="Suckers")])*0.02/(1-(1+0.02)^-30)

#worst pest in each guild across scenarios
data2$COMMON_NAM[which(data2$Guild=="Borers")][order(apply(short_st[,which(data2$Guild=="Borers")],2,sum), decreasing = T)][1:3]
data2$COMMON_NAM[which(data2$Guild=="Borers")][order(apply(mid_st[,which(data2$Guild=="Borers")],2,sum), decreasing = T)][1:3]
data2$COMMON_NAM[which(data2$Guild=="Borers")][order(apply(mid_st[,which(data2$Guild=="Borers")],2,sum), decreasing = T)][1:3]

data2$COMMON_NAM[which(data2$Guild=="Suckers")][order(apply(short_st[,which(data2$Guild=="Suckers")],2,sum), decreasing = T)][1:3]
data2$COMMON_NAM[which(data2$Guild=="Suckers")][order(apply(mid_st[,which(data2$Guild=="Suckers")],2,sum), decreasing = T)][1:3]
data2$COMMON_NAM[which(data2$Guild=="Suckers")][order(apply(long_st[,which(data2$Guild=="Suckers")],2,sum), decreasing = T)][1:3]

data2$COMMON_NAM[which(data2$Guild=="Defoliators")][order(apply(short_st[,which(data2$Guild=="Defoliators")],2,sum), decreasing = T)][1:3]
data2$COMMON_NAM[which(data2$Guild=="Defoliators")][order(apply(mid_st[,which(data2$Guild=="Defoliators")],2,sum), decreasing = T)][1:3]
data2$COMMON_NAM[which(data2$Guild=="Defoliators")][order(apply(long_st[,which(data2$Guild=="Defoliators")],2,sum), decreasing = T)][1:3]

## plot best guess quantiles

bestguess<-rowSums(readRDS('./output/costquantiles_bestguess.RDS')[[1]])*0.02/(1-(1+0.02)^-30)
pdf('./output/Scenario_range.pdf')
ggplot(data=data.frame(bestguess))+geom_density(aes(x=unlist(c(bestguess))), fill=viridis(1)[1])+theme_classic()+scale_x_continuous(name="Best Guess Scenario Annualized Street Tree Costs (2019 USD)",limits=c(2.5e+07,4e+07))+scale_y_continuous(name="Posterior Probability Density")+theme(axis.text=element_text(size=11))+geom_vline(xintercept=(quantile(bestguess, 0.025)), linetype="dotted", size=1.5, color="yellow")+ geom_vline(xintercept=(quantile(bestguess, 0.975)), linetype="dotted", size=1.5, color="yellow")+ geom_vline(xintercept=(mean(unlist(c(bestguess)))), linetype="dotted", size=1.5, color="red")
dev.off()

###vary guilds across scenarios
##examine resupting costs

bestguess<-apply(readRDS('./output/by_site_st_costs.RDS'),1,sum)

##toggle through list entries [[1]] street, [[2]] community [[3]] residential when loading
long<-readRDS('./output/costquantiles_long.RDS')[[3]]
short<-readRDS('./output/costquantiles_short.RDS')[[3]]
mid<-readRDS('./output/costquantiles_mid.RDS')[[3]]

scens<-c("short", "mid", "long")
scenarios<-matrix(0,10000,12)
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

# vary borers
quantile(scenarios[,1:3], 0.025)*(0.02/(1-(1+0.02)^-30))
quantile(scenarios[,1:3], 0.975)*(0.02/(1-(1+0.02)^-30))

# vary sap feeders
quantile(scenarios[,4:6], 0.025)*(0.02/(1-(1+0.02)^-30))
quantile(scenarios[,4:6], 0.975)*(0.02/(1-(1+0.02)^-30))

# vary defoliators
quantile(scenarios[,7:9], 0.025)*(0.02/(1-(1+0.02)^-30))
quantile(scenarios[,7:9], 0.975)*(0.02/(1-(1+0.02)^-30))

###treemortality

short<-readRDS('./output/mortality_short.RDS')[[3]]
mid<-readRDS('./output/mortality_mid.RDS')[[3]]
long<-readRDS('./output/mortality_long.RDS')[[3]]

scens<-c('short', 'mid', 'long')

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

#vary borers
quantile(scenarios[,1:3], 0.025)
quantile(scenarios[,1:3], 0.975)

#vary sap feeders
quantile(scenarios[,4:6], 0.025)
quantile(scenarios[,4:6], 0.975)

#vary defoliators
quantile(scenarios[,7:9], 0.025)
quantile(scenarios[,7:9], 0.975)


##temporal trends across mortality debt scenarios
bestguess<-readRDS('./output/temporal_costs_bestguess.RDS')
time_vs<-readRDS('./output/temporal_costs_short.RDS')
time_vm<-readRDS('./output/temporal_costs_mid.RDS')
time_vl<-readRDS('./output/temporal_costs_long.RDS')
data<-data.frame(cbind(time_vl, time_vm,  time_vs))

pal<-viridis
pdf("./output/Mortality_debt.pdf")
ggplot(data)+geom_col(aes(x=1:7, y=time_vs, fill="10 Year"), width=0.5,)+geom_col(aes(x=1.1:7.1, y=time_vm ,fill="50 Year"),width=0.4,position=position_dodge(0.3))+geom_col(aes(x=1.2:7.2, y=time_vl, fill="100 Year"), width=0.5, position=position_dodge(0.5))+scale_x_continuous(name="Time", breaks=c(1,3,5,7), labels=c(2020,2030,2040,2050))+scale_y_continuous(name="Cost (2019 USD)")+theme_classic()+theme(axis.text=element_text(size=16), axis.title = element_text(size=18), legend.text = element_text(size=16), legend.title = element_text(size=18))+scale_fill_manual(values=c("100 Year"=alpha(pal(3)[1],0.75),"50 Year"=alpha(pal(3)[2],0.75), "10 Year"=alpha(pal(3)[3],0.75)), name="Mortality Debt", breaks=c("10 Year", "50 Year", "100 Year"))+geom_path(aes(x=1:7, y=bestguess), colour='darkred', size=1,linetype="dashed")
dev.off()

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



##hotspot cities


costgrid<-readRDS('./output/costgrid.RDS')
mortgrid<-readRDS('./output/mortgrid.RDS')
city_details<-data.frame(PLACEFIPS=unique(costgrid$`sub_trees$PLACEFIPS`),FIPS=NA ,ST=NA, lat=NA,long=NA)
sp_city<-data.frame(cbind(costgrid[,c(1:2,4)],mortgrid[,4]))
colnames(sp_city)<-c("PLACEFIPS","genus", "cost", "mort")
sp_city<-sp_city[2:nrow(sp_city),]
for (i in c(3,4)){ sp_city[,i]<-as.numeric(sp_city[,i])}
spp_agg_city<-aggregate(sp_city[,c(3,4)], by=list(as.factor(sp_city$PLACEFIPS)), FUN=sum, na.rm=T)
colnames(spp_agg_city)[1]<-"PLACEFIPS"
city_details$FIPS<-pred_trees_huge$FIPS[match(city_details$PLACEFIPS,pred_trees_huge$PLACEFIPS)]
city_details$ST<-pred_trees_huge$STFIPS.x.x[match(city_details$PLACEFIPS,pred_trees_huge$PLACEFIPS)]
city_details$city<-pred_trees_huge$NAME.y[match(city_details$PLACEFIPS,pred_trees_huge$PLACEFIPS)]
layers<-ogrListLayers("./data/fixed_placeply.gpkg")
placeply<-readOGR("./data/fixed_placeply.gpkg", layers[1])
spcord<-cbind(centroid(placeply), placeply$PLACEFIPS)
city_details$lon<-as.numeric(spcord[match(city_details$PLACEFIPS, as.numeric(spcord[,3])),1])
city_details$lat<-as.numeric(spcord[match(city_details$PLACEFIPS, as.numeric(spcord[,3])),2])
spp_agg_city<-merge(spp_agg_city, city_details)
shadowtext <- function(x, y=NULL, labels, col='white', bg='black', 
                       theta= seq(0, 2*pi, length.out=50), r=0.1, ... ) {
  
  xy <- xy.coords(x,y)
  xo <- r*strwidth('A')
  yo <- r*strheight('A')
  
  # draw background text with small shift in x and y in background colour
  for (i in theta) {
    text( xy$x + cos(i)*xo, xy$y + sin(i)*yo, labels, col=bg, ... )
  }
  # draw actual text in exact xy position in foreground colour
  text(xy$x, xy$y, labels, col=col, ... )
}
m<-SpatialPointsDataFrame(coords=cbind(grid$X_coord, grid$Y_coord), data=setNames(as.data.frame(rowSums(bestguess_genus)),"cost")) #grid cell level mortality
m2<-SpatialPointsDataFrame(coords=cbind(spp_agg_city$lon, spp_agg_city$lat), data=spp_agg_city)# city-level mortality
proj4string(m2)<-CRS("+proj=longlat +datum=WGS84")
m2<-spTransform(m2, CRS("+proj=eqdc +lat_0=39 +lon_0=-96 +lat_1=33 +lat_2=45 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"))

pdf('./output/hotspots.pdf')
# # Create two panels side by side
par(mar=c(0.5,0.5,0.5,0.5))
par(oma=c(0,0,0,4))
layout(t(1:2), widths=c(6,1))
bins<-seq(0,6,length.out=50)
bins<-10^bins
m$Col<-pal(50)[findInterval(m$cost, bins)+1]
plot(transform_usa, lwd=0.2, main=NULL)
pal<-viridis
#m$Col <- pal(50)[as.numeric(cut(log10(m$cost+1),breaks = 50))]
points(cbind(grid$X_coord, grid$Y_coord), pch=15, cex=1, col=m$Col)
plot(transform_usa,add=TRUE, lwd=2)
points(m2@coords[order(m2$mort, decreasing=T)[c(1:3,7:10)],1:2], cex=1, col='black', bg=viridis(50)[50], pch=21)
shadowtext(m2@coords[order(m2$mort, decreasing=T)[c(1:3,7:10)],1:2], labels=c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J" )[c(1:7)],pos=4, offset=0.25, cex=1 , col=viridis(50)[50])
#
m2@data[order(m2$mort, decreasing=T)[c(1:10)],]

pred_trees_huge[match(m2$PLACEFIPS[order(m2$mort, decreasing=T)[c(1:10)]], pred_trees_huge$PLACEFIPS),]

m2$mort[order(m2$mort, decreasing=T)[c(1:10)]]
m2$cost[order(m2$mort, decreasing=T)[c(1:10)]]
sum(m2$cost[order(m2$mort, decreasing=T)[c(1:10)]]
)

statemort<-m2@data%>%group_by(ST)%>%summarise_if(is.numeric,sum)

image(1,1:50,t(1:50), col=pal(50), axes=FALSE)
par(las=1)
axis(4,labels=c("0",expression(10^{2}), expression(10^{4}),expression(10^{6})),at=c(seq(1,50,length.out=4)), cex.axis=1)
par(las=0)
mtext(side=4, "Street Tree Mortality (2020-2050)", line=3, cex=1.5)
dev.off()
# see https://www23.statcan.gc.ca/imdb/p3VD.pl?Function=getVD&TVD=53971 for state codes
states<-m2@data%>%group_by(ST)%>%summarise_at('mort', sum)

##EAB-asymptotic mortality zone
pest=1
lag2=10 # must correspond to bestguess lag for that species
mort_max<-unique(c(new_presences[[pest]][,(ncol(new_presences[[pest]])-10+4):(ncol(new_presences[[pest]])-((lag2-5)/5))]))

m<-SpatialPointsDataFrame(coords=cbind(grid$X_coord[mort_max], grid$Y_coord[mort_max]), data=setNames(as.data.frame(rowSums(bestguess_genus[mort_max,,])),"cost")) #grid cell level mortality
# # Create two panels side by side
par(mar=c(0.5,0.5,0.5,0.5))
par(oma=c(0,0,0,4))
layout(t(1:2), widths=c(6,1))
bins<-seq(0,6,length.out=50)
bins<-10^bins
m$Col<-pal(50)[findInterval(m$cost, bins)+1]
plot(transform_usa, lwd=0.2, main=NULL)
pal<-viridis
#m$Col <- pal(50)[as.numeric(cut(log10(m$cost+1),breaks = 50))]
points(cbind(grid$X_coord[unique(c(new_presences[[pest]][,(ncol(new_presences[[pest]])-10+4):(ncol(new_presences[[pest]])-((lag2-5)/5))]))], grid$Y_coord[unique(c(new_presences[[pest]][,(ncol(new_presences[[pest]])-10+4):(ncol(new_presences[[pest]])-((lag2-5)/5))]))]), pch=15, cex=0.5, col=m$Col)
plot(transform_usa,add=TRUE, lwd=2)

image(1,1:50,t(1:50), col=pal(50), axes=FALSE)
par(las=1)
axis(4,labels=c("0",expression(10^{2}), expression(10^{4}),expression(10^{6})),at=c(seq(1,50,length.out=4)), cex.axis=1)
par(las=0)
mtext(side=4, "Street Tree Mortality (2020-2050)", line=3)


m<-SpatialPointsDataFrame(coords=cbind(grid$X_coord, grid$Y_coord), data=setNames(as.data.frame(rowSums(bestguess_genus[,39:57,])),"cost")) #grid cell level mortality
m2<-SpatialPointsDataFrame(coords=cbind(spp_agg_city$lon, spp_agg_city$lat), data=spp_agg_city)# city-level mortality
proj4string(m2)<-CRS("+proj=longlat +datum=WGS84")
m2<-spTransform(m2, CRS("+proj=eqdc +lat_0=39 +lon_0=-96 +lat_1=33 +lat_2=45 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"))
# # Create two panels side by side
par(mar=c(0.5,0.5,0.5,0.5))
par(oma=c(0,0,0,4))
layout(t(1:2), widths=c(6,1))
bins<-seq(0,6,length.out=50)
bins<-10^bins
m$Col<-pal(50)[findInterval(m$cost, bins)+1]
plot(transform_usa, lwd=0.2, main=NULL)
pal<-viridis
#m$Col <- pal(50)[as.numeric(cut(log10(m$cost+1),breaks = 50))]
points(cbind(grid$X_coord, grid$Y_coord), pch=15, cex=0.5, col=m$Col)
plot(transform_usa,add=TRUE, lwd=2)
image(1,1:50,t(1:50), col=pal(50), axes=FALSE)
par(las=1)
axis(4,labels=c("0",expression(10^{2}), expression(10^{4}),expression(10^{6})),at=c(seq(1,50,length.out=4)), cex.axis=1)
par(las=0)
mtext(side=4, "Street Tree Mortality (2020-2050)", line=3)



m<-SpatialPointsDataFrame(coords=cbind(grid$X_coord, grid$Y_coord), data=setNames(as.data.frame(rowSums(bestguess_genus[,26:38,])),"cost")) #grid cell level mortality
m2<-SpatialPointsDataFrame(coords=cbind(spp_agg_city$lon, spp_agg_city$lat), data=spp_agg_city)# city-level mortality
proj4string(m2)<-CRS("+proj=longlat +datum=WGS84")
m2<-spTransform(m2, CRS("+proj=eqdc +lat_0=39 +lon_0=-96 +lat_1=33 +lat_2=45 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"))
# # Create two panels side by side
par(mar=c(0.5,0.5,0.5,0.5))
par(oma=c(0,0,0,4))
layout(t(1:2), widths=c(6,1))
bins<-seq(0,6,length.out=50)
bins<-10^bins
m$Col<-pal(50)[findInterval(m$cost, bins)+1]
plot(transform_usa, lwd=0.2, main=NULL)
pal<-viridis
#m$Col <- pal(50)[as.numeric(cut(log10(m$cost+1),breaks = 50))]
points(cbind(grid$X_coord, grid$Y_coord), pch=15, cex=0.5, col=m$Col)
plot(transform_usa,add=TRUE, lwd=2)
image(1,1:50,t(1:50), col=pal(50), axes=FALSE)
par(las=1)
axis(4,labels=c("0",expression(10^{2}), expression(10^{4}),expression(10^{6})),at=c(seq(1,50,length.out=4)), cex.axis=1)
par(las=0)
mtext(side=4, "Street Tree Mortality (2020-2050)", line=3)


m<-SpatialPointsDataFrame(coords=cbind(grid$X_coord, grid$Y_coord), data=setNames(as.data.frame(rowSums(bestguess_genus[,1:25,])),"cost")) #grid cell level mortality
m2<-SpatialPointsDataFrame(coords=cbind(spp_agg_city$lon, spp_agg_city$lat), data=spp_agg_city)# city-level mortality
proj4string(m2)<-CRS("+proj=longlat +datum=WGS84")
m2<-spTransform(m2, CRS("+proj=eqdc +lat_0=39 +lon_0=-96 +lat_1=33 +lat_2=45 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"))
# # Create two panels side by side
par(mar=c(0.5,0.5,0.5,0.5))
par(oma=c(0,0,0,4))
layout(t(1:2), widths=c(6,1))
bins<-seq(0,6,length.out=50)
bins<-10^bins
m$Col<-pal(50)[findInterval(m$cost, bins)+1]
plot(transform_usa, lwd=0.2, main=NULL)
pal<-viridis
#m$Col <- pal(50)[as.numeric(cut(log10(m$cost+1),breaks = 50))]
points(cbind(grid$X_coord, grid$Y_coord), pch=15, cex=0.5, col=m$Col)
plot(transform_usa,add=TRUE, lwd=2)
image(1,1:50,t(1:50), col=pal(50), axes=FALSE)
par(las=1)
axis(4,labels=c("0",expression(10^{2}), expression(10^{4}),expression(10^{6})),at=c(seq(1,50,length.out=4)), cex.axis=1)
par(las=0)
mtext(side=4, "Street Tree Mortality (2020-2050)", line=3)
