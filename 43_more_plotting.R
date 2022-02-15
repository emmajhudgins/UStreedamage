library(viridis)
library(here)
library(ggplot2)
library(sp)
library(maptools)
library(maps)
library(geosphere)
library(rgdal)
library(dplyr)
library(grid)

setwd(here())

new_presences<-readRDS('./output/new_presences.RDS')
library(stringr)
library(dplyr)
library(here)
setwd(paste0(here(), '/output'))
data2<-read.csv('../data/datanorm.csv', stringsAsFactors = FALSE)# pest data from Hudgins et al. 2017
rr<-which(data2$YEAR>=10) # subset to pests present for at least 10 yrs as of 2008
data2<-data2[-c(25,69),] # remove pests with no range in US forests
data2<-data2[-4,]# remove pests with no range in US forests
data<-read.csv('../data/countydatanorm_march.csv', stringsAsFactors = FALSE) # spatial data

# 50x50km grid spatial information
grid<-read.csv('../data/countydatanorm_march.csv')

cumulative_presences<-readRDS('presences_time_noforce3.rds') # pest spread predictions following Hudgins et al. (2020)
cumulative_presences[[1]]<-readRDS('presences_time_eab.rds')[[1]]# updated to include observed eab distributions to 2020
cumulative_presences[[49]]<-readRDS('presences_time_hwa.rds')[[49]]#updated to include HWA historical spread
cumulative_presences[[51]]<-readRDS('presences_time_gm.rds')[[51]]#updated to include GM historical spread
cumulative_presences[[54]]<-readRDS('presences_time_bbd.rds')[[54]]#updated to include BBD historical spread

for (i in c(1:2,4:64))# remove ALB
{
  cumulative_presences[[i]]<-cumulative_presences[[i]][,which(colSums(cumulative_presences[[i]])!=0)]
}
cumulative_presences[rr[c(-20,-61)]]<-cumulative_presences #  remove 2 spp without forested range

# now add species present <10yrs from separate file
new<-which(1:75%in%rr==F)
cumulative_presences2<-readRDS('presences_time_rr2.rds')
for (i in 1:9)
{
  cumulative_presences[[new[i]]]<-cumulative_presences2[[i]][,which(colSums(cumulative_presences2[[i]])!=0)]
}
cumulative_presences<-cumulative_presences[-c(4,25,69)] # remove ALB and 2 spp without forested range


USA_map<-map('usa', fill=TRUE)
IDs <- sapply(strsplit(USA_map$names, ":"), function(x) x[1])
sp_map_usa <- map2SpatialPolygons(USA_map, IDs=IDs, proj4string=CRS("+proj=longlat +datum=WGS84"))
transform_usa<-spTransform(sp_map_usa, CRS("+proj=eqdc +lat_0=39 +lon_0=-96 +lat_1=33 +lat_2=45 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"))
Pfull_all<-matrix(0,3372,10)
Pfull_new<-matrix(0,3372,10)
for (i in 1:72)
{
  for (cc in 1:10)
  {
    if(ncol(cumulative_presences[[i]])-10+cc>0)
    {
      if (sum(new_presences[[i]][,(ncol(new_presences[[i]])-10+cc)]>0)){
    Pfull_new[new_presences[[i]][,(ncol(new_presences[[i]])-10+cc)],cc]<-Pfull_new[new_presences[[i]][,(ncol(new_presences[[i]])-10+cc)],cc]+1}
    Pfull_all[cumulative_presences[[i]][,(ncol(cumulative_presences[[i]])-10+cc)],cc]<-Pfull_all[cumulative_presences[[i]][,(ncol(cumulative_presences[[i]])-10+cc)],cc]+1
    }
  }
}
  


my_palette <- colorRampPalette(c("yellow1", "violetred2", "mediumblue"))(23)
m<-SpatialPointsDataFrame(coords=cbind(data$X_coord, data$Y_coord), data=data.frame(X1=Pfull_all[,10]))
##maple tree results
pdf('IAFI_load.pdf')
spplot(m, "X1",cex=0.85,main="2050", cuts=seq(0, 57, length.out=16),sp.layout=list(transform_usa, lwd=3, first=F), col.regions=my_palette, auto.key=FALSE, colorkey=TRUE,par.settings=list(
  layout.widths=list(left.padding=0, right.padding=7), 
  layout.heights=list(top.padding=0, bottom.padding=0)))
grid.text("IAFI load", x=unit(0.95, "npc"), y=unit(0.50, "npc"), rot=-90, gp=gpar(fontsize=20))
dev.off()

m<-SpatialPointsDataFrame(coords=cbind(data$X_coord, data$Y_coord), data=data.frame(X1=Pfull_all[,10]-Pfull_all[,1]))
spplot(m, "X1",cex=0.85,main="2050", cuts=seq(0, 31, length.out=16),sp.layout=list(transform_usa, lwd=3, first=F), col.regions=viridis(16), auto.key=FALSE, colorkey=TRUE,par.settings=list(
  layout.widths=list(left.padding=0, right.padding=7), 
  layout.heights=list(top.padding=0, bottom.padding=0)))
grid.text("New local establishments", x=unit(0.95, "npc"), y=unit(0.50, "npc"), rot=-90, gp=gpar(fontsize=20))


pdf("New_local_establishments.pdf")
par(mar=c(0.5,0.5,0.5,0.5))
par(oma=c(0,0,0,4))
layout(t(1:2), widths=c(6,1))
pal<-viridis
bins<-seq(0,40,length.out=40)
m$Col<-pal(40)[findInterval(m$X1, bins)+1]
plot(transform_usa, lwd=0.2, main=NULL)
points(cbind(grid$X_coord, grid$Y_coord), pch=15, cex=1, col=m$Col)
plot(transform_usa,add=TRUE, lwd=2)
image(1,1:40,t(1:40), col=pal(40), axes=FALSE)
par(las=1)
axis(4, cex.axis=1.1)
par(las=0)
mtext(side=4, "New local establishments", line=3, cex=1.25)
dev.off()

exposure<-readRDS('spatial_exposure.RDS')

m<-SpatialPointsDataFrame(coords=cbind(data$X_coord, data$Y_coord), data=data.frame(X1=exposure))

pdf('Street_tree_exposure.pdf')
par(mar=c(0.5,0.5,0.5,0.5))
par(oma=c(0,0,0,4))
layout(t(1:2), widths=c(6,1))
pal<-viridis
bins<-seq(0,6.1,length.out=40)
bins<-10^bins
m$Col<-pal(40)[findInterval(m$X1, bins)+1]
plot(transform_usa, lwd=0.2, main=NULL)
points(cbind(grid$X_coord, grid$Y_coord), pch=15, cex=1, col=m$Col)
plot(transform_usa,add=TRUE, lwd=2)
image(1,1:40,t(1:40), col=pal(40), axes=FALSE)
axis(4,labels=c("0",expression(10^{2}), expression(10^{4}),expression(10^{6})),at=c(seq(1,40,length.out=4)), cex.axis=1.1)
par(las=0)
mtext(side=4, "Street Tree Exposure (2020-2050)", line=3, cex=1.25)
dev.off()

grid_small<-readRDS('grid_small.RDS')
grid_med<-readRDS('grid_med.RDS')
grid_large<-readRDS('grid_large.RDS')
streettrees<-rowSums(grid_small)+rowSums(grid_med)+rowSums(grid_large)
m<-SpatialPointsDataFrame(coords=cbind(data$X_coord, data$Y_coord), data=data.frame(X1=streettrees))

pdf('Pred_street_trees.pdf')
par(mar=c(0.5,0.5,0.5,0.5))
par(oma=c(0,0,0,4))
layout(t(1:2), widths=c(6,1))
pal<-viridis
bins<-seq(0,6.1,length.out=40)
bins<-10^bins
m$Col<-pal(40)[findInterval(m$X1, bins)+1]
plot(transform_usa, lwd=0.2, main=NULL)
points(cbind(grid$X_coord, grid$Y_coord), pch=15, cex=1, col=m$Col)
plot(transform_usa,add=TRUE, lwd=2)
image(1,1:40,t(1:40), col=pal(40), axes=FALSE)
axis(4,labels=c("0",expression(10^{2}), expression(10^{4}),expression(10^{6})),at=c(seq(1,40,length.out=4)), cex.axis=1.1)
par(las=0)
mtext(side=4, "Predicted Street Trees", line=3, cex=1.25)
dev.off()
