rm(list=ls()) 
require(here)
setwd(paste0(here(), './data/'))
presences_time<-list()
data<-read.csv('countydatanorm_march.csv', stringsAsFactors = FALSE) # spatial data
data2<-read.csv('datanorm.csv', stringsAsFactors = FALSE) # species data, see Hudgins et al. corrigendum for information on why ALB (spp=3) cannot be accurately fit
rr<-which(data2$YEAR<10)
data2<-data2[rr,] # only look at species with less than 10 years of data (originally not forecast)
host.density2<-read.csv('hostden_rr.csv')[,2:10] # tree host density by pest species
prez<-read.csv('prez_rr.csv')[,2:10] # invasible host range (from FIA)
prez2<-read.csv('prez2_rr.csv')[,2:10] # pest distributions in 2005
L<-rep(0,9) # size of each pest's host range
for (sppp in 1:9)
{
  L[sppp]<-length(which(prez[,sppp]!=0))
}
#sources<-as.list(read.csv('Psources_notypos.csv')[,1])
currpopden<-as.matrix(read.csv("currpopden_5.csv", stringsAsFactors = FALSE))
currpopden2<-as.matrix(read.csv("future_scaled_pop2.csv"))
twenty35<-rowMeans(cbind(currpopden[,47], currpopden2[,4]))
twenty45<-rowMeans(cbind(currpopden2[,4], currpopden2[,5]))
twenty55<-rowMeans(cbind(currpopden2[,5], currpopden2[,6]))
currpopden<-cbind(currpopden, twenty35, currpopden2[,4], twenty45, currpopden2[,5], twenty55, currpopden2[,6])
#host.density2<-read.csv("hostden_clean_gdk.csv", stringsAsFactors = FALSE)

Tr1<-function(x)
{
  sqrt((data$X_coord-data$X_coord[x])^2+(data$Y_coord-data$Y_coord[x])^2)
}
dists<-sapply(1:3372, Tr1)
T1<-exp(-dists/50000)
rm(dists)
YEARS<-data2$YEAR

LLfit=function(par)
{
  pars<-rep(0,32)
  temp_t=F # minimum temperature threshold, should be set to true for spp=48
  hum_t=F # maximum humidity threshold, should be set to true for spp=44
  if (i %in% c(44,48,49)==F)
  {
  pars[c(1,21,22,4,18,20,8)]<-as.numeric(c(par,c(0.000538410692229749, 0.299034706404549, -0.525670755351726, 15.6132848183217,-0.163552592351765, 0.323831382884772)))
  }
  if (i==44)
  {
    hum_t=T
    pars[c(1,21,22,4,18,20,8,32)]<-as.numeric(c(par[1],c(0.000538410692229749, 0.299034706404549, -0.525670755351726, 15.6132848183217,-0.163552592351765, 0.323831382884772, par[2])))
  }
  if (i %in% c(48,49)==T)
  {
    temp_t=T
    pars[c(1,21,22,4,18,20,8,31)]<-as.numeric(c(par[1],c(0.000538410692229749, 0.299034706404549, -0.525670755351726, 15.6132848183217,-0.163552592351765, 0.323831382884772, par[2])))
  }
  par<-pars
  par[21]<-abs(par[21])
  par[22]<-abs(par[22])+1
  spp=i
  
  
  current_temp<-current_temp2<-0
  if (temp_t==T)
  {
    temp2<-temp<-read.csv('bio6_5yr.csv') 
    current_temp<-current_temp2<-temp2[,1]
  }
  hum=0
  if (hum_t==T)
  {
    hum<-read.csv('interp_rel_humidity.csv')[,1]
  }
  
  #Pest Parameters
  Pfull<<-matrix(0, 3372,64)
  Pfull_time<<-Pfull
  
  constpD=rep(0,64)
  constpD=matrix(rep(constpD),3372,64, byrow=TRUE)
  constpD2<-matrix(rep(par[9]*data[,18]+par[10]*data[,16]+par[16]*data[,19]+par[17]*data[,20]+par[18]*data[,21]),3372,64)+par[19]*host.density2
  constpD<-as.numeric(constpD)+constpD2
  constpD3<-matrix(rep(par[4]*data[,19]+par[12]*data[,20]+par[3]*data[,21]+par[13]*data[,18]+par[15]*data[,16]),3372,64)+par[5]*host.density2
  
  #Pest Parameters
  if (start=="centroid")
  {
    load('Psources_rr.Rdata')
  }
  if (start=="real")
  {
    load("Psources_actual_rr.Rdata")
  }
  Psource=sources[[spp]]
  YEAR<-YEARS[spp]
  Discovery<-2009-YEAR
  Pfull<<-matrix(0, 3372, 64)
  T2<-T1[prez[1:L[spp],spp],prez[1:L[spp],spp]]
  vecP<-rep(0,L[spp])
  for (rrr in 1:length(Psource))
  {vecP[which(prez[,spp]==Psource[rrr])]=1}
  r0<-par[22]
  for (time in 1:(floor(YEAR/5)+9))
  {
    if (temp_t==T)
    {
      if (time<(floor(YEAR/5)-4))
      {
        current_temp<-temp2[,1]
        current_temp2<-temp2[,1]
      }
      if (time>=(floor(YEAR/5)-4))
      {
        current_temp2<-temp2[,time-floor(YEAR/5)+6]
      }
    }
    vecP[which(prez[,spp]==Psource)]=1
    if (temp_t==T)
    {vecP[which(current_temp[prez[,spp]]<par[31])]<-0}
    if (hum_t==T)
    {vecP[which(hum[prez[,spp]]<par[32])]<-0}
     vecP[which(prez[,spp]==Psource)]=1
    Pnext<-rep(0,L[spp]) # vecP for next timestep
    qq<-0 # dispersal kernel
    column<-(((Discovery+5*(time-1))-1790)/5)+1 # which year of human population density to consider
    qq<-matrix(rep(constpD[prez[which(vecP>=par[21]),spp],spp]+par[8]*currpopden[prez[which(vecP>=par[21]),spp],column], L[spp]), nrow=length(which(vecP>=par[21])), ncol=L[spp])
    zzz<-matrix(rep(constpD3[prez[1:L[spp],spp],spp]+par[20]*currpopden[prez[1:L[spp],spp],column], L[spp]), nrow=L[spp], ncol=L[spp], byrow=TRUE) # add in current human populations
    qq<-(2*par[1]*exp((zzz[which(vecP>=par[21]),]+qq)))/(1+exp(zzz[which(vecP>=par[21]),]+qq)) 
    qq<-T2[which(vecP>=par[21]),]^qq # add in distance
    #scale dispersal kernel
    if (length(which(vecP>=par[21]))>1){qq<-qq/rowSums(qq)}
    if (length(which(vecP>=par[21]))==1){qq<-qq/sum(qq)}
    qq[which(qq<0.001)]=0
    Pnext=(vecP[which(vecP>=par[21])])%*%(qq) # dispersal into and out of all sites
    if (temp_t==T)
    {
      Pnext[which(current_temp[prez[,spp]]<par[31])]<-0
    }
    if (hum_t==T)
    {
      Pnext[which(hum[prez[,spp]]>par[2])]<-0
    }
    Pnext[which(prez[,spp]==Psource)]=1
    Pfull[,time]<<-c(prez[which(Pnext>=par[21]),spp], rep(0, 3372-length(which(Pnext>=par[21])))) 
    Pfull_time[,time]<<-c(prez[which(Pnext>=par[21]),spp], rep(0, 3372-length(which(Pnext>=par[21])))) #threshold at 'discoverable' value
    if (time>1)
    {
      dddd<-which(!(Pfull_time[1:length(which(Pfull_time[,time-1]!=0)),time-1]%in%Pfull_time[1:length(which(Pfull_time[,time]!=0)),time]))
      ffff<-which(prez[1:length(which(prez[,spp]!=0)), spp]%in%Pfull_time[dddd,time-1])
      Pnext[ffff]<-par[21]
    }

    current_temp<-current_temp2
    Pnext[which(prez[,spp]==Psource)]=1
    Pnext[which(Pnext>=par[21])]=Pnext[which(Pnext>=par[21])]*r0
    Pnext[which(Pnext>=1)]<-1
    vecP=Pnext
    vecP[which(prez[,spp]==Psource)]=1
    Pfull_time[,time]<<-c(prez[which(Pnext>=par[21]),spp], rep(0, 3372-length(which(Pnext>=par[21])))) #threshold at 'discoverable' value
    Pfull[,time]<<-c(prez[which(Pnext>=par[21]),spp], rep(0, 3372-length(which(Pnext>=par[21])))) #threshold at 'discoverable' value  
  }
}
startpt<-read.csv('startpt.csv')[,1]
startpt=0
centroids<-c(2,4,8,9)
for (i in 1:9)
{
  spp=i
  if (i %in% centroids==F)
  {
    start='real'
    pars<-read.csv(paste("./par_rr_GDK.ic.real",i,"csv", sep="."))[,1]
    }
  if (i %in% centroids==T)
  {
    start=='centroid'
    pars<-read.csv(paste("./par_rr_GDK.ic.centroid",i,"csv", sep="."))[,1]
  }
  if (i==48)
  {
   pars<-read.csv('par_GDK_forecast.48.csv')[,1]
  }
  if (i==44)
  {
    pars<-read.csv('par_GDK_forecast.44.csv')[,1]
  }
  if (i==49)
  {
    pars<-read.csv("par_GDK_HWA_s_ic_c_forecast_.csv")[,2]
  }
  if (i==51)
  {
    pars<-read.csv("par_GDK_GM_s_ic__forecast_.csv")[,1]
  }
  if (i==54)
  {
    pars<-read.csv("par_GDK_BBD_s_ic__forecast_.csv")[,1]
  }
  LLfit(c(pars))
  presences_time[[i]]<-Pfull_time
}

saveRDS(presences_time, file="../output/presences_time_rr2.rds")
