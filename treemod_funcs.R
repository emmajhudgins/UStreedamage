r2mse<-function(obs, pred)
{
  1-((sum((wpred_sub[,obs]-(pred))^2))/(sum((wpred_sub[,obs]-mean(wpred_sub[,obs]))^2)))
}

plot_pred<-function(obs, pred)
{
  plot(wpred_sub[,obs]~pred, xlab="Predicted # Trees", ylab="Observed # Trees")
  abline(0,1)
}

rescale_constant<-function(prednum, predexist,sizex)
{
  return(prednum*predexist*(sum(wpred_sub[,sizex])/sum(prednum*predexist)))
}

rescale_all<-function(prednum,sizex)
{
  return(prednum*(sum(wpred_data[,sizex]/sum(prednum))))
}

rescale_city<-function(prednum,predexist,size)
{
  nt<-rep(0,nrow(rich_dat))
  rescale<-rep(0,nrow(rich_dat))
  for (i in 1:length(unique(data$city))) # city-specific rescaling - predicted trees in city sum to total predicted in sum in all-tree model
  {
    cityx<-which(wpred_data$city%in%unique(data$city)[i])
    rescale[cityx]<-sum(prednum[cityx]*predexist[cityx])
    nt[cityx]<-(subset(wpred_data, city==unique(data$city)[i])[,size])[1]
  }
  return(prednum*predexist*(nt/rescale))
}

rescale_city2<-function(pred,size)
{
  nt<-rep(0,nrow(wpred_data))
  rescale<-rep(0,nrow(wpred_data))
  for (i in 1:length(unique(data$city))) # city-specific rescaling - predicted trees in city sum to total predicted in sum in all-tree model
  {
    cityx<-which(wpred_data$city%in%unique(data$city)[i])
    rescale[cityx]<-sum(pred[cityx])
    nt[cityx]<-(subset(wpred_data, city==unique(data$city)[i])[,size])[1]
  }
  return(pred*(nt/rescale))
}

rescale_all_city<-function(prednum,predexist,size)
{
  nt<-rep(0,nrow(rich_dat))
  rescale<-rep(0,nrow(rich_dat))
  for (i in 1:56) # city-specific rescaling - predicted trees in city sum to total predicted in sum in all-tree model
  {
    cityx<-which(wpred_data$city%in%data$city[i])
    rescale[cityx]<-sum(prednum[cityx])
    nt[cityx]<-eval(parse(text=paste("pred",size,"trees",sep="_")))[i]
  }
  return(prednum*(nt/rescale))
}

preds_gam<-function(mod, size=NULL)
{
  if (is.null(size)==T)
  {
    preds<-(predict.gam(mod, newdata=wpred_sub, type="response"))
  }
  if (is.null(size)==F)
  {
    preds<-rep(0,nrow(wpred_sub))
    preds[which(wpred_sub[,size]>0)]<-predict.gam(mod, type="response")
  }
  preds[which(is.na(preds)==T)]<-0
  return(preds)
}

preds_ziP<-function(mod)
{
  preds<-rep(0,nrow(wpred_sub))
  preds<-predict.gam(mod, type="response", newdata=wpred_sub)
  preds[which(is.na(preds)==T)]<-0
  return(preds)
}

preds_gam<-function(mod)
{
  if (class(mod)[1]=="lm")
  {
    preds<-rep(0,nrow(wpred_sub))
    preds<-predict.lm(mod, type="response", newdata=wpred_sub)
    preds[which(is.na(preds)==T)]<-0
    return(preds)
  }
  preds<-rep(0,nrow(wpred_sub))
  preds<-predict.gam(mod, type="response", newdata=wpred_sub)
  preds[which(is.na(preds)==T)]<-0
  return(preds)
}

preds_brt<-function(mod)
{
  if (class(mod)[1]=="lm")
  {
    preds<-rep(0,nrow(wpred_sub))
    preds<-log(predict.lm(mod, type="response", newdata=wpred_sub))
    preds[which(is.na(preds)==T)]<-0
    return(preds)
  }
  return(predict.gbm(mod, newdata=wpred_sub, n.trees=mod$n.trees, type="response"))
}

preds_gam_new<-function(mod, newdata)
{
  if (class(mod)[1]=="lm")
  {
    #preds<-rep(0,nrow(wpred_sub))
    preds<-predict.lm(mod, type="response", newdata=newdata)
preds[which(is.na(preds)==T)]<-0
return(preds)
  }
  #preds<-rep(0,nrow(wpred_sub))
  preds<-predict.gam(mod, type="response", newdata=newdata)
  preds[which(is.na(preds)==T)]<-0
  return(preds)
}

preds_brt_new<-function(mod, newdata)
{
  if (class(mod)[1]=="lm")
  {
    #preds<-rep(0,nrow(wpred_sub))
    preds<-log(predict.lm(mod, type="response", newdata=newdata))
    preds[which(is.na(preds)==T)]<-0
    return(preds)
  }
  return(predict.gbm(mod, newdata=newdata, n.trees=mod$n.trees, type="response"))
}

preds_brtzip<-function(mod, size)
{
  preds<-rep(0,nrow(wpred_sub))
  if(size=="med"){preds[which(wpred_sub$SP%in%wpred_sub_med$SP)]<-predict.gbm(mod, newdata=wpred_sub_med, n.trees=mod$n.trees, type="response")}
  if(size=="large"){preds[which(wpred_sub$SP%in%wpred_aub_large$SP)]<-predict.gbm(mod, newdata=wpred_sub_large, n.trees=mod$n.trees, type="response")}
  if(size=="small"){preds[which(wpred_sub$SP%in%wpred_sub_large$SP)]<-predict.gbm(mod, newdata=wpred_sub_large, n.trees=mod$n.trees, type="response")}
  return(preds)
}
