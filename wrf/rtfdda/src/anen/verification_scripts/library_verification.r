
##This is a collection of functions for ensemble verification
##
##The first two inputs of all the functions are:
## 1) a netcdf file of the measurements with 3 dimensions (stations,forecast cicle, lead times )
## 2) a netcdf file of the ensemble forecast with 4 dimensions (stations,forecast cycle, lead times, members)
## The first three indexes of both files must correspond and the dimension must be the same
## Missing values are accepted, must be included as NaN
##
###
###function bias return the average bias over all the leadtimes as mean(measurements- ensemblemean)
###
library(ncdf4) 
library(boot) 

#err.function is just a support for boot strap function called in many of following functions
err.fun=function(d,i){ 
  mean(d$errore[i])
}
############################

bias=function(meas,forec,bstrap,adding,plotfile,close,type,colour){ 
  #  meas="E:/analog_ensemble/measurements.nc"
  #  forec="E:/analog_ensemble/forecast.nc"

  ## Opening file netcdf 
  swap <- nc_open(forec)
  forecasts = ncvar_get(swap)
  nc_close(swap)
  swap1 <- nc_open(meas)
  measurements = ncvar_get(swap1)
  nc_close(swap1)
  #end reading netcdf files
  #finding matrix dimension
  n_station=dim(forecasts)[1]
  n_days=dim(forecasts)[2]
  n_ltimes=dim(forecasts)[3]
  n_members=dim(forecasts)[4]
  #####the forecast datatset are put in the matrix eps_analog
  ##### Each row corresponds to a single forecast, each column to an ensemble member
  length_dataset=(n_station)*(n_days*n_ltimes)
  epsanalog = array(dim=c(length_dataset,n_members))
  test = array(dim=c(length_dataset,2))
  for(i in  seq(n_station)){
    for(j in  seq(n_days)){
      for(k in  seq(n_ltimes)){
        for(z in  seq(n_members)){
          indice=(i-1)*(n_days*n_ltimes) +(j-1)*n_ltimes+k
          epsanalog[indice,z]=forecasts[i,j,k,z]  #on the columns put the members           
        } 
      }       
    }
  }
  #####the measurements datatset are put in the matrix Test
  ##### Each row corresponds to a forecast  column 1 is the forecast leadtime
  ##### Column 2 is the measrement value
  ##### epsanalog and test have the same nmber of rows
    for(i in  seq(n_station)){
    for(j in  seq(n_days)){
      for(k in  seq(n_ltimes)){
        indice=(i-1)*(n_days*n_ltimes) +(j-1)*n_ltimes+k
        test[indice,1]=k #leadtimes on the first column
        test[indice,2]=measurements[i,j,k] #measurements on the second  column          
      }       
    }
  }
  
  ####end of chenaging of dataset structure
  #### begin of index comptation
  potens=epsanalog                               
  media_ens=apply(potens,1,mean)
  
  bias_ens=(media_ens-test[,2])
  
  biaspotens=vector(mode="numeric",length=n_ltimes)
  bias_tot=mean(bias_ens,na.rm=T)
  for(i in seq(n_ltimes)){
    biasens=bias_ens[which(test[,1]==i)]
    
    
    biaspotens[i]=mean(biasens,na.rm=T)
  }
  
  if(bstrap==T){ #bootstrap confidence intervals only if required
    boot_t0=vector(mode="numeric",length=n_ltimes)
    bootstraplow=vector(mode="numeric",length=n_ltimes)     
    bootstraphigh=vector(mode="numeric",length=n_ltimes)
    
    for(i in seq(n_ltimes)){
      errtemp=na.omit(bias_ens[which(test[,1]==i)])
      oggetto=as.data.frame(errtemp)
      names(oggetto)="errore"
      err.boot=boot(oggetto,err.fun,R=1000)
      bootresult=boot.ci(err.boot,type="basic")  
      boot_t0[i]=bootresult$t0            
      bootstraplow[i]=bootresult$basic[4]        
      bootstraphigh[i]=bootresult$basic[5]
      boot_t0[i]=  (boot_t0[i])       
      bootstraplow[i]=(bootstraplow[i])  
      bootstraphigh[i]=( bootstraphigh[i])
    }
  }

 if(adding==F){ 
  if(plotfile!="") {postscript(plotfile)}
  if(bstrap==T){
#####plotting phase  with boot strap confidence intervals  
    leadlabel=seq(n_ltimes)   
    plot(boot_t0,type="l",axes=F,ylim=c(-1,1),xlim=c(1,n_ltimes),lwd=2,xlab="Lead Time (Hour)",ylab=expression("BIAS (ms" ^"-1"~")"),cex.lab=1.,pch=16)
    axis(1,at=seq(n_ltimes),labels=leadlabel,cex.axis=1.1)
    axis(2,cex.axis=1.3)
    for(i in seq(n_ltimes)){
      arrows(i,bootstraplow[i],i,bootstraphigh[i],length=0.05,angle=90,code=3)   #consistency bars          
    }  
    
  }else{  
    leadlabel=seq(1,n_ltimes)           
    plot(biaspotens,type="l",axes=F,ylim=c(-1,1),xlim=c(1,n_ltimes),lwd=2,xlab="Lead Time (Hour)",ylab=expression("BIAS (ms" ^"-1"~")"),cex.lab=1.,pch=16)
    axis(1,at=seq(0,n_ltimes-1),labels=leadlabel,cex.axis=1.1)
    # axis(2,at=seq(max(rmsepotens*1.2)),cex.axis=1.3) 
    #axis(1)
    axis(2,cex.axis=1.3)    
  }
 }
  if(adding==T){ 
    if(bstrap==T){
      #####plotting phase  with boot strap confidence intervals  
     lines(boot_t0,type="l",lty=type,col=colour,lwd=2)
      for(i in seq(n_ltimes)){
        arrows(i,bootstraplow[i],i,bootstraphigh[i],length=0.05,angle=90,code=3,col=colour,lwd=2)   #consistency bars          
      }  
      
    }else{          
      lines(biaspotens,lty=type,col=colour,lwd=2)
    }
  }
   
  ##close the file only if there is just this model to plot 
  if(close==T){ 
    if(plotfile!=""){ dev.off() } 
  }
  
  
  return(bias_tot)
}  

#####################
#dispersion diagram
#RMSE vs Leadtime
#ensemble spread vs leadtime
#####################

dispersion =function(meas,forec,bstrap,adding,plotfile,close,type,colour){ 
  #  meas="E:/analog_ensemble/measurements.nc"
  #  forec="E:/analog_ensemble/forecast.nc"
 
  swap <- nc_open(forec)
  forecasts = ncvar_get(swap)
  nc_close(swap)
  swap1 <- nc_open(meas)
  measurements = ncvar_get(swap1)
  nc_close(swap1)
  #end reading netcdf files
  #finding matrix dimension
  n_station=dim(forecasts)[1]
  n_days=dim(forecasts)[2]
  n_ltimes=dim(forecasts)[3]
  n_members=dim(forecasts)[4]
  length_dataset=(n_station)*(n_days*n_ltimes)
  epsanalog = array(dim=c(length_dataset,n_members))
  test = array(dim=c(length_dataset,2))
  for(i in  seq(n_station)){
    for(j in  seq(n_days)){
      for(k in  seq(n_ltimes)){
        for(z in  seq(n_members)){
          indice=(i-1)*(n_days*n_ltimes) +(j-1)*n_ltimes+k
          epsanalog[indice,z]=forecasts[i,j,k,z]  #on the columns put the members           
        } 
      }       
    }
  }
  for(i in  seq(n_station)){
    for(j in  seq(n_days)){
      for(k in  seq(n_ltimes)){
        indice=(i-1)*(n_days*n_ltimes) +(j-1)*n_ltimes+k
        test[indice,1]=k #leadtimes on the first column
        test[indice,2]=measurements[i,j,k] #measurements on the second  column          
      }       
    }
  }
  potens=epsanalog
  spread=apply(potens,1,var)                                  #ensemble spread
  media_ens=apply(potens,1,mean)
  error_med_ens=(media_ens-test[,2])^2           
  mae_ens=abs(test[,2]-media_ens)  
  spreadpot=vector(mode="numeric",length=n_ltimes)
  rmsepotens=vector(mode="numeric",length=n_ltimes)
  maepotens=vector(mode="numeric",length=n_ltimes)
  rmse_tot=sqrt(mean(error_med_ens,na.rm=T))
  for(i in seq(n_ltimes)){
    spreadtemp=spread[which(test[,1]==i)]
    erroreens=(error_med_ens[which(test[,1]==i)])
    maeens=mae_ens[which(test[,1]==i)]
    spreadpot[i]=sqrt((mean(spreadtemp,na.rm=T)))
    rmsepotens[i]=sqrt(mean(erroreens,na.rm=T)*n_members/(n_members+1))
    #RMSE as suggested by 
    maepotens[i]=mean(mae_ens)
  }

  if(bstrap==T){ #bootstrap confidence intervals only if required
    boot_t0=vector(mode="numeric",length=n_ltimes)
    bootstraplow=vector(mode="numeric",length=n_ltimes)     
    bootstraphigh=vector(mode="numeric",length=n_ltimes)
    
    
    for(i in seq(n_ltimes)){
      errtemp=na.omit(error_med_ens[which(test[,1]==i)])
      oggetto=as.data.frame(errtemp)
      names(oggetto)="errore"
      err.boot=boot(oggetto,err.fun,R=1000)
      bootresult=boot.ci(err.boot,type="basic")  
      boot_t0[i]=bootresult$t0            #mean vealue
      bootstraplow[i]=bootresult$basic[4]        
      bootstraphigh[i]=bootresult$basic[5]
      boot_t0[i]=  sqrt(boot_t0[i]* n_members/(n_members+1))        #mean vale
      bootstraplow[i]=sqrt(bootstraplow[i]*n_members/(n_members+1))  
      bootstraphigh[i]=sqrt( bootstraphigh[i]*n_members/(n_members+1))
    }
  }
  
  if(adding==F){ 
    if(plotfile!="") {postscript(plotfile)}
  if(bstrap==T){
   leadlabel=seq(n_ltimes)   
    plot(boot_t0,type="l",axes=F,ylim=c(0,max(rmsepotens*1.3)),xlim=c(1,n_ltimes),lwd=2,xlab="Lead Time (Hour)",ylab=expression("RMSE (ms" ^"-1"~")"),cex.lab=1.,pch=16)
    axis(1,at=seq(0,n_ltimes-1),labels=leadlabel,cex.axis=1.1)
    axis(2,cex.axis=1.1)
    for(i in seq(n_ltimes)){
      arrows(i,bootstraplow[i],i,bootstraphigh[i],length=0.05,angle=90,code=3)   #consistency bars          
    }  
    lines(spreadpot,lty="dashed",lwd=2,pch=16)
  }else{
    
    #
    leadlabel=seq(0,n_ltimes-1)           #-------------------label
    plot(rmsepotens,type="l",axes=F,ylim=c(0,max(rmsepotens*1.3)),xlim=c(1,n_ltimes),lwd=2,xlab="Lead Time (Hour)",ylab=expression("RMSE (ms" ^"-1"~")"),cex.lab=1.,pch=16)
    axis(1,at=seq(0,n_ltimes-1),labels=leadlabel,cex.axis=1.1)
    # axis(2,at=seq(max(rmsepotens*1.2)),cex.axis=1.3) 
    #axis(1)
    axis(2,cex.axis=1.1)
    lines(spreadpot,lty="dashed",lwd=2,pch=16) 
  }
}
  if(adding==T){ 
    if(bstrap==T){ 
      lines(boot_t0,lty=type,col=colour,lwd=2)
   
      for(i in seq(n_ltimes)){
        arrows(i,bootstraplow[i],i,bootstraphigh[i],length=0.05,angle=90,code=3,col=colour,lwd=2)   #consistency bars          
      }  
      lines(spreadpot,lty="dashed",lwd=2,col=colour,pch=16)
    }else{
     lines(rmsepotens,lty=type,col=colour,lwd=2)
      lines(spreadpot,lty="dashed",lwd=2,col=colour,pch=16) 
    }
  }   
  
  
  
  ##close the file only if there is just this model to plot 
  if(close==T){ 
    if(plotfile!=""){ dev.off() } 
  }
  return(rmse_tot)
}  


#function for mean absolute erroe
mae=function(meas,forec,bstrap,adding,plotfile,close,type,colour) {
  #  meas="E:/analog_ensemble/measurements.nc"
  #  forec="E:/analog_ensemble/forecast.nc"  
 #reading netcdf files 
  swap <- nc_open(forec)
  forecasts = ncvar_get(swap)
  nc_close(swap)
  swap1 <- nc_open(meas)
  measurements = ncvar_get(swap1)
  nc_close(swap1)
  #end reading netcdf files
  #finding matrix dimension
  n_station=dim(forecasts)[1]
  n_days=dim(forecasts)[2]
  n_ltimes=dim(forecasts)[3]
  n_members=dim(forecasts)[4]
  length_dataset=(n_station)*(n_days*n_ltimes)
  epsanalog = array(dim=c(length_dataset,n_members))
  test = array(dim=c(length_dataset,2))
  
  #put the array in the right format
  for(i in  seq(n_station)){
    for(j in  seq(n_days)){
      for(k in  seq(n_ltimes)){
        for(z in  seq(n_members)){
          indice=(i-1)*(n_days*n_ltimes) +(j-1)*n_ltimes+k
          epsanalog[indice,z]=forecasts[i,j,k,z]  #on the columns put the members           
        } 
      }       
    }
  }
  for(i in  seq(n_station)){
    for(j in  seq(n_days)){
      for(k in  seq(n_ltimes)){
        indice=(i-1)*(n_days*n_ltimes) +(j-1)*n_ltimes+k
        test[indice,1]=k #leadtimes on the first column
        test[indice,2]=measurements[i,j,k] #measurements on the second  column          
      }       
    }
  }
  potens=epsanalog                                 #ensemble spread
  media_ens=apply(potens,1,mean)
  
  mae_ens=abs(test[,2]-media_ens)  
  
  maepotens=vector(mode="numeric",length=n_ltimes)
  mae_tot=mean(mae_ens,na.rm=T)
  for(i in seq(n_ltimes)){
    maeens=mae_ens[which(test[,1]==i)]    
    maepotens[i]=mean(maeens,na.rm=T)
  }
  
  if(bstrap==T){ #bootstrap confidence intervals only if required
    boot_t0=vector(mode="numeric",length=n_ltimes)
    bootstraplow=vector(mode="numeric",length=n_ltimes)     
    bootstraphigh=vector(mode="numeric",length=n_ltimes)
    
    for(i in seq(n_ltimes)){
      errtemp=na.omit(mae_ens[which(test[,1]==i)])
      oggetto=as.data.frame(errtemp)
      names(oggetto)="errore"
      err.boot=boot(oggetto,err.fun,R=1000)
      bootresult=boot.ci(err.boot,type="basic")  
      boot_t0[i]=bootresult$t0            
      bootstraplow[i]=bootresult$basic[4]        
      bootstraphigh[i]=bootresult$basic[5]
      boot_t0[i]=  (boot_t0[i])       
      bootstraplow[i]=(bootstraplow[i])  
      bootstraphigh[i]=( bootstraphigh[i])
    }
  }
  if(adding==F){ 
    if(plotfile!="") {postscript(plotfile)} 
  if(bstrap==T){
   leadlabel=seq(n_ltimes)   
    plot(boot_t0,type="l",axes=F,ylim=c(0,max(maepotens*1.3)),xlim=c(1,n_ltimes),lwd=2,xlab="Lead Time",ylab="MAE",cex.lab=1.6,pch=16)
    axis(1,at=seq(n_ltimes),labels=leadlabel,cex.axis=1.3)
    axis(2,cex.axis=1.3)
    for(i in seq(n_ltimes)){
      arrows(i,bootstraplow[i],i,bootstraphigh[i],length=0.05,angle=90,code=3)   #consistency bars          
    }  
    
  }else{
    
    #label for lead times
    leadlabel=seq(1,n_ltimes)           
    plot(maepotens,type="l",axes=F,ylim=c(0,max(maepotens*1.3)),xlim=c(1,n_ltimes),lwd=2,xlab="Lead Time",ylab="RMSE",cex.lab=1.6,pch=16)
    axis(1,at=seq(0,n_ltimes-1),labels=leadlabel,cex.axis=1.3)
    # axis(2,at=seq(max(rmsepotens*1.2)),cex.axis=1.3) 
    #axis(1)
    axis(2,cex.axis=1.3)  
  }
 }
  if(adding==T){ 
    if(bstrap==T){
      #####plotting phase  with boot strap confidence intervals  
      lines(boot_t0,lty=type,col=colour,lwd=2)
      for(i in seq(n_ltimes)){
        arrows(i,bootstraplow[i],i,bootstraphigh[i],length=0.05,angle=90,code=3,col=colour,lwd=2)   #consistency bars          
      }  
      
    }else{          
      lines(maepotens,lty=type,col=colour,lwd=2)
    }
  }
  ##close the file only if there is just this model to plot 
  if(close==T){ 
    if(plotfile!=""){ dev.off() } 
  }
  return(mae_tot)
}  

#function for the spread skill plot
spreadskill=function(meas,forec,bstrap,adding,plotfile,close,type,colour){ 
 # meas="/Volumes/d1/alessand/Excel//measurements_3week.nc"
#  forec="/Volumes/d1/alessand/Excel//forecast_10_4pred_3week_each.nc"
  
  swap <- nc_open(forec)
  forecasts = ncvar_get(swap)
  nc_close(swap)
  swap1 <- nc_open(meas)
  measurements = ncvar_get(swap1)
  nc_close(swap1)
  #end reading netcdf files
  #finding matrix dimension
  n_station=dim(forecasts)[1]
  n_days=dim(forecasts)[2]
  n_ltimes=dim(forecasts)[3]
  n_members=dim(forecasts)[4]
  length_dataset=(n_station)*(n_days*n_ltimes)
  epsanalog = array(dim=c(length_dataset,n_members))
  test = array(dim=c(length_dataset,2))
  for(i in  seq(n_station)){
    for(j in  seq(n_days)){
      for(k in  seq(n_ltimes)){
        for(z in  seq(n_members)){
          indice=(i-1)*(n_days*n_ltimes) +(j-1)*n_ltimes+k
          epsanalog[indice,z]=forecasts[i,j,k,z]  #on the columns put the members           
        } 
      }       
    }
  }
  for(i in  seq(n_station)){
    for(j in  seq(n_days)){
      for(k in  seq(n_ltimes)){
        indice=(i-1)*(n_days*n_ltimes) +(j-1)*n_ltimes+k
        test[indice,1]=k #leadtimes on the first column
        test[indice,2]=measurements[i,j,k] #measurements on the second  column          
      }       
    }
  }
  potens=epsanalog                             
  spread=apply(potens,1,var)
  media_ens=apply(potens,1,mean)
  error_med_ens=(media_ens-test[,2])^2   
  n_intervalli=13
  pulitotest=cbind(media_ens,spread,error_med_ens)
  test1=na.omit(pulitotest)
  test1=test1[order(test1[,2]),]
  step1=dim(test1)[1]/n_intervalli
  step1=floor(step1)
  errvel=matrix(nrow=20,ncol=2)
  
  
  if(bstrap==T){ #bootstrap confidence intervals only if required
    boot_t0=vector(mode="numeric",length=n_intervalli)
    bootstraplow=vector(mode="numeric",length=n_intervalli)     
    bootstraphigh=vector(mode="numeric",length=n_intervalli)
  }
  
  f=0
  i=0                
  for(x in seq(1,n_intervalli)){
    i=i+1
    filtro=test1[f:(f+step1),]
    
    if(bstrap==T){ #bootstrap confidence intervals only if required
      errtemp=na.omit(filtro[,3])
      oggetto=as.data.frame(errtemp)
      names(oggetto)="errore"
      err.boot=boot(oggetto,err.fun,R=1000)
      bootresult=boot.ci(err.boot,type="basic")  
      boot_t0[i]=bootresult$t0            
      bootstraplow[i]=bootresult$basic[4]        
      bootstraphigh[i]=bootresult$basic[5]
      boot_t0[i]=  sqrt(boot_t0[i]* n_members/(n_members+1))        #uguale a rmsepotens ad ogni lead time
      bootstraplow[i]=sqrt(bootstraplow[i]*n_members/(n_members+1))  
      bootstraphigh[i]=sqrt( bootstraphigh[i]*n_members/(n_members+1))
      
    } 
    
    errvel[i,2]=sqrt(mean(filtro[,3])*n_members/(n_members+1))
    errvel[i,1]=sqrt(mean(filtro[,2]))
    f=f+step1
  }
  
  R2=cor(errvel[,1],errvel[,2],use="pairwise.complete.obs")
  R2=R2*R2
  #R2=paste("R^2=",substr(R2,1,5))
  
  if(adding==F){ 
    if(plotfile!="") {postscript(plotfile)} 
  
  if(bstrap==T){
    plot(errvel[,1],errvel[,2],type="l",xlim=c(0,max(errvel[,1],na.rm=T)*1.2),ylim=c(0,max(errvel[,1],na.rm=T)*1.2),xlab="Binned Spread",pch=19,ylab="Binned RMSE",
         cex.lab=1.4,cex.axis=1.2)
    abline(0,1,lty=2)
    
    for(i in seq(n_intervalli)){
      arrows(errvel[i,1],bootstraplow[i],errvel[i,1],bootstraphigh[i],length=0.05,angle=90,code=3)   #consistency bars          
    }   
    
  }else{  
    plot(errvel[,1],errvel[,2],type="o",xlim=c(0,max(errvel[,1],na.rm=T)*1.2),ylim=c(0,max(errvel[,1],na.rm=T)*1.2),xlab="Binned Spread",pch=19,ylab="Binned RMSE",
         cex.lab=1.4,cex.axis=1.2)
    abline(0,1,lty=2)
    #   text(10,370,labels=R2,cex=1.4,pos=4)
  }
 }   
  if(adding==T){ 
    if(bstrap==T){
      #####plotting phase  with boot strap confidence intervals  
      lines(errvel[,1],errvel[,2],lty=type,col=colour,lwd=2)
      for(i in seq(n_intervalli)){
        arrows(errvel[i,1],bootstraplow[i],errvel[i,1],bootstraphigh[i],length=0.05,angle=90,code=3,col=colour,lwd=2)   #consistency bars          
      }  
      
    }else{          
      lines(errvel[,1],errvel[,2],lty=type,col=colour,lwd=2)
    }
  }
  ##close the file only if is the last model to plot 
  if(close==T){ 
    if(plotfile!=""){ dev.off() } 
  } 
  
  return(R2)
}  
reliability=function(meas,forec,threshold,plotfile){ 
  # meas="E:/analog_ensemble/measurements.nc"
  # forec="E:/analog_ensemble/forecast.nc"
  
  swap <- nc_open(forec)
  forecasts = ncvar_get(swap)
  nc_close(swap)
  swap1 <- nc_open(meas)
  measurements = ncvar_get(swap1)
  nc_close(swap1)
  #end reading netcdf files
  #finding matrix dimension
  n_station=dim(forecasts)[1]
  n_days=dim(forecasts)[2]
  n_ltimes=dim(forecasts)[3]
  n_members=dim(forecasts)[4]
  length_dataset=(n_station)*(n_days*n_ltimes)
  epsanalog = array(dim=c(length_dataset,n_members))
  test = array(dim=c(length_dataset,2))
  for(i in  seq(n_station)){
    for(j in  seq(n_days)){
      for(k in  seq(n_ltimes)){
        for(z in  seq(n_members)){
          indice=(i-1)*(n_days*n_ltimes) +(j-1)*n_ltimes+k
          epsanalog[indice,z]=forecasts[i,j,k,z]  #on the columns put the members           
        } 
      }       
    }
  }
  for(i in  seq(n_station)){
    for(j in  seq(n_days)){
      for(k in  seq(n_ltimes)){
        indice=(i-1)*(n_days*n_ltimes) +(j-1)*n_ltimes+k
        test[indice,1]=k #leadtimes on the first column
        test[indice,2]=measurements[i,j,k] #measurements on the second  column          
      }       
    }
  }
  potens=cbind(epsanalog,test)
  potens=na.omit(potens)
  test=potens[,(n_members+1):(n_members+2)]
  potens=potens[,1:n_members]
  misura=test[,2]
  
  dimset=dim(potens)[1]
  EPSfc=vector(mode="numeric",length=dimset)     #probability for enesmble forecast to be greater thanthreshold (es. number of members > x)
  MISfc=vector(mode="numeric",length=dimset)     #probability for measurements to be > threshold (0 o 1)
  
  for(i in seq(dimset)){
    EPSfc[i]=length(which(potens[i,]>threshold))/n_members
    if(misura[i]>threshold){
      MISfc[i]=1
    }else{
      MISfc[i]=0
    }
  }
  
  pk=seq(0.0,1.0,0.1)  #probability vector from 0 to 1
  nk=vector(mode="numeric",length=length(pk))         #case  for eache probabilistic category da 0 a 1
  nobsk=vector(mode="numeric",length=length(pk))      #n.case in whiche the event is obsreved for each probabilistci category
  ok=vector(mode="numeric",length=length(pk))         # n. observed events/n. of forecasts for each category
  nonobsk=vector(mode="numeric",length=length(pk))    #not observed
  sequenza=c(seq(0,0.05,0.05),seq(0.15,0.95,0.1),1.0) #vector of probability classes
  
  for(i in seq(pk)){
    nk[i]=length(which(EPSfc>=sequenza[i] & EPSfc<sequenza[i+1]))
    temp=(misura[which(EPSfc>=sequenza[i] & EPSfc<sequenza[i+1])])
    if(i==11){
      nk[i]=length(which(EPSfc>=sequenza[i] & EPSfc<=sequenza[i+1]))
      temp=(misura[which(EPSfc>=sequenza[i] & EPSfc<=sequenza[i+1])])
    }
    nobsk[i]=length(which(temp>threshold))
    nonobsk[i]=length(which(temp<=threshold))
  }
  
  totnk=sum(nk)
  totnobsk=sum(nobsk)
  totnonobsk=sum(nonobsk)
  ok=nobsk/nk
  ok[which(is.na(ok))]=0
  climatology=totnobsk/totnk     #------total observed events/total forecasts
  
  ll=vector(mode="numeric",length=length(pk))
  uu=vector(mode="numeric",length=length(pk))
  
  for(i in seq(nk)){
    ll[i]=qbinom(0.05,nk[i],ok[i],lower.tail=TRUE,log.p=FALSE)/nk[i]
    uu[i]=qbinom(0.95,nk[i],ok[i],lower.tail=TRUE,log.p=FALSE)/nk[i]
  }
  
  bsparziale=(EPSfc-MISfc)^2  
  BS=mean(bsparziale)
  omean=mean(MISfc)       #this value is equal to the sample climatology =tot(nobsk)/tot(nk), in the components of BS is c
  bsref=(MISfc-omean)^2
  BS_REF=mean(bsref)
  BSS=1-(BS/BS_REF)       #BRIER SKILL SCORE
  fobs=nobsk/nk
  fobs[which(fobs=="NaN")]=0           #in case the first value of n_obsk e n_k is 0
  REL=sum(nk*(pk-fobs)^2)/totnk         #reliability
  RES=sum(nk*(fobs-omean)^2)/totnk      #resolution
  UNC=omean*(1-omean)                    #uncertainty
  
  #------------------------------------------------------------------------plots
  if(plotfile!="") {postscript(plotfile)} 
  plot(pk,ok,type="l",xlim=c(0,1),ylim=c(0,1),lwd=1,xlab="Forecast probability",axes=F,
       ylab="Observed frequency ",pch=19,cex.lab=1.3,cex.main=1.4,cex.axis=1.3)         #cex=20*nk/totnk,
  axis(1,at=seq(0,1,0.2),labels=seq(0,1,0.2))
  axis(2,at=seq(0,1,0.2),labels=seq(0,1,0.2))
  abline(0,1,col="grey")#lty="dashed")
  abline(climatology,0,lty=2,lwd=2)
  for(i in seq(length(pk))){
    arrows(pk[i],ll[i],pk[i],uu[i],length=0.03,angle=90,code=3,lwd=1)  #consistency bars
  }
  lines(pk,nk/totnk,type="b",pch=15,lwd=1,cex=1.3,col="red")
  legend(0.1,0.9,# places a legend at the appropriate place 
         c("Sharpness","Climatology"), # puts text in the legend 
         lty=c(1,2), # gives the legend appropriate symbols (lines)
         lwd=c(2,2),col=c("red","black"), # gives the legend lines the correct color and width
         cex=c(0.8,0.8)) 
  
  if(plotfile!="") {
    dev.off()  
    } 
  
  return(c(REL,RES,UNC))
}  

#rank hisotgram

rhist=function(meas,forec, plotfile){ 
  #meas="E:/analog_ensemble/measurements.nc"
  #forec="E:/analog_ensemble/forecast.nc"

  swap <- nc_open(forec)
  forecasts = ncvar_get(swap)
  nc_close(swap)
  swap1 <- nc_open(meas)
  measurements = ncvar_get(swap1)
  nc_close(swap1)
  #end reading netcdf files
  #finding matrix dimension
  n_station=dim(forecasts)[1]
  n_days=dim(forecasts)[2]
  n_ltimes=dim(forecasts)[3]
  n_members=dim(forecasts)[4]
  length_dataset=(n_station)*(n_days*n_ltimes)
  epsanalog = array(dim=c(length_dataset,n_members))
  test = array(dim=c(length_dataset,2))
  for(i in  seq(n_station)){
    for(j in  seq(n_days)){
      for(k in  seq(n_ltimes)){
        for(z in  seq(n_members)){
          indice=(i-1)*(n_days*n_ltimes) +(j-1)*n_ltimes+k
          epsanalog[indice,z]=forecasts[i,j,k,z]  #on the columns put the members           
        } 
      }       
    }
  }
  for(i in  seq(n_station)){
    for(j in  seq(n_days)){
      for(k in  seq(n_ltimes)){
        indice=(i-1)*(n_days*n_ltimes) +(j-1)*n_ltimes+k
        test[indice,1]=k #leadtimes on the first column
        test[indice,2]=measurements[i,j,k] #measurements on the second  column          
      }       
    }
  }
  potens=cbind(epsanalog,test)
  potens=na.omit(potens)
  test=potens[,(n_members+1):(n_members+2)]
  potens=potens[,1:n_members]
  
  nrighe=dim(potens)[1]
  
  ordinata=matrix(nrow=nrighe,ncol=n_members)
  ncolonne=n_members
  for(i in seq(nrighe)){
    ordinata[i,]=sort(potens[i,])
  }
  
  frequenza=vector(length=n_members+1,mode="numeric")
  
  tot=0
  for(i in seq(nrighe)){
    if(test[i,2]==0){
      n0=length(which(ordinata[i,]==0))
      if(n0==0){
        frequenza[1]=frequenza[1]+1
      }else{
        for(iii in seq(n0)){
          frequenza[iii]=frequenza[iii]+1/n0
        }
      }
      tot=tot+1
    }else{
      for(p in seq(n_members)){
        if(test[i,2]<=ordinata[i,p]){
          frequenza[p]=frequenza[p]+1
          tot=tot+1
          break
        }
        if(p==ncolonne){
          frequenza[p+1]=frequenza[p+1]+1
          tot=tot+1
        }
      }
    }
  }
  
  MRE=100*((frequenza[1]+frequenza[ncolonne+1])/tot-2/(ncolonne+1))
  frequenza=frequenza/tot
  
  ll=qbinom(0.05,nrighe,1/(ncolonne+1),lower.tail=TRUE,log.p=FALSE)/nrighe
  uu=qbinom(0.95,nrighe,1/(ncolonne+1),lower.tail=TRUE,log.p=FALSE)/nrighe

  if(plotfile!="") {postscript(plotfile)} 
  barplot(frequenza,ylim=c(0,max(frequenza)*1.7),space=0,cex.axis=1.8,las=2,main="Rank Histogram",cex.main=1.8)
  for(i in seq(0.5,ncolonne+0.5,1)){
    segments(i,ll,i,uu,lwd=2)
  }
  abline(1/(ncolonne+1),0,lwd=2)
  writemre=paste("MRE=",sprintf("%.2f",MRE),"%",sep="")
  text(ncolonne/2,max(frequenza)*1.5,labels=writemre,cex=1.8)
  
  if(plotfile!="") {
    dev.off()  
  } 
  
  
  return(MRE) 
}  
###

###Function for CRPS

crps.fun=function(d,i){
  mean(d$crps[i])
}
#
crps=function(meas,forec,bstrap,adding,plotfile,close,type,colour){ 
  #meas="E:/analog_ensemble/measurements.nc"
  #forec="E:/analog_ensemble/forecast.nc"
  library(ncdf4) 
  library(boot)  
  swap <- nc_open(forec)
  forecasts = ncvar_get(swap)
  nc_close(swap)
  swap1 <- nc_open(meas)
  measurements = ncvar_get(swap1)
  nc_close(swap1)
  #end reading netcdf files
  #finding matrix dimension
  n_station=dim(forecasts)[1]
  n_days=dim(forecasts)[2]
  n_ltimes=dim(forecasts)[3]
  n_members=dim(forecasts)[4]
  length_dataset=(n_station)*(n_days*n_ltimes)
  epsanalog = array(dim=c(length_dataset,n_members))
  test = array(dim=c(length_dataset,2))
  for(i in  seq(n_station)){
    for(j in  seq(n_days)){
      for(k in  seq(n_ltimes)){
        for(z in  seq(n_members)){
          indice=(i-1)*(n_days*n_ltimes) +(j-1)*n_ltimes+k
          epsanalog[indice,z]=forecasts[i,j,k,z]  #on the columns put the members           
        } 
      }       
    }
  }
  for(i in  seq(n_station)){
    for(j in  seq(n_days)){
      for(k in  seq(n_ltimes)){
        indice=(i-1)*(n_days*n_ltimes) +(j-1)*n_ltimes+k
        test[indice,1]=k #leadtimes on the first column
        test[indice,2]=measurements[i,j,k] #measurements on the second  column          
      }       
    }
  }
  potens=cbind(epsanalog,test)
  potens=na.omit(potens)
  test=potens[,(n_members+1):(n_members+2)]
  potens=potens[,1:n_members]
  
  
  
  misura=test[,2]
  soglia=max(misura)/150 #defining the step for computing the integral of CRPS
  
  maxsoglia=max(misura)+soglia
  
  dimtest=dim(potens)[1]
  
  casi=matrix(nrow=dimtest,ncol=151)
  
  
  for(i in seq(dimtest)){
    e=1
    for(a in seq(soglia,maxsoglia,soglia)){
      casi[i,e]=length(which(potens[i,]<=a))
      e=e+1
    }
  }
  
  casi=casi/n_members
  skill=vector(mode="numeric",length=dimtest)
  
  for(i in seq(dimtest)){
    skill1=0
    skill2=0
    
    if(floor(misura[i]/soglia)==0){
      indice=1
    }else{
      indice=floor(misura[i]/soglia)
    }
    
    for(a in seq(1,indice,1)){
      skill1=skill1+(casi[i,a])^2*soglia
    }
    for(a in seq(indice+1,maxsoglia/soglia,1)){
      skill2=skill2+(casi[i,a]-1)^2*soglia
    }
    skill[i]=skill1+skill2
  }
  
  #----------------------------------------------------------------------bootstrap
  
  
  boot_t0=vector(mode="numeric",length=n_ltimes)
  bootstraplow=vector(mode="numeric",length=n_ltimes)
  bootstraphigh=vector(mode="numeric",length=n_ltimes)
  
  a=1
  for(i in seq(n_ltimes)){
    crpstemp=skill[which(test[,1]==i)]
    oggetto=as.data.frame(crpstemp)
    names(oggetto)="crps"
    crps.boot=boot(oggetto,crps.fun,R=1000)
    bootresult=boot.ci(crps.boot,type="basic")
    boot_t0[a]=bootresult$t0                   #mean value
    bootstraplow[a]=bootresult$basic[4]
    bootstraphigh[a]=bootresult$basic[5]
    a=a+1
  }
  
  boot_t0=boot_t0
  bootstraplow=bootstraplow
  bootstraphigh=bootstraphigh
  
  leadlabel=c(seq(n_ltimes))
  
  if(adding==F){ 
    if(plotfile!="") {postscript(plotfile)} 
   par(mar=c(4.5,4.5,1.5,1.5))
   plot(boot_t0,type="l",axes=F,ylim=c(0,max(boot_t0)*1.3),xlim=c(1,n_ltimes),lwd=2,xlab="Lead Time (Hour)",ylab=expression("CRPS (ms" ^"-1"~")"),cex.lab=1.,pch=16)
   axis(1,at=seq(n_ltimes),labels=leadlabel,cex.axis=1.1)
   axis(2,cex.axis=1.1)
   if(bstrap==T){
    for(i in seq(n_ltimes)){
     arrows(i,bootstraplow[i],i,bootstraphigh[i],length=0.04,lend=3,angle=90,code=3,lwd=2)   #consistency bars
    }
   }
  }
  
  if(adding==T){ 
      #####plotting phase  with boot strap confidence intervals  
      lines(boot_t0,lty=type,col=colour,lwd=2)
      if(bstrap==T){ 
       for(i in seq(n_ltimes)){
        arrows(i,bootstraplow[i],i,bootstraphigh[i],length=0.05,angle=90,code=3,col=colour,lwd=2)   #consistency bars          
       }  
      }
  }
  
  ##close the file only if is the last model to plot 
  if(close==T){ 
    if(plotfile!=""){ dev.off() } 
  } 
  
  crps=mean(skill)
  
  return(crps) 
}  

##function for ROCSS
#
rocss=function(meas,forec,threshold,bstrap,adding,plotfile,close,type,colour){ 

  swap <- nc_open(forec)
  forecasts = ncvar_get(swap)
  nc_close(swap)
  swap1 <- nc_open(meas)
  measurements = ncvar_get(swap1)
  nc_close(swap1)
  #end reading netcdf files
  #finding matrix dimension
  n_station=dim(forecasts)[1]
  n_days=dim(forecasts)[2]
  n_ltimes=dim(forecasts)[3]
  n_members=dim(forecasts)[4]
  length_dataset=(n_station)*(n_days*n_ltimes)
  epsanalog = array(dim=c(length_dataset,n_members))
  test = array(dim=c(length_dataset,2))
  for(i in  seq(n_station)){
    for(j in  seq(n_days)){
      for(k in  seq(n_ltimes)){
        for(z in  seq(n_members)){
          indice=(i-1)*(n_days*n_ltimes) +(j-1)*n_ltimes+k
          epsanalog[indice,z]=forecasts[i,j,k,z]  #on the columns put the members           
        } 
      }       
    }
  }
  for(i in  seq(n_station)){
    for(j in  seq(n_days)){
      for(k in  seq(n_ltimes)){
        indice=(i-1)*(n_days*n_ltimes) +(j-1)*n_ltimes+k
        test[indice,1]=k #leadtimes on the first column
        test[indice,2]=measurements[i,j,k] #measurements on the second  column          
      }       
    }
  }
  potens=cbind(epsanalog,test)
  potens=na.omit(potens)
  test=potens[,(n_members+1):(n_members+2)]
  potens=potens[,1:n_members]
  
  roc.fun=function(d,i){                              
    pk=seq(0.0,1.0,0.1)  #vettore probabilit? da 0 a 1
    nk=vector(mode="numeric",length=length(pk))         #n. di casi in ogni categoria probabilistica da 0 a 1
    nobsk=vector(mode="numeric",length=length(pk))      #n. casi in cui evento viene osservato per ogni categoria prob.
    ok=vector(mode="numeric",length=length(pk))         #frequenza relativa dell'osservazione, n. eventi osservati/n. previsioni per ogni categoria
    nonobsk=vector(mode="numeric",length=length(pk))    #non osservati
    sequenza=c(seq(0,0.05,0.05),seq(0.15,0.95,0.1),1.0) #vettore classi probabilistiche
    
    for(iii in seq(pk)){
      misura=d$misura[i]
      nk[iii]=length(which(d$EPSfc[i]>=sequenza[iii] & d$EPSfc[i]<sequenza[iii+1]))
      temp=(misura[which(d$EPSfc[i]>=sequenza[iii] & d$EPSfc[i]<sequenza[iii+1])])
      if(iii==11){
        nk[iii]=length(which(d$EPSfc[i]>=sequenza[iii] & d$EPSfc[i]<=sequenza[iii+1]))
        temp=(misura[which(d$EPSfc[i]>=sequenza[iii] & d$EPSfc[i]<=sequenza[iii+1])])
      }
      nobsk[iii]=length(which(temp>threshold))
      nonobsk[iii]=length(which(temp<=threshold))
    }
    
    totnk=sum(nk)
    totnobsk=sum(nobsk)
    totnonobsk=sum(nonobsk)
    ok=nobsk/nk
    ok[which(is.na(ok))]=0
    climatology=totnobsk/totnk     #-------tot. eventi osservati/tot. n. previsioni
    #numero di previsioni per ogni fascia probabilistica
    nforec=c(totnk,sum(nk[-1]),sum(nk[c(-1,-2)]),sum(nk[seq(-1,-3)]),sum(nk[seq(-1,-4)]),sum(nk[seq(-1,-5)]),
             sum(nk[seq(-1,-6)]),sum(nk[seq(-1,-7)]),sum(nk[seq(-1,-8)]),sum(nk[seq(-1,-9)]),sum(nk[seq(-1,-10)]),
             sum(nk[seq(-1,-11)]))
    #numero eventi osservati
    nobs_y=c(totnobsk,sum(nobsk[-1]),sum(nobsk[c(-1,-2)]),sum(nobsk[seq(-1,-3)]),sum(nobsk[seq(-1,-4)]),
             sum(nobsk[seq(-1,-5)]),sum(nobsk[seq(-1,-6)]),sum(nobsk[seq(-1,-7)]),sum(nobsk[seq(-1,-8)]),
             sum(nobsk[seq(-1,-9)]),sum(nobsk[seq(-1,-10)]),sum(nobsk[seq(-1,-11)]))
    #numero eventi NON osservati
    nobs_n=nforec-nobs_y
    #Hit Rate e False Alarm Rate
    HR=nobs_y/totnobsk
    FR=nobs_n/totnonobsk
    areac=vector(mode="numeric",length=length(pk))
    for(iv in seq(11)){
      areac[iv]=((HR[iv]+HR[iv+1])*(FR[iv]-FR[iv+1]))/2     #area under the curve
    }
    2*sum(areac)-1
  }
  
  
  boot_t0=vector(mode="numeric",length=n_ltimes)
  bootstraplow=vector(mode="numeric",length=n_ltimes)
  bootstraphigh=vector(mode="numeric",length=n_ltimes)
  
  a=1
  for(i in seq(n_ltimes)){
    misura=test[which(test[,1]==i),2]
    dataset=potens[which(test[,1]==i),]
    
    dimset=dim(dataset)[1]
    EPSfc=vector(mode="numeric",length=dimset)     #probability for EPS to be > threshold
    
    for(ii in seq(dimset)){
      EPSfc[ii]=length(which(dataset[ii,]>threshold))/n_members
    }
    
    oggetto=data.frame(EPSfc,misura)
    names(oggetto)=c("EPSfc","misura")
    roc.boot=boot(oggetto,roc.fun,R=1000)
    bootresult=boot.ci(roc.boot,type="basic")
    boot_t0[a]=bootresult$t0
    bootstraplow[a]=bootresult$basic[4]
    bootstraphigh[a]=bootresult$basic[5]  
    a=a+1
  }  
  
  bootstraphigh[which(bootstraphigh>1)]=1
  
  
  if(adding==F){ 
   if(plotfile!="") {postscript(plotfile)} 
 
   plot(boot_t0,type="l",ylim=c(0,1),axes=F,xlab="Lead Time (Hour)",ylab="ROCSS",cex.lab=1.5,lwd=2)
   axis(1,at=seq(0,n_ltimes-1),cex.axis=1.2)
   axis(2,at=seq(0,1,0.1),cex.axis=1.2)
    if(bstrap==T){
     for(i in seq(n_ltimes)){
      arrows(i,bootstraplow[i],i,bootstraphigh[i],length=0.04,lend=3,angle=n_ltimes,code=3,lwd=1,pch=16)   #consistency bars
     }
   }
  }
  if(adding==T){ 
    #####plotting phase  with boot strap confidence intervals  
    lines(boot_t0,lty=type,col=colour,lwd=2)
    if(bstrap==T){ 
      for(i in seq(n_ltimes)){
        arrows(i,bootstraplow[i],i,bootstraphigh[i],length=0.05,angle=90,code=3,col=colour,lwd=2)   #consistency bars          
      }  
    }
  }
  ##close the file only if is the last model to plot 
  if(close==T){ 
    if(plotfile!=""){ dev.off() } 
  }  
  
  return(boot_t0)
}  
###########
#function for BSS
#
bss=function(meas,forec,threshold,bstrap,adding,plotfile,close,type,colour){ 
  #meas="F:/analog_ensemble/measurements.nc"
  
  swap <- nc_open(forec)
  forecasts = ncvar_get(swap)
  nc_close(swap)
  swap1 <- nc_open(meas)
  measurements = ncvar_get(swap1)
  nc_close(swap1)
  #end reading netcdf files
  #finding matrix dimension
  n_station=dim(forecasts)[1]
  n_days=dim(forecasts)[2]
  n_ltimes=dim(forecasts)[3]
  n_members=dim(forecasts)[4]
  length_dataset=(n_station)*(n_days*n_ltimes)
  epsanalog = array(dim=c(length_dataset,n_members))
  test = array(dim=c(length_dataset,2))
  for(i in  seq(n_station)){
    for(j in  seq(n_days)){
      for(k in  seq(n_ltimes)){
        for(z in  seq(n_members)){
          indice=(i-1)*(n_days*n_ltimes) +(j-1)*n_ltimes+k
          epsanalog[indice,z]=forecasts[i,j,k,z]  #on the columns put the members           
        } 
      }       
    }
  }
  for(i in  seq(n_station)){
    for(j in  seq(n_days)){
      for(k in  seq(n_ltimes)){
        indice=(i-1)*(n_days*n_ltimes) +(j-1)*n_ltimes+k
        test[indice,1]=k #leadtimes on the first column
        test[indice,2]=measurements[i,j,k] #measurements on the second  column          
      }       
    }
  }
  potens=cbind(epsanalog,test)
  potens=na.omit(potens)
  test=potens[,(n_members+1):(n_members+2)]
  potens=potens[,1:n_members]
  
  
  ROCSS=vector(mode="numeric",length=n_ltimes)
  BS=vector(mode="numeric",length=n_ltimes)
  BSS=vector(mode="numeric",length=n_ltimes)
  REL=vector(mode="numeric",length=n_ltimes)
  RES=vector(mode="numeric",length=n_ltimes)
  UNC=vector(mode="numeric",length=n_ltimes)
  
  #bootstrap bss
  bss.fun=function(d,i){
    1-(mean(d$bsparz[i])/BS_REF)
  }
  boot_t0=vector(mode="numeric",length=n_ltimes)
  bootstraplow=vector(mode="numeric",length=n_ltimes)
  bootstraphigh=vector(mode="numeric",length=n_ltimes)
  
  a=1
  for(i in seq(n_ltimes)){
    
    misura=test[which(test[,1]==i),2]
    dataset=potens[which(test[,1]==i),]
    
    dimset=dim(dataset)[1]
    EPSfc=vector(mode="numeric",length=dimset)     #probabilit? per ogni scadenza con cui eps supera threshold (es. quanti membri danno ws > x)
    MISfc=vector(mode="numeric",length=dimset)     #probabilit? con cui misura supera threshold (0 o 1)
    
    for(ii in seq(dimset)){
      EPSfc[ii]=length(which(dataset[ii,]>threshold))/n_members
      if(misura[ii]>threshold){
        MISfc[ii]=1
      }else{
        MISfc[ii]=0
      }
    }
    
    pk=seq(0.0,1.0,0.1)  #vettore probabilit? da 0 a 1
    nk=vector(mode="numeric",length=length(pk))         #n. di casi in ogni categoria probabilistica da 0 a 1
    nobsk=vector(mode="numeric",length=length(pk))      #n. casi in cui evento viene osservato per ogni categoria prob.
    ok=vector(mode="numeric",length=length(pk))         #frequenza relativa dell'osservazione, n. eventi osservati/n. previsioni per ogni categoria
    nonobsk=vector(mode="numeric",length=length(pk))    #non osservati
    sequenza=c(seq(0,0.05,0.05),seq(0.15,0.95,0.1),1.0) #vettore classi probabilistiche
    
    for(iii in seq(pk)){
      nk[iii]=length(which(EPSfc>=sequenza[iii] & EPSfc<sequenza[iii+1]))
      temp=(misura[which(EPSfc>=sequenza[iii] & EPSfc<sequenza[iii+1])])
      if(iii==11){
        nk[iii]=length(which(EPSfc>=sequenza[iii] & EPSfc<=sequenza[iii+1]))
        temp=(misura[which(EPSfc>=sequenza[iii] & EPSfc<=sequenza[iii+1])])
      }
      nobsk[iii]=length(which(temp>threshold))
      nonobsk[iii]=length(which(temp<=threshold))
    }
    
    totnk=sum(nk)
    totnobsk=sum(nobsk)
    totnonobsk=sum(nonobsk)
    ok=nobsk/nk
    ok[which(is.na(ok))]=0
    climatology=totnobsk/totnk     #-------tot. eventi osservati/tot. n. previsioni
    #numero di previsioni per ogni fascia probabilistica
    nforec=c(totnk,sum(nk[-1]),sum(nk[c(-1,-2)]),sum(nk[seq(-1,-3)]),sum(nk[seq(-1,-4)]),sum(nk[seq(-1,-5)]),
             sum(nk[seq(-1,-6)]),sum(nk[seq(-1,-7)]),sum(nk[seq(-1,-8)]),sum(nk[seq(-1,-9)]),sum(nk[seq(-1,-10)]),
             sum(nk[seq(-1,-11)]))
    #numero eventi osservati
    nobs_y=c(totnobsk,sum(nobsk[-1]),sum(nobsk[c(-1,-2)]),sum(nobsk[seq(-1,-3)]),sum(nobsk[seq(-1,-4)]),
             sum(nobsk[seq(-1,-5)]),sum(nobsk[seq(-1,-6)]),sum(nobsk[seq(-1,-7)]),sum(nobsk[seq(-1,-8)]),
             sum(nobsk[seq(-1,-9)]),sum(nobsk[seq(-1,-10)]),sum(nobsk[seq(-1,-11)]))
    #numero eventi NON osservati
    nobs_n=nforec-nobs_y
    #Hit Rate e False Alarm Rate
    HR=nobs_y/totnobsk
    FR=nobs_n/totnonobsk
    areac=vector(mode="numeric",length=length(pk))
    for(iv in seq(11)){
      areac[iv]=((HR[iv]+HR[iv+1])*(FR[iv]-FR[iv+1]))/2     #area sotto la curva
    }
    areacurva=sum(areac)
    ROCSS[a]=2*areacurva-1
    bsparziale=(EPSfc-MISfc)^2   #(pi-oi)^2 per ogni scadenza
    BS[a]=mean(bsparziale)
    omean=mean(MISfc)       #questo valore ? uguale alla sample climatology =tot(nobsk)/tot(nk), nelle componenti del BS sarebbe c
    bsref=(MISfc-omean)^2
    BS_REF=mean(bsref)
    
    #X FARE BOOTSTRAP BSS
    oggetto=as.data.frame(bsparziale)
    names(oggetto)="bsparz"
    bss.boot=boot(oggetto,bss.fun,R=1000)
    bootresult=boot.ci(bss.boot,type="basic")
    boot_t0[a]=bootresult$t0
    bootstraplow[a]=bootresult$basic[4]
    bootstraphigh[a]=bootresult$basic[5]
    
    BSS[a]=1-(BS[a]/BS_REF)       #BRIER SKILL SCORE
    fobs=nobsk/nk
    fobs[which(fobs=="NaN")]=0            #in caso che il primo valore di n_obsk e n_k sia 0
    REL[a]=sum(nk*(pk-fobs)^2)/totnk      #reliability
    RES[a]=sum(nk*(fobs-omean)^2)/totnk   #resolution
    UNC[a]=omean*(1-omean)                #uncertainty
    a=a+1
  }
#####plotting phase  with boot strap confidence intervals 
    
  if(adding==F){ 
    if(plotfile!="") {postscript(plotfile)} 
    
   plot(BSS,type="l",ylim=c(0,1),axes=F,xlab="Lead Time",ylab="BSS",cex.lab=1.5,lwd=2)
   axis(1,at=seq(0,n_ltimes-1),cex.axis=1.2)
   axis(2,at=seq(0,1,0.1),cex.axis=1.2)
   if(bstrap==T){ 
    for(i in seq(n_ltimes)){
      arrows(i,bootstraplow[i],i,bootstraphigh[i],length=0.04,lend=3,angle=n_ltimes,code=3,lwd=1,pch=16)   #consistency bars
    }
   }
   lines(UNC,lty="dashed",lwd=2)
  } 
  if(adding==T){ 

    lines(boot_t0,lty=type,col=colour,lwd=2)
    if(bstrap==T){ 
      for(i in seq(n_ltimes)){
        arrows(i,bootstraplow[i],i,bootstraphigh[i],length=0.05,angle=90,code=3,col=colour,lwd=2)   #consistency bars          
      }  
    }
    lines(UNC,col=colour,lty="dashed",lwd=2)
  }
  ##close the file only if is the last model to plot 
  if(close==T){ 
    if(plotfile!=""){ dev.off() } 
    }  
  
  return(c(BS,REL,RES,UNC))
}  

#function for correlation

#
correlation=function(meas,forec,bstrap,adding,plotfile,close,type,colour){ 
  #  meas="E:/analog_ensemble/measurements.nc"
  #  forec="E:/analog_ensemble/forecast.nc"
  library(ncdf4) 
  library(boot)  
  swap <- nc_open(forec)
  forecasts = ncvar_get(swap)
  nc_close(swap)
  swap1 <- nc_open(meas)
  measurements = ncvar_get(swap1)
  nc_close(swap1)
  #end reading netcdf files
  #finding matrix dimension
  n_station=dim(forecasts)[1]
  n_days=dim(forecasts)[2]
  n_ltimes=dim(forecasts)[3]
  n_members=dim(forecasts)[4]
  length_dataset=(n_station)*(n_days*n_ltimes)
  epsanalog = array(dim=c(length_dataset,n_members))
  test = array(dim=c(length_dataset,2))
  for(i in  seq(n_station)){
    for(j in  seq(n_days)){
      for(k in  seq(n_ltimes)){
        for(z in  seq(n_members)){
          indice=(i-1)*(n_days*n_ltimes) +(j-1)*n_ltimes+k
          epsanalog[indice,z]=forecasts[i,j,k,z]  #on the columns put the members           
        } 
      }       
    }
  }
  for(i in  seq(n_station)){
    for(j in  seq(n_days)){
      for(k in  seq(n_ltimes)){
        indice=(i-1)*(n_days*n_ltimes) +(j-1)*n_ltimes+k
        test[indice,1]=k #leadtimes on the first column
        test[indice,2]=measurements[i,j,k] #measurements on the second  column          
      }       
    }
  }
  
  
  potens=cbind(epsanalog,test)
  potens=na.omit(potens)
  test=potens[,(n_members+1):(n_members+2)]
  potens=potens[,1:n_members]
  
  media_ens=apply(potens,1,mean)
  meas=test[,2]
  boot_t0=vector(mode="numeric",length=n_ltimes)
  bootstraplow=vector(mode="numeric",length=n_ltimes)     
  bootstraphigh=vector(mode="numeric",length=n_ltimes)
  
  corr.fun=function(d,i){ 
    cor(d$meas1[i],d$media_ens1[i],use="pairwise.complete.obs")
  }
  
  
  for(i in seq(n_ltimes)){    
 
    
    meas1=meas[which(test[,1]==i)]
    media_ens1=media_ens[which(test[,1]==i)]
    
    oggetto=data.frame(meas1,media_ens1)

    names(oggetto)=c("meas1","media_ens1")
    err.boot=boot(oggetto,corr.fun,R=1000)
    bootresult=boot.ci(err.boot,type="basic")  
    boot_t0[i]=bootresult$t0            
    bootstraplow[i]=bootresult$basic[4]        
    bootstraphigh[i]=bootresult$basic[5]
    boot_t0[i]=  (boot_t0[i])       
    bootstraplow[i]=(bootstraplow[i])  
    bootstraphigh[i]=( bootstraphigh[i])
  }
  
  if(adding==F){ 
    if(plotfile!="") {postscript(plotfile)} 
    
    leadlabel=seq(n_ltimes)   
    
    plot(boot_t0,type="l",axes=F,ylim=c(0,1),xlim=c(1,n_ltimes),lwd=2,xlab="Lead Time (Hours)",ylab="Correlation",cex.lab=1.1,pch=16)
    axis(1,at=seq(n_ltimes),labels=leadlabel,cex.axis=1.1)
    axis(2,cex.axis=1.1)
   if(bstrap==T){
    for(i in seq(n_ltimes)){
     arrows(i,bootstraplow[i],i,bootstraphigh[i],length=0.05,angle=90,code=3)   #consistency bars          
    }  
   }
  }
if(adding==T){   
  lines(boot_t0,lty=type,col=colour,lwd=2)
  if(bstrap==T){ 
    for(i in seq(n_ltimes)){
      arrows(i,bootstraplow[i],i,bootstraphigh[i],length=0.05,angle=90,code=3,col=colour,lwd=2)   #consistency bars          
    }  
  }
  
}
##close the file only if is the last model to plot 
if(close==T){ 
  if(plotfile!=""){ dev.off() } 
}  

return(mean(boot_t0))
}  

rmse_det =function(meas,forec,bstrap,adding,plotfile,close,type,colour){ 
  #similar to dispersion but deal with deterministi forecast, i.e. two equal memebers are supplied
  
  #  meas="E:/analog_ensemble/measurements.nc"
  #  forec="E:/analog_ensemble/forecast.nc"
  
  swap <- nc_open(forec)
  forecasts = ncvar_get(swap)
  nc_close(swap)
  swap1 <- nc_open(meas)
  measurements = ncvar_get(swap1)
  nc_close(swap1)
  #end reading netcdf files
  #finding matrix dimension
  n_station=dim(forecasts)[1]
  n_days=dim(forecasts)[2]
  n_ltimes=dim(forecasts)[3]
  n_members=dim(forecasts)[4]
  length_dataset=(n_station)*(n_days*n_ltimes)
  epsanalog = array(dim=c(length_dataset,n_members))
  test = array(dim=c(length_dataset,2))
  for(i in  seq(n_station)){
    for(j in  seq(n_days)){
      for(k in  seq(n_ltimes)){
        for(z in  seq(n_members)){
          indice=(i-1)*(n_days*n_ltimes) +(j-1)*n_ltimes+k
          epsanalog[indice,z]=forecasts[i,j,k,z]  #on the columns put the members           
        } 
      }       
    }
  }
  for(i in  seq(n_station)){
    for(j in  seq(n_days)){
      for(k in  seq(n_ltimes)){
        indice=(i-1)*(n_days*n_ltimes) +(j-1)*n_ltimes+k
        test[indice,1]=k #leadtimes on the first column
        test[indice,2]=measurements[i,j,k] #measurements on the second  column          
      }       
    }
  }
  potens=epsanalog
  spread=apply(potens,1,var)                                  #ensemble spread
  media_ens=apply(potens,1,mean)
#  media_ens=apply(potens,1,median)
  error_med_ens=(media_ens-test[,2])^2           
  mae_ens=abs(test[,2]-media_ens)  
  spreadpot=vector(mode="numeric",length=n_ltimes)
  rmsepotens=vector(mode="numeric",length=n_ltimes)
  maepotens=vector(mode="numeric",length=n_ltimes)
  rmse_tot=sqrt(mean(error_med_ens,na.rm=T))
  for(i in seq(n_ltimes)){
    spreadtemp=spread[which(test[,1]==i)]
    erroreens=(error_med_ens[which(test[,1]==i)])
    maeens=mae_ens[which(test[,1]==i)]
    spreadpot[i]=sqrt((mean(spreadtemp,na.rm=T)))
    rmsepotens[i]=sqrt(mean(erroreens,na.rm=T))
    #RMSE as suggested by 
    maepotens[i]=mean(mae_ens)
  }
  
  if(bstrap==T){ #bootstrap confidence intervals only if required
    boot_t0=vector(mode="numeric",length=n_ltimes)
    bootstraplow=vector(mode="numeric",length=n_ltimes)     
    bootstraphigh=vector(mode="numeric",length=n_ltimes)
    
    
    for(i in seq(n_ltimes)){
      errtemp=na.omit(error_med_ens[which(test[,1]==i)])
      oggetto=as.data.frame(errtemp)
      names(oggetto)="errore"
      err.boot=boot(oggetto,err.fun,R=1000)
      bootresult=boot.ci(err.boot,type="basic")  
      boot_t0[i]=bootresult$t0            #mean vealue
      bootstraplow[i]=bootresult$basic[4]        
      bootstraphigh[i]=bootresult$basic[5]
      boot_t0[i]=  sqrt(boot_t0[i])        #mean vale
      bootstraplow[i]=sqrt(bootstraplow[i])  
      bootstraphigh[i]=sqrt( bootstraphigh[i])
    }
  }
  
  if(adding==F){ 
    if(plotfile!="") {postscript(plotfile)}
    if(bstrap==T){
      leadlabel=seq(n_ltimes)   
      plot(boot_t0,type="l",axes=F,ylim=c(0,max(rmsepotens*1.3)),xlim=c(1,n_ltimes),lwd=2,xlab="Lead Time (Hour)",ylab=expression("RMSE (ms" ^"-1"~")"),cex.lab=1.,pch=12)
      axis(1,at=seq(n_ltimes),labels=leadlabel,cex.axis=1.1)
      axis(2,cex.axis=1.1)
      for(i in seq(n_ltimes)){
        arrows(i,bootstraplow[i],i,bootstraphigh[i],length=0.05,angle=90,code=3)   #consistency bars          
      }  
      #lines(spreadpot,lty="dashed",lwd=2,pch=16)
    }else{
      
      #
      leadlabel=seq(1,n_ltimes)           #-------------------label
      plot(rmsepotens,type="l",axes=F,ylim=c(0,max(rmsepotens*1.3)),xlim=c(1,n_ltimes),lwd=2,xlab="Lead Time (Hour)",ylab=expression("RMSE (ms" ^"-1"~")"),cex.lab=1.,pch=16)
      axis(1,at=seq(0,n_ltimes-1),labels=leadlabel,cex.axis=1.1)
      # axis(2,at=seq(max(rmsepotens*1.2)),cex.axis=1.3) 
      #axis(1)
      axis(2,cex.axis=1.1)
    #  lines(spreadpot,lty="dashed",lwd=2,pch=16) 
    }
  }
  if(adding==T){ 
    if(bstrap==T){ 
      lines(boot_t0,lty=type,col=colour,lwd=2)
      
      for(i in seq(n_ltimes)){
        arrows(i,bootstraplow[i],i,bootstraphigh[i],length=0.05,angle=90,code=3,col=colour,lwd=2)   #consistency bars          
      }  
     # lines(spreadpot,lty="dashed",lwd=2,col=colour,pch=16)
    }else{
      lines(rmsepotens,lty=type,col=colour,lwd=2)
    #  lines(spreadpot,lty="dashed",lwd=2,col=colour,pch=16) 
    }
  }   
  
  
  
  ##close the file only if there is just this model to plot 
  if(close==T){ 
    if(plotfile!=""){ dev.off() } 
  }
  return(rmse_tot)
}  

serie_prob =function(meas,forec,forec2,adding,plotfile,close,staz,day){ 
  #similar to dispersion but deal with deterministi forecast, i.e. two equal memebers are supplied
  
 # meas="/Volumes/d1/alessand/Excel/measurements.nc"
#    forec="/Volumes/d1/alessand/Excel/forecast_10_2pred.nc"
#  forec2="/Volumes/d1/alessand/Excel/forecast_di.nc"
  
  swap <- nc_open(forec)
  forecasts = ncvar_get(swap)
  nc_close(swap)
  swap <- nc_open(forec2)
  forecasts2 = ncvar_get(swap)
  nc_close(swap)
  swap1 <- nc_open(meas)
  measurements = ncvar_get(swap1)
  nc_close(swap1)
  #end reading netcdf files
  #finding matrix dimension
  n_station=dim(forecasts)[1]
  n_days=dim(forecasts)[2]
  n_ltimes=dim(forecasts)[3]
  n_members=dim(forecasts)[4]
  length_dataset=(n_station)*(n_days*n_ltimes)
  epsanalog = array(dim=c(length_dataset,n_members))
  
  test = array(dim=c(length_dataset,2))
  for(i in  seq(n_station)){
    for(j in  seq(n_days)){
      for(k in  seq(n_ltimes)){
        for(z in  seq(n_members)){
          indice=(i-1)*(n_days*n_ltimes) +(j-1)*n_ltimes+k
          epsanalog[indice,z]=forecasts[i,j,k,z]  #on the columns put the members           
        } 
      }       
    }
  }
  epsanalog2 = array(dim=c(length_dataset,2))
  for(i in  seq(n_station)){
    for(j in  seq(n_days)){
      for(k in  seq(n_ltimes)){
        for(z in  seq(2)){
          indice=(i-1)*(n_days*n_ltimes) +(j-1)*n_ltimes+k
          epsanalog2[indice,z]=forecasts2[i,j,k,z]  #on the columns put the members           
        } 
      }       
    }
  }
  
  
  
  
  for(i in  seq(n_station)){
    for(j in  seq(n_days)){
      for(k in  seq(n_ltimes)){
        indice=(i-1)*(n_days*n_ltimes) +(j-1)*n_ltimes+k
        test[indice,1]=k #leadtimes on the first column
        test[indice,2]=measurements[i,j,k] #measurements on the second  column          
      }       
    }
  }
  potens=epsanalog
  setpot=potens
  
  #---------------------------------------------------------------grafici quantile
  
  dimtest=dim(test)[1]
  
  qpot10=vector(mode="numeric",length=dimtest)
  qpot25=vector(mode="numeric",length=dimtest)
  qpot75=vector(mode="numeric",length=dimtest)
  qpot90=vector(mode="numeric",length=dimtest)
  a=(staz-1)*(n_days*n_ltimes) +(day-1)*n_ltimes+1
  b=a+n_ltimes-1
  
  for(i in seq(a,b)){
    qpot10[i]=quantile(setpot[i,],prob=0.05)
    qpot25[i]=quantile(setpot[i,],prob=0.25)
    qpot75[i]=quantile(setpot[i,],prob=0.75)
    qpot90[i]=quantile(setpot[i,],prob=0.95)
  }
  
  media=vector(mode="numeric",length=dimtest)
  
  for(i in seq(a,b)){
    media[i]=mean(setpot[i,])
  }
  
  
  

  
  
  #   a=10598
  #   b=10670
  leadlabel=seq(0,n_ltimes-1)           #-------------------etichetta asse x grafici
 
  
  plot(setpot[a:b,1],axes=F,type="n",ylim=c(0,25),xlab="Lead Time (Hour)",ylab=expression("Wind Speed (ms" ^"-1"~")"),main="",cex.main=1.2,cex.lab=1.2)
  #abline(h=seq(0,900,50),v=seq(1,72),col="lightgray",lwd=2,lty="dotted")   
  axis(1,at=seq(0,n_ltimes-1),labels=leadlabel,cex.axis=1.2)
  axis(2,at=seq(0,20,1),cex.axis=1.2)
  lines(qpot90[a:b],type="n")
  lines(qpot75[a:b],type="n")
  lines(qpot25[a:b],type="n")
  lines(qpot10[a:b],type="n")           
  
  polygon(c(1:(n_ltimes),(n_ltimes):1),c(qpot75[a:b],rev(qpot25[a:b])),col="#0000ff70",border=NA) 
  polygon(c(1:(n_ltimes),(n_ltimes):1),c(qpot90[a:b],rev(qpot75[a:b])),col="#0000ff40",border=NA)
  polygon(c(1:(n_ltimes),(n_ltimes):1),c(qpot25[a:b],rev(qpot10[a:b])),col="#0000ff40",border=NA)
  
  lines(media[a:b],col="yellow",lty="dashed",lwd=3)
  lines(test[a:b,2],col="black",lwd=3)
  lines(epsanalog2[a:b,1],col="red",lwd=3)
  
 
  return()
}  


rmse_diff =function(meas,forec,forec2,bstrap,adding,plotfile,close,type,colour){ 
  #similar to dispersion but deal with deterministi forecast, i.e. two equal memebers are supplied
  
  #  meas="E:/analog_ensemble/measurements.nc"
  #  forec="E:/analog_ensemble/forecast.nc"
  
  swap <- nc_open(forec)
  forecasts = ncvar_get(swap)
  nc_close(swap)
  swap <- nc_open(forec2)
  forecasts2 = ncvar_get(swap)
  nc_close(swap)
  
  swap1 <- nc_open(meas)
  measurements = ncvar_get(swap1)
  nc_close(swap1)
  #end reading netcdf files
  #finding matrix dimension
  n_station=dim(forecasts)[1]
  n_days=dim(forecasts)[2]
  n_ltimes=dim(forecasts)[3]
  n_members=dim(forecasts)[4]
  length_dataset=(n_station)*(n_days*n_ltimes)
  epsanalog = array(dim=c(length_dataset,n_members))
  test = array(dim=c(length_dataset,2))
  for(i in  seq(n_station)){
    for(j in  seq(n_days)){
      for(k in  seq(n_ltimes)){
        for(z in  seq(n_members)){
          indice=(i-1)*(n_days*n_ltimes) +(j-1)*n_ltimes+k
          epsanalog[indice,z]=forecasts[i,j,k,z]  #on the columns put the members           
        } 
      }       
    }
  }
  
  epsanalog2 = array(dim=c(length_dataset,2))
  for(i in  seq(n_station)){
    for(j in  seq(n_days)){
      for(k in  seq(n_ltimes)){
        for(z in  seq(2)){
          indice=(i-1)*(n_days*n_ltimes) +(j-1)*n_ltimes+k
          epsanalog2[indice,z]=forecasts2[i,j,k,z]  #on the columns put the members           
        } 
      }       
    }
  }
  
  for(i in  seq(n_station)){
    for(j in  seq(n_days)){
      for(k in  seq(n_ltimes)){
        indice=(i-1)*(n_days*n_ltimes) +(j-1)*n_ltimes+k
        test[indice,1]=k #leadtimes on the first column
        test[indice,2]=measurements[i,j,k] #measurements on the second  column          
      }       
    }
  }
  potens=epsanalog
  spread=apply(potens,1,var)                                  #ensemble spread
  media_ens=apply(potens,1,mean)
  error_med_ens=(media_ens-test[,2])^2  
  error_med_ens2=(epsanalog2[,2]-test[,2])^2  
  error_med_ens3=error_med_ens-error_med_ens2
  mae_ens=abs(test[,2]-media_ens) 
  
  spreadpot=vector(mode="numeric",length=n_ltimes)
  rmsepotens=vector(mode="numeric",length=n_ltimes)
  maepotens=vector(mode="numeric",length=n_ltimes)
  rmse_tot=sqrt(mean(error_med_ens,na.rm=T))
  for(i in seq(n_ltimes)){
    spreadtemp=spread[which(test[,1]==i)]
    erroreens=(error_med_ens3[which(test[,1]==i)])
#    maeens=mae_ens[which(test[,1]==i)]
#    spreadpot[i]=sqrt((mean(spreadtemp,na.rm=T)))
    rmsepotens[i]=(mean(erroreens,na.rm=T))
    #RMSE as suggested by 
    maepotens[i]=mean(mae_ens)
  }
  
  if(bstrap==T){ #bootstrap confidence intervals only if required
    boot_t0=vector(mode="numeric",length=n_ltimes)
    bootstraplow=vector(mode="numeric",length=n_ltimes)     
    bootstraphigh=vector(mode="numeric",length=n_ltimes)
    
    
    for(i in seq(n_ltimes)){
      errtemp=na.omit(error_med_ens3[which(test[,1]==i)])
      oggetto=as.data.frame(errtemp)
      names(oggetto)="errore"
      err.boot=boot(oggetto,err.fun,R=1000)
      bootresult=boot.ci(err.boot,type="basic")  
      boot_t0[i]=bootresult$t0            #mean vealue
      bootstraplow[i]=bootresult$basic[4]        
      bootstraphigh[i]=bootresult$basic[5]
      boot_t0[i]= (boot_t0[i])        #mean vale
      bootstraplow[i]=(bootstraplow[i])  
      bootstraphigh[i]=( bootstraphigh[i])
    }
  }
  
  if(adding==F){ 
    if(plotfile!="") {postscript(plotfile)}
    if(bstrap==T){
      leadlabel=seq(n_ltimes)   
      plot(boot_t0,type="l",axes=F,ylim=c(-3,3),xlim=c(1,n_ltimes),lwd=2,xlab="Lead Time",ylab="RMSE ",cex.lab=1.6,pch=16)
      axis(1,at=seq(n_ltimes),labels=leadlabel,cex.axis=1.3)
      axis(2,cex.axis=1.3)
      for(i in seq(n_ltimes)){
        arrows(i,bootstraplow[i],i,bootstraphigh[i],length=0.05,angle=90,code=3)   #consistency bars          
      }  
      #lines(spreadpot,lty="dashed",lwd=2,pch=16)
    }else{
      
      #
      leadlabel=seq(1,n_ltimes)           #-------------------label
      plot(rmsepotens,type="l",axes=F,ylim=c(0,max(rmsepotens*1.3)),xlim=c(1,n_ltimes),lwd=2,xlab="Lead Time",ylab="RMSE",cex.lab=1.6,pch=16)
      axis(1,at=seq(0,n_ltimes-1),labels=leadlabel,cex.axis=1.3)
      # axis(2,at=seq(max(rmsepotens*1.2)),cex.axis=1.3) 
      #axis(1)
      axis(2,cex.axis=1.3)
      #  lines(spreadpot,lty="dashed",lwd=2,pch=16) 
    }
  }
  if(adding==T){ 
    if(bstrap==T){ 
      lines(boot_t0,lty=type,col=colour,lwd=2)
      
      for(i in seq(n_ltimes)){
        arrows(i,bootstraplow[i],i,bootstraphigh[i],length=0.05,angle=90,code=3,col=colour,lwd=2)   #consistency bars          
      }  
      # lines(spreadpot,lty="dashed",lwd=2,col=colour,pch=16)
    }else{
      lines(rmsepotens,lty=type,col=colour,lwd=2)
      #  lines(spreadpot,lty="dashed",lwd=2,col=colour,pch=16) 
    }
  }   
  
  
  
  ##close the file only if there is just this model to plot 
  if(close==T){ 
    if(plotfile!=""){ dev.off() } 
  }
  return(rmse_tot)
}  
bss_comp=function(meas,forec,forec2,threshold,bstrap,adding,plotfile,close,type,colour){ 
 
# meas="/Volumes/d1/alessand/Excel//measurements_3week.nc"
#     forec="/Volumes/d1/alessand/Excel//forecast_10_4pred_3week_each.nc"
# forec2="/Volumes/d1/alessand/Excel//forecast_di_3week.nc"
  swap <- nc_open(forec)
  forecasts = ncvar_get(swap)
  nc_close(swap)
  swap <- nc_open(forec2)
  forecasts2 = ncvar_get(swap)
  nc_close(swap)
  
  
  swap1 <- nc_open(meas)
  measurements = ncvar_get(swap1)
  nc_close(swap1)
  #end reading netcdf files
  #finding matrix dimension
  n_station=dim(forecasts)[1]
  n_days=dim(forecasts)[2]
  n_ltimes=dim(forecasts)[3]
  n_members=dim(forecasts)[4]
  length_dataset=(n_station)*(n_days*n_ltimes)
  epsanalog = array(dim=c(length_dataset,n_members))
  test = array(dim=c(length_dataset,2))
  for(i in  seq(n_station)){
    for(j in  seq(n_days)){
      for(k in  seq(n_ltimes)){
        for(z in  seq(n_members)){
          indice=(i-1)*(n_days*n_ltimes) +(j-1)*n_ltimes+k
          epsanalog[indice,z]=forecasts[i,j,k,z]  #on the columns put the members     ANEN      
        } 
      }       
    }
  }
  for(i in  seq(n_station)){
    for(j in  seq(n_days)){
      for(k in  seq(n_ltimes)){
        indice=(i-1)*(n_days*n_ltimes) +(j-1)*n_ltimes+k
        test[indice,1]=k #leadtimes on the first column
        test[indice,2]=measurements[i,j,k] #measurements on the second  column          
      }       
    }
  }
  epsanalog2 = array(dim=c(length_dataset,2))
  for(i in  seq(n_station)){
    for(j in  seq(n_days)){
      for(k in  seq(n_ltimes)){
        for(z in  seq(2)){
          indice=(i-1)*(n_days*n_ltimes) +(j-1)*n_ltimes+k
          epsanalog2[indice,z]=forecasts2[i,j,k,z]  #on the columns put the members      Dicast     
        } 
      }       
    }
  }
  
  
  potens=cbind(epsanalog,test,epsanalog2[,2])
  potens=na.omit(potens)
  test=potens[,(n_members+1):(n_members+2)]
  det=potens[,(n_members+3)]
  potens=potens[,1:n_members]

  mean_eps=apply(potens,1,mean)
  
  ROCSS=vector(mode="numeric",length=n_ltimes)
  BS=vector(mode="numeric",length=n_ltimes)
  BSS=vector(mode="numeric",length=n_ltimes)
  REL=vector(mode="numeric",length=n_ltimes)
  RES=vector(mode="numeric",length=n_ltimes)
  UNC=vector(mode="numeric",length=n_ltimes)
  
  #bootstrap bss
  bss.fun=function(d,i){
    1-(mean(d$bsparz[i])/BS_REF)
  }
  boot_t0=vector(mode="numeric",length=n_ltimes)
  bootstraplow=vector(mode="numeric",length=n_ltimes)
  bootstraphigh=vector(mode="numeric",length=n_ltimes)
  
  a=1
  for(i in seq(n_ltimes)){
    
    misura=test[which(test[,1]==i),2]
    dataset=potens[which(test[,1]==i),]
    dataset2=det[which(test[,1]==i)]
    dimset=dim(dataset)[1]
    EPSfc=vector(mode="numeric",length=dimset)     #probabilit? per ogni scadenza con cui eps supera threshold (es. quanti membri danno ws > x)
    MISfc=vector(mode="numeric",length=dimset)     #probabilit? con cui misura supera threshold (0 o 1)
    MISfc2=vector(mode="numeric",length=dimset)    #Probabilita' con cui modello deterministico supera soglia
    MISfc3=vector(mode="numeric",length=dimset)    #Probabilita' con cui ensemble mean supera soglia
    for(ii in seq(dimset)){
      EPSfc[ii]=length(which(dataset[ii,]>threshold))/n_members
      if(misura[ii]>threshold){
        MISfc[ii]=1
      }else{
        MISfc[ii]=0
      }
      if(dataset2[ii]>threshold){
        MISfc2[ii]=1
      }else{
        MISfc2[ii]=0
      }
      if(mean(dataset[ii,])>threshold){
        MISfc3[ii]=1
      }else{
        MISfc3[ii]=0
      }
      
    }
    
    pk=seq(0.0,1.0,0.1)  #vettore probabilit? da 0 a 1
    nk=vector(mode="numeric",length=length(pk))         #n. di casi in ogni categoria probabilistica da 0 a 1
    nobsk=vector(mode="numeric",length=length(pk))      #n. casi in cui evento viene osservato per ogni categoria prob.
    ok=vector(mode="numeric",length=length(pk))         #frequenza relativa dell'osservazione, n. eventi osservati/n. previsioni per ogni categoria
    nonobsk=vector(mode="numeric",length=length(pk))    #non osservati
    sequenza=c(seq(0,0.05,0.05),seq(0.15,0.95,0.1),1.0) #vettore classi probabilistiche
    
    for(iii in seq(pk)){
      nk[iii]=length(which(EPSfc>=sequenza[iii] & EPSfc<sequenza[iii+1]))
      temp=(misura[which(EPSfc>=sequenza[iii] & EPSfc<sequenza[iii+1])])
      if(iii==11){
        nk[iii]=length(which(EPSfc>=sequenza[iii] & EPSfc<=sequenza[iii+1]))
        temp=(misura[which(EPSfc>=sequenza[iii] & EPSfc<=sequenza[iii+1])])
      }
      nobsk[iii]=length(which(temp>threshold))
      nonobsk[iii]=length(which(temp<=threshold))
    }
    
    totnk=sum(nk)
    totnobsk=sum(nobsk)
    totnonobsk=sum(nonobsk)
    ok=nobsk/nk
    ok[which(is.na(ok))]=0
    climatology=totnobsk/totnk     #-------tot. eventi osservati/tot. n. previsioni
    #numero di previsioni per ogni fascia probabilistica
    nforec=c(totnk,sum(nk[-1]),sum(nk[c(-1,-2)]),sum(nk[seq(-1,-3)]),sum(nk[seq(-1,-4)]),sum(nk[seq(-1,-5)]),
             sum(nk[seq(-1,-6)]),sum(nk[seq(-1,-7)]),sum(nk[seq(-1,-8)]),sum(nk[seq(-1,-9)]),sum(nk[seq(-1,-10)]),
             sum(nk[seq(-1,-11)]))
    #numero eventi osservati
    nobs_y=c(totnobsk,sum(nobsk[-1]),sum(nobsk[c(-1,-2)]),sum(nobsk[seq(-1,-3)]),sum(nobsk[seq(-1,-4)]),
             sum(nobsk[seq(-1,-5)]),sum(nobsk[seq(-1,-6)]),sum(nobsk[seq(-1,-7)]),sum(nobsk[seq(-1,-8)]),
             sum(nobsk[seq(-1,-9)]),sum(nobsk[seq(-1,-10)]),sum(nobsk[seq(-1,-11)]))
    #numero eventi NON osservati
    nobs_n=nforec-nobs_y
    #Hit Rate e False Alarm Rate
    HR=nobs_y/totnobsk
    FR=nobs_n/totnonobsk
    areac=vector(mode="numeric",length=length(pk))
    for(iv in seq(11)){
      areac[iv]=((HR[iv]+HR[iv+1])*(FR[iv]-FR[iv+1]))/2     #area sotto la curva
    }
    areacurva=sum(areac)
    ROCSS[a]=2*areacurva-1
    bsparziale=(EPSfc-MISfc)^2  #(pi-oi)^2 per ogni scadenza
   bsparziale=(MISfc3-MISfc)^2 
    BS[a]=mean(bsparziale)
    omean=mean(MISfc)  #questo valore ? uguale alla sample climatology =tot(nobsk)/tot(nk), nelle componenti del BS sarebbe c
#    bsref= mean((MISfc2-omean)^2)
    bsref=(MISfc2-MISfc)^2
    BS_REF=mean(bsref)
    
    #X FARE BOOTSTRAP BSS
    oggetto=as.data.frame(bsparziale)
    names(oggetto)="bsparz"
    bss.boot=boot(oggetto,bss.fun,R=1000)
    bootresult=boot.ci(bss.boot,type="basic")
    boot_t0[a]=bootresult$t0
    bootstraplow[a]=bootresult$basic[4]
    bootstraphigh[a]=bootresult$basic[5]
    
    BSS[a]=1-(BS[a]/BS_REF)       #BRIER SKILL SCORE
    fobs=nobsk/nk
    fobs[which(fobs=="NaN")]=0            #in caso che il primo valore di n_obsk e n_k sia 0
    REL[a]=sum(nk*(pk-fobs)^2)/totnk      #reliability
    RES[a]=sum(nk*(fobs-omean)^2)/totnk   #resolution
    UNC[a]=omean*(1-omean)                #uncertainty
    a=a+1
  }
  #####plotting phase  with boot strap confidence intervals 
  
  if(adding==F){ 
    if(plotfile!="") {postscript(plotfile)} 
    
    plot(BSS,type="l",ylim=c(-1,1),axes=F,xlab="Lead Time (Hours)",ylab="BSS",cex.lab=1.5,lwd=2)
    axis(1,at=seq(0,n_ltimes-1),cex.axis=1.2)
    axis(2,at=seq(-1,1,0.1),cex.axis=1.2)
    if(bstrap==T){ 
      for(i in seq(n_ltimes)){
        arrows(i,bootstraplow[i],i,bootstraphigh[i],length=0.04,lend=3,angle=n_ltimes,code=3,lwd=1,pch=16)   #consistency bars
      }
    }
#    lines(UNC,lty="dashed",lwd=2)
  } 
  if(adding==T){ 
    
    lines(boot_t0,lty=type,col=colour,lwd=2)
    if(bstrap==T){ 
      for(i in seq(n_ltimes)){
        arrows(i,bootstraplow[i],i,bootstraphigh[i],length=0.05,angle=90,code=3,col=colour,lwd=2)   #consistency bars          
      }  
    }
    lines(UNC,col=colour,lty="dashed",lwd=2)
  }
  ##close the file only if is the last model to plot 
  if(close==T){ 
    if(plotfile!=""){ dev.off() } 
  }  
  
  return(c(BS,REL,RES,UNC))
}  


