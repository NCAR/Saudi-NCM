
DirData="/Volumes/d1/alessand/Excel/Tom/"
forec1="test_howard.nc"  ## output file from the AnEn of the fortran code
forec2="atc.verif.201412090000.0.cdf" ## input file to the AnEn
obs_name="wspd_o"    #name of the obs to extract to put in the obs.nc file
library(ncdf4)
forec=paste(DirData,forec1,sep="")
forec_inp=paste(DirData,forec2,sep="")
forec_out=paste(DirData,"forec.nc",sep="")
obs_out=paste(DirData,"obs.nc",sep="")
swap <- nc_open(forec)
     data4d=ncvar_get(swap,varid="AnEn")
nc_close(swap)
stations=dim(data4d)[1]
days=dim(data4d)[2]
flt=dim(data4d)[3]
n_members=dim(data4d)[4]
dimstaz <- ncdim_def( "Staz", "number", 1:stations )
dimdays <- ncdim_def( "Days", "nday", 1:days )
dimleadt<-ncdim_def("Lead times","hours",1:flt)
dimmembers<- ncdim_def("Members","number",1:n_members)
var4d<- ncvar_def( "Power", "kW", list(dimstaz,dimdays,dimleadt,dimmembers),NA)
data4d[data4d==-9999]<-NA  ##putting missing data to NA
forecast= nc_create(forec_out, list(var4d))
ncvar_put( forecast, var4d, data4d[])
nc_close(forecast)
##extrating the right period for the obs from the input ncdf file provided to AnEn
swap <- nc_open(forec_inp)
 data4d=ncvar_get(swap,varid=obs_name)
nc_close(swap)
data4d[data4d>10^10]<-NA  ##putting missing data to NA, attention missing code for forec2 different to forec1!!!
data4d[data4d==-9999]<-NA
day_start=dim(data4d)[3]-days+1
day_end=dim(data4d)[3]
data4d=data4d[,1:flt,day_start:day_end]
data4dnew=array(dim=c(stations,days,flt))
##flipping the array order dimension for compatibility with the verification libraries 
for(i in  seq(stations)){
  for(j in  seq(days)){
    for(k in  seq(flt)){
     data4dnew[i,j,k]=data4d[i,k,j]
    }
   }
}
var3d<- ncvar_def( "Power", "kW", list(dimstaz,dimdays,dimleadt),NA)
measurement= nc_create(obs_out, list(var3d))

ncvar_put( measurement, var3d, data4dnew )  #
nc_close(measurement)
rmse1=rmse_det(obs_out,forec_out,bstrap=F,adding=F,plotfile="",close=F,type="solid",colour="red")
mre=rhist(obs_out,forec_out,plotfile="")
Spread=spreadskill(obs_out,forec_out,bstrap=T,adding=F,plotfile="",close=F,type,colour)
reli=reliability(obs_out,forec_out,threshold=5,plotfile="")