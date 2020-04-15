

rm(list = ls())
####Examples of calling functions in the source library_verification
####Let's include them first


source("F:/analog_ensemble/library_verification.r")
 

##The first two inputs of all the functions are:
## 1) a netcdf file of the measurements with 3 dimensions (stations,forecast cicle, lead times )
## 2) a netcdf file of the ensemble forecast with 4 dimensions (stations,forecast cycle, lead times, members)
## The first three indexes of both files must correspond and the dimension must be the same
## Missing values are accepted, must be included as NaN
# bstrap = T plot boot strap confidence intervals
# plotfile = name of the post script file to be plotted, if plotfile ="" just plot on video device
# adding = F if  the function is called for the first time,adding =T if I'm adding to a plot another model for comparison
# close = T if I want to close the graphic file (no more line will be added), close=F if the function will be called again to add lines of another forecast model
# type=  type of lines to be plotted, can be =  "dashed", "blank", "solid", "dashed", "dotted", "dotdash", "longdash", or "twodash"
# colour= colour of added lines
##IMPORTANT= type and colour are used only if adding =T, when the functino is called for the fist time a black continuous line is plotted

##calling bias for to different models to be plotted on the same file bias.ps
Bias=bias("E:/analog_ensemble/measurements.nc","E:/analog_ensemble/forecast.nc",bstrap=T,adding=F,plotfile="",close=F,type,colour)
Bias2=bias("E:/analog_ensemble/measurements.nc","E:/analog_ensemble/forecast_2.nc",bstrap=F,adding=T,plotfile="",close=F,type="dashed",colour="grey")



#calling rmse for two different models
#the spread for each model is always plotted as a dashed line
rmse=dispersion("E:/analog_ensemble/measurements.nc","E:/analog_ensemble/forecast.nc",bstrap=T,adding=F,plotfile="",close=F,type,colour)
rmse2=dispersion("E:/analog_ensemble/measurements.nc","E:/analog_ensemble/forecast_2.nc",bstrap=F,adding=T,plotfile="F:/analog_ensemble/rmse.ps",close=F,type="solid",colour="grey")
##
#calling MAE for two different models
Mae=mae("E:/analog_ensemble/measurements.nc","E:/analog_ensemble/forecast.nc",bstrap=T,adding=F,plotfile="",close=F,type,colour)
Mae2=mae("E:/analog_ensemble/measurements.nc","E:/analog_ensemble/forecast_2.nc",bstrap=F,adding=T,plotfile="",close=T,type="solid",colour="grey")
##
#calling spread/skill plot
Spread=spreadskill("E:/analog_ensemble/measurements.nc","E:/analog_ensemble/forecast.nc",bstrap=T,adding=F,plotfile="",close=F,type,colour)
Spread2=spreadskill("E:/analog_ensemble/measurements.nc","E:/analog_ensemble/forecast_2.nc",bstrap=F,adding=T,plotfile="",close=T,type="solid",colour="grey")
##
#calling reliabitity diagram, only one model is allowed, bootstrap always active
#the event considered for reliability is measurements > threshold
#the function returns the three components of Brier Score, Reliability, Resolution, Uncertainty
reli=reliability("E:/analog_ensemble/measurements.nc","E:/analog_ensemble/forecast.nc",threshold=500,plotfile="")

#creating rank histogram, only one model is allowed
#the function returns the MRE index 
##
mre=rhist("E:/analog_ensemble/measurements.nc","E:/analog_ensemble/forecast.nc",plotfile="")

#calling CRPS, the functiron returns the average crps over all the leadtimes 
Crps=crps("E:/analog_ensemble/measurements.nc","E:/analog_ensemble/forecast.nc",bstrap=T,adding=F,plotfile="",close=F,type,colour)
Crps=crps("E:/analog_ensemble/measurements.nc","E:/analog_ensemble/forecast_2.nc",bstrap=F,adding=T,plotfile="",close=T,type="solid",colour="grey")

#calling ROCSS, Roccs values for every leadt ime are returned
Rocss=rocss("E:/analog_ensemble/measurements.nc","E:/analog_ensemble/forecast.nc",threshold=500,bstrap=T,adding=F,plotfile="",close=F,type,colour)
Rocss=rocss("E:/analog_ensemble/measurements.nc","E:/analog_ensemble/forecast_2.nc",threshold=500,bstrap=F,adding=T,plotfile="",close=T,type="solid",colour="grey")

#calling BSS, the values of Brier Score and it components, Reliability, Resolution and Uncertainty are returned

Bss=bss("F:/analog_ensemble/measurements.nc","F:/analog_ensemble/forecast.nc",threshold=500,bstrap=T,adding=F,plotfile="",close=F,type,colour)
Bss=bss("F:/analog_ensemble/measurements.nc","F:/analog_ensemble/forecast_2.nc",threshold=500,bstrap=F,adding=T,plotfile="",close=T,type="solid",colour="grey")
####
##calling correlation function, the function return the avrage correlation over the lead times
Corr=correlation("F:/analog_ensemble/measurements.nc","F:/analog_ensemble/forecast.nc",bstrap=T,adding=F,plotfile="",close=F,type,colour)
Corr=correlation("F:/analog_ensemble/measurements.nc","F:/analog_ensemble/forecast_2.nc",bstrap=F,adding=T,plotfile="",close=T,type="solid",colour="grey")
