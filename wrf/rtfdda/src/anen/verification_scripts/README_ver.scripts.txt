!----------------------------------------------------------------------
!
! Copyright University Corporation for Atmospheric Research (UCAR) 2014
! Research Application Laboratory (RAL)
! National Center for Atmospheric Research (NCAR)
! All Rights Reserved
!
! CONTACTS: Luca Delle Monache, lucadm@ucar.edu
!           Stefano Alessandrini, alessand@ucar.edu
!----------------------------------------------------------------------

Readme for using verification libraries

The first two inputs of all the functions are:
1) a netcdf file of the measurements with 3 dimensions (stations, forecast run, lead times).  
2) a netcdf file of the ensemble forecast with 4 dimensions (stations, forecast run, lead times, members).
The first three indexes of both files must correspond and the dimension must
be consistent.
Missing values are accepted, must be included as NA (not a number).

Input flags:
bstrap: plot boot strap confidence intervals (T or F).
plotfile:  name of the post script file to be plotted, if plotfile ="" just plot on video device.
adding = F: if the function is called for the first time, 
adding = T: if an additional model is being added for comparison.
close = T: if the graphic file has to be closed (no more lines will be added),
close = F: if the function is going to be called again to add new lines.
type: type of lines to be plotted, it can be "dashed", "blank", "solid", "dashed", "dotted", "dotdash", "longdash", or "twodash".
colour:: colour of added lines, it can be  "black", "blue", "red".

IMPORTANT: type and colour are used only if adding = T, when the function is called for the first time (adding=F) a black continuous line is always plotted.

np:value by which Y axis is normalized, np=1 if normalization is not required.


Examples and specific options:
##########################################################
bias()

Bias=bias(obs.nc,forec1.nc,bstrap=T,adding=F,plotfile="",close=F,type,colour,np=4.99)
Bias2=bias(obs.nc,forec2.nc,bstrap=F,adding=T,plotfile="",close=F,type="dashed",colour="red",np=4.99)

Bias of the ensemble mean of model 1 and model 2 are plotted versus forecast lead time on the same plot.
The function returns the average bias of the members mean over all the stations, lead times and forecast runs
##########################################################
dispersion()

rmse=dispersion(obs.nc,forec1.nc,bstrap=F,adding=F,plotfile="",close=F,type,colour,np=1)

root mean square error (RMSE) of the ensemble mean and observation is plotted as a function of forecast lead time. The spread is also plotted as a dash line.

RMSE =sqrt[mean(ensmean-obs)^2*n_members/(n_members+1)]
SPREAD=standard deviation about the ensemble mean

##########################################################
correlation()
Corr=correlation(obs.nc,forec1.nc,bstrap=T,adding=F,plotfile="",close=F,type,colour)

plots the correlation between  the ensemble mean and the observation versus lead time
##########################################################
rmse_det()

rmse=rmse_det(obs.nc,forec1.nc,bstrap=F,adding=F,plotfile="",close=F,type,colour,np=1)

RMSE is plottted without the correction factor:

RMSE =sqrt[mean(ensmean-obs)^2]

##########################################################
mae()

Mae=mae(obs,forecast_an,bstrap=T,adding=F,plotfile="",close=F,type,colour,np=1)

Mean absolute error of the ensemble mean and the observation versus lead time.

##########################################################
spreadskill()

Spread=spreadskill(obs,forecast_an,bstrap=T,adding=F,plotfile="",close=F,type,colour,np=5.21)

Binned spread/skill diagram see: 
See Wang, X., and C. H. Bishop, 2003: A comparison of breeding and ensemble transform Kalman filter ensemble forecast schemes. J. Atmos. Sci., 60, 1140-1158.

##########################################################
reliability()

reli=reliability(obs,forec_qr,threshold=1.5,plotfile="")

Reliability diagram, threshold is the value defining the event considered (obs > threshold).

##########################################################
rhist()

mre=rhist(obs,forec_qr,plotfile="")

rank histogram with confidence bars, returns the Missing Rate Error (MRE) index

see:
S Alessandrini, L. Delle Monache, S. Sperati, J. N. Nissen: Short-term wind power forecasting with an analog ensemble. Renewable Energy 76, 768-781, 2015

##########################################################
crps()

Crps=crps(obs.nc,forec1.nc,bstrap=T,adding=F,plotfile="",close=F,type,colour,np=1)

Plots continuous ranked probability score versus lead time.

Returns crps mean, crps low, crps high of the boot strap distribution computed over all the lead times.

##########################################################
rocss()

Rocss=rocss(obs.nc,forec1.nc,threshold=2.5,bstrap=T,adding=F,plotfile="",close=F,type,colour)

Plots Roc Skill Score versus lead time. Threshold defines the event (obs>threshold).

##########################################################
bss()

Bss=bss(obs.nc,forec1.nc,threshold=2.5,bstrap=F,adding=F,plotfile="",close=F,type,colour)

Plots the Brier Skill Score versus lead time.
Returns n values (n = number of lead times) of Brier Score, Reliability, Resolution and Uncertainty.

##########################################################
bss_comp_threshold()

Bss1=bss_comp_threshold(obs.nc,forec1.nc,forec2.nc,bstrap=F,adding=F,plotfile="",close=F,type,colour)

Plots the Brier Skill score versus lead time of model1 using model 2 as a reference model (i.e. BSS=1-BS1/BS2). 
The event considered is different for any lead time and it is obs>mean(obs(lt)), where obs(lt) are the obs at any lead time, lt.

##########################################################

bss_comp()

Bss1=bss_comp(obs.nc,forec1.nc,forec2.nc,threshold,bstrap=F,adding=F,plotfile="",close=F,type,colour)

Plots the Brier Skill score versus lead time of model1 using model 2 as a reference model (i.e. BSS=1-BS1/BS2) 
The event considered is the same for any lead time and is obs>threshold 
##########################################################

