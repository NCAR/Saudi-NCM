#!/usr/bin/env python
""" 
WRF forecast timeseries for inter-domain comparison 
Wanli Wu (wu80012@gmail.com)
09-30-2012
"""

import numpy as np
import matplotlib; matplotlib.use('Agg')
import matplotlib.pyplot as plt
from matplotlib.ticker import ScalarFormatter, FormatStrFormatter
import os 
import sys
from netCDF4 import MFDataset
import pylab 

from atmoscons import atmconstants 
from stationdic import wmostations
from wrfpylib import *
#from adaptedpylib import draw_meteo
from wmoinfosa import wmostations_sa 
from stationijsa import *

def wrf_timeseries(wrffile, varname, i, j, offs, scale):
    from netCDF4 import Dataset

# generate time series for given location (i,j indices)
# offs: offset; scale: scale 

    k  = 0 

    varts = []
    times = []
    for ncfile in wrffile:
        wrfdat = Dataset(ncfile)
        a =scale * (wrfdat.variables[varname][k,j,i] -offs)
        varts.append(a)
        times.append(wrfdat.variables['Times'][k])
    return varts, times

def wrfmftimeseries(wrfdir, dom = 'd01', sitename = 'Jeddah'):
    from netCDF4 import MFDataset
    import os 
    from wrfpylib import wrf_wind
    from stationijsa import ijindeces_d01, ijindeces_d02, ijindeces_d03

    if dom == 'd01':
       onestation = ijindeces_d01(dom = dom, site = sitename)
       (j0,i0) = onestation['jlat'], onestation['ilon']
    if dom == 'd02':
       onestation = ijindeces_d02(dom = dom, site = sitename)
       (j0,i0) = onestation['jlat'], onestation['ilon']
    if dom == 'd03':
       onestation = ijindeces_d03(dom = dom, site = sitename)
       (j0,i0) = onestation['jlat'], onestation['ilon']

    file  = 'wrfout_' + dom + '*GRM_P+FCST'
    dirfile = os.path.join(wrfdir,cycle,file)

# read multiple netCDF files at once 
    wrfall = MFDataset(dirfile)

    TD = np.subtract(wrfall.variables['T2'][:,j0,i0], T0)
    QD = np.multiply(wrfall.variables['Q2'][:,j0,i0], Q0)
    PS = np.multiply(wrfall.variables['PSFC'][:,j0,i0], 0.01)

    US = wrfall.variables['U10'][:,j0,i0]
    VS = wrfall.variables['V10'][:,j0,i0]
    (wspd, wdir) = wrf_wind(US, VS)  

    timesD = wrfall.variables['Times'][:]

    wrfall.close()

    return TD, QD, PS, wspd, wdir, timesD


# === main program ====

atmcon = atmconstants()

T0 = atmcon['T0']
Q0 = 1.0e3 
#sitename = 'Jeddah'
sitename = sys.argv[2]
onesite = wmostations_sa(site = sitename)
slon, slat = onesite['lon'], onesite['lat']

# WRF model output:
wrfdir = '/d2/pmefdda/cycles/GWPME/GRM'
#cycle = '2012092412'
cycle = sys.argv[1]
varname = ['T2', 'Q2', 'U10', 'V10', 'PSFC']
# direction for gif files
dirout = '/d2/pmefdda/timesgraphs/' + cycle
if os.path.isdir(dirout):
   pass
else:
   cmd = 'mkdir -p %s' %dirout
   os.system(cmd)

# time series 
dom = 'd01'
(TD1, QD1, PSD1, wspd1, wdir1, timesD1) = wrfmftimeseries(wrfdir, dom = dom, sitename = sitename)
nfiles = len(timesD1)
hourticksD1 = range(0,nfiles)
#(rhd1, td1, tc1) = wrf_wvapor(TD1, QD1, PSD1)
#print np.min(rhd1), np.max(rhd1)
#print np.min(tc1) , np.max(tc1) 
#print np.min(td1) , np.max(td1) 

dom = 'd02'
(TD2, QD2, PSD2, wspd2, wdir2, timesD2) = wrfmftimeseries(wrfdir, dom = dom, sitename = sitename)
nfiles = len(timesD2)
hourticksD2 = range(0,nfiles)

dom = 'd03'
(TD3, QD3, PSD3, wspd3, wdir3, timesD3) = wrfmftimeseries(wrfdir, dom = dom, sitename = sitename)
nfiles = len(timesD3)
hourticksD3 = range(0,nfiles)

# create a list of time ticks to lable in x-axis
(hours, dates, dateticks) = wrf_timelable(timesD1)

fig = plt.figure(figsize=(10,8))
ax = fig.add_subplot(511)
ax.plot(hourticksD1,TD1,'ro',hourticksD2,TD2,'bo',hourticksD3,TD3,'ko')
#ax.set_ylim([15,45])
ax.set_xlim([-3,75])
plt.xticks(hourticksD1, hours)
ax.grid(True)
ax.set_ylabel('Air Temperature (C) ')

# split multi-word site name
#multiword = sitename.split(' ') 
#print multiword
#fstword = multiword[0]
#print fstword
#multiword = fstword.split('-')
#fstword = multiword[0]
#print fstword
#exit()

gtitle = '(%5.2fN,%5.2fE)' %(float(slat), float(slon))
gtitle = cycle + ' WRF forecast for ' + sitename + ' ' + gtitle
ax.set_title(gtitle)
#ax.set_title('WRF Forecast from ' + cycle + ' for ' + sitename)

ymin1 = np.ma.min(TD1)
ymin2 = np.ma.min(TD2)
ymin3 = np.ma.min(TD3)
ymin = min(ymin1, ymin2, ymin3)
ymax1 = np.ma.max(TD1)
ymax2 = np.ma.max(TD2)
ymax3 = np.ma.max(TD3)
ymax = max(ymax1, ymax2, ymax3)
ybase = ymin - (ymax - ymin) * 0.050
for d in range(len(dates)):
                plt.text(dateticks[d] , ybase, dates[d],rotation='horizontal',fontsize = 15, color = 'k', horizontalalignment = 'center')

leg = ax.legend(('21.6km D', ' 7.2km D', ' 2.4km D'),
           'upper right', shadow=True, prop={'size':6},bbox_to_anchor=(1.00,1.55))

ax = fig.add_subplot(513)
ax.plot(hourticksD1,wspd1,'ro',hourticksD2,wspd2,'bo',hourticksD3,wspd3,'ko')
#ax.set_ylim([15,45])
ax.set_xlim([-3,75])
plt.xticks(hourticksD1, hours)
ax.grid(True)
ax.set_ylabel('Wind speed (m/s) ')

ymin1 = np.ma.min(wspd1)
ymin2 = np.ma.min(wspd2)
ymin3 = np.ma.min(wspd3)
ymin = min(ymin1, ymin2, ymin3)
ymax1 = np.ma.max(wspd1)
ymax2 = np.ma.max(wspd2)
ymax3 = np.ma.max(wspd3)
ymax = max(ymax1, ymax2, ymax3)
ybase = ymin - (ymax - ymin) * 0.050
for d in range(len(dates)):
                plt.text(dateticks[d] , ybase, dates[d],rotation='horizontal',fontsize = 15, color = 'k', horizontalalignment = 'center')
# set some legend properties.  All the code below is optional.  The
# defaults are usually sensible but if you need more control, this
# shows you how

ax = fig.add_subplot(514)
ax.plot(hourticksD1,wspd1,'ro',hourticksD2,wspd2,'bo',hourticksD3,wspd3,'ko')
#ax.set_ylim([15,45])
ax.set_xlim([-3,75])
plt.xticks(hourticksD1, hours)
ax.grid(True)
ax.yaxis.tick_right()
ax.yaxis.set_label_position("right")
ax.set_ylabel('Wind direction (deg) ')

ymin1 = np.ma.min(wdir1)
ymin2 = np.ma.min(wdir2)
ymin3 = np.ma.min(wdir3)
ymin = min(ymin1, ymin2, ymin3)
ymax1 = np.ma.max(wdir1)
ymax2 = np.ma.max(wdir2)
ymax3 = np.ma.max(wdir3)
ymax = max(ymax1, ymax2, ymax3)
ybase = ymin - (ymax - ymin) * 0.050
for d in range(len(dates)):
                plt.text(dateticks[d] , ybase, dates[d],rotation='horizontal',fontsize = 15, color = 'k', horizontalalignment = 'center')
# set some legend properties.  All the code below is optional.  The
# defaults are usually sensible but if you need more control, this
# shows you how
# wind direction
ax.plot(hourticksD1,wdir1,'ro',hourticksD2,wdir2,'bo',hourticksD3,wdir3,'ko')
ax.set_ylim([0,360])
pylab.yticks([0.,45.,90.,135.,180.,225.,270.,315.,360], ['N','NE','E','SE','S','SW','W','NW','N'])

ax = fig.add_subplot(512)
ax.plot(hourticksD1,QD1,'ro',hourticksD2,QD2,'bo',hourticksD3,QD3,'ko')
#leg = ax.legend(('21.6km D', ' 7.2km D', ' 2.4km D'),
#           'upper center', shadow=True)
ymin1 = np.ma.min(QD1)
ymin2 = np.ma.min(QD2)
ymin3 = np.ma.min(QD3)
ymin = min(ymin1, ymin2, ymin3)
ymax1 = np.ma.max(QD1)
ymax2 = np.ma.max(QD2)
ymax3 = np.ma.max(QD3)
ymax = max(ymax1, ymax2, ymax3)
ybase = ymin - (ymax - ymin) * 0.050

ax.set_xlim([-3,75])
plt.xticks(hourticksD1, hours)
ax.grid(True)
ax.yaxis.tick_right()
ax.yaxis.set_label_position("right")
ax.set_ylabel('Mixing Ratio (g/Kg) ')

for d in range(len(dates)):
      plt.text(dateticks[d] ,ybase, dates[d],rotation='horizontal',fontsize = 15, color = 'k', horizontalalignment = 'center')

ax = fig.add_subplot(515)

ax.plot(hourticksD1,PSD1,'ro',hourticksD2,PSD2,'bo',hourticksD3,PSD3,'ko')
#leg = ax.legend(('21.6km D', ' 7.2km D', ' 2.4km D'),
#           'upper right', shadow=True, prop={'size':6},bbox_to_anchor=(1.1, 1.05))
ymin1 = np.ma.min(PSD1)
ymin2 = np.ma.min(PSD2)
ymin3 = np.ma.min(PSD3)
ymin = min(ymin1, ymin2, ymin3)
ymax1 = np.ma.max(PSD1)
ymax2 = np.ma.max(PSD2)
ymax3 = np.ma.max(PSD3)
ymax = max(ymax1, ymax2, ymax3)

ybase = ymin - (ymax - ymin) * 0.050

ax.set_xlim([-3,75])
plt.xticks(hourticksD1, hours)
ax.grid(True)
#ax.set_xlabel('Forecasting time ')
ax.set_xlabel('TIME (UTC) ')
ax.yaxis.set_major_formatter(FormatStrFormatter('%0.0f'))
ax.set_ylabel('Psfc (hPa)')


for d in range(len(dates)):
      plt.text(dateticks[d] ,ybase, dates[d],rotation='horizontal',fontsize = 15, color = 'k', horizontalalignment = 'center')
#     plt.text(dateticks[d] , 5.5, dates[d],rotation='horizontal',fontsize = 15, color = 'k', horizontalalignment = 'center')
# set some legend properties.  All the code below is optional.  The
# defaults are usually sensible but if you need more control, this
# shows you how

# the matplotlib.patches.Rectangle instance surrounding the legend
frame  = leg.get_frame()
frame.set_facecolor('0.80')    # set the frame face color to light gray

# matplotlib.text.Text instances
for t in leg.get_texts():
    t.set_fontsize('small')    # the legend text fontsize

# matplotlib.lines.Line2D instances
for l in leg.get_lines():
    l.set_linewidth(1.5)  # the legend line width

fileout = sitename + '_MDtimeseries'
filepng = fileout + '.png'
filegif = os.path.join(dirout, fileout + '.gif')
if os.path.exists(filegif):
   os.system('rm -f %s' %filegif)

plt.savefig(filepng)
os.system('convert -render -flatten %s %s' % (filepng, filegif))
os.system('rm -f %s' %filepng)
#plt.show()
plt.close()
