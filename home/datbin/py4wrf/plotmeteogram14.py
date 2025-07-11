#!/usr/bin/env python 
''' meterogram plot by python 
    WWU (wanliwu@ucar.edu)
    Dec. 2013 - Jan. 2014
'''
import os
import sys
from netCDF4 import Dataset, MFDataset, chartostring
import glob
import numpy as np
from datetime import datetime, timedelta
import pandas as pd
import matplotlib; matplotlib.use('Agg')
import matplotlib.pyplot as plt
from mpl_toolkits.basemap import cm
from matplotlib.ticker import ScalarFormatter, FormatStrFormatter

def tzgetRH(T,P,Q):
    es_w = es(T-T_zero)
    qsat = eps*es_w/(P-0.378*es_w)
    qvapor = Q
    rh = 100.*qvapor*(1-qsat)/(qsat*(1-qvapor))
    rh = np.minimum(100, rh)
    rh = np.maximum(0, rh)
    return rh

def es(T):
    """Returns saturation vapor pressure (Pascal) at temperature T (Celsius)
    Formula 2.17 in Rogers&Yau"""
    return 611.2*np.exp(17.67*T/(T+243.5))

def phycons():
    T_base = 300.0
    T_zero = 273.15
    L = 2.501e6 # latent heat of vaporization
    R = 287.04  # gas constant air
    Rv = 461.5  # gas constant vapor
    eps = R/Rv
    cp = 1005.
    cv = 718.
    kappa = (cp-cv)/cp
    g = 9.81

    return T_base, T_zero, L, R, Rv, eps, cp, cv, kappa, g

def interp(geopot, pres, p):
    """ Returns the interpolated geopotential at p using the values in pres. 
    The geopotential for an element in pres must be given by the corresponding
    element in geopot. The length of the geopot and pres arrays must be the same. 
    """
    if (len(geopot) != len(pres)):
        raise Exception, "Arrays geopot and pres must have same length"
    
    k = len(pres)-1
    while (k > 1 and pres[k-1] <= p):
        k = k-1

    if (pres[k] > p):
        w = 0.0
    elif (pres[k-1] < p):
        w = 1.0
    else: 
        w = (p-pres[k])/(pres[k-1]-pres[k])

    return (1.0-w)*geopot[k] + w*geopot[k-1]

def tzMetegramPlot(nc,offset=0, title='meterogram', fpng = 'pme.png', ftlab=False, z_max=5000, dz=1000):
    m2ft = 3.28084
    
    heightground_t, heighthalf_tz = tzgetHeight(nc, Nz, x_nr, y_nr)
    if ftlab:
       heightground_t, heighthalf_tz =  heightground_t * m2ft, heighthalf_tz * m2ft
    z_min = heightground_t[0]-heightground_t[0]%dz
    if ftlab:
       z_max = z_max/1000
       z_min = z_min/1000
       dz = dz/1000
       heightground_t = heightground_t/1000.0
       heighthalf_tz = heighthalf_tz /1000.0
#   print dz, z_min, z_max, heightground_t[0], heighthalf_tz[0,0]
    
    uvkts = 1.94384449
    thin = 2
    u = (nc.variables['U'][:,:,:,0:Nx-2] + nc.variables['U'][:,:,:,1:Nx-1])*0.5
    v = (nc.variables['V'][:,:,0:Ny-2,:] + nc.variables['V'][:,:,1:Ny-1,:])*0.5
#   print 'get u v t-Z and transpose for plotting ...'
    utz = u[:,:,y_nr,x_nr]
    vtz = v[:,:,y_nr,x_nr]
#   print 'array transpose for u...'
    u_tz = utz.T
#   print 'array transpose for v...'
    v_tz = vtz.T
#   print 'done with array transpose'
    theta = nc.variables['T'][:,:,y_nr,x_nr] + T_base 
    P = nc.variables['P'][:,:,y_nr,x_nr] + nc.variables['PB'][:,:,y_nr,x_nr] 
    Q = nc.variables['QVAPOR'][:,:,y_nr,x_nr]
    Ttz = theta*(P/P00)**kappa   # temperature in Kelvin
    RH = tzgetRH(Ttz, P, Q)
    T_tz = Ttz.T
    rh_tz = RH.T
    u_tz[:,0:Nt:thin] = np.nan
    v_tz[:,0:Nt:thin] = np.nan

    rlab = np.arange(1000,0,-100)  # pressure levels to be labeled 
    print len(rlab), rlab

    pres = 0.01 * P.mean(axis=0)   # time-averaged pressure in hPa
    H = heighthalf_tz.mean(axis=1) # time-averaged height
    pmin = pres.min()
    pmax = pres.max()
    hmin = H.min()
    hmax = H.max()
    print pmin, pmax, hmin, hmax, z_min, z_max 
    yhp = []
    yhv = []
    ns = 0
    for p in rlab:                # find pressure label postions
        if p < pmin or p > pmax:
           pass
        else:
           for n in range(ns,len(pres)):
               pb = pres[n]
               pt = pres[n+1]
               if p >= pt and p <= pb:
                  w = (p-pt)/(pb-pt)
                  h = w * H[n] + (1.0-w) * H[n+1]
                  ns = n
                  break
           if h >= z_min and h <= z_max:
              yhp.append(h)
              yhv.append(str(p))

    fig = plt.figure(figsize=(18,18))
    ax1 = fig.add_subplot(111)
  
    ax1.axis([-offset,Nt-1,z_min,z_max])
    
    grid = np.reshape(np.tile(np.arange(Nt),Nz),(Nz,-1))
    
    afig = ax1.contourf(grid, heighthalf_tz, rh_tz, alpha=0.9,levels=tz_rh_levels,cmap=cm.GMT_drywet)
    # another color map candidates gist_ncar_r
    cbar = fig.colorbar(afig, orientation='horizontal',shrink=0.5, pad = 0.10)
    
    cbar.ax.get_xaxis().labelpad = 10
    cbar.ax.set_xlabel('relative humidity (%)')
    
    ax1.barbs(grid,heighthalf_tz,u_tz,v_tz, length=8, sizes={'spacing':0.2}, pivot='middle',barb_increments=dict(half=5, full=10, flag=30))
      
    cs = ax1.contour(grid, heighthalf_tz, T_tz-T_zero, temp_int,colors='white',linestyles='solid')

    cs.collections[25].set_linewidth(2)           # highlight zero T line 
    cs.collections[25].set_linestyle('dashed')
    cs.collections[25].set_color('red')
        
    ax1.clabel(cs, inline=1,  fmt='%1.0f', fontsize=12,colors='black')
    
    ax1.fill_between(np.arange(-offset,Nt),heightground_t[0],z_min,facecolor='lightgrey')
        
    ax1.set_xlabel('GAMEP Time (UTC)', fontsize=14)
    if ftlab:
        ax1.set_ylabel(u'Altitude above Sea Level (X1000 ft)',fontsize=14)
    else:
        ax1.set_ylabel(u'Altitude above Sea Level (m)',fontsize=14)
 
    ax1.set_yticks(np.arange(z_min,z_max,dz))
    ax2 = ax1.twinx()
    ax2.set_ylabel('Pressure (hPa)', color='k', fontsize=14)
    ax2.set_ylim([z_min,z_max])
    plt.yticks(yhp, yhv)
    plt.xticks(range(Nt),xticks)
    plt.title(title, fontsize=15)

    plt.savefig(fpng)

    plt.show()
    plt.close()
def tzgetHeight(nc,Nz,x_nr,y_nr):
    # Calculate height above sea level for mass levels
    # Note: geopotential defined at full levels (w-levels)
    #       Must interpolate to find geopotential at half levels (u-levels)
    geopot = (nc.variables['PH'][:,0:Nz,y_nr,x_nr] + nc.variables['PHB'][:,0:Nz,y_nr,x_nr])
    mu = (nc.variables['MU'][:,y_nr,x_nr]+nc.variables['MUB'][:,y_nr,x_nr])
    znw = nc.variables['ZNW'][:,0:Nz] # full (w) levels
    znu = nc.variables['ZNU'][:,0:Nz] # half (u,mass) levels

    heighthalf = np.zeros((Nz,Nt))# height in meters
    for t in np.arange(Nt):
        pfull = mu[t]*znw[t,0:Nz]+PTOP
        phalf = mu[t]*znu[t,0:Nz]+PTOP
        for k in np.arange(Nz):
            heighthalf[k,t]=interp(geopot[t,:],pfull[:],phalf[k])/g
    heightground = geopot[:,0]/g
    return heightground,heighthalf

def fcstTimesPlot(nc,offset=0, title='meterogram', fpng = 'pme.png', tidx=[]):
    
    tscale = 273.15
    pscale = 0.01
    uvkts = 1.94384449

    u = nc.variables['U10'][:,y_nr,x_nr]
    v = nc.variables['V10'][:,y_nr,x_nr]
    t = nc.variables['T2'][:,y_nr,x_nr]
    q = nc.variables['Q2'][:,y_nr,x_nr]
    p = nc.variables['PSFC'][:,y_nr,x_nr]
    rainc = nc.variables['RAINC'][:,y_nr,x_nr]
    rainl = nc.variables['RAINNC'][:,y_nr,x_nr]
    rh = tzgetRH(t, p, q)
    t = t - tscale
    p = p * pscale
    u = u * uvkts 
    v = v * uvkts 
    rain = rainc + rainl 

    fcst = pd.DataFrame(t, index=tidx, columns=['T2'])
    fcst.index.name = 'DATE'
    fcst['Q2'] = q
    fcst['RH'] = rh
    fcst['PS'] = p 
    fcst['U10'] = u 
    fcst['V10'] = v 

    fig = plt.figure(figsize=(11,7))
    ax = fig.add_subplot(311)
    xa = np.arange(Nt)
    print("xa",xa)

    ya = np.sqrt(u*u+v*v)
    plt.barbs(xa,ya,u,v, length=5, sizes={'spacing':0.2}, pivot='tip',barb_increments=dict(half=5, full=10, flag=30))
    ax.set_xlim([0,Nt])
    ax.plot(xa,ya,'ko')
    ax.set_ylabel('Wind (knots)', color='k', fontsize=14)
    plt.xticks(xa, xticks)
    plt.title(title, fontsize=15)

    ax = fig.add_subplot(312)
    ax.set_xlim([0,Nt])
    ax.plot(xa,t,'ro')
    ax.set_ylabel('Temperature (C)', color='r', fontsize=14)
    axr = ax.twinx()
    axr.set_xlim([0,Nt])
    axr.set_ylim([0,100])
    axr.yaxis.tick_right()
    axr.yaxis.set_label_position("right")
    axr.set_ylabel('Relative Humidity (%)', color='b')
    axr.plot(xa,rh,'bo', alpha=0.7)
    plt.xticks(xa, xticks)

    ax = fig.add_subplot(313)
    ax.set_xlim([0,Nt])
    ax.plot(xa,p,'ko')
    plt.xticks(xa, xticks)
    ax.yaxis.set_major_formatter(FormatStrFormatter('%0.0f'))
    ax.set_ylabel('Surface Pres. (hPa)', color='k', fontsize=14)
    if rain.max() > 0.5 :
       axr = ax.twinx()
       axr.set_xlim([0,Nt])
       axr.yaxis.tick_right()
       axr.yaxis.set_label_position("right")
       axr.set_ylabel('Hourly Preci. (mm)', color='g')
       axr.bar(xa,rain, color = 'g', alpha=0.7)
       plt.xticks(xa, xticks)
    else:
       y = p.min() + 0.1
       x = 3.0 * Nt / 4.0
       print("x ",x,int(x))
       x = xa[int(x)]
       plt.text(x,y, 'no preci. this period', bbox=dict(facecolor='grey', alpha=0.5))
       plt.xticks(xa, xticks)
     # plt.text(0.75, 0.15, 'no preci. in this period', bbox=dict(facecolor='red', alpha=0.5))
 #  ax.yaxis.set_major_formatter(FormatStrFormatter('%0.0f'))

    plt.savefig(fpng)

    plt.show()
    plt.close()
    
    return fcst 

# main program for meterogram plot: T, RH, and Wind

tz_rh_levels = np.arange(0, 105, 5)

barb_increments = {'half': 2.5,'full':5.0,'flag':25.0}

# Max height used on z-axis
z_max = 12000.0   # km
# Tick increment used for vertical axis (z)
dz = 1000
# Temperature interval used when plotting contour lines

ftlab = True      # plot height as feet
if ftlab:
   z_max = 36000
   dz = 3000
else:
   z_max = 12000  # plot height as meters
   dz = 1000 
    
temp_int = np.arange(-50.0,50.0,2.0)

dom = sys.argv[1]
sinfo = sys.argv[2]
wrfdir = sys.argv[3]
pngdir = sys.argv[4]

#dom = 'd02'
#sinfo = '/glade/u/home/wanliwu/wupycodes/pmewrftable_d02.csv'
#wrfdir = '/glade/scratch/wanliwu/GRM/2013123100'
#pngdir = '/glade/scratch/wanliwu/WRF35EXPS'

infopath = wrfdir.split('/')
date = infopath[-1].strip()
#date = '2013-12-31 00UTC'
stations = sinfo
#stations = '/glade/u/home/wanliwu/wupycodes/pmewrftable_d02.csv'
stationinfo = pd.read_csv(stations, sep=',')

stationinfo = stationinfo.set_index('ID')
allstations = stationinfo.index
#print stationinfo.head()

T_base, T_zero, L, R, Rv, eps, cp, cv, kappa, g = phycons()

#pngdir = '/glade/scratch/wanliwu/WRF35EXPS'
#wrfdir = '/glade/scratch/wanliwu/GRM/2013123100'
mfile = 'wrfout_' + dom + '_*.GRM_P+FCST'

# MFDataset does not have attributes like getncattr
# for ncattr we have to use Dataset
mf = os.path.join(wrfdir,mfile)
afiles = glob.glob(mf)
f = afiles[0]
print f, mf

ncfile = Dataset(f,'r+')
Nx = ncfile.getncattr('WEST-EAST_GRID_DIMENSION')-1
Ny = ncfile.getncattr('SOUTH-NORTH_GRID_DIMENSION')-1
Nz = ncfile.getncattr('BOTTOM-TOP_GRID_DIMENSION')-1
#print Nx, Ny, Nz
ncfile.close()

ncmf = MFDataset(mf)

vars= ncmf.variables

PTOP = ncmf.variables['P_TOP'][0]
P00  = ncmf.variables['P00'][0]
T00  = ncmf.variables['T00'][0]
#print PTOP, P00, T00
#T_base = T00

dates = ncmf.variables['Times']

Nt = len(dates)
print Nx, Ny, Nz, Nt

times = []
xticks = []    # create x-axis ticks
for n in range(len(dates)):
    wrfdate = str(chartostring(dates[n,:])).replace('_',' ')
    cymdh = datetime.strptime(wrfdate,'%Y-%m-%d %H:%M:%S')
    times.append(cymdh)
    if cymdh.hour%6 == 0:
       xlab = '%02d'%cymdh.hour
       if cymdh.hour == 0:
          xlab = '%02d/%02d'%(cymdh.month, cymdh.day)
    else:
        xlab = ''
    xticks.append(xlab)

for n, sid in enumerate(allstations):
    print n, sid 
    station = stationinfo.loc[sid]['SITE']
    x_nr, y_nr = stationinfo.loc[sid]['I'], stationinfo.loc[sid]['J']

  # title = 'T (contour), RH (color) and Wind (barb) at %s  WRF initial time: %s' %(station,date)
  # title = 'T (contour), RH (color) and Wind (barb) at %s  WRF cycle: %s' %(station,date)
    title = '%s  WRF cycle: %s \n T (contour), RH (color) and Wind (barb)' %(station,date)
  # filepng = 'p' + sid.strip() + '_' + dom + '.png'
    filepng = sid.strip() + '_' + dom + '.png'
    filepng = os.path.join(pngdir, filepng)
    if os.path.exists(filepng):
      os.system('rm -f %s' %filepng)

    print title 
    print filepng
    tzMetegramPlot(ncmf,offset=0, title=title, fpng='top.png', ftlab=ftlab, z_max=z_max, dz=dz)
    title = 'Surface Wind, T, RH, PS and Preci. at %s WRF cycle: %s'%(station,date)
    fcst = fcstTimesPlot(ncmf,offset=0, title=title, fpng='bot.png', tidx = times)
    filecsv = sid.strip() + '_' + dom + '.txt'
    filecsv = os.path.join(pngdir, filecsv)
    fcst.to_csv(filecsv, sep=' ')

#   cmd = 'convert -trim +repage top.png top.png ' 
#   os.system(cmd)
#   cmd = 'convert -trim +repage bot.png bot.png '
#   os.system(cmd)
#   cmd = 'convert -bordercolor White -border 2x20 top.png bot.png -append %s'%filepng
    cmd = 'convert top.png bot.png -append %s'%filepng
    os.system(cmd)
    os.system('rm -f top.png bot.png')

ncmf.close()
