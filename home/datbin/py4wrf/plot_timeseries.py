#!/usr/bin/env python

# Working script to generate maps from wrfout netCDF files
# using matplot lib with basemap
# Original code by David John Gagne II
# Additions with mesonet by Luke Madaus
# Some mesonet acquisition code from Dr. Brian Fiedler
import sys
import matplotlib
matplotlib.use('agg')
import pylab
import math
import numpy as np
import os, getopt
from netCDF4 import Dataset
from matplotlib.ticker import MultipleLocator, FormatStrFormatter
import glob 

from wrfpylib import *
from adaptedpylib import draw_meteo
from wmoinfosa import wmostations_sa 
from stationijsa import *

#allsite = wmostations_sa()
#print allsite

#allstations = ijindeces_d01(dom = 'd01')
#print allstations

(opts,args) = getopt.getopt(sys.argv[1:],'s:')
for o,a in opts:
	if o == '-s':
		sitename = str(a)

# WRF model output: 
wrfdir = '/d2/pmefdda/cycles/GWPME/GRM'
cycle = sys.argv[1] 
#cycle = '2012100212'
file  = 'wrfout_d02*GRM_P+FCST'
rundir = os.path.join(wrfdir,cycle,file)
print rundir 
# get files
wrfpdfiles = glob.glob(rundir)
nfiles = len(wrfpdfiles)
print wrfpdfiles[0]

sitename = 'Abha'

onesite = wmostations_sa(site = sitename)
print sitename, onesite

onestation = ijindeces_d02(dom = 'd02', site = sitename)
print onestation
(j0,i0) = onestation['jlat'], onestation['ilon']
print sitename, i0, j0 

sfcT = []
sfcQ = []
sfcP = []
sfcSwdown = []
sfcRain = []
sfcU = []
sfcV = []
times = []
for ncfile in wrfpdfiles:
    wrfdat = Dataset(ncfile)     
    sfcT.append(wrfdat.variables['T2'][0,j0,i0])
    sfcQ.append(wrfdat.variables['Q2'][0,j0,i0])
    sfcP.append(wrfdat.variables['PSFC'][0,j0,i0] * 0.01)
    sfcSwdown.append(wrfdat.variables['SWDOWN'][0,j0,i0])
    sfcU.append(wrfdat.variables['U10'][0,j0,i0])
    sfcV.append(wrfdat.variables['V10'][0,j0,i0])
    sfcRain.append(np.add(wrfdat.variables['PREC_ACC_NC'][0,j0,i0],wrfdat.variables['PREC_ACC_C'][0,j0,i0]))
    times.append(wrfdat.variables['Times'][0])

# following calculation on water vapor related variables are based on
# formulas in http://www.geog.ucsb.edu/~joel/g266_s10/lecture_notes/chapt03/oh10_3_01/oh10_3_01.html

(rh, td, tc) = wrf_wvapor(sfcT, sfcQ, sfcP)

# wind speed, wind direction:
(wspd, wdir) = wrf_wind(sfcU, sfcV)

#sys.exit()

hourticks = range(0,nfiles)
(hours, dates, dateticks) = wrf_timelable(times)

print "Plotting"

draw_meteo(cycle, sitename, tc, td, wspd, wdir, sfcP, sfcRain, sfcSwdown, times)

#os.system('scp *meteo.gif hoot@10.197.1.220:/usr/home/hoot/http/models_data/wrf/.')
print "Done."
