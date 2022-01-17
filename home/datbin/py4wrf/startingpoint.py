#!/usr/bin/env python
"""
WMO station information and corresponding i j indecies in WRF domains
Reuired files:
 1. WMO station information: icao_sa.txt
 2. WRF WPS geogrid files: geo.em.d01, geo.em.d02, ...
Wanli Wu (wu80012@gmail.com)
09-30-2012
"""
import os 
import sys
import numpy as np
from netCDF4 import Dataset


from stationdic import wmostations
from wrfpylib import wrflonlat2ij 

# wmo station information:
stationlist = wmostations(wmofile = '/home/wanliwu/WUPYTHON/icao_sa.txt')

sitelist = stationlist.keys()
sitelist.sort()

# WPS geogrid files:
wrfdir = '/d1/pmefdda/GMODJOBS/GWPME/wps'

alldoms = ['d01', 'd02', 'd03']

# i j indecies:

domsiteij = wrflonlat2ij(wrfdir, alldoms, stationlist)

print 'stationlist'
for wmo in sitelist:
    print "'%s ': %s ," %(wmo, stationlist[wmo])

domlist = domsiteij.keys()
domlist.sort()

for dom in domlist: 
    print dom
    onedom = domsiteij[dom]
    sitelist = onedom.keys()
    sitelist.sort()
    for wmo in sitelist:
        print "'%s ': %s ," %(wmo, onedom[wmo])

sys.exit()
print stationlist
print domsiteij
sitename = 'Riyadh'
sinfo = stationlist[sitename]
for dom in alldoms:
    dsite = domsiteij[dom]
    site = dsite[sitename]
    print sitename, sinfo['lat'], sinfo['lon'], dom, site['jlat'], site['ilon']
