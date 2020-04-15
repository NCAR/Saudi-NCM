#!/usr/bin/env python

# test GFS4 download issue  June 17 2013 W. Wu
# usage: python downloadGFS.py offhours GFS3[4]
#        offhours to the latest GFS model cycle from current time
#        GFS3[4]: gfs3 or gfs4 
#        python downloadGFD.py 4 gfs4     
#         downloading gfs4 for its 12Z cycle  if the current time is 16ish when the python program is executing 

import os 
import sys
from datetime import datetime, timedelta

#import argparse
#parser = argparse.ArgumentParser(description='download GFS4 data for WRF')
#parser.add_argument('-f', dest='offhours', type=str, default='4',
#                    help = 'offset hours from present')
#args = parser.parse_args()
#offhours  = int(args.offhours) 

offhours  = int(sys.argv[1])
modeltype = sys.argv[2]

ctimeutc = datetime.utcnow()
ptimeutc = datetime.utcnow() - timedelta(hours = offhours)
gfsdate =  ptimeutc.strftime('%Y%m%d')
gfscyc  =  ptimeutc.strftime('%H')

mydate = gfsdate + gfscyc

if modeltype == 'gfs3':
   datdir = '/scratch/project/k1206/datainput/gfs3/'
   fsizelim = 17000000
   fend = '_tl.press_gr.1p0deg'
elif modeltype == 'gfs4':
   datdir = '/scratch/project/k1206/datainput/gfs4/'
   fsizelim = 50000000
   fend = '_tl.press_gr.0p5deg'
else:
   print modeltype, ' is not valid data type to download!'
   sys.exit()

urlsite = 'http://weather.noaa.gov/pub/SL.us008001/ST.opnl/MT.gfs_CY.' + gfscyc + '/RD.' + gfsdate + '/PT.grid_DF.gr2/'
#urlsite = 'http://66.172.231.102/pub/SL.us008001/ST.opnl/MT.gfs_CY.' + gfscyc + '/RD.' + gfsdate + '/PT.grid_DF.gr2/'
# 66.172.231.102
# 129.19.157.17
print urlsite 
hdop = '--header=Host: weather.noaa.gov'

#cmd = 'cd /d1/pmeop/datainput/gfs4'
#os.system(cmd)

# download GFS4 forecasts every three hours up to 84 hours to drive WRF 
hr0 = 0
hr1 = 87        # python excludes the end 
hri = 3 
for h in range(hr0,hr1,hri):
    if h < 10:
       chour = '000' + str(h)
    else:
       chour = '00' + str(h)

    filer = 'fh.' + chour + fend                      # file name in the remote site 
    filel = datdir + mydate + '_' + filer + '.grib2'  # file name to be saved in local disk 
    print h, filer, filel
    fok = 0                                           # check if the file is already available in local disk and if the size is right
    if os.path.isfile(filel):
       fsize= os.path.getsize(filel)
       if int(fsize) > fsizelim:
          fok = 1
          print filel, ' is OK-ed !'
       else: 
          cmd = 'rm -f ' + filel
          os.system(cmd)
    if fok == 0:  
       print 'downloading ', filer
       cmd = 'wget ' + urlsite + filer + ' -O ' + filel
#      cmd = 'wget --header=Host: weather.noaa.gov ' + urlsite + filer + ' -O ' + filel
       print cmd 
       os.system(cmd)
# check if downloaded file has right size 
       if os.path.isfile(filel):
          fsize= os.path.getsize(filel)
          if int(fsize) > fsizelim:
             print filel, ' is OK-ed !'
          else:
             print 'downloading ', filer,' incompleted !'
             cmd = 'rm -f ' + filel
             os.system(cmd)            # remove incompleted file 
       else:
             print 'downloading ', filer,' failed !'

# wget http://weather.noaa.gov/pub/SL.us008001/ST.opnl/MT.gfs_CY.00/RD.20130617/PT.grid_DF.gr2/fh.0003_tl.press_gr.0p5deg -O 2013061700_fh.0003_tl.press_gr.0p5deg.grib2
