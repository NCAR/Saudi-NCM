#!/usr/bin/env python

# test GFS4 download issue  June 17 2013 W. Wu

import os 
import sys
#import argparse
from datetime import datetime, timedelta

#parser = argparse.ArgumentParser(description='download GFS4 data for WRF')
#parser.add_argument('-f', dest='offhours', type=str, default='4',
#                    help = 'offset hours from present')

#b= os.path.getsize("/path/isa_005.mp3")
#os.path.isfile(fname) 
#b= os.path.getsize("/path/isa_005.mp3")
#args = parser.parse_args()

#offhours  = int(args.offhours) 
offhours  = int(sys.argv[1])

fsizelim = 50000000
ctimeutc = datetime.utcnow()
ptimeutc = datetime.utcnow() - timedelta(hours = offhours)
gfsdate =  ptimeutc.strftime('%Y%m%d')
gfscyc  =  ptimeutc.strftime('%H')

mydate = gfsdate + gfscyc
datdir = '/d1/pmeop/datainput/gfs4/'

prefixm = 'gfs.t' + gfscyc + 'z.pgrb2.0p50.'
#fend = '_tl.press_gr.0p5deg'
fend = '_tl.press_gr.0p50deg'
# fh.0FH_tl.press_gr.0p50deg
#urlsite = 'http://weather.noaa.gov/pub/SL.us008001/ST.opnl/MT.gfs_CY.' + gfscyc + '/RD.' + gfsdate + '/PT.grid_DF.gr2/'
urlsite = 'http://66.172.231.102/pub/SL.us008001/ST.opnl/MT.gfs_CY.' + gfscyc + '/RD.' + gfsdate + '/PT.grid_DF.gr2/'
#urlsite = 'ftp://tgftp.nws.noaa.gov/nfs/nwstg/ftp/SL.us008001/ST.opnl/MT.gfs_CY.' + gfscyc + '/RD.' + gfsdate + '/'
# 66.172.231.102
# 129.19.157.17
print urlsite 
hdop = '--header=Host: weather.noaa.gov'

#cmd = 'cd /d1/pmeop/datainput/gfs4'
#os.system(cmd)

# download GFS4 every three hours up to 84 hours forecast to drive WRF 
for h in range(0,5,3):
    chour = '%04d'%h 
    chouro = '%03d'%h

    filer = 'fh.' + chour + fend
#   filer = prefixm + 'f' + chouro  
    filel = datdir + mydate + '_' + filer + '.grib2'
    print h, filer, filel
# gfs.t18z.pgrb2.0p50.f000
#   sys.exit()
    fok = 0 
    if os.path.isfile(filel):
       fsize= os.path.getsize(filel)
       if int(fsize) > fsizelim:
          fok = 1
          print filel, ' is OK-ed !'
    if fok == 0:  
       print 'downloading ', filer
       cmd = 'wget --header=Host: weather.noaa.gov ' + urlsite + filer + ' -O ' + filel
       print cmd 
#      os.system(cmd)
       fok = 0 
       if os.path.isfile(filel):
          fsize= os.path.getsize(filel)
          if int(fsize) > fsizelim:
             fok = 1
             print filel, ' is OK-ed !'
          else:
             print 'downloading ', filer,' incompleted !'
       else:
             print 'downloading ', filer,' failed !'

# wget http://weather.noaa.gov/pub/SL.us008001/ST.opnl/MT.gfs_CY.00/RD.20130617/PT.grid_DF.gr2/fh.0003_tl.press_gr.0p5deg -O 2013061700_fh.0003_tl.press_gr.0p5deg.grib2
