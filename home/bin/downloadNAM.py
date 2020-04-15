#!/usr/bin/python

# test NAM212 download issue  June 17 2013 W. Wu
# usage: python downloadGFS.py offhours GFS3[4]
#        offhours to the latest NAM model cycle from current time
#                 if offhours is negative, it will find the offset for the latest cycle.
#        python downloadNAM.py 12
#         downloading nam212 for its 12Z cycle
#        python downloadNAM.py -1
#         downloading nam212 for the last cycle

import os 
import sys
from datetime import datetime, timedelta
import urllib2

#import argparse
#parser = argparse.ArgumentParser(description='download NAM212 data for WRF')
#parser.add_argument('-f', dest='offhours', type=str, default='4',
#                    help = 'offset hours from present')
#args = parser.parse_args()
#offhours  = int(args.offhours) 

MAX_FORECAST_HOURS = 84    # max available hours: 84
NAM_BASE_URL  = 'http://www.ftp.ncep.noaa.gov/data/nccf/com/nam/prod/nam.'
#NAM_BASE_URL2 = 'http://nomads.ncep.noaa.gov/pub/data/nccf/com/nam/prod/nam.2015011500/'

offhours  = -1
modeltype = 'NAM212'
if 1 == len(sys.argv):
    print('  Uasge: {n} <cycle_hour>'.format(n=sys.argv[0]))
    print('        <cycle_hour>: optional, cycle_hour to be downloaded. default: -1 which downloads the latest NAM model cycle')
    #print('      <model_name>: optional, NAM212, default: NAM212')
    print('')
elif 1 < len(sys.argv):
    offhours  = int(sys.argv[1])
    #if 2 < len(sys.argv):
    #    modeltype = sys.argv[2]

if modeltype == 'NAM212':
    template_prefixm = 'nam.t{h:02}z.awip3d00.tm00.grib2'
else:
    print modeltype, ' is not valid data type to download!'
    sys.exit()

ctimeutc = datetime.utcnow()

urlsite = '{b}{d}'.format(b=NAM_BASE_URL, d=ctimeutc.strftime('%Y%m%d'))
offday = 0
for day_offset in range(0,2):
    ptimeutc = ctimeutc - timedelta(days = day_offset)
    t_urlsite = '{b}{d}/'.format(b=NAM_BASE_URL, d=ptimeutc.strftime('%Y%m%d'))
    try:
        #print('urlsite: {u}'.format(u=urlsite))
        ret = urllib2.urlopen(t_urlsite)
        ret.close()
        offday = day_offset
        break
    except urllib2.HTTPError as e:
        pass

ptimeutc = ctimeutc - timedelta(days = offday)
namdate =  ptimeutc.strftime('%Y%m%d')
urlsite = '{b}{d}/'.format(b=NAM_BASE_URL, d=namdate)
if 0 > offhours:
    for hour_offset in range(23, -1, -1):
        remote_name = template_prefixm.format(h=hour_offset)
        t_urlsite = '{s}{f}'.format(s=urlsite, f=remote_name)
        try:
            #print('urlsite: {u}'.format(u=t_urlsite))
            ret = urllib2.urlopen(t_urlsite)
            ret.close()
            offhours = hour_offset
            break
        except urllib2.HTTPError as e:
            pass

#ptimeutc = ctimeutc - timedelta(hours = offhours)
#namcyc  =  ptimeutc.strftime('%H')
namcyc  =  '{h:02}'.format(h=offhours)

mydate = namdate + namcyc
#print('mydate: {d}'.format(d=mydate))

user_name = os.environ.get('USER','atec4dwx')
localdatadir = '{p}/{u}/datainput'.format(
        p=os.environ.get('PROJECTS_HOME','/scr/projects'), u=user_name)
if not os.path.exists(localdatadir):
    localdatadir = '/scratch/project/k1206/datainput'
datdir = os.path.join(localdatadir, modeltype)
if not os.path.exists(datdir):
    os.makedirs(datdir)

print ('  modeltype: {m}  local_dir: {l}'.format(m=modeltype, l=datdir))
if modeltype == 'NAM212':
    fsizelim = 4000000
    fend = '_tl.press_gr.awip3d'
    prefixm = 'nam.t' + namcyc + 'z.awip3'
else:
    print modeltype, ' is not valid data type to download!'
    sys.exit()

print('  URL: {u}'.format(u= urlsite))
hdop = '--header=Host: www.ftp.ncep.noaa.gov'

try:
    ret = urllib2.urlopen(urlsite)
    ret.close()
except:
    print 'The cycle %s does not exist!'%(urlsite)
    sys.exit()

# download NAM212 forecasts every three hours up to 84 hours to drive WRF 
hr0 = 0
hr1 = MAX_FORECAST_HOURS + 1    # python excludes the endl
hri = 3
for h in range(hr0,hr1,hri):
    chour = '%04d'%h 
    chouro = '%02d'%h

    filer = prefixm + 'd' + chouro + '.tm00.grib2'                     # file name in the remote site 
    filel2 = 'fh.' + chour + fend
    lfile = mydate + '_' + filel2 + '.grib2'  # file name to be saved in local disk
    filel = os.path.join(datdir, lfile)
    print('   hour: {h:02} remote_file: {r}\tlocalfile: {l}'.format(h=h, r=filer, l=lfile))
    fok = 0                                           # check if the file is already available in local disk and if the size is right
    try:
        ret = urllib2.urlopen(urlsite + filer)
        file_size_at_server = int(ret.info().get('content-length', '0'))
        ret.close()
            
        if os.path.isfile(filel):
            fsize = os.path.getsize(filel)
            if fsize == file_size_at_server:
                fok = 1
                print('   Downloaded {f} is OK-ed !'.format(f=filel))
            else:
                cmd = 'rm -f ' + filel
                os.system(cmd)
        if fok == 0:  
            print('   Downloading {f} ...'.format(f=filer))
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
                    print('   Downloading {f} incompleted !'.format(f=filer))
                    cmd = 'rm -f ' + filel
                    os.system(cmd)            # remove incomplete file 
            else:
                print('   Downloading {f} failed !'.format(f=filer))
    except urllib2.HTTPError as e:
        print '%s%s does not exist!'%(urlsite, filer)


# wget http://www.ftp.ncep.noaa.gov/data/nccf/com/nam/prod/nam.20160824/nam.t12z.awip3d81.tm00.grib2 -O /scr/projects/atec4dwx/datainput/NAM212/2016082412_fhpress_gr.awip3d.grib2

