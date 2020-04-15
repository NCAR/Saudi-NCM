#!/usr/bin/env python2.7
'''Download 1/4 DEG GLDAS data from 
   ftp://agdisc.gsfc.nasa.gov/data/s4pa/GLDAS_SUBP/GLDAS_NOAH025SUBP_3H
   for WRF soil field initialization.
   The GLDAS data are in grib (1) format and soil moisture is in the 
   unit of Kg/m^2, which is not consistent with WRF requirement of 'fraction'.
   This program download grib file then converts it into netcdf so that
   anothe progran (fortran) does unit conversion and write it out in WRF/WPS
   intermidate format for metgrid.exe program.

   Requirements: python2.7 or later, lftp, ncl_convert2nc
   Tested on yellowstone, ngic-c2 
   Usage: ./gldasinquiry -h 

   WWU (wanliwu@ucar.edu) Feb. 2014, revised in Apr. 2014 
'''

import os
import sys
from datetime import datetime, timedelta
import glob
import argparse
import shutil
import subprocess

def runcmd(mycmd):
    p = subprocess.Popen(mycmd, shell=True,
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT)
    return p.communicate()

parser = argparse.ArgumentParser(description='require GLDAS data from archives')
parser.add_argument('-b', dest='startdate', type=str, default='',
                    help = 'start date ccyymmdd')
parser.add_argument('-e', dest='enddate', type=str, default='', 
                    help = 'end   date ccyymmdd')
parser.add_argument('-d', dest='locdir', type=str, default='./', 
                    help = 'local directory to hold downloaded GLDAS data')

args = parser.parse_args()
if len(args.startdate) < 10:
   sdate   = datetime.strptime(args.startdate,"%Y%m%d")
else:
   sdate   = datetime.strptime(args.startdate,"%Y%m%d%H")
if len(args.enddate) < 10:
   edate   = datetime.strptime(args.enddate,"%Y%m%d")
   edate = edate + timedelta(days=1)
else:
   edate   = datetime.strptime(args.enddate,"%Y%m%d%H")

locdir = args.locdir + '/025deg/data'

datasite = 'ftp://agdisc.gsfc.nasa.gov/data/s4pa/GLDAS_SUBP/GLDAS_NOAH025SUBP_3H'
#filepref = 'GLDAS_NOAH025SUBP_3H.ACCYYJDAY.HH00.001.*.grb'
filepref = 'GLDAS_NOAH025SUBP_3H.A'

fmt = '%Y%m%d'

NCARG_ROOT = os.environ['NCARG_ROOT']
print 'NCARG_ROOT = ',NCARG_ROOT
# grib to netcdf converting command :
nclcmd = 'export NCARG_ROOT=' + NCARG_ROOT + '; ' + 'export PATH=.:' + NCARG_ROOT + '/bin:$PATH; ' + ' echo $PATH; ' + NCARG_ROOT+'/bin/ncl_convert2nc'
# nclcmd = '$NCARG_ROOT/bin/ncl_convert2nc'
# nclcmd = '/opt/ncl_ncarg-6.2.0-gcc/bin/ncl_convert2nc'
print 'nclcmd: ', nclcmd 
#nclcmd = '/glade/u/apps/opt/ncl/6.2.0/gnu/4.7.2/bin/ncl_convert2nc'
# selected fields to be converted:
neededflds = 'WEASD_GDS0_SFC,TSOIL_GDS0_DBLY,SOIL_M_GDS0_DBLY,g0_lat_0,g0_lon_1,lv_DBLY2_l1,lv_DBLY2_l0'

nfperday = 8   # expect 8 files per day (three-hourly)
nfperday = 4   # expect 8 files per day (three-hourly subsampled to 6-hourly)

curdir = os.getcwd()
print 'Current diectory: %s'%curdir 
#NOAH025SUBP.2013120115.grib
print 'GLDAS data will be in %s with name convention of NOAH025SUBP.ccyymmddhh.nc'%locdir 
cdate = sdate
while cdate <= edate:
      mydate = cdate.strftime('%Y%m%d')

      outdir = os.path.join(locdir, cdate.strftime('%Y'))
      outdir = os.path.join(outdir, cdate.strftime('%m'))
    
      if not os.path.isdir(outdir):  # check if local directory exists
         os.makedirs(outdir)
         lfs = 0 
      else:
         # check if file avaiale in local disk: outdir
#        lfs = 'NOAH025SUBP.' + mydate + '*.grib'
         lfs = 'NOAH025SUBP.' + mydate + '*.nc'
         lfiles = os.path.join(outdir,lfs)
         lfs = len(glob.glob(lfiles))

      if lfs < nfperday:   # download files from the web
         print 'downloading GLDAS data on %s from the web ...'%mydate

      # convert date into julian day (GLDAS data are labled in julian day)
         dt = datetime.strptime(str(mydate), fmt)
         y,d = dt.timetuple().tm_year, dt.timetuple().tm_yday
         jday = '%04d%03d' % (y, d)
         if int(jday) < 2000055:
            print 'GLDAS 025DEG data start on 2000055 (julian day)'
            print '%s you are requiring is earlier than 2000055'%jday
            print 'please check your dates for data inquiry.'
         else:
            fpath = datasite + '/%04d/%03d'%(y, d)
            fname = filepref + '%04d%03d'%(y,d) + '.*.grb'
            rfile = os.path.join(fpath,fname)
            cmd = 'rm -f %s*'%filepref
            out, err = runcmd(cmd)
            cmd = "lftp -e 'set net:timeout 10; mget -e %s -O %s; bye'"%(rfile,curdir)
            out, err = runcmd(cmd)

            # rename downloaded file in WRF/WPS file convention 
            fname = os.path.join(curdir,fname)
            allfiles = glob.glob(fname)
            print 'grib to netcdf conversion...'

            for fd in sorted(allfiles):
                f = fd.split('/')[-1]
                fp = f.split('.')
                fdate = fp[1][1:] + fp[2][0:2]
                fdate = datetime.strptime(fdate, '%Y%j%H').strftime('%Y%m%d%H')
                f  = fp[0].split('_')[1] + '.' + fdate + '.grib'
                fc = fp[0].split('_')[1] + '.' + fdate + '.nc'
                fc = os.path.join(outdir,fc)
                shutil.move(fd, f)
                # grib to netcdf:
                if not os.path.isfile(fc):  # check if local file exists
                #  mycmd = '%s %s -i %s -o %s -v %s'%(nclcmd, f, outdir, outdir, neededflds)
                   mycmd = '%s %s -o %s -v %s'%(nclcmd, f, outdir, neededflds)
                   print mycmd 
                   out, err = runcmd(mycmd) # grib to netcdf 
                   print out
                   print err 
                if os.path.isfile(fc): 
                   print 'Successful conversion of GRIB file '+f+' into CDF file '+fc
                   mycmd = 'rm -f %s'%f
                   out, err = runcmd(mycmd) # remove grib file once the converting is OK-ed. 
      else: print 'GLDAS data on %s is available on the local disk already ...'%mydate

      cdate = cdate + timedelta(days=1)
