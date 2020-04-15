#!/sw/xc40cle6/python/2.7.14/sles12.3_gnu7.2.0/bin/python
'''update wps/wrf namelist: namelist.wps or namelist.input
   edit namelist.input based on an template one 
   usage: python updatewrfnml.py -h  for help 

   WWU wanliwu@icar.edu Nov. 05 2013 
   WWU wanliwu@ucar.edu Dec. 20 2013 Revised 
   WWU wanliwu@ucar.edu Sept 02 2014 Revised 
   WWU wanliwu@ucar.edu Sept 12 2014 Revised 
'''
import os
import sys
from datetime import datetime, timedelta
import argparse              # require python2.7 or later 
from wpylib.nmlbuilder import nmlupdate

if __name__ == '__main__':

   parser = argparse.ArgumentParser(description='update WPS namelist')
   parser.add_argument('-i', dest='nmlin', type=str, default='./namelist.wps', \
                       help = 'input: namelist.wps or namelist.input')
   parser.add_argument('-o', dest='nmlout', type=str, default='', \
                       help = 'output file: namelist.wps or namelist.input.')
   parser.add_argument('-s', dest='secname', type=str, default=None, help = 'namelist name &name')
   parser.add_argument('-k', dest='varname', type=str, default=None, help = 'namelist varaible')
   parser.add_argument('-v', dest='varvalue', type=str, default=None, help = 'varaible values: v1,v2,v3')
   parser.add_argument('-N', dest='domains', type=int, default=None, \
                       help = 'number of domains.')
   parser.add_argument('-n', dest='domain', type=int, default=None, \
                       help = 'specific domain to be updated')
   parser.add_argument('-b', dest='starttime', type=str, default='', help = 'WPS/WRF start time:ccyymmddhh')
   parser.add_argument('-e', dest='endtime', type=str, default='', help = 'WPS/WRF end time:ccyymmddhh')
   parser.add_argument('-f', dest='fhours', type=str, default=None, help = 'WPS/WRF forecasting hours')
   parser.add_argument('-r', dest='rpc', type=str, default='', help = 'string to be replaced')

   tmpnmldir = os.getcwd()
   print 'Current directory: %s for namelist update.'%tmpnmldir

   args = parser.parse_args()

   numdoms = args.domains          # max_dom
   ndom = args.domain              # specific domain

   starttime = args.starttime
   endtime = args.endtime
   fcsthours = args.fhours
   fnmlin = args.nmlin.strip()
   fnmlout = args.nmlout.strip()

   nmlname = args.secname
   varname = args.varname
   vals = args.varvalue

   rpc = args.rpc 

   if fnmlin and numdoms:
      nmlupdate(infile=fnmlin, mxdoms=numdoms)
   if starttime:
      nmlupdate(starttime=starttime)
      if fcsthours and not endtime:           # endtime would over write fcsthours if both were given
         mstarts = starttime.strip().strip(',').split(',')
         mstarts = filter(None,mstarts)
         mfhours = fcsthours.strip().strip(',').split(',')
         mfhours = filter(None,mfhours)
         while len(mstarts) < len(mfhours):
            mstarts.append(mstarts[-1])
         endtime = ''
         for ms, mf in zip(mstarts,mfhours):
             mytime = datetime.strptime(ms,"%Y%m%d%H") + timedelta(hours=int(mf))
             endtime += ',%s'%mytime.strftime("%Y%m%d%H")
         endtime = endtime.strip(',')
      nmlupdate(starttime=starttime,endtime=endtime)

   if nmlname and varname:
      if ndom:
         nmlupdate(nmlname=nmlname,nmlvar=varname,varvalues=vals,domn=ndom)
      else:
         nmlupdate(nmlname=nmlname,nmlvar=varname,varvalues=vals)
   if rpc: 
      if nmlname and varname:
         nmlupdate(nmlname=nmlname,nmlvar=varname,rpc=rpc)
      elif nmlname:
         nmlupdate(nmlname=nmlname,rpc=rpc)
      else:
         nmlupdate(rpc=rpc)
         
   if fnmlout: nmlupdate(outfile=fnmlout) 
