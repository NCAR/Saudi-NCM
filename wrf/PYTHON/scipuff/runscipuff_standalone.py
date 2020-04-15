#!/usr/bin/env python2.7
'''run scipuff program with medoc files prepared from WRF output
  1. extract release and model domain information from a given XML file
  2. copy or soft-link data files
  3. run scipuff
  4. run surface dosage resampling onto WRF grid
  5. merge dosage files into WRF output
  to run the program: the following files shall be ready in the running direcotry:
     template namelists:
         template.inp
         template.msc
         template.scn 
     release file:
         ex_type-a_v.erl
     xml file that defines parameters:
         scipuff_example.xml
  ./runscipuff_standalone.py -h      for help 
  execute the program:
  ./runscipuff_standalone.py -r /ngic/fddasys/sheuGCW106/runscipuffVXMY -x scipuff_exampleW.xml -l ex_type-a_v.rel -p /ngic/fddasys/sheuGCW106
                       run directory                              xml file                release file       path to SCIPUFF home

  Requirements: Python 2.7.5 or later 

  W. Wu (wanliwu@ucar.edu) May 19 2015
  W. Wu (wanliwu@ucar.edu) August 4-31 2015 Revised
  W. Wu (wanliwu@ucar.edu) Sept 15-25  2015 Revised'''

import os
import sys
import fileinput 
from datetime import datetime, timedelta
from glob import glob
import fileinput 
import argparse
from module4scipuff import datefromxml, domaininfo, releaseinfo, infofromxml, \
                           readnml, writenml, updateinp, updatescn, xml2parameters  

parser = argparse.ArgumentParser(description='stand alone scipuff')

parser.add_argument('-p', dest='shomedir', type=str, default='',
                    help = 'SCIPUFF HOME directory')        
parser.add_argument('-r', dest='rundir', type=str, default='',
                    help = 'run directory')
parser.add_argument('-x', dest='xml', type=str, default='', 
                    help = 'xml file')
parser.add_argument('-l', dest='rlsfile', type=str, default='', 
                    help = 'release file')
parser.add_argument('-t', dest='deltat', type=str, default='', 
                    help = 'timestep in seconds')
parser.add_argument('-s', dest='dtsave', type=str, default='', 
                    help = 'dose output frequency in seconds')

args = parser.parse_args()


SCIPUFF_HOME = args.shomedir.strip()               # path (tip directory) to scipuff program 
runpath      = args.rundir.strip()                 # run direcotry 
xmlfile      = args.xml.strip()                    # xml file 
relfile      = args.rlsfile.strip()                # release file 
deltats      = args.deltat.strip()                 # timestep in seconds 
dtsave       = args.dtsave.strip()                 # dose output frequency in seconds 
if deltats == '':
   deltats = 0.                                    # use default timestep in namelist: template.inp
else:
   deltats = float(deltats)                        # overwite the timestep in namelist: template.inp with this delta T
if dtsave == '':
   dtsave = 0.                                     # use default output frequency in namelist: template.inp
else:
   dtsave = float(dtsave)                          # overwite the output frequency in namelist: template.inp with this dt_save 

print ('SCIPUFF_HOME: ', SCIPUFF_HOME)
print ('run directory: ', runpath)
print ('xmlfile: ', xmlfile)
print ('relfile: ', relfile)

os.chdir(runpath)
print ('current directory: %s'%os.getcwd())

# 1. extract release and model domain information from a given XML file

startdate, enddate, reltype ,\
ihour, mduration, \
lonmin, latmin, lonmax, latmax, \
domainid, medocdir, medocf, geof = xml2parameters(xmlfile, relfile) 
print (startdate, enddate, reltype)
print (ihour, mduration)
print (lonmin, latmin, lonmax, latmax)
print domainid, medocdir, medocf, geof

# 2. copy constant files  
os.system('cp %s .'%os.path.join(SCIPUFF_HOME, 'config', 'scipuff.ini'))
os.system('cp %s .'%os.path.join(SCIPUFF_HOME, 'config', 'landuse.dat'))
os.system('cp %s .'%geof)

# executables to run scipuff and to merge dosage files into WRF 
SCIPUFF_EXE = os.path.join(SCIPUFF_HOME, 'bin', 'runsci')
RESAMPLE_EXE = os.path.join(SCIPUFF_HOME,'bin', 'sfc_dosage_resample_nc.exe')
NCKS = '/opt/nco-4.2.1-gcc/bin/ncks'

inpnml = './template.inp'
inpdict = readnml(inpnml)          # read in template namelist and put it in python dictionary
outnml = '%s.inp'%reltype
outfilename = 'medoc.txt'                               # file name for concated medoc files

begyear = int(startdate.year)
endyear = int(enddate.year)
begmon = int(startdate.month)
endmon = int(enddate.month)
begday = int(startdate.day)
endday = int(enddate.day)
print 'For the period: %s %d to %s %d'%(startdate.strftime("%B"), begday, enddate.strftime("%B"), endday)
# run scipuff year by year 
year0 = begyear
while year0 <= endyear:
      fstdate = datetime(year0, begmon, begday)
      if endmon < begmon:                                     # specified period crossing the new year day
         year0 += 1
      lstdate = datetime(year0, endmon, endday)
      print ('update namelist inp: %s for %s to %s'%(outnml, fstdate, lstdate))
      inpdict = updateinp(inpdict, fstdate, lstdate, ihour, mduration, \
                       lonmin, latmin, lonmax, latmax, deltats, dtsave)        # update namelist
      writenml(inpdict, outnml)                               # write out namelist 
      # concat medoc files
      cdate = fstdate + timedelta(hours=ihour)                # start time
      lastdate = lstdate + timedelta(hours=ihour)             # end time 

      medocfiles = [os.path.join(medocdir,'header.txt')]      # take the header file first
      while cdate <= lastdate:                                # collect medoc file names for the period
            fname = 'medoc_d%02d_'%domainid + datetime.strftime(cdate,'%Y-%m-%d_%H')     # medoc_d03_2000-05-01_15
            medocfiles.append(os.path.join(medocdir,fname))
            cdate = cdate + timedelta(hours=1)

      with open(outfilename, 'w') as fout:                    # concat files 
           for line in fileinput.input(medocfiles):
               fout.write(line)

      fchar = '@%03d'%len(outfilename) + outfilename          # this line needs to be in namelist MSC
      fnml = '%s.msc'%reltype
      print ('update namelist msc: %s'%fnml)
      if os.path.isfile(fnml):
         os.remove(fnml)
      with open(fnml, 'w') as fout:
           for line in fileinput.input('tmp.msc'):
               fout.write(line)
           fout.write(fchar)                                  # append this special line to the end of the MSC
      # 3. run scipuff
      mycmd = '%s -I:scipuff.ini -P:%s'%(SCIPUFF_EXE,reltype)
      print ('%s'%mycmd)
      os.system(mycmd) 
  
      # 4. run surface dosage resampling onto WRF grid
  
      os.system('rm -f *.dos_*') 
      mycmd = '%s -dos %s.dos -geo %s'%(RESAMPLE_EXE,reltype,geof)
      print ('%s'%mycmd)
      os.system(mycmd) 

      # 5. merge dosage files into WRF output
      wrfdir =  os.path.join(SCIPUFF_HOME, 'WRF')
      fdos = glob('%s.dos_*'%reltype)             # dosage files (netcdf) from resampling program above 
      fdos = sorted(fdos)
      ndos = len(fdos)
      if ndos > 0: 
         cdate = fstdate
         dosintv = 1                              # hourly dos output, so does WRF
         n = 0
         while n < ndos:
               fd = fdos[n]
               cdate = cdate + timedelta(hours=dosintv)
               year, month, day, hour = int(cdate.year), int(cdate.month), int(cdate.day), int(cdate.hour) 
               tmstamp = '%04d-%02d-%02d_%02d'%(year, month, day, hour)
               fwrf = 'wrfout_d03_%s:00:00.GRM_F'%tmstamp
               nwrf = fwrf[0:-12] + '.nc'
               newfd = fd.split('_')[0] + '.%s.nc'%tmstamp         # add time stamp to dosage files
        #      print tmstamp, fwrf, nwrf, fd, newfd  
               os.rename(fd, newfd)
               # add XLAT, XLONG to dosage files
               mycmd = '%s -A -v XLAT,XLONG %s %s'%(NCKS, os.path.join(wrfdir, fwrf), newfd)
               os.system(mycmd)
               # make a copy of wrfout file
               mycmd = '%s -O %s %s'%(NCKS, os.path.join(wrfdir, fwrf), fwrf)    # keep original wrf file name
               print (os.path.join(wrfdir, fwrf))
               print (mycmd)
               os.system(mycmd)
               # merge dosage file to wrfout just copied 
               mycmd = '%s -A -v DOSAGE %s %s'%(NCKS, newfd, fwrf)
               print (mycmd)
        #      print 'no merge yet: %s'%mycmd 
               os.system(mycmd)
               n += 1
      year0 += 1
