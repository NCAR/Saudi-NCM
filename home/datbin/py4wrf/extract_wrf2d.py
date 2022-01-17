#!/usr/local/env python 
"""
archive selected WRF fields
W. Wu (wanliwu@ucar.edu) May 1, 2013
"""
import os
import sys
import glob 
from datetime import datetime, timedelta

wrfoutdir = '/d2/pmefdda/cycles/GWPME/GRM'   # directory for wrf output 
wrfcycles = ['00','06','12','18']            # WRF model cycle time (UTC)
wrfcycles = '00'            # WRF model cycle time (UTC)
doms = ['d02','d03']                                  # domain to be extracted/archived 
offday = 1                                   # extracting previoys day WRF output  
afend = '*GRM_F'                             # wrf analysis file suffix
ffend = '*GRM_P+FCST'                        # wrf forecast file suffix 
qcout = 'RAP_RTFDDA'
qcend = '*.all.obs.GRM*.gz'
qcobs = 'qc_obs_for_assimilation_s'
wrf2dflds = '/d2/pmefdda/archive'            # directory holds the extracted data 

# slected 2d fields from wrf output 
grp00 = "XLAT,XLONG,XLAT_U,XLONG_U,XLAT_V,XLONG_V"
grp01 = "LU_INDEX,XLAND,IVGTYP,ISLTYP,VEGFRA"
grp02 = "HGT,SST,SSTSK,TSK,T2,Q2,TH2,PSFC,U10,V10,UST,PBLH"
grp03 = "RAINC,RAINNC,SNOW,SNOWH,SNOWNC"
grp04 = "TSLB,SMOIS,SH2O,SFROFF,UDROFF,CANWAT,POTEVP"     
grp05 = "GRDFLX,SWDOWN,GLW,SWNORM,OLR,ALBEDO,EMISS,NOAHRES,HFX,QFX,LH"
grp06 = "PREC_ACC_C,PREC_ACC_NC,SNOW_ACC_NC"
list2da = [grp00, grp01, grp02, grp03, grp04, grp05]
list2df = [grp00, grp01, grp02, grp03, grp04, grp05, grp06]
my2dfldsa = ','.join(list2da)
my2dfldsf = ','.join(list2df)
# delete the following fields:
dgrp01 = "HFX_FORCE,LH_FORCE,TSK_FORCE,HFX_FORCE_TEND,LH_FORCE_TEND,TSK_FORCE_TEND"
dgrp02 = "FNM,FNP,RDNW,RDN,DNW,DN,CFN,CFN1,RDX,RDY,RESM,ZETATOP,CF1,CF2,CF3,ITIMESTEP"
dgrp03 = "MAPFAC_M,MAPFAC_U,MAPFAC_V,MAPFAC_MX,MAPFAC_MY,MAPFAC_UX,MAPFAC_UY,MAPFAC_VX,MF_VX_INV,MAPFAC_VY"
dgrp04 = "E,SINALPHA,COSALPHA,MAX_MSTFX,MAX_MSTFY,STEPAVE_COUNT,AVG_FUEL_FRAC,UAH,VAH,SEED1,SEED2"
listd = [dgrp01, dgrp02, dgrp03, dgrp04]
offflds = ','.join(listd)


#print wrfoutdir 
# previoys day (UTC)
ctimeutc = datetime.utcnow()
ptimeutc = datetime.utcnow() - timedelta(days = 2)
#print ctimeutc, ptimeutc
for hh in wrfcycles:
  cycle    = ptimeutc.strftime('%Y%m%d') + hh
  for dom in doms:

    # WRF analysis files
    files  = 'wrfout_' + dom + afend    
    print files 
    dirfiles = os.path.join(wrfoutdir,cycle,files)
    anafiles = glob.glob(dirfiles)
    print 'number of files: ', len(anafiles)
    for ifile in anafiles:
        print ifile 
        pdir, file = os.path.split(ifile)
        print pdir, file 
        wrfout, fend = file.split('.')
        wrf2df = wrfout + '.nc'
        outdir = os.path.join(wrf2dflds, 'anal', cycle)
        if os.path.isdir(outdir):
           pass
        else:
           cmd = 'mkdir -p %s' %outdir 
           os.system(cmd)
        ofile = os.path.join(outdir, wrf2df)
        if os.path.isfile(ofile):
           cmd = 'rm -f %s' %ofile   
           os.system(cmd)
    #   cmd = '/usr/local/bin/ncks -v %s %s %s' % (my2dfldsa, ifile, ofile)
        cmd = '/usr/local/bin/ncks -x -v %s %s %s' % (offflds, ifile, ofile)
    #   print cmd 
        os.system(cmd)

    # WRF forecast files
    files  = 'wrfout_' + dom + ffend    
    print files 
    dirfiles = os.path.join(wrfoutdir,cycle,files)
    fcsfiles = glob.glob(dirfiles)
    print 'number of files: ', len(fcsfiles)
    for ifile in fcsfiles:
        print ifile 
        pdir, file = os.path.split(ifile)
        print pdir, file 
        wrfout, fend = file.split('.')
        wrf2df = wrfout + '.nc'
        outdir = os.path.join(wrf2dflds, 'fcst', cycle)
        if os.path.isdir(outdir):
           pass
        else:
           cmd = 'mkdir -p %s' %outdir 
           os.system(cmd)
        ofile = os.path.join(outdir, wrf2df)
        if os.path.isfile(ofile):
           cmd = 'rm -f %s' %ofile   
           os.system(cmd)
    #   cmd = '/usr/local/bin/ncks -v %s %s %s' % (my2dfldsf, ifile, ofile)
        cmd = '/usr/local/bin/ncks -x -v %s %s %s' % (offflds, ifile, ofile)
    #   print cmd 
        os.system(cmd)
    # OBS data sets 
        obsdir = os.path.join(wrf2dflds, 'obs', cycle)
        if os.path.isdir(obsdir):
           pass
        else:
           cmd = 'mkdir -p %s' %obsdir 
           os.system(cmd)
        dirfiles = os.path.join(wrfoutdir,cycle,qcout.qcend)
        obsfiles = glob.glob(dirfiles)
        cmd = 'cp %s %s' %(obsfiles, obsdir + '/'')
        os.system(cmd)
        dirfiles = os.path.join(wrfoutdir,cycle,qcobs)
        obsfiles = glob.glob(dirfiles)
        cmd = 'cp %s %s' %(obsfiles, obsdir + '/'')
        os.system(cmd)
