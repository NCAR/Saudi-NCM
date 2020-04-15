import os
import sys
import tarfile 
import re
import calendar
import shutil
from datetime import datetime, timedelta
from urllib2 import urlopen, HTTPError
from forsh import runcmd

def dateblk(year = 2010, month = 1, day = 1, blkint = 5):   
# get block date that the data on this give day belongs to

# first and last day of the month/year
    tlist = [year, month, day, blkint]
    year, month, day, blkint = map(int, tlist)

    if day == 31:
       startblk = datetime(year,month,26)
       endblk = datetime(year,month,31) 
       return startblk.strftime('%Y%m%d'), endblk.strftime('%Y%m%d'),endblk + timedelta(days = +1)
       
    fstday, endday = calendar.monthrange(year,month)
    m5 = day%blkint
    if m5 == 0: m5 = blkint
    m5 -= 1
    startblk = datetime(year,month,day) - timedelta(days = m5)
    if int(day) < 26:
        m5 = blkint - 1
        endblk = startblk + timedelta(days = m5)
    else:
        endblk = datetime(year,month,endday)

    return startblk.strftime('%Y%m%d'), endblk.strftime('%Y%m%d'), endblk + timedelta(days = +1)

class cfsdata():
    filehd  = ['cfs.upr', 'cfs.sfc', 'cfs.spl']
    datahh  = ['00', '06', '00']
    cfsstatus = 'OK' 

    def __init__(self,
                 uprdat = True,
                 sfcdat = False,
                 ldir   = '/tmp/$USER',
                 cfsdir = '/glade/p/rda/data', 
                 ldisk  = True, 
                 rdate  = datetime.strptime('20110331',"%Y%m%d"), 
                 delay  = 8,
                 intval = 6,
                 cfsmem = 1, 
                 hpssbin = ''):

        self.upr = uprdat 
        self.sfc = sfcdat
        self.udir = ldir  
        self.dir = cfsdir
        self.disk = ldisk 
        self.rdat = rdate  + timedelta(days=1)       # first day of cdas data 
        self.delay = delay
        self.intval = intval
        self.mem = '%02d'%cfsmem   
        self.hsi = hpssbin + 'hsi'
        self.htar = hpssbin + 'htar'

        if not self.upr: 
           self.datahh[0] = '06'  
        if not self.sfc:                             # no surface analysis required
           self.filehd.pop()
           self.datahh.pop()

        if not self.disk:                            # inquiry data from HPSS
           self.tmpdir = self.dir
           self.dir    = '/DSS'                      # NCAR data archive HPSS
           self.wtedf = 'wantedfiles.txt'
           self.tmpfile = 'mycfs.tar'

    def remotegdas(self,
             sdate = datetime.strptime('20100105',"%Y%m%d"),
             edate = datetime.strptime('20100107',"%Y%m%d")): 

        blkint = 5    # cfs reanalysis files are tar-ed as 5-days block

        atmflds = ['pgbhnl','flxf06','splanl']
        fend = 'grb2'

        if not self.upr: 
           atmflds[0] = 'pgbh06'      # take 6-hour forecast instead of analysis  

        cdate = sdate -timedelta(days=1)   # ensure complete sfc data (6-hours forecast)
        while cdate <= edate:
            cymd    = cdate.strftime('%Y%m%d')
            yy, mm, dd = cdate.strftime('%Y'), cdate.strftime('%m'), cdate.strftime('%d')

            startblk, endblk, cdate = dateblk(year = yy, month = mm, day = dd, blkint = blkint)
            for atmfld, fhead, dhour in zip(atmflds, self.filehd, self.datahh):
                cfsfile = atmfld + '.gdas.' + startblk + '-' + endblk + '.tar'
                if int(yy) <= 2010:
                   cfsfile = self.dir + '/ds093.0/' + yy + '/' + cfsfile
                else:
                   cfsfile = self.dir + '/ds094.0/' + yy + '/' + cfsfile
                   hsicmd = '%s "ls %s"'%(self.hsi,cfsfile)
                   (out,err) = runcmd(hsicmd)                # check if the file is availale 
                   if re.search(r'No such file or directory', out):
                      print '%s is not available!' %cfsfile
                      return 'F', cdate
                hsicmd = '%s "cd %s; rm my*tar; '%(self.hsi,self.tmpdir) + 'ln -s %s %s"'%(cfsfile, self.tmpfile)
                (out,err) = runcmd(hsicmd)
                lfile = os.path.join(self.tmpdir, self.tmpfile)
                print 'HTAR index building may take a while...'
                print 'HTAR: Build Index for %s'%cfsfile 
                hcmd = '%s -Xvf %s'%(self.htar,lfile)
                (out,fl) = runcmd(hcmd)             # create file index (file list)
                
                wantednews = []
                wantednams = []
                if not fl: 
                   flist = open(self.wtedf,'w')
                   allfiles = out.split('\n')
                   print 'processing %s' %cfsfile 
                   for m, fname in enumerate(allfiles):
                     print m, fname 
                     if fname.strip().endswith(fend):
                        gdate = re.search(r'\.(\d+)\.',fname)   # pgblnl.gdas.2011033100.grb2 
                        fdate = datetime.strptime(gdate.group(1),"%Y%m%d%H")
                        fdate = fdate + timedelta(hours = int(dhour))
                        if fdate >= sdate and fdate <=  edate + timedelta(days = +1):
                           print 'FOUND: ', fname 
                           slist = fname.strip().split(' ')
                           curname = slist[-1].strip()
                           flist.write('%s\n'%curname)
                           wantednams.append(curname)
                           udir = os.path.join(self.udir,fdate.strftime('%Y'),fdate.strftime('%m'))
                           createdir(udir)
                           newfname = fhead + '.' + fdate.strftime('%Y%m%d%H') + '.grb2'
                           wantednews.append(os.path.join(udir,newfname))
                   flist.close()
# extract needed files only 
                   hcmd = '%s -L%s -xvf %s' %(self.htar,self.wtedf,lfile)
                   out,err = runcmd(hcmd)
                   for fold, fnew in zip(wantednams, wantednews):
                       shutil.move(fold, fnew)
                       print '%s -> %s' %(fold, fnew)
                   out,err = runcmd('rm -f %s %s*'%(self.wtedf, atmfld))
                else:
                   out,err = runcmd('%s "rm lfile"'%self.hsi)
  
            if edate <= datetime.strptime(endblk,"%Y%m%d"): break

        return 'P', cdate


    def remotecdas(self,
             sdate = datetime.strptime('20120105',"%Y%m%d"),
             edate = datetime.strptime('20120107',"%Y%m%d")): 

        atmflds = ['pgrbh', 'sfluxgrbf','splgrbanl']
        fldkeys = ['pgrbhanl', 'sfluxgrbf06', 'splgrbanl']

        if not self.upr: 
           fldkeys[0] = 'pgrbh06'                        # take 6-hour forecast instead of analysis  
        
        fend = 'grib2'

        cdate = sdate - timedelta(days=1)
        if cdate < self.rdat: cdate = self.rdat 
        while cdate <= edate:
            cymd    = cdate.strftime('%Y%m%d')
            yy, mm, dd = cdate.strftime('%Y'), cdate.strftime('%m'), cdate.strftime('%d')

            for atmfld,fldkey,fhead,dhour in zip(atmflds,fldkeys,self.filehd,self.datahh):
                if not atmfld.startswith('sflux') and cdate < sdate: 
                   pass
                else:
                   cfsfile0 = 'cdas1.' + cymd + '.' + atmfld + '.tar'
                   if int(yy) <= 2011:
                      cfsfile = self.dir + '/ds094.0/' + cfsfile0
                      hsicmd = '%s "ls %s"'%(self.hsi,cfsfile)
                      (out,err) = runcmd(hsicmd)                # check if the file is availale 
                      if re.search(r'No such file or directory', out):
                         print '%s is not available!' %cfsfile
                         return 'F', cdate
                   else:
                      cfsfile = self.dir + '/ds094.0/' + yy + '/' + cfsfile0
                      hsicmd = '%s "ls %s"'%(self.hsi,cfsfile)
                      (out,err) = runcmd(hsicmd)
#*** ls: No such file or directory [-2: HPSS_ENOENT]
                      if re.search(r'No such file or directory', out):
                         print '%s is not available!' %cfsfile
                         cfsfile = '/FS' + self.dir + '/ds094.0/' + yy + '/' + cfsfile0
                         hsicmd = '%s "ls %s"'%(self.hsi,cfsfile)
                         (out,err) = runcmd(hsicmd)
                         if re.search(r'No such file or directory', out):
                            print '%s is not available!' %cfsfile
                            print 'We now try CFS forecast at NCEP website for the latest.'
                            initime = findinitime(self.delay,self.intval)
                            fetchcfsforecast(initime, cdate, edate+timedelta(days=1),self.udir, self.mem)
                            return 'P', cdate

                   hsicmd = '%s "cd %s; rm my*tar; '%(self.hsi,self.tmpdir) + 'ln -s %s %s"'%(cfsfile, self.tmpfile)
                   (out,err) = runcmd(hsicmd)
                   lfile = os.path.join(self.tmpdir, self.tmpfile)
                   print 'HTAR index building may take a while...'
                   print 'HTAR: Build Index for %s'%cfsfile 
                   hcmd = '%s -Xvf %s'%(self.htar,lfile)
                   (out,fl) = runcmd(hcmd)             # create file index (file list)
                
                   wantednews = []
                   wantednams = []
                   if not fl: 
                      flist = open(self.wtedf,'w')
                      allfiles = out.split('\n')
                      print 'processing %s' %cfsfile 
                      for m, fname in enumerate(allfiles):
                        print m, fname 
                        if fname.strip().endswith(fend):
                           slist = fname.strip().split(' ')
                           curname = slist[-1].strip()
                           grp1, grp2, grp3, grp4 = curname.split('.')
                           if grp3 == fldkey:
                              print 'FOUND: ', curname
                              flist.write('%s\n'%curname)
                              wantednams.append(curname)
                      flist.close()
# extract needed files only 
                      hcmd = '%s -L%s -xvf %s' %(self.htar,self.wtedf,lfile)
                      (out,err) = runcmd(hcmd)
                      print err, hcmd 
                      frname = filerename(wantednams, fhead, dhour, cdate, self.udir)
                      (out,err) = runcmd('rm -f %s %s*'%(self.wtedf, atmfld))
                   else:
                      (out,err) = runcmd('%s "rm lfile"'%self.hsi)
                      print '%s does not exist in HPSS!' %cfsfile 
                      self.cfsstatus = 'Fail'
                      return 'F', cdate 
  
            cdate = cdate + timedelta(days = +1) 
 
        return 'P', cdate

    def gdas(self,
             sdate = datetime.strptime('20100105',"%Y%m%d"),
             edate = datetime.strptime('20100107',"%Y%m%d")): 
#atmflds = ['flxf06', 'splanl', 'pgbhnl', 'pgbh06']

        blkint = 5    # cfs reanalysis files are tar-ed as 5-days block

        atmflds = ['pgbhnl','flxf06','splanl']

        if not self.upr: 
           atmflds[0] = 'pgbh06'      # take 6-hour forecast instead of analysis  

        cdate = sdate - timedelta(days=1)
        while cdate <= edate:
            cymd    = cdate.strftime('%Y%m%d')
            yy, mm, dd = cdate.strftime('%Y'), cdate.strftime('%m'), cdate.strftime('%d')

            startblk, endblk, cdate = dateblk(year = yy, month = mm, day = dd, blkint = blkint)
            for atmfld, fhead, dhour in zip(atmflds, self.filehd, self.datahh):
                cfsfile = atmfld + '.gdas.' + startblk + '-' + endblk + '.tar'
                fl, cfsfile = checkfileloc(self.dir, cfsfile, yy)  # check file availability
                if fl:
                   print 'processing %s' %cfsfile 
                   uprfile = tarfile.open(cfsfile)
                   uprmems = uprfile.getmembers()
                   uprnams = uprfile.getnames()
                   wantedmems = []
                   wantednams = []
                   wantednews = []
                   for m, fname in enumerate(uprnams):
                     mem = uprmems[m] 
                     gdate = re.search(r'\.(\d+)\.',fname)   # pgblnl.gdas.2011033100.grb2 
                     fdate = datetime.strptime(gdate.group(1),"%Y%m%d%H")
                     fdate = fdate + timedelta(hours = int(dhour))
                     if fdate >= sdate and fdate <=  edate + timedelta(days = +1):
                        print 'FOUND: ', fname 
                        wantedmems.append(mem)
                        wantednams.append(fname)
                        udir = os.path.join(self.udir,fdate.strftime('%Y'),fdate.strftime('%m'))
                        createdir(udir)
                        newfname = fhead + '.' + fdate.strftime('%Y%m%d%H') + '.grb2'
                        wantednews.append(os.path.join(udir,newfname))
# untar needed files only 
                   uprfile.extractall(path=os.getcwd(),members=wantedmems)
                   for fold, fnew in zip(wantednams, wantednews):
                       shutil.move(fold, fnew)
                       print '%s -> %s' %(fold, fnew)
                else: 
                    print 'CFS analysis file  does not exist!'
                    return 'F', cdate
  
            if edate <= datetime.strptime(endblk,"%Y%m%d"): return 'P', cdate

    def cdas(self,
             sdate = datetime.strptime('20110705',"%Y%m%d"),
             edate = datetime.strptime('20110707',"%Y%m%d")): 

        atmflds = ['pgrbh', 'sfluxgrbf','splgrbanl']
        fldkeys = ['pgrbhanl', 'sfluxgrbf06', 'splgrbanl']

        if not self.upr: 
           fldkeys[0] = 'pgrbh06'         # take 6-hour forecast instead of analysis  

        cdate = sdate - timedelta(days=1)
        if cdate < self.rdat: cdate = self.rdat 
        while cdate <= edate:
            cymd    = cdate.strftime('%Y%m%d')
            yy, mm, dd = cdate.strftime('%Y'), cdate.strftime('%m'), cdate.strftime('%d')

            for atmfld,fldkey,fhead,dhour in zip(atmflds,fldkeys,self.filehd,self.datahh):
                if not atmfld.startswith('sflux') and cdate < sdate: 
                   pass
                else:
                  cfsfile = 'cdas1.' + cymd + '.' + atmfld + '.tar'
                  fl, cfsfile = checkfileloc(self.dir, cfsfile, yy)
                  if fl:
                     print 'processing %s' %cfsfile 
                     uprfile = tarfile.open(cfsfile)
                     uprmems = uprfile.getmembers()
                     uprnams = uprfile.getnames()

                     wantedmems = []
                     wantednams = []
                     for m, fname in enumerate(uprnams):
                       mem = uprmems[m] 
                       grp1, grp2, grp3, grp4 = str(fname).split('.')
                       if grp3 == fldkey:
                          print 'FOUND: ', fname 
                          wantedmems.append(mem)
                          wantednams.append(fname)
# untar needed files only 
                     uprfile.extractall(path=os.getcwd(),members=wantedmems)
                     frname = filerename(wantednams, fhead, dhour, cdate, self.udir)
                     print 'file name change: %s' %frname 

                  else: 
                     print '%s does not exist in the local disk!' %cfsfile 
                     self.cfsstatus = 'Fail'
                     break 
  
            if self.cfsstatus == 'Fail': break 
            cdate = cdate + timedelta(days = +1) 
        if self.cfsstatus == 'Fail':
           print 'We now try CFS forecast at NCEP website for the latest.'
           initime = findinitime(self.delay,self.intval)
           fetchcfsforecast(initime, cdate, edate+timedelta(days=1),locdir, self.mem)
        return 'P', cdate

def filerename(fileset, fldkey, h06, cdate, ldir):

# rename files for WRF/WPS/ungrib convention for a give day  
# define by cdate. Original files have initial hours, but not
# necessarily have time-stamp on it. New file names will have time-stamp
# associated with the actual time of the data. For example,
# 2010 04 05 12UTC initalized 6-hours forecast will bear time-stamp of
# 2010040518

    print 'insider filerename: ',  fileset, fldkey, h06, cdate, ldir
    print 'cdate: ', cdate 
    hh = int(cdate.strftime('%H'))
    
    frname = 'OK'
    for file in fileset:
        if os.path.isfile(file):     # cfs cdas file name convention:
                                     # cdas1.t00z.pgrbhanl.grib2
            itime = re.search(r't(\d+)z',file).group(1)       # data initial hours (UTC: hh)
            if itime:
                dt = int(itime) + int(h06) - hh
            else:
               print 'incorrect file name %s' %file
               frname = 'Fail'
               sys.exit ()
            sfcutc = cdate + timedelta(hours = dt) 
            strn = sfcutc.strftime('%Y%m%d%H')

            udir = os.path.join(ldir,sfcutc.strftime('%Y'),sfcutc.strftime('%m'))
            createdir(udir)
            filen = fldkey + '.' + strn + '.grb2'
            filen = os.path.join(udir,filen)

            print file, '->', filen 
            shutil.move(file, filen)
        else:
            frname = 'Fail'
    return frname 

def checkfileloc(fdir, file, year ):
    
    if os.path.isfile(os.path.join(fdir,file)):
        return True, os.path.join(fdir,file)
    elif os.path.isfile(os.path.join(fdir,'ds094.0',file)): 
        return True, os.path.join(fdir,'ds094.0',file)
    elif os.path.isfile(os.path.join(fdir,'ds094.0',str(year),file)): 
        return True, os.path.join(fdir,'ds094.0',str(year),file)
    elif os.path.isfile(os.path.join(fdir,'ds093.0',str(year),file)): 
        return True, os.path.join(fdir,'ds093.0',str(year),file)
    else:
        print 'need to try CFS website for %s'%file 
        return False, file

def fetchcfsforecast(initime, now, endtime,ldir, mem = '01'):

    print 'CFS ensemble member %d forecasts initialized at %s to be downloaded.'%(int(mem),initime)
    fcstint = 6         # CFS forecast output frequency in hours 
    fdic = dict()
    ufiles = []
    sfiles = []
    while now <=  endtime:
          validtime = now.strftime("%Y%m%d%H")
          ruprfile = 'pgbf%s.%s.%s.grb2'%(validtime,mem,initime)
          luprfile = 'cfs.upr.%s.grb2'%validtime
          rsfcfile = 'flxf%s.%s.%s.grb2'%(validtime,mem,initime)
          lsfcfile = 'cfs.sfc.%s.grb2'%validtime
          ndir = os.path.join(ldir,now.strftime("%Y"),now.strftime("%m"))
          createdir(ndir)           # create directory: ldir/ccyy/mm
          luprfile = os.path.join(ndir,luprfile)
          lsfcfile = os.path.join(ndir,lsfcfile)
          ufiles.append((ruprfile,luprfile))
          sfiles.append((rsfcfile,lsfcfile))
          now = now + timedelta(hours = fcstint)
    fdic ['upr'] = ufiles
    fdic ['sfc'] = sfiles
    url = 'http://nomads.ncep.noaa.gov/pub/data/nccf/com/cfs/prod/cfs/\
           cfs.%s/%02d/6hrly_grib_%s/'%(initime[0:8],int(initime[8:10]),mem)
    url = url.replace(' ','')
    print 'URL: %s'%url
    print 'Downloaded files are in: %s'%ldir

    for u, v in fdic.iteritems():
        for n,f in enumerate(v):
            rf, lf = f[0], f[1]
            try:   # check remote file availability    
                req = urlopen(url+rf)
                print '%s is available'%rf
                cmd = "lftp -e 'set net:timeout 10; get -e %s -o %s; bye'"%(url+rf,lf)
                out, err = runcmd(cmd)
                if err != None:
                   print err
                   print out
                else: print 'Downloaded as %s.'%lf.split('/')[-1]
            #lftp -e 'set net:timeout 10; get yourfile.mp4 -o /local/path/yourfile.mp4; bye' -u user,password ftp.foo.com
            except HTTPError:
                print '%s is not found'%rf
                v.pop(n)

def findinitime(cfsdelay, cfsint):
    curtime = datetime.utcnow()
    bkhrs = curtime-timedelta(hours=cfsdelay) 
    hh = int(bkhrs.strftime('%H'))
    while hh % cfsint:
          bkhrs = bkhrs - timedelta(hours=1)
          hh -= 1
    return bkhrs.strftime('%Y%m%d%H')

def createdir(newdir):
   if not os.path.exists(newdir):
      print "%s does not exist. let's create it."%newdir
      os.makedirs(newdir)

def cymdh2date(cymdh):

    if len(cymdh) < 8:
       print 'date should be in format of CCYYMMDD or CCYYMMDDHH'
       print 'but your date is in form of %s'%cymdh
       sys.exit()
    elif len(cymdh) == 8:
       return datetime.strptime(cymdh,'%Y%m%d')
    elif len(cymdh) == 10:
       return datetime.strptime(cymdh,'%Y%m%d%H')
