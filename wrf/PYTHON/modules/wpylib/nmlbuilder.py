# coding: utf-8
import os
import sys
import glob
import re
from collections import OrderedDict
from datetime import datetime, timedelta
import textwrap
import cPickle

def rnmlvarfromregistry(wrfregistry, wrfcore):

    wrfvarlist = []
    wrfvartype = []
    wrfnmlvar = OrderedDict()
    for rcfile in glob.glob(os.path.join(wrfregistry,'*')):
        if wrfcore == 'EM' or wrfcore == 'ARW':
           v = re.search(r'\.NMM', rcfile)
        elif wrfcore == 'NMM':
           v = re.search(r'\.EM', rcfile)
        if v:
           pass
        else:
           print rcfile
           with open(rcfile,'r') as rf: 
                for sline in rf:
                    m = re.search(r'namelist,\w+',sline)
                    if sline.strip().lower().startswith('rconfig') and m:
                       sline = sline.replace('\t',' ')
                       partslist = sline.strip().split(' ')
          #    print len(partslist)
                       partslist = filter(None, partslist)
              
              #    if 'max_domains' in partslist:
              #   print partslist
                       nml, vgrp = m.group(0).lower().split(',') 
                       if vgrp not in wrfnmlvar.keys(): wrfnmlvar[vgrp.strip().lower()] = OrderedDict()
                       if partslist[2].strip().lower() not in wrfnmlvar[vgrp].keys():
                          doms = 1
                          if 'max_domains' in partslist: doms = 0
                          wrfnmlvar[vgrp.strip().lower()][partslist[2].strip().lower()] = (
                             partslist[1].strip().lower(),
                             str(doms), 
                             partslist[5].strip().lower())
    return wrfnmlvar

def wnmldict2txt(wrfregistry, wrfcore, wrfnmlvar, listfile):
    '''write WRF nml dictionary into text file'''
    with open(listfile,'w') as fout:
         fout.write('List of WRF namelist variables built from %s for %s dynamics core\n'%(wrfregistry, wrfcore))
         for state in sorted(wrfnmlvar.keys()):
             fout.write('%s %d\n'%(state,len(wrfnmlvar[state].keys())))
             for n, ks in enumerate(sorted(wrfnmlvar[state].keys())):
                 vtype, dom, vv = wrfnmlvar[state][ks]
                 fout.write('%5d %30s %4d %12s %s\n'%(n+1,ks,int(dom),vtype,vv))                      

def buildnmlvarlist(wrfregistry, wrfcore, listfile):
    '''build a list of WRF nml variables based on WRF Registry
       wrfregistry: WRF Registry directory
       wrfcore    : WRF core (ARW/EM or NMM)
       listfile   : text file for the variable list '''
    wrfnmldict = rnmlvarfromregistry(wrfregistry, wrfcore)
    outf = wnmldict2txt(wrfregistry, wrfcore, wrfnmldict, listfile)

# read in the list of WRF namelist variables, and put them into a dictionary
def rnmltxt2dict(varlistfile):
    wrfnmldict = OrderedDict()
    with open(varlistfile,'r') as fnmlvar:
         for aline in fnmlvar:
             if aline.strip().startswith('List'): continue
             lineitems = aline.strip().split(' ')
             lineitems = filter(None,lineitems)
             if len(lineitems) == 2:
                nmlstate = lineitems[0] 
                numvars = int(lineitems[1])
                wrfnmldict[nmlstate] = OrderedDict()            
             else:
               if int(lineitems[0]) <= numvars:
                  wrfnmldict[nmlstate][lineitems[1]] = (lineitems[2:])
               #  print numvars, lineitems[0], nmlstate, lineitems[1]
    return wrfnmldict
def readwrfnml(cwrfnml):
# read wrf namelist and return it as a python dictionary
# the values (right side) are set in a list
# ignore comments and any lines after '/' and before next '&'

    if os.path.isfile(cwrfnml):
       wrfnmldict = OrderedDict()
       with open(cwrfnml,'r') as nmlfile:
            commentline = True
            for line in nmlfile:
                line = line.strip().replace('\t', ' ').strip()
                if line == '': continue
                if line.startswith('&'):
                   gkey = re.search(r'&\s*(\w+)',line)
                   nmlname = gkey.group(1).lower()
                   wrfnmldict[nmlname] = OrderedDict()
                   commentline = False
                   continue
                elif line == '/':
                   commentline = True
                   continue
                if not commentline:
                   if '=' in line:
                      objkey, objval = line.split('=')
                      nmlvar = objkey.strip().lower()
                      values = objval.strip().rstrip(',').split(',')
                      wrfnmldict[nmlname][nmlvar] = filter(None,values)
                   else:
                      values = line.strip(',').split(',')
                      prev = wrfnmldict[nmlname][nmlvar]
                      if prev == None:
                         wrfnmldict[nmlname][nmlvar] = filter(None,values)
                      else:
                         prev.extend(filter(None,values))
                         wrfnmldict[nmlname][nmlvar] = prev
               #    print wrfnmldict[nmlname][nmlvar]
    else:
      print 'Your namelist file: %s does not exist!'%cwrfnml
      sys.exit()

    return wrfnmldict
 
def writewrfnml(nmldict, wrfnml = 'namelist.input', regvarlist = None):
#  write updated namelist: nmldict, into namelist file: wrfnml
#  regvarlist: a dictionary holds namelist variables from WRF Registry
    if 'share' in nmldict.keys():
       numdoms = nmldict['share']['max_dom'][0]
       nmltype = 'WPS'
    elif 'domains' in  nmldict.keys():
       numdoms = nmldict['domains']['max_dom'][0]
       nmltype = 'WRF'
    numdoms = int(numdoms)
    
    wrfnml = open(wrfnml,'w')

    for nmlname, nmlvar in nmldict.iteritems():
        wrfnml.write('&%s \n'%nmlname)

        for varname, varvals in nmlvar.iteritems():
            wskip = False
            numvals = len(varvals)
            valtype = None
            if numvals > numdoms:
               # multiple values but may not be domain-related. If so
               # keep all the values, otherwise, keep the number of values
               # up to the max_dom: numdoms
               if nmlname == 'mod_levs' and varname == 'press_pa':
                  pass
               elif nmlname == 'metgrid' and varname == 'fg_name':
                  pass
               elif nmlname == 'domains' and varname == 'eta_levels':
                  pass
               else:
                  numvals = numdoms 
            if nmltype == 'WRF' and regvarlist:
               if regvarlist.get(nmlname,None):
                  if regvarlist[nmlname].get(varname,None):
                     infolist = regvarlist[nmlname][varname]
                     #print nmlname, varname, infolist
                     if int(infolist[0]) == 0: # domain-dependent variable
                        numvals = numdoms
                     vt = infolist[1].strip().lower()
                     if vt == 'integer':
                        valtype = type(1)        # integer
                     elif vt == 'float':
                        valtype = type(1.0)    # float
                     elif vt == 'logical':
                        valtype = type(True)   # logical
                     elif vt == 'character':
                        valtype = type('hi')   # string
                     else:
                        valtype = None
                  else:
                     valtype = None
               else:
                   print '%s %s is not a namelist variable in WRF Registry!'%(nmlname,varname)
                   wskip = True
            #if wskip: continue
            
            varvals, valtype = list2str(varvals[0:numvals],valtype)   # convert list to string
            line = ' %-30s = %s'%(varname,varvals)
        
            lvals = len(varvals[0:numvals])
            if lvals <= 1 and valtype == type('hi'):
               wrfnml.write(line + '\n')
            else:
               if valtype == type('hi') or lvals > 1:
                  wrfnml.write(textwrap.fill(line, initial_indent='', subsequent_indent=' '*34,
                                          break_long_words=False, replace_whitespace=False,
                                          width=80)+'\n')
               elif len(line) < 80:
              # wrfnml.write('%s\n'%line)
                    wrfnml.write(line + '\n')
               else:
                    velems = varvals.rstrip(',').split(',')
                    newline = ''
                    lwidth = 80
                    lw = 50
                    for v in velems:
                        newline += '%s, '%v.strip()
                        if len(newline) > lw: 
                           newline += '\n' + 34*' '
                           lw += lwidth
                    wrfnml.write(' %-30s = %s \n'%(varname,newline)+'\n')
              
        wrfnml.write('/\n')
    wrfnml.close()
def isnumber(sn):
    try:
        float(sn)
    except:
        try:
            complex(sn)
        except:
            return False
    return True

def isbool(sn):
     if sn.strip().strip('.').upper() in ['TRUE', 'FALSE', 'T', 'F']:
        return True
     return False

def listtype(mylist):
    ltypes = [isnumber(x) for x in mylist]
    if all(ltypes): return True
    return False

def listcontype(mylist):
    # type of a list content
    ltypes = [isnumber(x) for x in mylist]
    if all(ltypes):
       for x in mylist:
           if '.' in x or 'E' in x.strip().upper():
              return type(1.0)
       return type(1)          # be careful! it can be a complex number.
    else:
       ltypes = [isbool(x) for x in mylist]
       if all(ltypes): 
          return type(True) # this is a set of booleans
       else: 
          return type('hi') # this is a set of strings
    
def list2str(mylist, lcontype = None):
# list to str (preparing for printing)
    if not lcontype:
       lcontype = listcontype(mylist)

    if lcontype == type(True):
       line = ''
       for se in mylist:
           se = se.upper().strip()
           if 'TRUE' in se: se = se.replace('TRUE','T')
           if 'FALSE' in se: se = se.replace('FALSE','F')
           line += '%s, '%se
       return line, lcontype
    
    if lcontype == type('hi'):
       line = ''
       for se in mylist:
           se = se.strip()
           if se.startswith("'"):
              line += "'%s', "%se.strip("'")
           elif se.startswith('"'):
              line += "'%s', "%se.strip('"')
           else:
              line += "'%s', "%se
       return line.strip(), lcontype
    
    if lcontype == type(1):
       line = ''
       for se in mylist:
           line += '%d, '%int(se)
       return line.strip(), lcontype
    
    if lcontype == type(1.0):
       line = ''
       for se in mylist:
           if 'e' in str(se): se = str(se).upper()
           line += '%s, '%str(se) 
       return line.strip(), lcontype
            
def updatenmltime(tmpnmldict, mytimes, startorend, mxdoms, domn):
    mytimes = mytimes.strip(',').split(',')
    mytimes = filter(None,mytimes)
    ltime = len(mytimes)
    timekws = ['year', 'month', 'day', 'hour', 'minute', 'second']
    
    if not domn:
       while ltime < mxdoms:
             mytimes.append(mytimes[-1])
             ltime = len(mytimes)
       
    if 'share' in tmpnmldict.keys():
        varname = startorend.strip() + '_date'
        cvalues = tmpnmldict['share'][varname]
        for n, mytime in enumerate(mytimes):      
            try:
                mytime = datetime.strptime(mytime,"%Y%m%d%H")
            except:
                print 'Time shall be in the form of CCYYMMDDHH ...'
                return None
            stimelist= (mytime.year, mytime.month,  mytime.day, 
                       mytime.hour, mytime.minute, mytime.second)
            if domn:
               cvalues[domn-1] = '%04d-%02d-%02d_%02d:%02d:%02d'%(stimelist)
            else:
               cvalues[n] = '%04d-%02d-%02d_%02d:%02d:%02d'%(stimelist)
        tmpnmldict['share'][varname] = cvalues
    elif 'time_control' in tmpnmldict.keys():
        for m, tvar in enumerate(timekws):
            varname = startorend.strip() + '_' + tvar
            cvalues = tmpnmldict['time_control'][varname]
            for n, mytime in enumerate(mytimes):      
                try:
                    mytime = datetime.strptime(mytime,"%Y%m%d%H")
                except:
                    print 'Time shall be in the form of CCYYMMDDHH ...'
                    return None
                stimelist= (mytime.year, mytime.month,  mytime.day, 
                              mytime.hour, mytime.minute, mytime.second)
                if domn:
                   cvalues[domn-1] = str(stimelist[m])
                else:
                   cvalues[n] =  str(stimelist[m])
            tmpnmldict['time_control'][varname] = cvalues
            
    return tmpnmldict

def nmlupdate(**kws):

    infile = kws.get('infile', None)
    outfile = kws.get('outfile', None)
    pklfile = kws.get('pklfile', None)
    mxdoms = kws.get('mxdoms', None)
    starttime = kws.get('starttime', None)
    endtime = kws.get('endtime', None)
    nmlname = kws.get('nmlname', None)
    nmlvar = kws.get('nmlvar', None)
    varvalues = kws.get('varvalues', None)
    rpc = kws.get('rpc', None)
    domn = kws.get('domn', None)
    
    tmppkl = './nml.pkl'
    if not pklfile: pklfile = tmppkl
    if infile:                    # get initial template namelist file
       templatedict = readwrfnml(cwrfnml=infile)   # read in a template namelist file
       print 'read in initial template namelist file: %s'%infile
    elif pklfile:
       pklf = open(pklfile, 'rb')
       templatedict = cPickle.load(pklf) 
       pklf.close()
#      print 'read in intermediate namelist file: %s'%pklfile
    else:
       print 'template namelist file is not specified.'
       return None
       
    
    if 'share' in templatedict.keys():
       nmltype = 'WPS'
       mxdomrec = 'share'
    elif 'time_control' in templatedict.keys():
       nmltype = 'WRF'
       mxdomrec = 'domains'       
    else:
       nmltype = None
        
    if not nmltype:
       print '%s does not look like WPS or WRF namelist ...'%nmltemplate
       return None
    
    tmpmxdom = templatedict[mxdomrec]['max_dom']
    
    if mxdoms:
       mxdoms = int(mxdoms)
       print 'updating max_dom = %4d' %mxdoms
       templatedict[mxdomrec]['max_dom'] = str(mxdoms)
#      print 'updated max_dom = ', templatedict[mxdomrec]['max_dom']
    else:
       try:
           mxdoms = int(tmpmxdom[0])
       except:
           print 'max_dom: %s is not defined ...'%tmpmxdom[0]
           return None
    
 #   if nmltype == 'WRF':      # read in the list namelist variables from Registry
 #      varlistf = 'wrfnmlvarlist.txt'
 #      wrfnmlregistry = rnmltxt2dict(varlistf)
    # update starttime
    if starttime:
       print 'updating starttime ...'
       templatedict = updatenmltime(templatedict, starttime, 'start', mxdoms, domn)
    if endtime:
       print 'updating endtime ...'
       templatedict = updatenmltime(templatedict, endtime, 'end', mxdoms, domn)
    if nmlname and nmlvar:
       if nmltype == 'WPS':
          if domn > 0:       # update specified domain only
               print 'update  domain %d only ...'%domn
               nd = int(domn) - 1
               v = templatedict[nmlname.strip()][nmlvar.strip()]
               v[nd] = str(varvalues)
          else:
               print 'update %s %s with '%(nmlname, nmlvar), varvalues
               v = str(varvalues).split(',')
               print v, filter(None,v)
               v = filter(None,v)
          templatedict[nmlname.strip()][nmlvar.strip()] = v
       elif nmltype == 'WRF':
         #   place holder for doing consistency checky with WRF RRegistry
         #   print nmlname.strip(), nmlvar.strip()
         #   try:
         #       ndom, vtype, dfval = wrfnmlregistry[nmlname.strip()][nmlvar.strip()]
         #   except:
         #       print '%s or %s or both are not in WRF Registry namelist'%(nmlname.strip(),nmlvar.strip())
         #      return None
            
            if domn > 0:       # update specified domain only
               print 'update  domain %d only ...'%domn
               nd = int(domn) - 1
               v = templatedict[nmlname.strip()][nmlvar.strip()]
               v[nd] = str(varvalues)
            else:
               print 'update %s %s with '%(nmlname, nmlvar), varvalues
               v = str(varvalues).split(',')
               v = filter(None,v)
            templatedict[nmlname.strip()][nmlvar.strip()] = v
         #   print templatedict[nmlname.strip()][nmlvar.strip()]
    if rpc:
       olds, news = rpc.strip().split(',')
       print 'replace %s with %s'%(olds, news)
       if nmlname:
          if nmlvar: # replace a string by specifyin old and new
             sv = templatedict[nmlname.strip()][nmlvar.strip()]
             svnew = [w.replace(olds,news) for w in sv]
             templatedict[nmlname.strip()][nmlvar.strip()] = svnew
          else:
             for nmlvar in templatedict[nmlname.strip()].iterkeys():
                 sv = templatedict[nmlname.strip()][nmlvar]
                 svnew = [w.replace(olds,news) for w in sv]
                 templatedict[nmlname.strip()][nmlvar] = svnew
       else:
         for nmlname, nmlvar in templatedict.iteritems():
             sv = templatedict[nmlname][nmlvar]
             svnew = [w.replace(olds,news) for w in sv]
             templatedict[nmlname][nmlvar] = svnew

    if outfile:   # output final namelist file
       writewrfnml(nmldict = templatedict, wrfnml = outfile)
       print 'namelist file is saved as %s'%outfile
    elif pklfile:
       fpkl = open(pklfile, 'wb')
       cPickle.dump(templatedict, fpkl, -1)
       fpkl.close()
       print 'intermediate namelist file is saved as %s'%pklfile
    else:
       print 'namelist file is nor saved !'
