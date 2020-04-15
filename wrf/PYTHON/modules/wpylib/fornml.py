import os
import sys
import re
from datetime import datetime, timedelta
from collections import OrderedDict

def readwrfnml(cwrfnml = 'namelist.input'):
# read wrf namelist and return it as a python dictionary
# ignore comments and any lines after '/' and before next '&'

    if os.path.isfile(cwrfnml):
       with open(cwrfnml,'r') as nmlfile:
          nmldat = nmlfile.readlines()
          commentline = True
       wrfnmldict = OrderedDict()
       for k, line in enumerate(nmldat):
           sline = line.strip()
           if sline == '':
              pass
           else:
              if sline.startswith('&'):
                 gkey = re.search(r'&\s*(\w+)',sline)
                 gkey = gkey.group(1).lower()
                 wrfnmldict[gkey] = OrderedDict()
                 commentline = False
              elif sline == '/':
                 commentline = True
              elif not commentline:
                 if '=' in sline:
                    objkey, values = sline.split('=')
                    objkey = objkey.strip().lower()
                    values = values.strip()
                    wrfnmldict[gkey][objkey] = values.rstrip(',')
                 else:          # a continuaton line so that it is treated as part of previous record.
                    if wrfnmldict[gkey][objkey].strip() == '': 
                       wrfnmldict[gkey][objkey] = sline.rstrip(',')
                    else:
                       wrfnmldict[gkey][objkey] += ',' + sline.rstrip(',')
    else:
      print 'Your namelist file: %s does not exist!'%cwrfnml
      sys.exit()
    return wrfnmldict

def writewrfnml(nmldict = {}, wrfnml = 'namelist.input'):
    numdoms = nmldict['share']['max_dom'] if 'share' in nmldict.keys() else nmldict['domains']['max_dom']
    numdoms = int(numdoms)
    
    wrfnml = open(wrfnml,'w')

    for nmlkey, nmlrec in nmldict.iteritems():
        wrfnml.write('&%s \n'%nmlkey)

        for varname, varvalu in nmlrec.iteritems():
            varlist = varvalu.split(',')
            numvars = len(varlist)
            if len(varlist) > numdoms:
               if nmlkey == 'mod_levs' and varname == 'press_pa':
                  pass
               elif nmlkey == 'metgrid' and varname == 'fg_name':
                  pass
               elif nmlkey == 'domains' and varname != 'eta_levels':
                  pass
               else:
                  numvars = numdoms 
            varvalu = list2str(varlist[0:numvars])
            if len(varvalu) > 50 and ',' in varvalu:     # break longline if multiple elements
                velem = varvalu.split(',')
                nline = 0
                fstl = True
                oneline = ''
                nelem = len(velem)
                while nline < nelem:
                    oneline += '%s,'%velem[nline]
                    nline += 1
                    if len(oneline) > 50 or nline >= nelem:
                        if fstl:          
                           wrfnml.write(' %-24s = %s \n'%(varname,oneline))
                           fstl = False 
                        else:
                           wrfnml.write(28*" " + '%s \n'%oneline)
                        oneline = ''
            else:
               wrfnml.write(' %-24s = %s, \n'%(varname,varvalu))
        wrfnml.write('/ \n')
    wrfnml.close() 

def updatewrfnml(wrfnmldict = {}, nmlrec = '', varname = '', varvalu = ''):
    varlist = varvalu.strip().split(',')
    varvalu = list2str(varlist) 
    if nmlrec in wrfnmldict.keys():
       pnml = wrfnmldict[nmlrec]
       if varname in pnml.keys():
          wrfnmldict[nmlrec][varname] = '%s'%varvalu
    else:
       wrfnmldict[nmlrec] = {}
       wrfnmldict[nmlrec][varname] = '%s'%varvalu
      
    return wrfnmldict 

def sdate2ymd(sdate):

    if len(sdate) >= 10:
        mytime = datetime.strptime(sdate,"%Y%m%d%H")
    elif len(sdate) >=8:
        mytime = datetime.strptime(sdate,"%Y%m%d")
    elif len(sdate) >=6:
        mytime = datetime.strptime(sdate,"%Y%m")
    elif len(sdate) >=4:
        mytime = datetime.strptime(sdate,"%Y")
    else:
       print 'sdate2ymd: checking format of %s, which should be ccyymmddhh'%sdate

    return mytime, mytime.year, mytime.month, mytime.day, mytime.hour, mytime.minute, mytime.second 

def list2str(mylist):
    s0 = str(mylist[0]).strip()
    if re.match("^\d+?\.\d+?$", s0) or re.match("^\d+?\.$", s0):
       c = 'F'
    elif re.match("^\d+?\.[Ee][+-]\d+?$", s0):
       c = 'E'
    elif re.match("^\.true.$", s0.lower()) or re.match("^\.false.$", s0.lower()):
       c = 'L'
    elif re.search(r'\D',s0):
       c = 'S'
    else:
       c = 'I'

    cs = ''
    for s in mylist:
        s = str(s).strip()
        if c == 'S':
           if s.startswith("'"):
              cs += "'%s', "%s.strip("'")
           elif s.startswith('"'):
              cs += "'%s', "%s.strip('"')
           else:
              cs += "'%s', "%s
        elif c == 'L':
              cs += '%s, '%s
        elif c == 'F':
           cs += '%f,'%float(s)
        elif c == 'E':
           cs += '%s, '%s
        else:
           cs += '%5d,'%int(s)
    
    cs = cs.strip()
    return cs[0:-1]
