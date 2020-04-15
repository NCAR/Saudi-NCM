#!/usr/bin/env python2.7
'''modules for scipuff
   W. WU (wanliwu@ucar.edu)
   May 20 2015'''

import os
from xml.dom import minidom
from datetime import datetime, timedelta
import textwrap
from collections import OrderedDict
import re
import fileinput 

def datefromxml(fxml, tagname):
    '''extract date (year, month, day, hour) from xml'''
    xmldate = fxml.getElementsByTagName(tagname)[0]

    xmlyear = xmldate.getElementsByTagName('year')[0].firstChild.data
    xmlmonth = xmldate.getElementsByTagName('month')[0].firstChild.data
    xmlday = xmldate.getElementsByTagName('day')[0].firstChild.data
    xmlhour = xmldate.getElementsByTagName('hour')[0].firstChild.data

    xmldate = datetime(int(xmlyear), int(xmlmonth), int(xmlday), \
                     int(xmlhour))
    return xmldate

def domaininfo(fxml, tagname):
    '''extract domain info (min/max lat/lon, duration, initial hour) from a given xml'''
    xmldomain = fxml.getElementsByTagName(tagname)[0]
    
    return \
           int(xmldomain.getElementsByTagName('domainid')[0].firstChild.data), \
           float(xmldomain.getElementsByTagName('lonmin')[0].firstChild.data), \
           float(xmldomain.getElementsByTagName('lonmax')[0].firstChild.data), \
           float(xmldomain.getElementsByTagName('latmin')[0].firstChild.data), \
           float(xmldomain.getElementsByTagName('latmax')[0].firstChild.data), \
           int(xmldomain.getElementsByTagName('model_duration')[0].firstChild.data), \
           int(xmldomain.getElementsByTagName('initial_hour')[0].firstChild.data), \
           xmldomain.getElementsByTagName('geo_file')[0].firstChild.data

def releaseinfo(fxml, tagname):
    '''extract releae info (location, type, duration) from a given xml'''
    xmldate = fxml.getElementsByTagName(tagname)[0]

    return float(xmldate.getElementsByTagName('lon')[0].firstChild.data), \
           float(xmldate.getElementsByTagName('lat')[0].firstChild.data), \
           xmldate.getElementsByTagName('type')[0].firstChild.data, \
           xmldate.getElementsByTagName('reltime')[0].firstChild.data, \
           int(xmldate.getElementsByTagName('duration')[0].firstChild.data)

def infofromxml (xmlfile):
    '''extract required release info from a given xml file
       W. Wu (wanliwu@ucar.edu) May 19 2015'''
    xmldoc = minidom.parse(xmlfile)

    # extract start and end dates
    sdate = datefromxml(xmldoc, 'start')     
    edate = datefromxml(xmldoc, 'end')      
    
    # extract release location, type and duration
    rlon, rlat, rtype, reltime, rduration = releaseinfo(xmldoc, 'release')    
    relhour, relmins = reltime.split(':')
    # release time relative to the starting time 
    reltime = float(relhour) * 60 + float(relmins) - (float(sdate.hour) * 60 + float(sdate.minute))
    reltime = reltime / 60.0     # in hours 
    # extract medoc file location and template file name
    mdir = xmldoc.getElementsByTagName('medocdir')[0].firstChild.data 
    mfile = xmldoc.getElementsByTagName('medocfile')[0].firstChild.data
    
    # extract domain file
    domainid, lonmin, lonmax, latmin, latmax, mduration, ihour, gfile = domaininfo(xmldoc, 'domain')
    
    return sdate, edate, rlon, rlat, rtype, reltime, rduration, mdir, mfile, gfile, \
           domainid, lonmin, lonmax, latmin, latmax, mduration, ihour

def dequote(s):
    """
    If a string has single or double quotes around it, remove them.
    Make sure the pair of quotes match.
    If a matching pair of quotes is not found, return the string unchanged.
    http://stackoverflow.com/questions/3085382/python-how-can-i-strip-first-and-last-double-quotes
    W. WU (May 28 2015): ensure the string is more than one chatacter. if len(s) <= 1: return s
    """
    if len(s) <= 1: return s
    if (s[0] == s[-1]) and s.startswith(("'", '"')):
        return s[1:-1]
    return s

def readnml(cwrfnml):
# read namelist and return it as a python dictionary
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
                      wrfnmldict[nmlname][nmlvar] = [dequote(_f.strip()) for _f in values if _f]
                   else:
                      values = line.strip(',').split(',')
                      prev = wrfnmldict[nmlname][nmlvar]
                      if prev == None:
                         wrfnmldict[nmlname][nmlvar] = [dequote(_f.strip()) for _f in values if _f]
                      else:
                         if prev[-1].startswith(('"', "'")):
                            if len(prev[-1]) > 1 and prev[-1].endswith(('"', "'")):
                               prev.extend([dequote(_f.strip()) for _f in values if _f])
                            elif values[0].strip().endswith(('"', "'")):
                               prev[-1] = prev[-1] + values[0].strip()
                               if len(values) > 1: prev.extend([dequote(_f.strip()) for _f in values[1:] if _f])
                         else:
                            prev.extend([dequote(_f.strip()) for _f in values if _f])
                         wrfnmldict[nmlname][nmlvar] = prev
               
    else:
      print ('Your namelist file: %s does not exist!'%cwrfnml)
      sys.exit()

    return wrfnmldict

def isnumber(sn):
    if '*' in sn:
       fi, sf = sn.split('*')
       sn = sf.strip()
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

    if lcontype == type(True):      # type of boolean
       line = ''
       for se in mylist:
           se = se.upper().strip()
           if 'TRUE' in se: se = se.replace('TRUE','T')
           if 'FALSE' in se: se = se.replace('FALSE','F')
           line += '%s, '%se
       return line, lcontype
    
    if lcontype == type('hi'):      # type of str
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
    
    if lcontype == type(1):        # type of integer
       line = ''
       for se in mylist:
           line += '%d, '%int(se)
       return line.strip(), lcontype
    
    if lcontype == type(1.0):     # type of float
       line = ''
       for se in mylist:
           if 'e' in str(se): se = str(se).upper()
           line += '%s, '%str(se) 
       return line.strip(), lcontype

def writenml(nmldict,foutnml):
    wrfnml = open(foutnml,'w')
    for nmlname, nmlvar in nmldict.items():
        wrfnml.write('&%s \n'%nmlname)

        for varname, varvals in nmlvar.items():
            wskip = False
            numvals = len(varvals)
            varvals = [_f.strip() for _f in varvals if _f]
            valtype = None
            varvals, valtype = list2str(varvals[0:numvals],valtype)   # convert list to string
            if varvals == None: varvals = [' ']
            line = ' %-20s = %s'%(varname,varvals)

            wrfnml.write(textwrap.fill(line, initial_indent='', subsequent_indent=' '*24,
                                          break_long_words=False, replace_whitespace=False,
                                          width=80)+'\n')

        wrfnml.write('/\n')
    wrfnml.close()

def updateinp(inpnml, startdate, enddate, ihour, mduration, \
              lonmin, latmin, lonmax, latmax, deltats, dtsave):
    
    inpnml['time1']['year_start'] = [datetime.strftime(startdate,'%Y')]
    inpnml['time1']['month_start'] = [datetime.strftime(startdate,'%m')]
    inpnml['time1']['day_start'] = [datetime.strftime(startdate,'%d')]
    inpnml['time1']['tstart'] = ['%i'%ihour]

    inpnml['time2']['year_end'] = [datetime.strftime(enddate,'%Y')]
    inpnml['time2']['month_end'] = [datetime.strftime(enddate,'%m')]
    inpnml['time2']['day_end'] = [datetime.strftime(enddate,'%d')]
    inpnml['time2']['tend'] = ['%i'%ihour]
    if deltats > 0.0: 
       inpnml['time2']['delt'] = ['%f'%deltats]
    if dtsave > 0.0: 
       inpnml['time2']['dt_save'] = ['%f'%dtsave]
    inpnml['time2']['tend_hr'] = ['%i'%mduration]

    inpnml['domain']['xmin'] = ['%f'%lonmin]
    inpnml['domain']['xmax'] = ['%f'%lonmax]
    inpnml['domain']['ymin'] = ['%f'%latmin]
    inpnml['domain']['ymax'] = ['%f'%latmax]
    
    return inpnml

def updatescn(scndict, rellon, rellat, reltype, reltime, rduration, namerel):
    
    scndict['scn']['xrel'] = ['%f'%rellon]
    scndict['scn']['yrel'] = ['%f'%rellat]
#   scndict['scn']['reltyp'] = [reltype]
    scndict['scn']['trel'] = ['%f'%reltime]
    scndict['scn']['tdur'] = ['%i'%rduration]
    scndict['scn']['name_rel'] = ['%s'%namerel]

    return scndict


def xml2parameters(xmlfile, relfile):
#  ... extract info from given xml file
#  1. extract release and domain information from a XML file
#  2. update namelists: INP, SCN and MSC, based on templates and extracted info
#  3. concate necesary medoc files for the given period defined in XML ...

    startdate, enddate, rellon, rellat, reltype, reltime, relduration, \
    medocd, medocf, geof, domainid, lonmin, lonmax, latmin, latmax, \
    mduration, ihour = infofromxml(xmlfile)

# check if release site is within the given domain
    if rellon > lonmin and rellon < lonmax and rellat > latmin and rellat < latmax:
       print ('release site is within the domain.')
    else:
       print ('relase site is out of the domain ! \n Check the site and domain ...')
       sys.exit()

    print ('information extracted from %s'%xmlfile)
#   print ('    startdate: %s'%startdate)
#   print ('    enddate: %s'%enddate)
    print ('    release site: (%f10.5, %f10.5)'%(rellon, rellat))
    print ('            type: %s'%reltype)
    print ('Relative Reltime: %s hours'%reltime)
    print ('            duration: %3i'%relduration)
    print ('    meddoc file location: %s'%os.path.join(medocd, medocf))
    print ('    domain ID: %2i'%domainid)
    print ('           range: (%f10.5,%f10.5) (%f10.5,%f10.5)'%(lonmin, latmin, lonmax, latmax))
    print ('    model duration: %3i hours'%mduration)
    print ('          initial hour: %3iZ'%ihour)
    print ('    geogrid file: %s'%geof)

# 2. update namelists

# update namelist  SCN
#scnnml = './ex_densegas_tank.scn'
    scnnml = './template.scn'
    scndict = readnml(scnnml)
    outnml = '%s.scn'%reltype 

    print ('update namelist scn: %s'%outnml)
#   namerel= './ex_type-a_v.rel'                     # release file
    namerel= relfile
    scndict = updatescn(scndict, rellon, rellat, reltype, reltime, relduration, namerel)
    writenml(scndict, outnml)

# update namelist  MSC
    mscnml = './template.msc'
    mscdict = readnml(mscnml)
    outnml = 'tmp.msc'  
    writenml(mscdict, outnml)

    return startdate, enddate, reltype, \
           ihour, mduration, \
           lonmin, latmin, lonmax, latmax, \
           domainid, medocd, medocf, geof
