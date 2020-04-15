#!/usr/bin/env python
#*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*
# * Copyright (c) 2015 UCAR
# * University Corporation for Atmospheric Research (UCAR)
# * National Center for Atmospheric Research (NCAR)
# * Research Applications Laboratory (RAL)
# * P.O. Box 3000, Boulder, Colorado, 80307-3000 USA
# * All rights reserved. Licenced use only.
# * Do not copy or distribute without authorization.
# *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*
#
# netcdf_output.py:  Common routines to convert data from mysql
# to netcdf for use in the operational models.
#
# Note: The db_host is from commom/db.commom.
#       Other db configuration is from the configuration files
#       (sams2nc.cfg, XXX2nc.cfg)
#
############################################################

'''
Created on Jan 16, 2015

@author: hsoh
'''

import os

MODULE = 'CdlCreator'

def get_ncfile_name(work_dir,site,start_date,data_type='sams'):
    debug = False
    f_name = '   %s@%s' % (MODULE, 'get_ncfile_name')
    nc_count = 0
    ncname = "%s.%s%s.%d.cdf" % (site,data_type,start_date,nc_count)
    ncfile = os.path.join(work_dir,ncname)
    while os.path.exists( ncfile ):
        nc_count = nc_count + 1
        ncname = "%s.%s%s.%d.cdf" % (site,data_type,start_date,nc_count)
        ncfile = os.path.join(work_dir,ncname)
        if debug: print ('%s ncfile (loop): %s' % (f_name, ncfile))
    return ncfile

    
class CdlCreator(object):
    '''
    classdocs
    '''

    debug = False

    def __init__(self, template_cdl, site):
        '''
        Constructor
        '''
        self.debug = False
        #self.debug = True
        self.site = site
        self.data_type = 'sams'
        self.template_cdl = template_cdl
  

    def createCDL(self, out_dir, attributeList):
        method_name = '   %s.%s' % (MODULE, 'createCDL()')

        cdl_name = "%s.cdl" % (self.site)
        new_cdl = os.path.join(out_dir, cdl_name)
        if self.debug or CdlCreator.debug:
            print ("%s configure cdlfile output to %s, attributes: %r" % (
                    method_name, new_cdl, attributeList))
        with open (new_cdl, "w") as outfile:
            with open (self.template_cdl, "r") as cdl_file:
                for line in cdl_file:
                    if attributeList:
                        for (key,value) in attributeList.items():
                            if key in line:
                                line=line.replace(key,str(value))
                                if self.debug or CdlCreator.debug:
                                        print ("%s replaced %s with %s" % (method_name, key, value))
                    outfile.write(line)
        return new_cdl

    def get_ncfile_name(self, work_dir,start_date):
        #print (' get_ncfile_name() ncname : %s' % get_ncfile_name(work_dir,self.site,start_date))
        return get_ncfile_name(work_dir,self.site,start_date,self.data_type)

    def set_data_type(self, data_type):
        #print (' get_ncfile_name() ncname : %s' % get_ncfile_name(work_dir,self.site,start_date))
        self.data_type = data_type

    