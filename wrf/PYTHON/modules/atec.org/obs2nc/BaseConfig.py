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

class BaseConfig(object):
    '''
    classdocs
    '''
    CLASS_NAME = 'BaseConfig'
    sDebug = False
    
    def __init__(self, config_file):
        '''
        Constructor
        '''
        debug = False
        #debug = not debug
        method_name = '%s.%s()' % (BaseConfig.CLASS_NAME,'__init__')
        
        self.configs     = None
        self.config_file = config_file
        self.configs     = self.read_config_file(config_file)
        
        log_dir = self.get_log_dir()
        work_dir = self.get_work_dir()
        #print ('   log_dir: %s' % log_dir)
        if not os.path.isdir(log_dir):  os.makedirs(log_dir)
        if not os.path.isdir(work_dir): os.makedirs(work_dir)
        if debug:   print ('%s   work_dir: %s' % (method_name, work_dir))
        
        self.station_ids = []
        self.excluded_station_ids = []
        self.build_station_ids()
        self.sDebug = True


    def build_station_ids(self):
        site_id_array = self.get_id_list();
        id_prefix = self.get_site_prefix()
        for site_id in site_id_array:
            if isinstance(id_prefix, list):
                for tmp_id_prefix in id_prefix:
                    self.station_ids.append('%s%s' % (tmp_id_prefix, site_id.zfill(2)))
            else:
                self.station_ids.append('%s%s' % (id_prefix, site_id.zfill(2)))
        site_id_array = self.get_excluded_id_list();
        for site_id in site_id_array:
            if isinstance(id_prefix, list):
                for tmp_id_prefix in id_prefix:
                    self.excluded_station_ids.append('%s%s' % (tmp_id_prefix, site_id.zfill(2)))
            else:
                self.excluded_station_ids.append('%s%s' % (id_prefix, site_id.zfill(2)))
        
    def extract_value(self, line):
        (param, value) = line.split('=')
        return (param, value.rstrip("\n").rstrip("\r"))

    def get_cdlfile(self):
        return self.get_value('cdlfile')
    
    def get_db_name_key(self, default_value = None):
        db_name_key = self.get_value('db_name', None)
        if db_name_key is None:
            db_name_key = self.get_value('db_name_key', default_value)
        return db_name_key
    
    #def get_db_user(self):
    #    return self.get_value('db_user')
    
    def get_excluded_id_list(self):
        id_array = ()
        id_list = self.get_value('id_list_excluded')
        if id_list is None:
            id_list = self.get_value('sams_list_excluded')
        if id_list is None:
            id_list = self.get_value('pwids_list_excluded')
        if id_list is not None:
            id_array = id_list.split(',')
        return id_array

    def get_excluded_station_ids(self):
        #(param, value) = line.split('=')
        #return (param, value)
        return self.excluded_station_ids

    def get_id_list(self):
        id_array = []
        id_list = self.get_value('id_list')
        if id_list is None: id_list = self.get_value('sams_list')
        if id_list is None: id_list = self.get_value('pwids_list')
        if id_list is not None:
            id_array = id_list.split(',')
        return id_array

    def get_log_dir(self):
        return self.get_value('log_dir')
    
    def get_ncgen(self):
        return self.get_value('ncgen')
    
    def get_range_name(self):
        return self.get_value('range', self.get_site())
    
    def get_site(self):
        return self.get_value('site')
    
    def get_site_prefix(self):
        cfg_site = self.get_site()
        cfg_site_prefixes = self.get_value('site_prefix','s').split(",")
        if 1 < len(cfg_site_prefixes):
            site_prefix = []
            for tmp_site_prefix in cfg_site_prefixes:
                site_prefix.append('%s_%s' % (cfg_site, tmp_site_prefix.strip()))
        else:
            site_prefix = '%s_%s' % (cfg_site, cfg_site_prefixes[0])
        return site_prefix
    
    def get_station_ids(self):
        #(param, value) = line.split('=')
        #return (param, value)
        return self.station_ids

    def get_table_name(self):
        #(param, value) = line.split('=')
        #return (param, value)
        return self.get_value('table_name')

    def get_station_table_name(self):
        #(param, value) = line.split('=')
        #return (param, value)
        return self.get_value('station_table_name','station_information')

    def get_template_cdl(self):
        #(param, value) = line.split('=')
        #return (param, value)
        return self.get_value('cdlfile')

    def get_work_dir(self):
        work_dir = self.get_value('work_dir')
        if not work_dir:
            work_dir = self.get_log_dir()
        return work_dir
    
    def get_value(self, key, default_value = None):
        #(param, value) = line.split('=')
        #return (param, value)
        return self.configs.get(key, default_value)

    def isDebug(self):
        #(param, value) = line.split('=')
        #return (param, value)
        return BaseConfig.sDebug

    def is_opt_exclude_stations_no_data(self):
        value = self.get_value('exclude_stations_no_data', 'false')
        return ("true" == value.lower()) or ("yes" == value.lower())
    
    def read_config_file(self, config_file):
        method_name = '%s.%s()' % (BaseConfig.CLASS_NAME,'read_config_file')
        if self.isDebug(): print ("processing common cfg_file %s" % config_file)
        
        configs = {}
        with open(config_file, 'r') as cfg_file:
            for line_buf in cfg_file:
                if self.isDebug(): print (" line_buf: [%s]" % (line_buf))
                if 0 == len(line_buf.strip()) or line_buf.startswith("#") or line_buf.startswith("="):
                    continue
                if 0 < line_buf.find("="):
                    (param, value) = self.extract_value(line_buf)
                    if self.isDebug(): print (" param: %s, value: [%s]" % (param, value))
                    configs[param] = value
                else:
                    print ('  === ERROR === %s: [%s] %s' % (method_name,line_buf,'Invalid key value pair'))
        
        return configs
        
