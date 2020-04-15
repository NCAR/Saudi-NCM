#!/usr/bin/env python
'''
Created on Jan 16, 2015

@author: hsoh

@version $Revision: 1.13 $Date: 2018/09/13 19:18:13 $
'''

import os
import sys
import math
import time
import traceback
from abc import ABCMeta, abstractmethod
from subprocess import Popen, PIPE

Use_PyNIO = False
is_MS_Windows = sys.platform.startswith('win')
#is_linux   = sys.platform.startswith('linux')
try:
    from netCDF4 import Dataset
except ImportError:
    if os.getenv('NCARG_ROOT') is None:
        if os.path.exists('/opt/ncl'):
            os.environ['NCARG_ROOT'] = '/opt/ncl'
            print("    Info] base2nc: set NCARG_ROOT environment variable")
    import Nio 
    import PyNIO 
    Use_PyNIO = True
    
from optparse import OptionParser

from atec.obs2nc.BaseConfig import BaseConfig
try:
    from atec.config.DbConfig import DbConfig
except ImportError:
    from atec.db_config.DbConfig import DbConfig
    
from atec.obs2nc.Stations import Stations
from atec.obs2nc.CdlCreator import CdlCreator
from atec.util.DateUtil import DateUtil, LogTime
from atec.util.ExceptionHandler import handle_sql_exception

debug = 0
MODULE_NAME = 'base2nc'
OPT_support_multiple_report_type = False

def compute_rh(tmpc, dwpc):
    if tmpc != base2nc.MISSING_VALUE and dwpc != base2nc.MISSING_VALUE:
        es = (6.11 * (10** ( (7.5 * tmpc) / (237.7 + tmpc) ) ) ) 
        e  = (6.11 * (10** ( (7.5 * dwpc) / (237.7 + dwpc) ) ) )
        rh = (e/es)*100
    else:
        rh = base2nc.MISSING_VALUE
    return rh
    
    
    
def create_parser():
    usage_str = "%prog [options] "
    parser = OptionParser(usage = usage_str, version="%prog MIR Database version")
    parser.add_option("-c", dest="config", help=" Config file - required")
    #parser.add_option("--domain", dest="domain", default=3,
    #        help=" Set the domain id - optional, default 3")
    #parser.add_option("--cycle_hour", dest="cycle_hour", default=2,
    #        help=" Set the first cycle hour, 0 or 2 - optional, default 2")
    #parser.add_option("-q", action="store_const", const=1, dest="quick_dates",
    #        help=" Generate cycle date quickly - optional")
    #parser.add_option("--model",  dest="model",  default=0,
    #        help=" Set the model id - optional, default 0")
    parser.add_option("-o", dest="out_interval", default='day',
            help=" output interval day or hour - optional")
    parser.add_option("-s", dest="start_time", help=" Start date time - required")
    parser.add_option("-e", dest="end_time",   help=" End date Time   - required")
    parser.add_option("-d", dest="debug", action="store_true", default=False,
            help=" Enable debug - optional")
    parser.add_option("-D", dest="debug_sql", action="store_true", default=False,
            help=" Enable debug for SQL - optional")
    parser.add_option("--debug-config", "--debug_config", dest="debug_config",
            action="store_true", default=False,
            help=" Enable debugging configuration - optional")
    #parser.add_option("-s", action="store_const", const=1, dest="short_output",
    #        help=" Output short type - optional")
    parser.add_option("-t", dest="opt_test", action="store_true", default=False,
            help=" Enable opt_test - optional")
    parser.add_option("-l", dest="opt_log", action="store_true", default=False,
            help=" Log to file (default to screen) - optional")

    return parser

def get_start_date_from_end_date( end_date, time_interval_in_seconds = 60):
    #return DateUtil.add_offset_for_ymdhm( end_date, time_interval_in_seconds)
    return DateUtil.add_time_to_date(end_date, time_interval_in_seconds)

def get_single_value_from_db(cursor, sql_string):
    method_name = '%s@%s()' % (MODULE_NAME, 'get_single_value_from_db')
    try:
        cursor.execute( sql_string )
    except:
        exit_message="Can't execute SQL data request \n"
        handle_sql_exception(sql_string, exit_message, method_name)

    single_value = None
    rows = cursor.fetchall()
    if rows and rows[0]:
        single_value = rows[0][0]
    return single_value
    
def next_end_daily(cur_start_date, last_date):
    global debug
    #debug = False
    #debug = not debug
    method_name = '     %s@%s' % (MODULE_NAME, 'next_end_daily()')
    #
    # given a start_date and an last_date
    # figure out when to end each loop (one file per day)
    #
    if type(cur_start_date) is str:
        short_start = cur_start_date[0:8]
    else:
        short_start = DateUtil.format_to_ymd(cur_start_date)
    if type(last_date) is str:
        short_end   = last_date[0:8]
    else:
        short_end = DateUtil.format_to_ymd(last_date)
    if short_start == short_end:
        return_val = last_date
    else:
        return_val = short_start+"235959" 

    if debug: print ("%s start: %s -> %s, end %s -> %s, result: %s" % (
            method_name, cur_start_date,short_start,last_date,short_end,return_val))
    return return_val

def next_end_hourly(cur_start_date, last_date):
    global debug
    #debug = False
    #debug = not debug
    method_name = '     %s@%s' % (MODULE_NAME, 'next_end_hourly()')
    #
    # given a start_date and an last_date
    # figure out when to end each loop (one file per day)
    #
    if type(cur_start_date) is str:
        short_start = cur_start_date[0:10]
    else:
        short_start = DateUtil.format_to_ymd(cur_start_date)
    if type(last_date) is str:
        short_end   = last_date[0:10]
    else:
        short_end   = DateUtil.format_to_ymdh(last_date)
    if short_start == short_end:
        return_val = last_date
    else:
        return_val = short_start+"5959"

    if debug: print ("%s start: %s -> %s, end %s -> %s, result: %s" % (
            method_name, cur_start_date,short_start,last_date,short_end,return_val))
    return return_val

def set_debug_option(enabled):
    global debug
    debug = enabled
    CdlCreator.debug = enabled
    
class base2nc(object):
    '''
    classdocs
    '''
    __metaclass__ = ABCMeta

    ORDER_BY          = "ORDER BY obs_time ASC"

    BAD_INDEX = -1
    MISSING_VALUE         = -9999
    OLD_MIR_MISSING_VALUE = -999

    DEF_FIELD_LENGTH = 20
    WIND_RAD         = 4.0*math.atan(1.0)/180

    INFO_P  = '=== INFO ==='
    ERROR_P = '=== ERROR ==='
    
    MIR_JOIN_ON      = 'O.unique_site_id=S.unique_site_id AND S.range_code IS NOT NULL'

    SQL_CURRENT_DATA_IN_HOUR = "SELECT var_value FROM mir_globals WHERE var_name='current_data'"

    #def __init__(self, config_name, params):
    def __init__(self, db_name_key='mir'):
        '''
        Constructor
        '''
        method_name = '     %s.%s()' % (MODULE_NAME, '__init__')
        
        self.log_time = LogTime()
        self.db_name_key = db_name_key  # will be override by config file (db_name)
        parser = create_parser()
        self.add_options(parser)
        (options, args) = parser.parse_args()
        if options.debug:
            os.environ["OPT_DEBUG"] = "True"
        if options.debug_sql:
            os.environ["OPT_DEBUG_SQL"] = "True"

        self.args = args
        self.options = options
        if options.config is None:
            parser.print_help()
            sys.exit(2)

        #self.current_data_in_hour = None
        self.config = self.get_config(options.config)
        if options.debug:
            set_debug_option(options.debug)
        if options.debug_config:
            DbConfig.set_debug_option(options.debug_config)
        
        self.setup_logs()
        self.connect_db()
        
        self.stationActor = Stations(self.cursor)
        self.add_station_list()

        self.cdl_creator = CdlCreator(self.config.get_template_cdl(),
                self.config.get_range_name())
        self.cdl_creator.set_data_type(self.get_datatype())
        self.log_time_db_access = LogTime()
        self.log_time_fill_var  = LogTime()
        self.log_time_nc_write  = LogTime()

        if 0 < self.log_time.get_current_duration():
            self.log_time.stop()
            init_duration = self.log_time.get_durations()
            self.log_time.save(method_name, init_duration)
            self.log_time.reset()
            print ('{m} duration: {w} (CPU: {c})'.format(
                    m=method_name, w=init_duration[0], c=init_duration[1]))
        
        
    @staticmethod
    def is_missing_value( value ):
        return (value == base2nc.MISSING_VALUE or value == base2nc.OLD_MIR_MISSING_VALUE)
    
    @abstractmethod
    def build_select_sql(self, start_date, end_date):
        pass
    
    @abstractmethod
    def fill_variable_values(self, rows, start_seconds):
        pass
    
    @abstractmethod
    def get_datatype(self):
        pass
    
    @abstractmethod
    def get_date_column_name(self):
        pass
    
    @abstractmethod
    def set_initial_values(self, station_count):
        pass
    
    @abstractmethod
    def update_arrays(self, station_count):
        pass
    
    def add_more_attributes_for_cdl(self, attributes):
        pass
    
    def add_more_station_list(self, site_prefix, station_ids, is_exclusive_list=True):
        debug = False
        debug = not debug
        debug = debug or self.options.debug
        method_name = '%s.%s()' % (MODULE_NAME,'add_more_station_list')
        table_name = self.get_table_name()
        if debug:
            print ('%s table_name: %s, site_prefix: %s is_exclusive_list: %r' % (
                    method_name, table_name, site_prefix, is_exclusive_list))
            if station_ids is None or 0 == len(station_ids):
                station_ids_msg = 'Add all stations begin with {s}'.format(s=site_prefix)
            else:
                station_ids_msg = 'station_ids: %r' % station_ids
            print ('            %s' % (station_ids_msg))
        #self.stationActor.add_stations(self.config.get_station_table_name(),
        #         id_pattern=site_prefix, station_ids=station_ids,
        #         interval_days = self.get_number_of_days_for_stations(),
        #         obs_table_name=table_name, obs_time_column=date_col_name)
        station_table_name = self.config.get_station_table_name()
        if self.config.is_opt_exclude_stations_no_data():
            date_col_name = self.get_date_column_name()
            self.stationActor.add_stations_with_real_data(station_table_name,
                    id_pattern=site_prefix, station_ids=station_ids,
                    interval_days=self.get_number_of_days_for_stations(),
                    obs_table_name=table_name, obs_time_column=date_col_name,
                    is_exclusive_list=is_exclusive_list, l_debug=debug)
        else:
            self.stationActor.add_stations(station_table_name,
                     id_pattern=site_prefix, station_ids=station_ids,
                     is_exclusive_list=is_exclusive_list, l_debug=debug)

        
    def add_options(self, parser):
        pass
    
    def add_station_list(self):
        debug = True
        debug = not debug
        #method_name = '%s.%s()' % (MODULE_NAME,'add_station_list')
        station_ids = self.config.get_excluded_station_ids()
        if OPT_support_multiple_report_type:
            site_code = self.config.get_site()
        else:
            site_code = self.config.get_site_prefix()
            
        is_exclusive_list = True
        if 0 == len(station_ids):
            station_ids = self.config.get_station_ids()
            is_exclusive_list = False
        #if debug:
        #    table_name  = self.get_table_name()
        #    print (' table_name: %s, site_code: %s' % (
        #            table_name, site_code), ' station_ids: ', station_ids)
        self.add_more_station_list(site_code, station_ids, is_exclusive_list)

    def build_atributes_for_cdl(self):
        debug = False
        method_name = '   %s.%s()' % (MODULE_NAME, 'build_atributes_for_cdl')
        if debug:print ('"%s %s' % (method_name, 'is called'))
        attributes = {}
        attributes['__NSTATIONS__'] = self.get_station_count() 
        attributes['__FIELD_LENGTH__'] = base2nc.DEF_FIELD_LENGTH
        self.add_more_attributes_for_cdl(attributes) 
        return attributes
    
    def check_union_or_historic_data(self, start_date, end_date):
        debug = False
        #debug = not debug
        method_name = '%s.%s' % (MODULE_NAME, 'check_union_or_historic_data()')
        
        sql_string = 'SELECT MIN(%s) FROM `%s`' % (
                self.get_date_column_name(), self.get_table_name())
        first_obs_time = get_single_value_from_db(self.cursor, sql_string)

        yyyymm = None
        use_ym_table = False
        use_union = False
        
        if debug:
            print ('%s start_date: %r %s first_obs_time: %r %s' % (
                    method_name, start_date, type(start_date).__name__,
                    first_obs_time, type(first_obs_time).__name__))
            print ('%s start_date: %r first_obs_time: %r' % (
                    method_name, DateUtil.convert_to_datetime(start_date) ,
                    DateUtil.convert_to_datetime(first_obs_time)))
        start_datetime = DateUtil.convert_to_datetime(start_date)
        if start_datetime < first_obs_time:
            if debug: print ('%s start_date [%s] is less than first_obs_time [%s]' % (
                        method_name, start_date, first_obs_time))
            use_ym_table = True
            yyyymm = '%04d%02d' % (start_datetime.year, start_datetime.month)
            
            if DateUtil.convert_to_datetime(end_date) >= first_obs_time:
                use_union = True
        
        return (use_ym_table, use_union, yyyymm)
    
    def compute_rh(self, tmpc, dwpc):
        return compute_rh(tmpc, dwpc)
    
    def connect_db(self):
        method_name = '     %s.%s()' % (MODULE_NAME, 'connect_db')
        self.db_config = DbConfig()
        db_name_key = self.config.get_db_name_key()
        db_key_from = ' from config'
        if db_name_key is None:
            db_name_key = self.db_name_key
            db_key_from = ''
        if self.is_debug_enabled():
            print ('%s db_name_key%s: %s' % (method_name, db_key_from, db_name_key))
        self.db_conn = self.db_config.get_db_connection(db_name_key)
        self.cursor  = self.db_config.get_db_cursor(self.db_conn)

    def convert_to_time( self, date ):
        return DateUtil.convert_to_time( date )
    
    def convert_to_nc(self):
        debug = False
        debug = debug or self.is_debug_enabled()
        method_name = '     %s.%s()' % (MODULE_NAME, 'convert_to_nc')
        
        self.log_time.stop()
        pref_duration = self.log_time.get_durations()
        pref_duration_str = self.log_time.get_duration_strings()
        self.log_time.reset()
        
        max_obs_time = self.get_max_obs_time()
        if max_obs_time is None:
            print ('========\n\n%s No data is available, Exit...\n\n========')
            sys.exit(1)
            
        opt_start_date = self.get_start_date()
        opt_last_date  = self.get_end_date()

        start_date = opt_start_date

        cdlfile = self.create_cdl()
        if debug:
            opt_start_date_str = "Missing"
            if opt_start_date is not None:
                opt_start_date_str = DateUtil.format_to_ymdhm(opt_start_date)
            opt_last_date_str = "Missing"
            if opt_last_date is not None:
                opt_last_date_str = DateUtil.format_to_ymdhm(opt_last_date)
            print('%s start_date: %r, last_date: %r, max_obs_time: %r, out_interval: %s' % (
                    method_name, opt_start_date_str, opt_last_date_str,
                    DateUtil.format_to_ymdhm(max_obs_time), self.options.out_interval))
            print ('%s new_cdl: %s.' % (method_name, cdlfile))
        
        self.log_time.stop()
        cdl_duration = self.log_time.get_durations()
        cdl_duration_str = self.log_time.get_duration_strings()
        self.log_time.reset()
        
        if opt_start_date is None:
            print ( '%s %s start date is missing, nothing to do' % (method_name, base2nc.INFO_P))
        elif opt_last_date is None:
            print ( '%s %s   end date is missing, nothing to do' % (method_name, base2nc.INFO_P))
        else:
            while start_date < opt_last_date and start_date < max_obs_time:
                # Compute end_date
                end_date = self.get_loop_end_date( DateUtil.format_to_ymdhms(start_date), opt_last_date)
        
                self.set_initial_values(self.get_station_count())
                # next, create the sams.cdf from the cdlfile and fill in base_time
                # Fill dimensions (platform, lat, lon and elev) by the derived class
                #
                self.cdf_file = self.create_netcdf(cdlfile, start_date)
                #if debug: print ( '%s cdf_file.variables: ' % (method_name), self.cdf_file.variables)
                mysql_sdate = DateUtil.format_to_mysql_date(start_date)
                start_seconds = int(DateUtil.convert_to_time( start_date ))
                if 'base_time' in self.cdf_file.variables:
                    self.cdf_file.variables['base_time'].string = mysql_sdate
                    if Use_PyNIO:
                        self.cdf_file.variables['base_time'].assign_value( start_seconds )
                    else:
                        self.cdf_file.variables['base_time'][:] = start_seconds
    
                if debug: 
                    ncfile = self.get_netcdf_name(start_date)
                    print ('%s created CDF output file %s with start date/time of %r\n' % (
                            method_name, ncfile, start_date))
    
                self.log_time_db_access.reset()
                data_request = self.build_select_sql(start_date, end_date)
                try:
                    self.cursor.execute( data_request )
                except:
                    exit_message="Can't execute SQL data request \n"
                    handle_sql_exception(data_request, exit_message, method_name)
    
                rows = self.cursor.fetchall()
                self.log_time_db_access.stop()
                mysql_edate = DateUtil.convert_to_mysql_date(end_date)
                if rows and rows[0]:
                    if debug:
                        print('%s found %d rows for %s and %s SQL: %s' % (
                            method_name, len(rows), mysql_sdate, mysql_edate, data_request))
                    self.fill_variable_values(rows, start_seconds)
                else:
                    print('%s %s  No row for %s and %s SQL: %s' % (
                        method_name, base2nc.INFO_P, mysql_sdate, mysql_edate, data_request))
                    
                start_date = DateUtil.add_time_to_date( end_date, 60)
                #start_date = DateUtil.format_to_ymdhms(start_date)
                if debug: print ('%s next start_date: %r from %r' % (method_name, start_date, end_date))
                (db_access_duration, db_access_process) = self.log_time_db_access.get_duration_strings()
                (fill_var_duration,  fill_var_process)  = self.log_time_fill_var.get_duration_strings()
                (nc_write_duration,  nc_write_process)  = self.log_time_nc_write.get_duration_strings()
                #print ('%s next start_date: %r from %r' % (method_name, start_date, end_date))
                print ('     durations: db access: %s [CPU: %s], fill: %s [CPU: %s], write: %s [CPU: %s]' % (
                        db_access_duration, db_access_process, fill_var_duration,
                        fill_var_process, nc_write_duration, nc_write_process))
            self.log_time.stop()
            db_core_duration = self.log_time.get_durations()
            db_core_duration_str = self.log_time.get_duration_strings()
            print('%s Prep: %s (CPU: %r), CDL: %s (CPU: %r), Core: %s (CPU: %r)' % (method_name,
                    pref_duration_str[0], pref_duration[1], cdl_duration_str[0], cdl_duration[1],
                    db_core_duration_str[0], db_core_duration[1]))


    def create_cdl(self):
        #cdl_file = self.cdl_creator.createCDL(self.get_work_dir(), self.build_atributes_for_cdl())
        return self.cdl_creator.createCDL(self.get_work_dir(), self.build_atributes_for_cdl())
            
    def create_netcdf(self, cdlfile, start_date = '' ):
        local_debug = self.options.debug
        method_name = '     %s.%s' % (MODULE_NAME, 'create_netcdf')
        #cdl_file = self.cdl_creator.createCDL(self.get_work_dir(), self.build_atributes_for_cdl())
        ncgen  = self.config.get_ncgen()
        ncgen_paths = ncgen.split(os.path.sep)
        if ncgen != ncgen_paths[-1]:
            if not os.path.exists(ncgen):
                print(" ncgen from config [{f}] does not exist!".format(f=ncgen))
                ncgen = ncgen_paths[-1]
        ncfile = self.get_netcdf_name(start_date)
        cmd = "%s -o %s %s" % ( ncgen, ncfile, cdlfile )
        #if local_debug: print ('%s cmd: %s' % (method_name, cmd))
        if is_MS_Windows:
            p = Popen(cmd, shell=True, bufsize=4096, stdin=PIPE, stdout=PIPE, stderr=PIPE)
        else:
            p = Popen(cmd, shell=True, bufsize=4096, stdin=PIPE, stdout=PIPE, stderr=PIPE, close_fds=True)
        #p.wait()
        (std_output, std_error) = p.communicate()
        if local_debug: print ('%s cmd: %s:\n%s' % (method_name, cmd, std_output))
        if 0 != p.returncode:
            print('  == ERROR from command [%s]:\n%s' % (cmd, std_error))

        try:
            if Use_PyNIO:
                cdf_file = Nio.open_file(ncfile,"w")
            else:
                cdf_file = Dataset(ncfile, "a")
        except Exception as e:
            print('  The filename is {f}, exception: {e}'.format(f=ncfile,e=e))
            traceback.print_exc()
            raise e

        return cdf_file
            
    def get_config(self, config_name):
        return BaseConfig(config_name)
        #pass

    def get_current_time_string(self):
        return time.strftime('%Y-%m-%d %X %Z')
    
    def get_db_cursor(self):
        return self.cursor
    
    def get_end_date(self):
        end_date = self.options.end_time
        if end_date is None and None != self.options.start_time:
            end_date = self.options.start_time.replace('-','').replace(':','').replace(' ','')[0:8]
        if end_date is not None:
            end_date = end_date.replace('-','').replace(':','').replace(' ','')
            if len(end_date) == 8:
                end_date = '%s2359' % end_date
            elif len(end_date) == 10:
                end_date = '%s59' % end_date
            end_date = DateUtil.convert_to_datetime(end_date)
        return end_date

    def get_number_of_days_for_stations(self):
        return 2
    
    # Alias: O for observation table and S for station information
    def get_join_table_name_with_station(self, join_on=None, yyyymm=None):
        table_name = self.get_table_name()
        if yyyymm is not None:
            table_name = '%s_%s' % (table_name, yyyymm)
        if join_on is None:
            join_on = 'O.unique_site_id=S.unique_site_id AND S.range_code IS NOT NULL'
        return '`%s` O LEFT JOIN `%s` S ON %s' % (
            table_name, self.config.get_station_table_name(), join_on)
    
    def get_log_dir(self):
        return self.config.get_log_dir()

    def get_loop_end_date(self, cur_start_date, end_date):
        method_name = '%s.%s()' % (MODULE_NAME, 'get_loop_end_date')
        next_end_date = end_date
        if self.options.out_interval == 'hour':
            next_end_date = next_end_hourly(cur_start_date, end_date)
        elif self.options.out_interval == 'day':
            next_end_date = next_end_daily(cur_start_date, end_date)
        else:
            print ('%s unknown option [%s] for out_interval [-o] ' % (method_name, self.options.out_interval))
            #next_end_date = next_end_daily(cur_start_date, end_date)
        return next_end_date
    
    def get_max_obs_time(self):
        sql_string = 'SELECT MAX(%s) FROM `%s`' % (self.get_date_column_name(), self.get_table_name())
        max_obs_time = get_single_value_from_db(self.cursor, sql_string)
        return max_obs_time
        
    def get_min_obs_time(self):
        sql_string = 'SELECT MIN(%s) FROM `%s`' % (self.get_date_column_name(), self.get_table_name())
        max_obs_time = get_single_value_from_db(self.cursor, sql_string)
        return max_obs_time
        
    def get_name(self):
        return ('%s2nc' % self.get_datatype())
    
    def get_netcdf_name(self, start_date):
        debug = False
        #debug = not debug
        #cdl_file = self.cdl_creator.createCDL(self.get_work_dir(), self.build_atributes_for_cdl())
        #nc_name = self.cdl_creator.get_ncfile_name(self.get_work_dir(), start_date)
        #print ( ' base2nc.get_netcdf_name() nc_name: %s ' % nc_name)
        #return nc_name
        method_name = '%s.%s' % (MODULE_NAME, 'get_netcdf_name')
        if debug:   print('%s is called, start_date: %r' % (method_name, start_date))
        return self.cdl_creator.get_ncfile_name(self.get_work_dir(),
                DateUtil.format_to_ymdhms(start_date))
            
    def get_next_date(self, prev_end_date):
        return get_start_date_from_end_date(prev_end_date)

    def get_range_codes_for_nc(self):
        range_codes = self.stationActor.get_range_codes()
        f_range_codes = []
        for range_code in range_codes:
            f_range_codes.append(range_code.ljust(base2nc.DEF_FIELD_LENGTH, ' '))
        return f_range_codes

    #@abstractmethod    
    def get_station_count(self):
        return self.stationActor.get_station_count()

    def get_station_index(self, range_code):
        return self.stationActor.get_station_index(range_code)

    def get_station_list(self):
        #if len(self.stationActor.stationActor) == 0:
        #        self.add_station_list()
        return self.stationActor.get_station_list()

    def get_station_actor(self):
        return Stations(self.cursor)
    
    def get_start_date(self):
        start_date = self.options.start_time
        if start_date is not None:
            while len(start_date) < 14:
                start_date = '%s0' % start_date
            start_date = DateUtil.convert_to_datetime(start_date)
        else:
            #start_date = DateUtil.format_to_ymdhms(DateUtil.get_now())
            start_date = DateUtil.get_now()
        return start_date

    def get_table_name(self):
        return self.config.get_table_name()
    
    def get_work_dir(self):
        return self.config.get_work_dir()
    
    def is_debug_enabled(self):
        return self.options.debug
    
    def release_db(self):
        if self.cursor: self.cursor.close()
        if self.db_conn: self.db_conn.close()
    
    def setup_logs(self):
        # set up log files
        #
        ymd = DateUtil.format_to_ymd(DateUtil.get_now())
        #yday = datetime.datetime.strftime(yesterday, "%Y%m%d")
        self.logfile = '%s/%s.%s.%s.log' % (
            self.get_log_dir(), self.get_name(), ymd, self.config.get_site())
        #print('%s/%s.%s.%s.log' % (self.get_log_dir(), self.get_name(), ymd, self.config.get_site()))


        if self.options.opt_log:
            #logfile_path = "%s/%s.log" % (self.get_log_dir(), self.get_name())
            logfile = open( self.logfile, 'a')
            self.logfile = logfile 
            sys.stdout = logfile
            sys.stderr = logfile
        
