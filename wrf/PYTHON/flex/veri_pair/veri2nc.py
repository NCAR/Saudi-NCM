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
# veri2nc.py:  Save the verification pair data into NetCDF.
#
# Note: The db_host is from ~/config/db.config. The db_key should be defined
#       at the configuration files (veri2nc.cfg, XXX2nc.cfg)
#
############################################################

'''
Created on Jan 16, 2015

@author: hsoh
'''

import os.path
from VerificationConfigReader import ConfigReader
from atec.obs2nc.base2nc import base2nc, Use_PyNIO
from atec.obs2nc.ObsDates import VerificationDates
#from atec.obs2nc.Stations import Stations
from atec.util.DateUtil import DateUtil, LogTime

MODULE_NAME = 'veri2nc'

#OPT_Use_model_table = False

def convert_to_short_by_10(value):
    converted_value = value
    if not base2nc.is_missing_value():
        converted_value = value*10
    return short(converted_value)

def convert_to_short_by_100(value):
    converted_value = value
    if not base2nc.is_missing_value():
        converted_value = value*100
    return short(converted_value)



class veri2nc(base2nc):
    '''
    classdocs
    '''
    #COLUMN_LIST="obs_time,unique_site_id, pres, temp, rh, wdir, wspd, uwind, vwind, wwind"

    SQL_LEAD_TIME = 'M.lead_time'
    SQL_LEAD_TIME = 'HOUR(TIMEDIFF(M.valid_time,M.cycle_time))'
    data_request_order_by = 'ORDER BY M.cycle_time, M.lead_time, M.valid_time DESC'
    data_request_template = '''
      SELECT YEAR(cycle_time)*10000+MONTH(cycle_time)*100+DAY(cycle_time) cycle_date, {lt}, SI.range_code,
             M.temp, M.q2, M.pres, M.wspd_10m, M.wdir_10m, O.temp, O.q2, O.pres, O.wspd_10m, O.wdir_10m,
             O.temp_qc, O.q2_qc, O.pres_qc, O.wspd_10m_qc, O.wdir_10m_qc,
             O.temp_qc2, O.pres_qc2, O.wspd_10m_qc2, O.wdir_10m_qc2
      FROM surface_model_data M
           LEFT JOIN surface_obs_data O ON M.unique_site_id=O.unique_site_id AND M.valid_time=O.obs_time
           LEFT JOIN station_information SI on M.unique_site_id = SI.unique_site_id
      WHERE M.model_id={m} AND M.domain_id={d} AND M.unique_site_id like '{s}_s%%' AND HOUR(M.cycle_time)={i} 
            AND M.valid_time >= M.cycle_time AND {lt}<{ml} AND M.{cn} BETWEEN {sb} AND SI.range_code IS NOT NULL'''
    
    OPT_SAVE_AS_SHORT = False

    SITE_NAME_LENGTH = 24
    DEF_LEAD_TIMES = 24
    
    def __init__(self, db_name='verification'):
        '''
        Constructor
        '''
        super(veri2nc, self).__init__(db_name)
        self.debug = self.options.debug
        method_name = '     %s.%s()' % (MODULE_NAME, '__init__')
        
        self.lead_times = int(self.options.lead_hours) + 1
        self.cycle_hour = int(self.options.cycle_hour)
        self.max_recent_days = int(self.options.max_recent_days)
        #print("   __init__(): self.options.max_recent_days=%r" % (self.options.max_recent_days))
        self.model_id = int(self.options.model)
        self.domain_id = int(self.options.domain)
        self.verification_dates = VerificationDates.get_singleton(
                self.config.get_range_name(), self.cursor)
        self.verification_dates.set_arguments(self.model_id,
                self.cycle_hour, self.domain_id, self.max_recent_days)
        if self.options.end_time is not None:
            end_time_sql = "'%s'" % self.options.end_time
            self.verification_dates.set_argument('end_time',end_time_sql)
        self.verification_dates.get_cycle_dates_from_db()
        if self.debug or self.is_debug_enabled():
            print('%s cycle_hour: %d, model_id: %d, domain_id: %d' % (
                method_name, self.cycle_hour, self.model_id, self.domain_id))
        if 0 < self.log_time.get_current_duration():
            self.log_time.stop()
            duration = self.log_time.get_durations()
            duration_str = self.log_time.get_duration_strings()
            print('%s took: %s (CPU: %s) to get dates' % (
                    method_name, duration_str[0], duration[1]))
            self.log_time.reset()
        self.debug = self.is_debug_enabled()
    
    def add_more_attributes_for_cdl(self, attributes): 
        attributes['__NGENDATES__'] = len(self.verification_dates.dates) 
        attributes['__N_LEAD_TIMES__'] = self.lead_times
        
    def add_options(self, parser):
        parser.add_option("--cycle_hour", dest="cycle_hour", default=2,
                help=" Set the first cycle hour, 0 or 2 - optional, default 2")
        parser.add_option("--domain", dest="domain", default=3, help=" Set the domain id - optional, default 3")
        parser.add_option("--model",  dest="model",  default=0, help=" Set the model id - optional, default 0")
        parser.add_option("-m", "--max_recent_days",  dest="max_recent_days",  default=0,
                help=" Maximum recent days, default 0 which is for all")
        parser.add_option("--lead_hours", "--lead-hours",  dest="lead_hours",  default=veri2nc.DEF_LEAD_TIMES,
                help=" The lead time in hours, default is {d}".format(d=veri2nc.DEF_LEAD_TIMES))
        parser.add_option("--use-obs-tbl", "--use_obs_tbl", dest="opt_use_obs_tbl",
                action="store_true", default=False,
                help=" Use surface_model_data instead of surface_obs_data as meta data - optional")
    
    def build_select_sql(self, start_date, end_date):
        debug = False
        debug = debug or self.debug
        method_name = '%s.%s()' % (MODULE_NAME, 'build_select_sql')
        
        site = self.config.get_range_name()

        #if self.max_recent_days > 0:
        #    sql_between_date = "DATE_ADD(NOW(), interval -%d DAY) AND NOW()" % self.max_recent_days
        #else:
        #    sql_between_date = self.verification_dates.SQL_ANALOG_BETWEEN
        sql_extra_where = "model_id={m}".format(m=self.model_id)
        date_column_name = self.verification_dates.get_obs_time_column_name()
        sql_between_date = self.verification_dates.get_sql_date_between(sql_extra_where)

        if debug or self.options.debug:
            print( '%s %s' % (method_name, veri2nc.data_request_template))
            print( self.domain_id, site, self.cycle_hour, self.model_id,
                    self.lead_times, date_column_name, sql_between_date)

        data_request = veri2nc.data_request_template.format(lt=veri2nc.SQL_LEAD_TIME,
                m=self.model_id, d=self.domain_id, s=site, i=self.cycle_hour,
                ml=self.lead_times, cn=date_column_name, sb=sql_between_date)

        data_request = "%s %s" % (data_request, veri2nc.data_request_order_by)

        if debug or self.options.debug_sql == 1:
            #print("data_request_template: ", data_request_template)
            print("   ==================")
            print("   %s data request: %s" % (method_name, data_request))
            print("   ==================")
        return data_request 

    def get_config(self, config_name):
        debug = False
        method_name = '   %s.%s()' % (MODULE_NAME, 'get_config')
        if debug:  print('%s %s' % (method_name, 'is called'))
        return ConfigReader(config_name)
    
    def get_datatype(self):
        return 'veri'
    
    def get_date_column_name(self):
        return 'obs_time' if (self.options is not None and self.options.opt_use_obs_tbl) else 'cycle_time'

    def get_end_date(self):
        if self.options.end_time is None:
            #end_date = self.get_max_obs_time()
            end_date = self.verification_dates.get_max_cycle_time()
        else:
            end_date = super(veri2nc, self).get_end_date()
        if end_date is not None:
            end_date = end_date.replace(second=59)
        return end_date
    
    def get_loop_end_date(self, cur_start_date, end_date):
        return end_date

    def get_netcdf_name(self, start_date):
        debug = False
        debug = debug or self.debug
        method_name = '%s.%s' % (MODULE_NAME, 'get_netcdf_name')
        max_cycle_time = self.verification_dates.get_max_cycle_time()
        if debug:   print("%s max_cycle_time: %r" % (method_name, max_cycle_time))
        if max_cycle_time is None:
            cycle_str = 'yyyymmdd'
        else:
            cycle_str = DateUtil.format_to_ymd(max_cycle_time)
        ymdh = '%s%02d' % (cycle_str, self.cycle_hour)
        if self.max_recent_days is not None and self.max_recent_days > 0:
            ymdh = '%s.recent' % (ymdh)
        return self.cdl_creator.get_ncfile_name(self.get_work_dir(), ymdh)
            
    def get_number_of_days_for_stations(self):
        #return 15
        return 60
        
    
    def get_start_date(self):
        #return self.get_min_obs_time()
        return self.verification_dates.get_min_cycle_time()

    def get_table_name(self):
        return 'surface_obs_data' if (self.options is not None and self.options.opt_use_obs_tbl) else 'surface_model_data'

    def fill_variable_values(self, rows, start_seconds):
        debug = False
        debug = debug or self.debug
        method_name = '     %s.%s()' % (MODULE_NAME, 'fill_variable_values')
        #
        # okay, we've supposedly got lots of stuff to process, time to break out
        # the by column & by row processing.  For netcdf, we need to 1) set the
        # base time to the oldest time, then set a time increment = the difference
        # between original time and the time of each "set" of sams data and 2)
        # write the correct number of records for each "set", filling in empty/no
        # data fields with their default values.  Oh, we also need to re-scale
        # a few things - and we need to keep track of how many "sets" we've written
        # into this netcdf file.  Note that we initialize $irec to -1.  We use
        # this value to determine if we in fact got any data from the (-1 = no
        # data was retrieved from the DB).
        #
        #Set default values for all arrays
        #(tdry, q2, pressure, wdir, wspd) = set_values(sams_station_count)
        if rows is None:
            print("Error no record!!!")
            return
        if (0 == len(rows)):
            print("Error empty record!!!")
            return
        
        first_range_code = self.get_range_codes_for_nc()[0]
        verification_dates = self.verification_dates
        sams_station_count = self.get_station_count()
        print('%s  station count: %d, date count: %d, row_count: %d, start_seconds: %d' % (
                method_name, sams_station_count, len(verification_dates.dates), len(rows), start_seconds))
        self.set_initial_values(sams_station_count)
  
        row_count = 0
        total_row_count = len(rows)
        prev_gen_date = rows[0][0]
        last_gen_date = rows[total_row_count-1][0]
        station_list_allowed = {}
        station_list_not_allowed = []
        
        date_index = -1
        row_count_10percent = total_row_count / 10
        daily_row_count = 0
        temp_obs_null_count = 0
        temp_obs_missing_count = 0
        temp_obs_daily_null_count = 0
        temp_obs_daily_missing_count = 0
        self.log_time_fill_var.reset()
        for row in rows:
            (gen_date, lead_time, site_code, tmpc_m, q2_v_m, pres_m, sped_m, drct_m, \
             tmpc_o, q2_v_o, pres_o, sped_o, drct_o \
             , o_tmpc_qc, o_q2_qc, o_pres_qc, o_sped_qc, o_drct_qc \
             , o_tmpc_qc2, o_pres_qc2, o_sped_qc2, o_drct_qc2) = row
            #print(' range_code: %s, dt: %s type: %s' % (range_code, dt, type(dt).__name__))
            #if debug: print("  row: ", row)
            if site_code in station_list_not_allowed:
                continue
            snum = station_list_allowed.get(site_code, None)
            if snum is None: 
                snum = self.get_station_index( site_code )
                if snum > -1:
                    station_list_allowed[site_code] = snum
            
            #print(' === DEBUG HS station_index: %r from %r' % (snum, site_code))
            if snum == -1:
                station_list_not_allowed.append(site_code)
                print("%s == WARN == Skipping SAMS site_code %s" % (method_name, site_code))
            else:
                if prev_gen_date != gen_date:
                    if debug:
                        #print("  Write to NetCDF for [{p}], next date: [{n}]".format(
                        #        p=prev_gen_date, n=gen_date))
                        if temp_obs_daily_null_count > 0:
                            print("       {d} row count={r:4}, temp: missing={m:3}, null={n:3} [per_site={n1}]".format(
                                    d=prev_gen_date, r=daily_row_count, m=temp_obs_daily_missing_count,
                                    n=temp_obs_daily_null_count,
                                    n1=((temp_obs_daily_missing_count+temp_obs_daily_null_count)/sams_station_count)))
                    date_index = verification_dates.get_date_index(prev_gen_date)
                    if date_index == base2nc.BAD_INDEX:
                        print("     == WARN == main() The cycle date [%s] is not included" % (prev_gen_date))
                    else:
                        self.write_netcdf( date_index )
                    
                    self.set_initial_values(sams_station_count)
                    prev_gen_date = gen_date
                    daily_row_count = 0
                    temp_obs_daily_null_count = 0
                    temp_obs_daily_missing_count = 0
    
                reset_model_value = (last_gen_date != gen_date)
                self.update_arrays(reset_model_value,lead_time,snum,tmpc_m,pres_m,q2_v_m,drct_m,sped_m \
                                   , tmpc_o,pres_o,q2_v_o,drct_o,sped_o \
                                   , o_tmpc_qc, o_q2_qc, o_pres_qc, o_sped_qc, o_drct_qc \
                                   , o_tmpc_qc2, o_pres_qc2, o_sped_qc2, o_drct_qc2)
                row_count += 1
                daily_row_count += 1
                if debug:
                    if date_index > (verification_dates.get_date_count()-3) and site_code == first_range_code:
                        print("       gen_date: {g}, lead: {l:2} site: {c}, temp_m: {m:6} temp_o: {o}".format(
                            g=gen_date,l=lead_time,c=site_code,m=tmpc_m,o=tmpc_o))
                if tmpc_o is None:
                    temp_obs_null_count += 1
                    temp_obs_daily_null_count += 1
                elif tmpc_o == base2nc.MISSING_VALUE or tmpc_o == -9999.0:
                    temp_obs_missing_count += 1
                    temp_obs_daily_missing_count += 1
        
                if 0 == row_count % row_count_10percent:
                    print("     Processed %d records out of %d" % (row_count, total_row_count))

        print("  Write to NetCDF for [{n}]".format(n=gen_date))
        print("         row count: {r:5}, temp: missing: {m:5}, null: {n:5} [{n1}]".format(
                r=daily_row_count, m=temp_obs_daily_missing_count,
                n=temp_obs_daily_null_count, n1=(temp_obs_daily_null_count/sams_station_count)))
        print("    all: row count: {r:5}, temp: missing: {m:5}, null: {n:5} [{n1}]".format(
                r=row_count, m=temp_obs_missing_count,
                n=temp_obs_null_count, n1=(temp_obs_null_count/sams_station_count)))
        print("")
        
        self.log_time_fill_var.stop()
        
        self.log_time_nc_write.reset()
        self.write_netcdf( verification_dates.get_date_index(gen_date) )
        self.write_netcdf_dimensions()
        self.cdf_file.close()
        self.log_time_nc_write.stop()

    
    def set_initial_values(self, station_count):
        debug = False
        debug = debug or self.debug
        def_name = "  %s:%s" % (__name__, "set_initial_values")
        #
        # For each new time delta, we clear out the old array values and
        # populate them with the default value.  That way, if we are missing
        # information after reading in the sams data, we don't have to spend
        # time trying to figure out what we do/don't have.  Note that we
        # don't populate the latitude, longitude, and elevation/alt
        # arrays - we did that before we started.
        #
        if debug: print("%s setting default values" % (def_name))
        
        #global tdry_m,q2_m,pressure_m,wdir_m,wspd_m,tdry_o,q2_o,pressure_o,wdir_o,wspd_o
  
        self.tdry_m     = [[base2nc.MISSING_VALUE for y in range(station_count)] for x in range(self.lead_times)]
        self.q2_m       = [[base2nc.MISSING_VALUE for y in range(station_count)] for x in range(self.lead_times)]
        self.pressure_m = [[base2nc.MISSING_VALUE for y in range(station_count)] for x in range(self.lead_times)]
        self.wdir_m     = [[base2nc.MISSING_VALUE for y in range(station_count)] for x in range(self.lead_times)]
        self.wspd_m     = [[base2nc.MISSING_VALUE for y in range(station_count)] for x in range(self.lead_times)]
        self.tdry_o     = [[base2nc.MISSING_VALUE for y in range(station_count)] for x in range(self.lead_times)]
        self.q2_o       = [[base2nc.MISSING_VALUE for y in range(station_count)] for x in range(self.lead_times)]
        self.pressure_o = [[base2nc.MISSING_VALUE for y in range(station_count)] for x in range(self.lead_times)]
        self.wdir_o     = [[base2nc.MISSING_VALUE for y in range(station_count)] for x in range(self.lead_times)]
        self.wspd_o     = [[base2nc.MISSING_VALUE for y in range(station_count)] for x in range(self.lead_times)]

    def update_arrays(self,reset_model_value,lead_time,snum,tmpc_m,pres_m,q2_v_m,drct_m,sped_m \
                      ,tmpc_o,pres_o,q2_v_o,drct_o,sped_o \
                      ,o_tmpc_qc, o_q2_qc, o_pres_qc, o_sped_qc, o_drct_qc \
                      ,o_tmpc_qc2, o_pres_qc2, o_sped_qc2, o_drct_qc2):
        debug = self.debug
        def_name = "   update_arrays()"
        
        if debug:
            print("%s updating arrays for lead_time: %d, site index:%d temp_m: %r, temp_o: %r" % (def_name, lead_time, snum, tmpc_m, tmpc_o))
        #if debug == 1:  print("%s B updating arrays for lead_time: %d, site index:%d " % (ua_def_name, lead_time, snum), tdry)

        #if veri2nc.OPT_SAVE_AS_SHORT:
        #    self.tdry_m[lead_time][snum]      = convert_to_short_by_100(tmpc_m)
        #    self.q2_m[lead_time][snum]        = convert_to_short_by_100(q2_v_m)
        #    self.wdir_m[lead_time][snum]      = convert_to_short_by_100(drct_m)
        #    self.wspd_m[lead_time][snum]      = convert_to_short_by_100(sped_m)
        #    self.pressure_m[lead_time][snum]  = convert_to_short_by_100(pres_m)
        #    self.tdry_o[lead_time][snum]      = convert_to_short_by_100(tmpc_o)
        #    self.q2_o[lead_time][snum]        = convert_to_short_by_100(q2_v_o)
        #    self.wdir_o[lead_time][snum]      = convert_to_short_by_100(drct_o)
        #    self.wspd_o[lead_time][snum]      = convert_to_short_by_100(sped_o)
        #    self.pressure_o[lead_time][snum]  = convert_to_short_by_10(pres_o)
        #else:
        if not reset_model_value:
            self.tdry_m[lead_time][snum]      = tmpc_m
            self.wdir_m[lead_time][snum]      = drct_m
            self.wspd_m[lead_time][snum]      = sped_m
            self.q2_m[lead_time][snum]        = q2_v_m
            self.pressure_m[lead_time][snum]  = pres_m

        if tmpc_o is not None and (o_tmpc_qc > 0 or o_tmpc_qc2 > 0) and tmpc_o != base2nc.MISSING_VALUE:
            self.tdry_m[lead_time][snum]      = tmpc_m
            self.tdry_o[lead_time][snum]      = tmpc_o
        if  drct_o is not None and sped_o is not None and (o_drct_qc > 0 or o_drct_qc2 > 0) \
                and (o_sped_qc > 0 or o_sped_qc2 > 0) and (drct_o != 0 and sped_o != 0):
            self.wdir_m[lead_time][snum]      = drct_m
            self.wspd_m[lead_time][snum]      = sped_m
            self.wdir_o[lead_time][snum]      = drct_o
            self.wspd_o[lead_time][snum]      = sped_o
        if q2_v_o is not None and o_q2_qc > 0:
            self.q2_m[lead_time][snum]        = q2_v_m
            self.q2_o[lead_time][snum]        = q2_v_o
        if pres_o is not None and (o_pres_qc > 0 or o_pres_qc2 > 0):
            self.pressure_m[lead_time][snum]  = pres_m
            self.pressure_o[lead_time][snum]  = pres_o

    def write_netcdf( self, gen_date_index):
        debug = False
        debug = debug or self.debug
        def_name = "     %s:%s" % (__name__, "write_netcdf")

        if debug:  print("%s writing netcdf record number %d" % (def_name, gen_date_index))

        self.cdf_file.variables['temp_m'][gen_date_index]     = self.tdry_m
        self.cdf_file.variables['q2_m'][gen_date_index]       = self.q2_m
        self.cdf_file.variables['wdir_m'][gen_date_index]     = self.wdir_m
        self.cdf_file.variables['wspd_m'][gen_date_index]     = self.wspd_m
        self.cdf_file.variables['pres_m'][gen_date_index]     = self.pressure_m
        self.cdf_file.variables['temp_o'][gen_date_index]     = self.tdry_o
        self.cdf_file.variables['q2_o'][gen_date_index]       = self.q2_o
        self.cdf_file.variables['wdir_o'][gen_date_index]     = self.wdir_o 
        self.cdf_file.variables['wspd_o'][gen_date_index]     = self.wspd_o
        self.cdf_file.variables['pres_o'][gen_date_index]     = self.pressure_o

    def write_netcdf_dimensions( self ):
        debug = False
        debug = debug or self.debug
        method_name = '     %s.%s()' % (MODULE_NAME, 'write_netcdf_dimensions')
        
        gen_dates   = self.verification_dates.dates
        range_codes = self.get_range_codes_for_nc()
        if debug: print('%s dimensions  latitudes/longitudes/elevations: %d/%d/%d sites: [%d, %d] gen_dates: %d' % (
                method_name, len(self.stationActor.get_latitudes()),
                len(self.stationActor.get_longitudes()),
                len(self.stationActor.get_elevations_km()),
                len(range_codes), len(range_codes[0]),
                len(gen_dates)))
        self.cdf_file.variables['lat'][:]       = self.stationActor.get_latitudes()
        self.cdf_file.variables['lon'][:]       = self.stationActor.get_longitudes()
        self.cdf_file.variables['alt'][:]       = self.stationActor.get_elevations_km()

        self.cdf_file.variables['gen_date'][:]  = gen_dates
        self.cdf_file.variables['lead_time'][:] = [x for x in range(self.lead_times)]
        if Use_PyNIO:
            self.cdf_file.variables['site'][:]      = range_codes
            self.cdf_file.variables['init_hour'].assign_value( self.cycle_hour )
        else:
            self.cdf_file.variables['init_hour'][:] = self.cycle_hour
            site_var = self.cdf_file.variables['site']
            for index in range(len(range_codes)):
                site_var[index]      = [x for x in range_codes[index]]
        if debug:
            print('{m} last day: {d}{h}, temperature of last two stations'.format(
                    m=method_name,d=gen_dates[-1],h=self.cycle_hour))
            print('                {s1:12}     {s2:12}'.format(
                    s1=range_codes[-1][0:11], s2=range_codes[-2][0:11]))
            print('        idx   model    obs.    model    obs.')
            for index in range (len(self.tdry_m)):
                print('        {i:2}: {m2:7} {o2:7}  {m1:7} {o1:7}'.format(i=index,
                        m2=self.tdry_m[index][-1], o2=self.tdry_o[index][-1],
                        m1=self.tdry_m[index][-2], o1=self.tdry_o[index][-2]))


def main():
    #testDbConfig()
    nc_generator = veri2nc(None)

    debug = nc_generator.is_debug_enabled()
    if debug:
        print('  == %s is started at %r' % (
                os.path.basename(__file__), nc_generator.get_current_time_string()))
    

    #for station in nc_generator.stationActor:
    #  print('    veri2nc.stationActor: %s' % (station.to_string()))
    nc_generator.convert_to_nc()
    duration_db = nc_generator.log_time_db_access.get_duration_strings()
    duration_fill = nc_generator.log_time_fill_var.get_duration_strings()
    duration_nc = nc_generator.log_time_nc_write.get_duration_strings()
    print("   === %s took DB access: %s (%s), Fill: %s (%s), NetCDF: %s (%s)" % (
            MODULE_NAME, duration_db[0], duration_db[1],
            duration_fill[0], duration_fill[1], duration_nc[0], duration_nc[1]))
    return nc_generator
    
if __name__ == '__main__':
    log_time = LogTime()
    nc_generator = main()
    log_time.stop()
    duration_overall = log_time.get_duration_strings()
    print("  === Done %s, took %s, process time: %s,  at %r" % (
            os.path.basename(__file__), duration_overall[0], duration_overall[1],
            nc_generator.get_current_time_string()))
    