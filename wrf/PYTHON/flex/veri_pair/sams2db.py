#!/usr/bin/env python

#*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*
# * Copyright (c) 2018 UCAR
# * University Corporation for Atmospheric Research (UCAR)
# * National Center for Atmospheric Research (NCAR)
# * Research Applications Laboratory (RAL)
# * P.O. Box 3000, Boulder, Colorado, 80307-3000 USA
# * All rights reserved. Licenced use only.
# * Do not copy or distribute without authorization.
# *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*
#
############################################################

'''
Created on March 16, 2017
'''

import os
import sys
from datetime import date, datetime, timedelta
import traceback
import numpy
import math

from atec.util.DateUtil import LogTime
from atec.veri_pair.verification_pair import base_import_verification_dir
from atec.dao.DbActor import DbActor, create_db_actor
from MapHandler import get_singleton as get_map_handler_singleton
from Table_QcFieldClimaticData import make_key as make_key_ClimaticData
from Table_QcThreshold import get_1min_map, get_5min_map, get_15min_map
from Table_QcThreshold import make_key as make_threshold_key
try:
    from Table_SurfaceObsData import Table_SurfaceObsData, SurfaceObsData
except ImportError:
    from atec.veri_pair.Table_SurfaceObsData import Table_SurfaceObsData, SurfaceObsData
try:
    from BaseObject import BaseObject, debug_message, error_message
except ImportError:
    from atec.dao.BaseObject import BaseObject, debug_message, error_message
from sams_qc import sams_qc, convert_tools, sams_qc_tools

Use_PyNIO = False
try:
    from netCDF4 import Dataset
except:
    Use_PyNIO = True
    import Nio 
    import PyNIO 

MODULE_NAME = 'sams2db_vdb'
MODULE_VERSION = '1.0.0.0001'

def get_variable_value(nc_var):
    if Use_PyNIO:
        values = nc_var.get_value()
    else:
        values = nc_var[:]
    return values

# def get_variable_values(nc_var):
#     values = nc_var[:]
#     return values

def open_netcdf(ncfile_name, mode = 'r', grid_format=''):
    #method_name = '%s.%s' % (MODULE_NAME, 'open_netcdf')
    nc_file = None
    local_mode = mode
    if Use_PyNIO:
        if 'a' == local_mode:
            local_mode = 'w' 
        try:
            if 0 < len(grid_format):
                nc_file = Nio.open_file(ncfile_name, local_mode, format=grid_format)
            else:
                nc_file = Nio.open_file(ncfile_name,local_mode)
        except Nio.NIOError as ne:
            error_message('on opening {f}'.format(f=ncfile_name))
            raise ne
    else:
        # 'a': append
        # 'w': create a new netcdf
        nc_file = Dataset(ncfile_name, local_mode)
    return nc_file

class sams2db_vdb(base_import_verification_dir, sams_qc):

    DATE_FORMAT_IN_FILE = "%y%m%d"
    DATE_FORMAT_YMD = "%Y-%m-%d"
    
    INPUT_SUB_DIRS='final'

    TABLE_NAME = 'surface_obs_data'

    INSERT_SQL = "INSERT IGNORE INTO {tn} (obs_time,unique_site_id,last_update,temp,pres,wspd_10m,wdir_10m,q2,temp_qc2,wspd_10m_qc2,wdir_10m_qc2,pres_qc2) VALUES ('{ot}','{s}',NOW(),{t},{p},{ws},{wd},{q2},{tq},{pq},{wsq},{wdq})"
    
    SCORE_MAX            =    100
    SCORE_MISSING        =  -1000   # -2
    SCORE_NOT_IN_RANGE   =  -2000   # -3
    SCORE_NOT_IN_RANGE_C =  -4000   # -4

    def __init__(self, job_user):
        base_import_verification_dir.__init__(self, job_user=job_user)
        sams_qc.__init__(self,range_name=self.range_name_F)
        
#         self.db_actor = create_db_actor(self.db_config, self.db_name_key)
#         self.map_handler = get_map_handler_singleton(self.db_actor)
#         
#         self.field_id_temp = None
#         self.field_id_pres = None
#         self.field_id_wspd = None
#         self.field_id_wdir = None
#         self.field_data_temp = None
#         self.field_data_pres = None
#         self.field_data_wspd = None
#         self.field_data_wdir = None
#         self.climatic_data_temp = None
#         self.climatic_data_pres = None
#         self.climatic_data_wspd = None
#         self.climatic_data_wdir = None
#         self.temporal_threshold_temp = None
#         self.temporal_threshold_pres = None
#         self.temporal_threshold_wspd = None
#         self.temporal_threshold_wdir = None
#         self.dao_entities = {}

#     def compute_score(self, value, prev_value, time_diff, climatic_data, threshold, missing_value):
#         qc_score = sams2db_vdb.SCORE_MISSING
#         if value is not None and value != missing_value:
#             qc_score = sams2db_vdb.SCORE_NOT_IN_RANGE_C
#             do_temporal_score = False
#             if climatic_data is None:
#                 do_temporal_score = True
#             elif value>= climatic_data.get_min_value() and value <= climatic_data.get_max_value():
#                 do_temporal_score = True
#             if do_temporal_score:
#                 qc_score = sams2db_vdb.SCORE_MAX
#                 if prev_value is not None and prev_value != missing_value \
#                         and threshold is not None and self.time_interval == time_diff:
#                     diff_value = value - prev_value
#                     qc_score = sams_qc_tools.compute_score(diff_value,
#                             threshold.get_threshold(), threshold.get_threshold_max())
#         return qc_score
#         
#     def do_QC(self,time_delta,temp,pres,wspd,wdir,prev_temp,prev_pres,prev_wspd,prev_wdir, missing_value):
#         temp_qc = self.compute_score(temp, prev_temp, time_delta,
#                 self.climatic_data_temp, self.temporal_threshold_temp, missing_value)
#         pres_qc = self.compute_score(pres,prev_pres, time_delta,
#                 self.climatic_data_pres, self.temporal_threshold_pres, missing_value)
#         wspd_qc = self.compute_score(wspd, prev_wspd, time_delta,
#                 self.climatic_data_wspd, self.temporal_threshold_wspd, missing_value)
#         wdir_qc = self.compute_score(wdir, prev_wdir, time_delta,
#                 self.climatic_data_wdir, self.temporal_threshold_wdir, missing_value)
#         return (temp_qc,pres_qc,wspd_qc,wdir_qc)
# 
#     def find_obs_data_entity(self, station_id, obs_time_str):
#         data_entity = None
#         entity_list = self.dao_entities.get(str(station_id), None)
#         if entity_list is not None:
#             data_entity = entity_list.get(obs_time_str, None)
#         return data_entity

    #@abstractmethod
    def get_input_sub_dirs(self):
        pass
    
    def get_previous_data(self, previous_input_name, station_count):
        debug = False
        #debug = not debug
        prev_time = None
        prev_temp = None
        prev_pres = None
        prev_wspd = None
        prev_wdir = None
        
        if previous_input_name is not None and os.path.exists(previous_input_name):
            try:
                nc_file = open_netcdf(previous_input_name)
                platform_var    = nc_file.variables.get('platform', None)
                prev_station_count = 0
                if platform_var is not None:
                    stations = platform_var[:]
                    prev_station_count = len(stations)
                
                if prev_station_count == station_count:
                    temp_var = nc_file.variables.get('tdry', None)
                    pres_var = nc_file.variables.get('pres', None)
                    wdir_var = nc_file.variables.get('wdir', None)
                    wspd_var = nc_file.variables.get('wspd', None)
                    time_offset_var = nc_file.variables.get('time_offset', None)
                    if time_offset_var is not None: prev_time = time_offset_var[-1]
                    if temp_var is not None:        prev_temp = temp_var[-1][:]
                    if pres_var is not None:        prev_pres = pres_var[-1][:]
                    if wspd_var is not None:        prev_wspd = wspd_var[-1][:]
                    if wdir_var is not None:        prev_wdir = wdir_var[-1][:]
                else:
                    self.error_message("The number of stations do not match ({s1} VS {s2})".format(
                            s1=station_count, s2=prev_station_count))
            except:
                self.display_exception("Error on reading the previous day")
                
            self.debug_message("previous data: time offset: {d} temp: {t}, pres: {p}, wspd: {ws}, wdir: {wd}".format(
                    d=prev_time,t=prev_temp[:],p=prev_pres[:],ws=prev_wspd[:],wd=prev_wdir[:]))
                
        
        #station_indices = range(0, station_count)
        #if prev_temp is None: prev_temp = [missing_value for y in station_indices]
        #if prev_pres is None: prev_pres = [missing_value for y in station_indices]
        #if prev_wspd is None: prev_wspd = [missing_value for y in station_indices]
        #if prev_wdir is None: prev_wdir = [missing_value for y in station_indices]
        return (prev_time, prev_temp, prev_pres,prev_wspd, prev_wdir)
    
    def get_previous_data_from_db(self, dao_table, input_date, station_ids, missing_value):
        debug = False
        #debug = not debug
        prev_time = None
        prev_temp = [missing_value for station in station_ids]
        prev_pres = [missing_value for station in station_ids]
        prev_wspd = [missing_value for station in station_ids]
        prev_wdir = [missing_value for station in station_ids]
        records = dao_table.get_records('MAX(obs_time)', sql_where="obs_time<'{d}'".format(d=input_date))
        if records is not None and len(records) > 0:
            prev_date  = records[0][0]
            if prev_date is not None:
                print('prev time for tempral difference: {p}'.format(p=prev_date))
                station_count = len(station_ids)
                rows = dao_table.get_records('*', sql_where="obs_time='{d}'".format(d=prev_date),
                                             order_by='unique_site_id')
                #print('records prev_date: station_count', station_count, 'len(rows)', len(rows))
                if rows is not None and len(rows) > 0:
                    dao_entity = dao_table.get_data(rows[0])
                    time_diff = dao_entity.get_obs_time() - datetime(input_date.year, input_date.month, input_date.day)
                    prev_time = time_diff.total_seconds()
                    #if len(rows) == station_count:
                    #    for sIdx, row in rows:
                    #        prev_temp[sIdx] = row[SurfaceObsData.TEMP]
                    #        prev_pres[sIdx] = row[SurfaceObsData.PRES]
                    #        prev_wspd[sIdx] = row[SurfaceObsData.WSPD]
                    #        prev_wdir[sIdx] = row[SurfaceObsData.WDIR]
                    #else:
                    for sIdx, station in enumerate(station_ids):
                        for row in rows:
                            if row[SurfaceObsData.UNIQUE_SITE_ID] == station:
                                prev_temp[sIdx] = row[SurfaceObsData.TEMP]
                                prev_pres[sIdx] = row[SurfaceObsData.PRES]
                                prev_wspd[sIdx] = row[SurfaceObsData.WSPD]
                                prev_wdir[sIdx] = row[SurfaceObsData.WDIR]
        
#         if prev_time is None:
#             station_indices = range(0, station_count)
#             if prev_temp is None: prev_temp = [missing_value for y in station_indices]
#             if prev_pres is None: prev_pres = [missing_value for y in station_indices]
#             if prev_wspd is None: prev_wspd = [missing_value for y in station_indices]
#             if prev_wdir is None: prev_wdir = [missing_value for y in station_indices]
        return (prev_time, prev_temp, prev_pres,prev_wspd, prev_wdir)
    
    def get_station_list(self, platform_var):
        first_station_id = None
        station_ids = []
        if platform_var is not None:
            stations = platform_var[:]
            station_count = len(stations)
            if 0 < station_count:
                station_indices = range(0, station_count)
                for s_idx in station_indices:
                    station_id = None
                    #print(" DEBUG: data types: ", type(stations[s_idx]), type(stations[s_idx][0]))
                    if stations[s_idx] is not numpy.ma.masked:
                        try:
                            station_id = str(stations[s_idx], 'utf-8').strip()
                        except TypeError:
                            station_id = ''.join(stations[s_idx]).strip()
                    station_ids.append(station_id)
                for idx in range(len(station_ids)):
                    if station_ids[idx] is not None:
                        first_station_id = station_ids[idx]
                        break
        return (station_ids, first_station_id);
        
    #@abstractmethod
    def get_table_name(self):
        return sams2db_vdb.TABLE_NAME
    
#     def get_time_interval(self, time_offset_list):
#         debug = False
#         debug = not debug
#         count_01min = 0
#         count_05min = 0
#         count_15min = 0
#         time_count = len(time_offset_list)
#         for t_idx in range(0, time_count-2):
#             time_diff = time_offset_list[t_idx+1] - time_offset_list[t_idx]
#             if time_diff <= 60:
#                 count_01min += 1
#             elif time_diff <= 300:
#                 count_05min += 1
#             else:
#                 count_15min += 1
#         if count_05min > count_15min:
#             time_interval = 300 if (count_05min > count_01min) else 60
#         else:
#             time_interval = 900 if (count_15min > count_01min) else 60
#         if time_count < 10:
#             self.debug_message("Time interval {i} from {t}".format(
#                     i=time_interval, t=time_offset_list), debug)
#         else:
#             self.debug_message("Time interval {i} from {t1} ... {t2}".format(
#                     i=time_interval, t1=time_offset_list[:5], t2=time_offset_list[-5:]), debug)
#         return time_interval

    #@abstractmethod
    def get_update_sql_string(self, line_input):
        return None
    
    def import_file_to_db(self, input_name, processed_dir=None):
        method_name = '%s.%s()' % (sams2db_vdb.MODULE_NAME, 'import_file_to_db')
        debug = False
        #debug = not debug
        
        count_inserted    = 0
        count_updated     = 0
        count_not_updated = 0
        skip_record_count = 0
        processed_record_count = 0

        file_name = self.get_basename(input_name)
        if not os.path.exists(input_name):
            self.error_message('{m} The file [{f}] does not exist'.format(
                    m=method_name, f=input_name))
        elif not os.path.isfile(input_name):
            self.error_message('{m} The {f} is not a file'.format(
                    m=method_name, f=input_name))
        else:
            input_date = None
            previous_input_name = None
            dao_table = Table_SurfaceObsData(self.db_actor)

            input_date_strs = file_name.split(".")
            climo_month = date.today().month
            if 2 < len(input_date_strs):
                input_date = datetime.strptime(input_date_strs[1], sams2db_vdb.DATE_FORMAT_IN_FILE).date()
                climo_month = input_date.month
                self.dao_entities = dao_table.load_and_entity_map(input_date)
                
                #one_day_before = input_date - timedelta(days=1)
                #input_date_strs[1] = one_day_before.strftime(sams2db_vdb.DATE_FORMAT_IN_FILE) 
                #previous_input_name = os.path.join(self.get_dirname(input_name),".".join(input_date_strs))
                #self.debug_message("   input_date: {d} one_day_before: {y}, {f}".format(
                #        d=input_date, y=one_day_before,f=previous_input_name), debug)
                
            nc_file = None
            try:
                print ('     - Processing file: {f} for {r}'.format(
                        f=file_name, r=self.options.range))
                nc_file = open_netcdf(input_name)
                base_time_var   = nc_file.variables.get('base_time', None)
                time_offset_var = nc_file.variables.get('time_offset', None)
                platform_var    = nc_file.variables.get('platform', None)
                if base_time_var is None:
                    self.error_message('{m} The variable base_time is missing'.format(
                            m=method_name))
                elif time_offset_var is None:
                    self.error_message('{m} The variable time_offset is missing'.format(
                            m=method_name))
                elif platform_var is None:
                    self.error_message('{m} The variable platform is missing'.format(
                            m=method_name))
                else:
                    base_time = get_variable_value(base_time_var)
                    self.debug_message("{m} base_time: {t}".format(m=method_name,t=base_time), debug)
                    
                    time_offsets = time_offset_var[:]
                    has_temp = has_pres = has_wdir = has_wspd = False
                    temp_missing = pres_missing = wdir_missing = wspd_missing = -9999
                    (temp_data, temp_missing) = self.read_nc_data(nc_file, 'tdry')
                    (pres_data, pres_missing) = self.read_nc_data(nc_file, 'pres')
                    (wdir_data, wdir_missing) = self.read_nc_data(nc_file, 'wdir')
                    (wspd_data, wspd_missing) = self.read_nc_data(nc_file, 'wspd')
                    (dp_data,   dp_missing)   = self.read_nc_data(nc_file, 'dp')
                    (rh_data,   rh_missing)   = self.read_nc_data(nc_file, 'Rh')
                    has_temp = (temp_data is not None)
                    has_pres = (pres_data is not None)
                    has_wspd = (wspd_data is not None)
                    has_wdir = (wdir_data is not None)
                    has_dp   = (dp_data is not None)
                    has_rh   = (rh_data is not None)
                    
                    (station_ids, first_station_id) = self.get_station_list(platform_var)
                    station_count = len(station_ids)
                    station_indices = range(0, station_count)
                    
                    range_id = sams_qc_tools.get_range_id_from_site_id(
                            first_station_id, self.map_handler.get_range_map())
                    
                    #(prev_time, prev_temp, prev_pres,prev_wspd, prev_wdir) = self.get_previous_data(
                    #        previous_input_name, station_count)
                    #if prev_time is None:
                        # read from the database
                    (prev_time, prev_temp, prev_pres,prev_wspd, prev_wdir) = self.get_previous_data_from_db(
                            dao_table, input_date, station_ids, temp_missing)
                    
                    time_interval = self.get_time_interval(time_offsets)
                    self.time_interval = time_interval
                    self.setup_qc_data_climatic(range_id, climo_month)
                    self.setup_qc_data_temporal(range_id, time_interval)
                    
                    self.debug_message(" found {s} stations".format(s=station_count), debug)
                    
                    temp_qc = pres_qc = wspd_qc = wdir_qc = -2
                    table_name = self.get_table_name()
                    
                    #sec_for_1day = 24 * 60 * 60
                    for t_idx, time_offset in enumerate(time_offsets):
                        if time_offset is numpy.ma.masked or 0 > time_offset:
                            continue
                        
                        #Filter by minutes. Includes 0, 5 10, 50 and 55 min
                        second_offset = (time_offset%3600)
                        if second_offset > 600 and second_offset < 3000:
                            continue
                        
                        time_delta = None
                        if t_idx > 0:
                            time_delta = time_offset - time_offsets[t_idx-1]
                        elif t_idx == 0:
                            if prev_time is not None:
                                time_delta = time_offset - prev_time
                        obs_time = datetime.utcfromtimestamp(base_time + time_offset)
                        self.debug_message(" time_offset: {t}, obs_time: {o} temp_var[{i}]: {tmp}".format(
                                t=time_offset, o=obs_time, i=t_idx, tmp=temp_data[t_idx]), debug)
                        for s_idx in station_indices:
                            if station_ids[s_idx] is None:
                                continue
                            
                            temp = temp_data[t_idx][s_idx] if has_temp else numpy.ma.masked
                            pres = pres_data[t_idx][s_idx] if has_pres else numpy.ma.masked
                            wspd = wspd_data[t_idx][s_idx] if has_wspd else numpy.ma.masked
                            wdir = wdir_data[t_idx][s_idx] if has_wdir else numpy.ma.masked
                            missing_var_count = 0
                            specific_humidity = temp_missing
                            if temp is numpy.ma.masked:
                                temp = temp_missing
                                missing_var_count += 1
                            if pres is numpy.ma.masked:
                                pres = pres_missing
                                missing_var_count += 1
                            if wspd is numpy.ma.masked:
                                wspd = wspd_missing
                                missing_var_count += 1
                            if wdir is numpy.ma.masked:
                                wdir = wdir_missing
                                missing_var_count += 1
                            
                            if pres != pres_missing and temp != temp_missing:
                                dp = dp_data[t_idx][s_idx] if has_dp else numpy.ma.masked
                                if dp is not numpy.ma.masked and dp != dp_missing:
                                    specific_humidity = convert_tools.compute_specific_humidity_dp(pres, temp, dp, temp_missing)
                                if specific_humidity == temp_missing:
                                    rh = rh_data[t_idx][s_idx] if has_rh else numpy.ma.masked
                                    if rh is not numpy.ma.masked and rh != rh_missing:
                                        specific_humidity = convert_tools.compute_specific_humidity_rh(pres, temp, rh, temp_missing)
                            
                            if missing_var_count < 4:
                                if t_idx > 0:
                                    p_temp = temp_data[t_idx-1][s_idx] if has_temp else temp_missing
                                    p_pres = pres_data[t_idx-1][s_idx] if has_pres else pres_missing
                                    p_wspd = wspd_data[t_idx-1][s_idx] if has_wspd else wspd_missing
                                    p_wdir = wdir_data[t_idx-1][s_idx] if has_wdir else wdir_missing
                                elif t_idx == 0:
                                    p_temp = prev_temp[s_idx]
                                    p_pres = prev_pres[s_idx]
                                    p_wspd = prev_wspd[s_idx]
                                    p_wdir = prev_wdir[s_idx]
                                else:
                                    continue
                                
                                (temp_qc,pres_qc,wspd_qc,wdir_qc) = self.do_QC(
                                        time_delta,temp,pres,wspd,wdir,p_temp,p_pres,p_wspd,p_wdir, temp_missing)
                                self.debug_message("temp: {t}({tq}), pres: {p}({pq}), wspd: {ws}({wsq}), wdir: {wd}({wdq})".format(
                                       t=temp,tq=temp_qc,p=pres,pq=pres_qc,ws=wspd,wsq=wspd_qc,wd=wdir,wdq=wdir_qc), debug)
                                obs_time_str = dao_table.convert_datetime_to_seconds(obs_time)
                                obs_data_entity = self.find_obs_data_entity(station_ids[s_idx],obs_time_str)
                                not_modified = False
                                update_sql_string = None
                                if obs_data_entity is None:
                                    update_sql_string = self.INSERT_SQL.format(tn=table_name,
                                            ot=obs_time,s=station_ids[s_idx],t=temp,p=pres,ws=wspd,wd=wdir,
                                            q2=specific_humidity,tq=temp_qc,pq=pres_qc,wsq=wspd_qc,wdq=wdir_qc)
                                    count_inserted += 1
                                else:
                                    if temp is not None and temp != temp_missing:
                                        obs_data_entity.set_temp(temp);
                                    if pres is not None and pres != pres_missing:
                                        obs_data_entity.set_pres(pres);
                                    if wspd is not None and wspd != wspd_missing:
                                        obs_data_entity.set_wspd(wspd);
                                    if wdir is not None and wdir != wdir_missing:
                                        obs_data_entity.set_wdir(wdir);
                                    if specific_humidity is not None and specific_humidity != temp_missing:
                                        obs_data_entity.set_q2(specific_humidity);
                                    obs_data_entity.set_temp_qc2(temp_qc);
                                    obs_data_entity.set_pres_qc2(pres_qc);
                                    obs_data_entity.set_wspd_qc2(wspd_qc);
                                    obs_data_entity.set_wdir_qc2(wdir_qc);
                                    if obs_data_entity.is_modified():
                                        update_sql_string = obs_data_entity.update_sql()
                                        count_updated += 1
                                    else:
                                        not_modified = True
                                        count_not_updated += 1
                                self.debug_message(" {i} sql_string: {s}".format(
                                    i=('insert' if obs_data_entity is None else 'update'), s=update_sql_string), debug)
                                if update_sql_string is not None:
                                    self.cursor.execute(update_sql_string)
                                elif not not_modified:
                                    self.warn_message("Check why SQL string was not generated")
                                processed_record_count += 1
                            else:
                                skip_record_count += 1
                                self.debug_message(" all missing values for {s} time_offset: {o}".format(
                                        s=station_ids[s_idx], o=obs_time), debug)
                        
                        #prev_obs_time = obs_time
                
                if 0 < processed_record_count:
                    self.db_conn.commit()
                
                info_message = 'processed {p} records'.format(p=processed_record_count)
                if 0 < skip_record_count:
                    info_message= '{i}, skipped {c} records'.format(i=info_message, c=skip_record_count)
                info_message= '{i}, inserted: {c}'.format(i=info_message, c=count_inserted)
                info_message= '{i} updated: {c}'.format(i=info_message, c=count_updated)
                info_message= '{i}, not updated: {c}'.format(i=info_message, c=count_not_updated)
                self.info_message(info_message)
                
            except:
                self.display_exception()
            
            if nc_file is not None:
                nc_file.close()
            
        return (input_name is not None)
        
    #@abstractmethod
    def import_to_db_preprocess(self, input_full_name):
        pass

    #@abstractmethod
    def import_to_db_postprocess(self, input_full_name):
        pass
    
    #def load_obs_data_entities(self, dao_table, target_obs_time):
    #    my_sql_where = "DATE(obs_time)='{d}'".format(d=target_obs_time.strftime(sams2db_vdb.DATE_FORMAT_YMD))
    #    rows = dao_table.get_records('*', sql_where=my_sql_where, order_by='unique_site_id, obs_time')
    #    
    #    entity_list = {}
    #    prev_site_id = '___dummy_site___'
    #    if rows is not None:
    #        unique_site_id = None
    #        for row in rows:
    #            dao_entity = dao_table.get_data(row)
    #            unique_site_id = dao_entity.get_unique_site_id()
    #            if prev_site_id != unique_site_id:
    #                if prev_site_id != '___dummy_site___':
    #                    self.dao_entities[str(prev_site_id)] = entity_list
    #                entity_list = self.dao_entities.get(unique_site_id, None)
    #                if entity_list is None:
    #                    entity_list = {}
    #                prev_site_id = unique_site_id
    #            entity_list[dao_entity.get_obs_time_as_seconds()] = dao_entity
    #        if unique_site_id is not None:
    #            self.dao_entities[str(unique_site_id)] = entity_list
    

    def read_nc_data(self,nc_data_file, var_name):
        nc_data = None
        nc_missing = None
        nc_var = nc_data_file.variables.get(var_name, None)
        if nc_var is not None:
            nc_data= nc_var[:]
            nc_missing = nc_var.getncattr('_FillValue')
        return nc_data, nc_missing
    
#     def setup_qc_data(self, range_id, climatic_month, time_interval):
#         climatic_data_map  = self.map_handler.get_climatic_data_map()
#         qc_field_table_map = self.map_handler.qc_field_table_map
#         self.field_data_temp = qc_field_table_map.get('temp')
#         self.field_data_pres = qc_field_table_map.get('pres')
#         self.field_data_wspd = qc_field_table_map.get('wspd_10m')
#         self.field_data_wdir = qc_field_table_map.get('wdir_10m')
#         if self.field_data_temp is not None:
#             self.field_id_temp = self.field_data_temp.get_qc_field_id()
#             self.climatic_data_temp = sams_qc_tools.get_climatic_data(
#                     climatic_data_map, range_id, self.field_id_temp, climatic_month)
#         if self.field_data_pres is not None:
#             self.field_id_pres = self.field_data_pres.get_qc_field_id()
#             self.climatic_data_pres = sams_qc_tools.get_climatic_data(
#                     climatic_data_map, range_id, self.field_id_pres, climatic_month)
#         if self.field_data_wspd is not None:
#             self.field_id_wspd = self.field_data_wspd.get_qc_field_id()
#             self.climatic_data_wspd = sams_qc_tools.get_climatic_data(
#                     climatic_data_map, range_id, self.field_id_wspd, climatic_month)
#         if self.field_data_wdir is not None:
#             self.field_id_wdir = self.field_data_wdir.get_qc_field_id()
#             self.climatic_data_wdir = sams_qc_tools.get_climatic_data(
#                     climatic_data_map, range_id, self.field_id_wdir, climatic_month)
#                     
#         #time_offset = QcScoreHandler.get_time_interval_for_threshold(time_interval)
#         time_offset = time_interval
#         if time_offset is not None:
#             temporal_threshold_map = sams_qc_tools.get_temporal_threshold_map(
#                     self.db_actor, time_offset)
#             if temporal_threshold_map is not None:
#                 if self.field_id_temp is not None:
#                     key = make_threshold_key(range_id, self.field_id_temp)
#                     self.temporal_threshold_temp = temporal_threshold_map.get(key, None)
#                 if self.field_id_pres is not None:
#                     key = make_threshold_key(range_id, self.field_id_pres)
#                     self.temporal_threshold_pres = temporal_threshold_map.get(key, None)
#                 if self.field_id_wspd is not None:
#                     key = make_threshold_key(range_id, self.field_id_wspd)
#                     self.temporal_threshold_wspd = temporal_threshold_map.get(key, None)
#                 if self.field_id_wdir is not None:
#                     key = make_threshold_key(range_id, self.field_id_wdir)
#                     self.temporal_threshold_wdir = temporal_threshold_map.get(key, None)
        
        
        
def main(argv = None):
    sys.argv_back = None
    #if argv is not None:
    #    sys.argv_back = sys.argv
    #    sys.argv = []
    #    sys.argv.extend(argv)
    
    job_user = 'atecuser'
    actor = sams2db_vdb(job_user)
    #print(' input_name = %s' % actor.options.input_name)
    actor.import_to_db()
        
    # Cleanup
    actor.cursor.close()
    actor.db_conn.close()
    #if argv is not None and sys.argv_back is not None:
    #    sys.argv = sys.argv_back

if __name__ == '__main__':
    log_time = LogTime()
    main()
    (duration_wall, duration_process) = log_time.get_durations()
    print ("      === Done {m}, took {w:.3} [{w1}], process time: {p:.3} [{p1}]".format(
            m=MODULE_NAME, w=duration_wall, w1=log_time.format(duration_wall),
            p=duration_process, p1=log_time.format(duration_process)))
    
