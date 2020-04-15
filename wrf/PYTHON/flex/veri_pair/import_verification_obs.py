#!/usr/bin/env python

import os
#import sys
import time
import math
from os.path import isdir, isfile

#from atec.db_config.DbConfig import DbConfig
from atec.obs2nc.Stations import Stations
from atec.util.DateUtil import LogTime, DateUtil
#from atec.util.DateUtil import DateUtil
from atec.veri_pair.verification_pair import base_import_verification_dir
from atec.veri_pair.verification_pair import verification_pair_reader
from atec.veri_pair.verification_pair import verification_pair_record
from atec.veri_pair.Table_SurfaceObsData import Table_SurfaceObsData, SurfaceObsData 

MODULE_NAME = 'import_verification_obs'
DEFAULT_RANGE_NAME = 'CRTC'
#DEFAULT_RANGE_NAME = 'ATC'
MODULE_VERSION = '1.0.0.0001'

EPSILON = 0.0001

def is_equal_number(value1, value2):
    result = False
    if value1 is not None and value2 is not None:
        result = (math.fabs((value1 - value2)) < EPSILON)
    return result

def read_processed(processed_file):
    file_h = open(processed_file,'r')
    inputs = file_h.readline().strip().split(',')
    file_h.close()
    file_size = 0
    mtime = 0
    if 0 < len(inputs):
        file_size = int(inputs[0].strip())
    if 1 < len(inputs):
        mtime = float(inputs[1].split('#')[0].strip())
    return (file_size, mtime)

def write_processed(processed_file, file_size, mtime):
    file_h = open(processed_file,'w')
    file_h.write('%d, %f\t# file size and modified time\n' % (file_size, mtime))
    file_h.write('mtime: %s\n' % (time.ctime(mtime)))
    file_h.close()    

class import_obs_data(base_import_verification_dir):

    #INPUT_SUB_DIRS='veri_dat/sfc/final'
    INPUT_SUB_DIRS='final'

    MODULE_NAME = 'import_obs_data'
    TABLE_NAME = 'surface_obs_data'
    COLUMNS = 'obs_time,unique_site_id,last_update,temp,temp_qc,q2,q2_qc,pres,pres_qc,wspd_10m,wspd_10m_qc,wdir_10m,wdir_10m_qc'

    CHECK_QC_VALUE = False
    
    
    def __init__(self, job_user):
        super(import_obs_data, self).__init__(job_user)
        self.renamed_input = None
        self.min_obs_time = None
        self.missing_by_lat_long = {}
        self.dao_entities = {}

    def convert_binary_to_text(self, raw_input_name, in_fullname):
        verif_reader = sams_verification_pair_reader(raw_input_name, in_fullname)
        verif_reader.read_records()
        self.min_obs_time = verif_reader.get_min_obs_time()


    def find_obs_data_entity(self, station_id, obs_time_sec):
        data_entity = None
        entity_list = self.dao_entities.get(str(station_id), None)
        if entity_list is not None:
            data_entity = entity_list.get(obs_time_sec, None)
        return data_entity

    #@abstractmethod
    def get_input_sub_dirs(self):
        return import_obs_data.INPUT_SUB_DIRS
    
    #@abstractmethod
    def get_table_name(self):
        return import_obs_data.TABLE_NAME
    
    #@abstractmethod
    def get_update_sql_string(self, line_input):
        debug = False
        debug = debug or self.options.debug
        sql_insert = None
        method_name = '     %s.%s()' % (import_obs_data.MODULE_NAME, 'get_update_sql_string')
        #YYYYMMDDHHMN, FCST_HR, GRID,      STTN, PSFC_mb, PMSL_mb,     T_C,  Q_g/kg, SPD_m/s,  DIR
        
        # Skip header
        if debug:   print('%s data input: %s' % (method_name, line_input))
        if line_input != verification_pair_record.get_header():
            a_record = verification_pair_record(line_input)
            if import_obs_data.CHECK_QC_VALUE and not a_record.is_valid_qc():
                print('%s invalid qc %s, %s, %s' % (method_name,
                        a_record.obs_time, a_record.platform, a_record.qc_values()))
            else:
                unique_site_id = self.get_unique_site_id(a_record)
                if unique_site_id is not None:
                    #if debug:   print('%s unique_site_id: %s' % (
                    #        method_name, unique_site_id))
                    
                    obs_time = DateUtil.convert_to_datetime(a_record.obs_time)
                    if obs_time is None:
                        print('  ====== Check this: obs_time is None type: {t} from {o},  input: {i}'.format(
                                o=a_record.obs_time, i=line_input,t=type(a_record.obs_time)))
                    obs_time_sec = DateUtil.convert_datetime_to_seconds(obs_time)
                    obs_data_entity = self.find_obs_data_entity(unique_site_id,obs_time_sec)
                    if obs_data_entity is None:
                        #print('%s range_code: %s  unique_site_id: %s' % (method_name, range_code, unique_site_id))
                        #obs_time,unique_site_id,last_update,temp,temp_qc,q2,q2_qc,pres,pres_qc,wspd_10m,wspd_10m_qc,wdir_10m,wdir_10m_qc
                        values = "'%s','%s',now(),%f,%d,%f,%d,%f,%d,%f,%d,%f,%d" % (
                                a_record.obs_time,unique_site_id,a_record.t_o,a_record.t_qc,
                                a_record.q_o,a_record.q_qc,a_record.psfc_o,a_record.psfc_qc,
                                a_record.ws_o,a_record.ws_qc,a_record.wd_o,a_record.wd_qc)
                        sql_insert = base_import_verification_dir.INSERT_TEMPLATE % (
                                import_obs_data.TABLE_NAME, import_obs_data.COLUMNS,values)
                    else:
                        obs_data_entity.set_temp(a_record.t_o);
                        obs_data_entity.set_pres(a_record.psfc_o);
                        obs_data_entity.set_wspd(a_record.ws_o);
                        obs_data_entity.set_wdir(a_record.wd_o);
                        obs_data_entity.set_q2(a_record.q_o);
                        obs_data_entity.set_temp_qc(a_record.t_qc);
                        obs_data_entity.set_pres_qc(a_record.psfc_qc);
                        obs_data_entity.set_wspd_qc(a_record.ws_qc);
                        obs_data_entity.set_wdir_qc(a_record.wd_qc);
                        obs_data_entity.set_q2_qc(a_record.q_qc);
                        if obs_data_entity.is_modified():
                            sql_insert = obs_data_entity.update_sql()
                        elif debug:
                            print( '{m} The obs data is not updated {s} {t}'.format(
                                    m=method_name,  s=obs_data_entity.get_unique_site_id(),
                                    t=obs_data_entity.get_obs_time()))
                
                if debug:
                    if sql_insert is not None:
                        print( '%s %s sql_string: %s' % (method_name,
                                "insert" if obs_data_entity is None else "update", sql_insert))
        return sql_insert
        
    def get_actual_input_name(self, input_full_name):
        user_name = os.environ.get('LOGNAME', 'atecuser')
        tmp_base_dir = '/tmp'
        ram_dir = '/dev/shm'
        if isdir(ram_dir):
            tmp_base_dir = ram_dir
        tmp_dir = os.path.join(tmp_base_dir, user_name, 'veri_pair_final_obs')
        self.make_dir(tmp_dir)
        in_file =self.get_basename(input_full_name)
        input_name = os.path.join(tmp_dir, '{f}.ascii'.format(f=in_file))
        return input_name
    
    def get_unique_site_id(self, veri_pair_record):
        method_name = '     %s.%s()' % (import_obs_data.MODULE_NAME, 'get_unique_site_id')
        unique_site_id = None
        tmp_unique_site_id = None
        range_name_l = self.range_name_l
        sams_id = veri_pair_record.get_sams_id()
        if sams_id is not None:
            unique_site_id_by_lat_lon = self.stations.get_unique_site_id_by_lat_lon(
                        veri_pair_record.lat, veri_pair_record.lon)
            if unique_site_id_by_lat_lon is None:
                tmp_key = "%s_%f_%f" % (sams_id, veri_pair_record.lat, veri_pair_record.lon)
                count = self.missing_by_lat_long.get(tmp_key, None)
                if count is None:
                    print('%s -- info -- Can not find SAMS Station by lat/lon [%f, %f], id=%s' % (
                            method_name, veri_pair_record.lat, veri_pair_record.lon, sams_id))
                    count = 1
                else:
                    count += 1
                self.missing_by_lat_long[tmp_key] = count
                                
            range_code = '%s_s%s' % (range_name_l, sams_id)
            tmp_unique_site_id = self.stations.get_unique_site_id(range_code)
            if tmp_unique_site_id is None and "rtc" == range_name_l:
                range_name_l = "rttc"
                range_code = '%s_s%s' % (range_name_l, sams_id)
                tmp_unique_site_id = self.stations.get_unique_site_id(range_code)
            if tmp_unique_site_id is not None and unique_site_id_by_lat_lon != tmp_unique_site_id:
                t_station = self.stations.get_station_by_unique_site_id(tmp_unique_site_id)
                if t_station is not None:
                    epsilon = Stations.get_epsilon()
                    lat_diff = math.fabs(t_station.get_lat() - veri_pair_record.lat)
                    lon_diff = math.fabs(t_station.get_lon() - veri_pair_record.lon)
                    if lat_diff >= epsilon or lon_diff >= epsilon:
                        tmp_key = "%s_%f_%f" % (sams_id, veri_pair_record.lat, veri_pair_record.lon)
                        count = self.missing_by_lat_long.get(tmp_key, None)
                        if count is None:
                            print('%s -- info -- Ignored [%s] because of different lat/lon (diff: %f, %f)' % (
                                    method_name, sams_id, lat_diff, lon_diff))
                            count = 1
                        else:
                            count += 1
                        self.missing_by_lat_long[tmp_key] = count
                        tmp_unique_site_id = None
                    #else:
                    #    print('%s -- info -- Apply [%s] instead of [%s]' % (
                    #            method_name, tmp_unique_site_id, unique_site_id_by_lat_lon))
                else:
                    tmp_unique_site_id = None
                
                #if unique_site_id_by_lat_lon.startswith(range_name_l):
                #    print('%s -- info -- station: by lat_lon: %s, by sams_id: %s' % (
                #                method_name, unique_site_id_by_lat_lon, tmp_unique_site_id))
                
        if tmp_unique_site_id is not None and tmp_unique_site_id.startswith(range_name_l):
            unique_site_id = tmp_unique_site_id
        
        return unique_site_id
        
    #@abstractmethod
    def import_to_db_preprocess(self, input_full_name):
        (input_dir, file) = self.get_dir_and_base_name(input_full_name)
        raw_input_name = self.make_input_name(input_dir, file)
        input_name = self.get_actual_input_name(input_full_name)
        if isfile(input_name):
            tmp_input_name = '%s.%d' % (input_name, int(round(time.time() * 1000)))
            os.rename(input_name, tmp_input_name)
            self.renamed_input = tmp_input_name
        else:
            self.renamed_input = None
        
        self.convert_binary_to_text(raw_input_name, input_name)
        self.dao_entities = self.load_and_entity_map() 

    #@abstractmethod
    def import_to_db_postprocess(self, input_full_name):
        #(input_dir, file) = self.get_dir_and_base_name(input_full_name)
        #if self.renamed_input is not None:
        #    if isfile(self.renamed_input):  os.remove(self.renamed_input)
        pass
        
    # Map with station_id which is map by obs_time in seconds
    def load_and_entity_map(self):
        if self.min_obs_time is None:
            my_sql_where = "obs_time >= DATE_SUB(NOW(), INTERVAL 10 HOUR)"
        else:
            my_sql_where = "obs_time >= '{d}'".format(
                    d=DateUtil.format_to_mysql_date(self.min_obs_time))
        sql_string = "SELECT * FROM {t} WHERE {w} ORDER BY unique_site_id, obs_time".format(
                t=Table_SurfaceObsData.TABLE_NAME, w=my_sql_where)
        self.cursor.execute(sql_string)
        rows = self.cursor.fetchall()
        
        dao_entity_map = {}
        entity_list = {}
        prev_site_id = '___dummy_site___'
        if rows is not None:
            unique_site_id = None
            for row in rows:
                dao_entity = SurfaceObsData(row)
                unique_site_id = dao_entity.get_unique_site_id()
                if prev_site_id != unique_site_id:
                    if prev_site_id != '___dummy_site___':
                        dao_entity_map[str(prev_site_id)] = entity_list
                    entity_list = dao_entity_map.get(unique_site_id, None)
                    if entity_list is None:
                        entity_list = {}
                    prev_site_id = unique_site_id
                entity_list[dao_entity.get_obs_time_as_seconds()] = dao_entity
            if unique_site_id is not None:
                dao_entity_map[str(unique_site_id)] = entity_list
        return dao_entity_map


        

class sams_verification_pair_reader(verification_pair_reader):
    '''
    classdocs
    '''

    MODULE_NAME = 'sams_verification_pair_reader'

    def __init__(self, veri_pair_name, out_name):
        '''
        Constructor
        '''
        super(sams_verification_pair_reader, self).__init__(veri_pair_name)
        self.out_name = out_name
        self.file_handle = open(out_name, "w")
        self.min_obs_time = None
        
    
    #@abstractmethod  
    def pre_process(self):
        self.file_handle.write('%s\n' % verification_pair_record.get_header())
    
    #@abstractmethod  
    def handle_record(self, verification_record, index):
        if verification_record.is_sams_platform():
            self.file_handle.write('%s\n' % verification_record.toString())
            obs_time = DateUtil.convert_to_datetime(verification_record.get_obs_time())
            if self.min_obs_time is None:
                self.min_obs_time = obs_time
            elif obs_time < self.min_obs_time:
                self.min_obs_time = obs_time
    
    def get_min_obs_time(self):
        return self.min_obs_time;
    

class SurfaceObsData_del(object):
    
    TABLE_NAME = 'surface_obs_data'
    OBS_TIME        = 0
    UNIQUE_SITE_ID  = 1 + OBS_TIME
    LAST_UPDATE     = 1 + UNIQUE_SITE_ID
    TEMP            = 1 + LAST_UPDATE
    TEMP_QC         = 1 + TEMP
    Q2              = 1 + TEMP_QC 
    Q2_QC           = 1 + Q2 
    PRES            = 1 + Q2_QC
    PRES_QC         = 1 + PRES
    WSPD            = 1 + PRES_QC
    WSPD_QC         = 1 + WSPD
    WDIR            = 1 + WSPD_QC
    WDIR_QC         = 1 + WDIR
    
    DATA_COLUMNS = ['temp','temp_qc','q2','q2_qc','pres','pres_qc','wspd_10m','wspd_10m_qc','wdir_10m','wdir_10m_qc' ]

    VAR_START_INDEX = TEMP
    VAR_LAST_INDEX  = WDIR_QC
    
    def __init__(self, row):
        self.row = list(row)
        self.modified = False
        self.modified_fields = [False for y in range(self.VAR_LAST_INDEX - self.VAR_START_INDEX + 1)]
    
    def convert_datetime_to_seconds(self, datetime_obj):
        return time.mktime(datetime_obj.timetuple())


    def get_table_name(self):
        return SurfaceObsData_del.TABLE_NAME
    
    def get_last_update(self):
        return self.row[self.LAST_UPDATE]
        
    def get_last_update_sql(self):
        return DateUtil.format_to_mysql_date(self.get_last_update())
    
    def get_obs_time(self):
        return self.row[self.OBS_TIME]
    
    def get_obs_time_as_seconds(self):
        return DateUtil.convert_datetime_to_seconds(self.get_obs_time())
    
    def get_obs_time_sql(self):
        return DateUtil.format_to_mysql_date(self.get_obs_time())
        
    def get_unique_site_id(self):
        return self.row[self.UNIQUE_SITE_ID]
    def get_pres(self):
        return self.row[self.PRES]
    def get_pres_qc(self):
        return self.row[self.PRES_QC]
    def get_q2(self):
        return self.row[self.Q2]
    def get_q2_qc(self):
        return self.row[self.Q2_QC]
    def get_temp(self):
        return self.row[self.TEMP]
    def get_temp_qc(self):
        return self.row[self.TEMP_QC]
    def get_wdir(self):
        return self.row[self.WDIR]
    def get_wdir_qc(self):
        return self.row[self.WDIR_QC]
    def get_wspd(self):
        return self.row[self.WSPD]
    def get_wspd_qc(self):
        return self.row[self.WSPD_QC]

    def get_column_index(self, field_id):
        index = -1
        if   field_id ==  1:
            index = self.TEMP
        elif field_id ==  2:
            index = self.Q2
        elif field_id ==  3:
            index = self.PRES_STATION
        elif field_id ==  4:
            index = self.WSPD
        elif field_id ==  5:
            index = self.WDIR
        return index
  
    def get_value(self, field_id):
        index = self.get_column_index(field_id)
        value = None
        if index >= 0:
            value = self.row[index]
        return value
    
    def is_modified(self):
        return self.modified

    def set_row(self, row):
        self.row = row
    
    def set_modifed(self, value):
        self.modified = value
    
    def set_unique_site_id(self, value):
        index = self.UNIQUE_SITE_ID
        if self.row[index] != value:
            self.row[index] = value
            self.modified = True

    def set_pres(self, value):
        index = self.PRES
        if not is_equal_number(self.row[index], value):
            self.row[index] = value
            self.modified = True
            self.modified_fields[index - self.VAR_START_INDEX] = True

    def set_pres_qc(self, value):
        index = self.PRES_QC
        if not is_equal_number(self.row[index], value):
            self.row[index] = value
            self.modified = True
            self.modified_fields[index - self.VAR_START_INDEX] = True

    def set_q2(self, value):
        index = self.Q2
        if not is_equal_number(self.row[index], value):
            self.row[index] = value
            self.modified = True
            self.modified_fields[index - self.VAR_START_INDEX] = True

    def set_q2_qc(self, value):
        index = self.Q2_QC
        if not is_equal_number(self.row[index], value):
            self.row[index] = value
            self.modified = True
            self.modified_fields[index - self.VAR_START_INDEX] = True

    def set_temp(self, value):
        index = self.TEMP
        if not is_equal_number(self.row[index], value):
            self.row[index] = value
            self.modified = True
            self.modified_fields[index - self.VAR_START_INDEX] = True

    def set_temp_qc(self, value):
        index = self.TEMP_QC
        if not is_equal_number(self.row[index], value):
            self.row[index] = value
            self.modified = True
            self.modified_fields[index - self.VAR_START_INDEX] = True

    def set_wdir(self, value):
        index = self.WDIR
        if not is_equal_number(self.row[index], value):
            self.row[index] = value
            self.modified = True
            self.modified_fields[index - self.VAR_START_INDEX] = True

    def set_wdir_qc(self, value):
        index = self.WDIR_QC
        if not is_equal_number(self.row[index], value):
            self.row[index] = value
            self.modified = True
            self.modified_fields[index - self.VAR_START_INDEX] = True

    def set_wspd(self, value):
        index = self.WSPD
        if not is_equal_number(self.row[index], value):
            self.row[index] = value
            self.modified = True
            self.modified_fields[index - self.VAR_START_INDEX] = True

    def set_wspd_qc(self, value):
        index = self.WSPD_QC
        if not is_equal_number(self.row[index], value):
            self.row[index] = value
            self.modified = True
            self.modified_fields[index - self.VAR_START_INDEX] = True

    def update_sql(self):
        sql_set = ""
        for index in range(len(self.modified_fields)):
            if self.modified_fields[index]:
                if "" != sql_set:
                    sql_set = "{s}, ".format(s=sql_set)
                sql_set = "{s}{c}={v}".format(s=sql_set, c=self.DATA_COLUMNS[index],v=self.row[index+self.VAR_START_INDEX])
        
        sql_string = None
        if "" != sql_set:
            sql_string = "UPDATE {t} SET {ss} WHERE unique_site_id='{si}' and obs_time='{ot}'".format(
                    t=self.get_table_name(),ss=sql_set, si=self.get_unique_site_id(), ot=self.get_obs_time_sql())
        return sql_string
    
def main():
    job_user = 'atecuser'
    actor = import_obs_data(job_user)
    print(' input_name = %s' % actor.options.input_name)
    actor.import_to_db()
        
    # Cleanup
    actor.cursor.close()
    actor.db_conn.close()

if __name__ == '__main__':
    log_time = LogTime()
    main()
    (duration_wall, duration_process) = log_time.get_durations()
    print("      === Done %s, took %d [%s], process time: %d [%s]" % (
            MODULE_NAME, duration_wall, log_time.format(duration_wall),
            duration_process, log_time.format(duration_process)))