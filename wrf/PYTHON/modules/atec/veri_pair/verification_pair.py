'''
Created on Feb 24, 2015

@author: hsoh
'''

from os import getcwd, listdir, makedirs, environ
from os.path import isdir, basename, dirname, isfile, join, exists, getmtime, getsize
import sys
import time
import math
import struct
from abc import ABCMeta, abstractmethod
from optparse import OptionParser

try:
    from atec.config.DbConfig import DbConfig
except ImportError:
    from atec.db_config.DbConfig import DbConfig
from atec.dao.DbActor import create_db_actor
from atec.obs2nc.Stations import Stations
#from atec.util.DateUtil import DateUtil

MODULE_NAME = 'verification_pair'

MODULE_VERSION="Version 1.0"


def create_parser():
    usage_str = "%prog [options] "
    parser = OptionParser(usage = usage_str, version="%prog " + MODULE_VERSION)
    parser.add_option('-m', '--model', dest='model_id',         default=0, help=" Model_id 0 for wrf1 and 3 for wrf2")
    parser.add_option('-i', '--input', dest='input_name',       default='', help=" Input file/directory name - required")
    parser.add_option('-r', '--range', dest='range',            default='', help=" Range name - required")
    parser.add_option('-v', '--ver',   dest='model_version_id', default='', help=" Model version number, string")
    parser.add_option('-d', '--debug', dest='debug', action='store_true', default=False,
            help=" Enable debug - optional")
    parser.add_option('-D', '--debug_sql', dest='debug_sql', action='store_true', default=False,
            help=" Enable debug for SQL - optional")
    parser.add_option('-t', '--test',  dest='opt_test', action='store_true', default=False,
            help=" Enable opt_test - optional")
    return parser


def get_short_int_from_bytes(byte_buffer, offset):
    #return int.from_bytes(byte_buffer[offset:(offset+2)])
    return struct.unpack_from('h',byte_buffer, offset)[0]

#def get_float_of_2byte_from_bytes(byte_buffer, offset):
#    return float(struct.unpack_from('h',byte_buffer, offset)[0])

def get_file_list(dir_name):
    file_list = [ f for f in listdir(dir_name) if isfile(join(dir_name,f)) ]
    return file_list

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


class base_verification_pair(object):
    '''
    classdocs
    '''

    VERIFICATION_RAW_DATA_LENGTH = 56

    __metaclass__ = ABCMeta

    MISSING_VALUE = -9999
    MISSING_QC_VALUE = -1
    PAIR_DATA_MISSING_VALUE = -8888
    MODULE_NAME = 'base_verification_pair'

    def __init__(self, veri_pair_name):
        '''
        Constructor
        '''
        self.veri_pair_name = veri_pair_name
    
    @abstractmethod  
    def pre_process(self):
        pass
    
    @abstractmethod  
    def handle_record(self, verification_record, index):
        pass
    
    def read_records(self, build_array=False):
        method_name = '%s.%s()' % (base_verification_pair.MODULE_NAME, 'read_records')

        verification_records = []
        if self.veri_pair_name is None:
            print('  %%% ERROR %%% verification file name is missing [Nones]')
        elif 0 == len(self.veri_pair_name):
            print('  %%% ERROR %%% verification file name is empty')
        elif not isfile(self.veri_pair_name):
            print('  %%% ERROR %%% verification file does not exist [%s]' % (
                    method_name, self.veri_pair_name))
        else:
            file_h = open(self.veri_pair_name, "rb")
            try:
                self.pre_process()
                #print "  Index,%s" % verification_pair_record.get_header()
                count = 0
                byte_buffer = file_h.read(base_verification_pair.VERIFICATION_RAW_DATA_LENGTH)
                while len(byte_buffer) > 0:
                    a_verif_record = verification_pair_record(byte_buffer)
                    if build_array:
                        verification_records.append(a_verif_record)
                    self.handle_record(a_verif_record, count)
                    count = count + 1
                    #print "   %4d %r" % (count, bytes)
                    byte_buffer = file_h.read(base_verification_pair.VERIFICATION_RAW_DATA_LENGTH)
                    #if count > 10: break
              
            finally:
                if file_h is not None:  file_h.close()
        
        return verification_records

class base_import_verification(object):

    __metaclass__ = ABCMeta

    MODULE_NAME        = 'base_import_verification'
    STATION_TABLE_NAME = 'station_information'
    #SQL_SELECT = 'SELECT * FROM %s WHERE RANGE_CODE IS NOT NULL' % (STATION_TABLE_NAME)

    #COLUMNS = 'cycle_time,valid_time,unique_site_id,model_id,domain_id,temp,q2,pres,wspd_10m,wdir_10m,model_version_id,lead_time'

    INSERT_TEMPLATE = "REPLACE INTO %s (%s) VALUES(%s)"
    
    @staticmethod
    def get_file_list(dir_name):
        file_list = [ f for f in listdir(dir_name) if isfile(join(dir_name,f)) ]
        return file_list

    def __init__(self):
        method_name = '%s.%s()' % (base_import_verification.MODULE_NAME, '__init__')
        
        option_parser = create_parser()
        self.add_more_parse_optigns(option_parser)
        (self.options, self.args) = option_parser.parse_args()
        
        range_name = self.options.range
        if range_name is None or range_name == "" :
            print(' === ERROR === %s range_name is missing' % (method_name))
            sys.exit(-2)

        self.range_name_F = range_name.strip().upper()
        self.range_name   = self.range_name_F.replace('2','')
        self.range_name_l = self.range_name.lower()
        
        self.db_name_key = '%s_verification' % (range_name.lower())
        self.db_config = DbConfig()
        self.db_actor  = create_db_actor(self.db_config, self.db_name_key)
        self.db_conn   = self.db_actor.dbConn
        self.cursor    = self.db_config.get_db_cursor(self.db_conn)

        self.station_list = []
        self.stations = Stations(self.cursor)
        self.stations.add_stations(base_import_verification.STATION_TABLE_NAME)
        #print(' stations: [%r]' % stations.get_range_codes())

    @abstractmethod
    def get_table_name(self):
        pass
    
    #@abstractmethod
    #def import_a_line_to_db(self, model_id, model_version_id):
    #    pass
        
    @abstractmethod
    def get_update_sql_string(self, line_input):
        pass
    
    @abstractmethod
    def import_to_db_preprocess(self, input_full_name):
        pass

    @abstractmethod
    def import_to_db_postprocess(self, input_full_name):
        pass
    
    def add_more_parse_optigns(self, parser):
        pass
    
    def cleanup(self):
        if self.cursor is not None:
            self.cursor.close()
        if self.db_conn is not None:
            self.db_conn.close()
    
    def get_input_name_argument(self):
        return self.options.input_name

    def get_processed_dir(self, input_dir):
        print('  input_dir: [%s]' % input_dir)
        if input_dir is None or '' == input_dir:
            processed_dir = join('.','processed')
        else:
            processed_dir = join(input_dir, 'processed')
            if not exists(processed_dir):   makedirs(processed_dir)
        return processed_dir

    def get_unique_site_id(self, veri_pair_record):
        method_name = '%s.%s()' % (base_import_verification.MODULE_NAME, 'get_unique_site_id')
        unique_site_id = None
        tmp_unique_site_id = None
        sams_id = veri_pair_record.get_sams_id()
        if sams_id is not None:
            unique_site_id_by_lat_lon = self.stations.get_unique_site_id_by_lat_lon(
                        veri_pair_record.lat, veri_pair_record.lon)
            if '99' == sams_id or '999' == sams_id:
                tmp_unique_site_id = unique_site_id_by_lat_lon
            else:
                range_code = '%s_s%s' % (self.range_name_l, sams_id)
                tmp_unique_site_id = self.stations.get_unique_site_id(range_code)
                if unique_site_id_by_lat_lon  is not None and tmp_unique_site_id is not None \
                        and unique_site_id_by_lat_lon != tmp_unique_site_id:
                    #epsilon = Stations.get_epsilon()
                    lat_diff = math.fabs(tmp_unique_site_id.get_lat() - veri_pair_record.lat)
                    lon_diff = math.fabs(tmp_unique_site_id.get_lon() - veri_pair_record.lon)
                    #if lat_diff < epsilon and lon_diff < epsilon:
                    print('%s -- info -- Found station [%s] but lat/lon is different (diff: %f, %f)' % (
                            method_name, tmp_unique_site_id.get_unique_site_id(), lat_diff, lon_diff))
                    tmp_unique_site_id = None
        if tmp_unique_site_id is not None and tmp_unique_site_id.startswith(self.range_name_l):
            unique_site_id = tmp_unique_site_id
        return unique_site_id
        
    def import_file_to_db_initial(self, file_name):
        #method_name = '%s.%s()' % (base_import_verification.MODULE_NAME, 'import_file_to_db')
        #(input_dir, processed_dir) = self.get_and_check_directories()

        skip_count      = 0
        processed_count = 0
        #print(' file: %s' % file_name)
        self.import_to_db_preprocess(file_name)
        with open(file_name) as file_handle:
            for line in file_handle:
                sql_update = self.get_update_sql_string(line.strip())
                #is_processed = self.import_a_line_to_db(line.strip())
                if sql_update is None or 1 > len(sql_update):
                    skip_count += 1
                else:
                    self.cursor.execute(sql_update)
                    processed_count += 1
            
            if 0 < processed_count:
                self.db_conn.commit()
        
        self.import_to_db_postprocess(file_name)
        info_message = '   [INFO] processed %d records' % (processed_count)
        if 1 < skip_count:
            info_message= '%s and skipped %d records' % (info_message, skip_count)
        print(info_message)
        return (processed_count, skip_count)

    def import_file_to_db(self, input_name, processed_dir=None):
        #method_name = '%s.%s()' % (base_import_verification.MODULE_NAME, 'import_file_to_db')
        #debug_init = True
        skipped   = False
        skip_record_count      = 0
        processed_record_count = 0
        actual_input_name = None

        file_name = self.get_basename(input_name)
        if exists(input_name):
            #print('%s raw: %s, in_name: %s' % (method_name,raw_input_name, input_name))
            #mtime = getmtime(input_name)
            #file_size = getsize(input_name)
            if processed_dir is not None:
                processed_file = join(processed_dir,file_name)
                input_modified = self.is_modified(input_name, processed_file)
                if not input_modified:
                    skipped = True
        else:
            skipped = True
            print(' ===  ERROR === The file [%s] does not exist' % (input_name))
            
        if skipped:
            print('     - Skipped file: %s' % file_name)
        else:
            print('     - Processing file: %s' % file_name)
            
            self.import_to_db_preprocess(input_name)
            
            actual_input_name = self.get_actual_input_name(input_name)
            if not isfile(actual_input_name):
                skipped = True
                print('     - Does not exist actual input file: %s from %s' % (
                        actual_input_name, input_name))

        if actual_input_name is not None and not skipped:
            #cycle_time = file_name.split('_')[0].strip()
            #cycle_time_sql = DateUtil.convert_to_mysql_date(cycle_time.strip())
            with open(actual_input_name) as file_handle:
                for line in file_handle:
                    sql_update = self.get_update_sql_string(line.strip())
                    #is_processed = self.import_a_line_to_db(line.strip())
                    if sql_update is None or 0 == len(sql_update):
                        skip_record_count += 1
                    #elif debug_init:
                    #    print(" SQL: %s" % sql_update)
                        processed_record_count += 1
                    else:
                        self.cursor.execute(sql_update)
                        processed_record_count += 1
                
            if 0 < processed_record_count:
                self.db_conn.commit()
            
            if processed_dir is not None:
                mtime = getmtime(input_name)
                file_size = getsize(input_name)
                write_processed(processed_file, file_size, mtime)
                
            info_message = '     [INFO] processed %d records' % (processed_record_count)
            if 1 < skip_record_count:
                info_message= '%s and skipped %d records' % (info_message, skip_record_count)
            print(info_message)
    
            self.import_to_db_postprocess(actual_input_name)
        return (actual_input_name is not None)
    
    def make_dir(self, dir_name):
        if not exists(dir_name) and not isdir(dir_name):
            makedirs(dir_name)
    
    def get_basename(self, name):
        return basename(name)
    
    def get_dirname(self, name):
        return dirname(name)
    
    def get_dir_and_base_name(self, name):
        return (dirname(name), basename(name))
    
    
class base_import_verification_dir(base_import_verification):

    __metaclass__ = ABCMeta

    MODULE_NAME        = 'base_import_verification_dir'

    #INPUT_DIR_TEMPLATE='/model/__USER__/cycles/GW__RANGE_NAME_O__/__RANGE_NAME__/%s'
    INPUT_DIR_TEMPLATE='/model/{u}/cycles/GW{r}/1/sams_sites/{s}'
    #INPUT_DIR_TEMPLATE='/datainput/{u}/{r}/veri_pair.raw/{s}'

    
    DEF_JOB_USER = 'atecuser'
    INSERT_TEMPLATE = "REPLACE INTO %s (%s) VALUES(%s)"
    
    #@staticmethod
    #def get_file_list(dir_name):
    #    file_list = [ f for f in listdir(dir_name) if isfile(join(dir_name,f)) ]
    #    return file_list


    def __init__(self, job_user=None):
        super(base_import_verification_dir, self).__init__()
        #method_name = '%s.%s()' % (base_import_verification_dir.MODULE_NAME, '__init__')
        if job_user is None:
            self.job_user = base_import_verification_dir.DEF_JOB_USER
        else:
            self.job_user = job_user

    @abstractmethod
    def get_input_sub_dirs(self):
        pass
    
    #@abstractmethod
    #def get_table_name(self):
    #    pass
    
    #@abstractmethod
    #def import_a_line_to_db(self, model_id, model_version_id):
    #    pass
        
    #@abstractmethod
    #def get_update_sql_string(self, line_input):
    #    pass
        
    #@abstractmethod
    #def import_to_db_preprocess(self, raw_input_name, input_name):
    #    pass

    #@abstractmethod
    #def import_to_db_postprocess(self, raw_input_name, input_name):
    #    pass
    
    #def add_more_parse_optigns(self, parser):
    #    pass
    
    def get_and_check_directories(self, input_dir=None):
        if input_dir is None:
            input_dir = self.get_input_dir()
        elif input_dir == '' or input_dir == '.':
            input_dir = getcwd()
        if not exists(input_dir):
            print('The input directory [%s] does not exist!!!\nExit ...' % (input_dir))
            sys.exit(-1)

        processed_dir = self.get_processed_dir(input_dir)
        return (input_dir, processed_dir)
            
    def get_input_dir(self):
        #range_name_L = self.range_name.lower()
        #range_name_U = self.range_name.upper()
        input_dir = base_import_verification_dir.INPUT_DIR_TEMPLATE.format(
                u=environ.get('LOGNAME',''), r=self.range_name_F, s=self.get_input_sub_dirs())
        if not exists(input_dir) and self.range_name_F == 'ATC':
            input_dir = base_import_verification_dir.INPUT_DIR_TEMPLATE.format(
                    u=environ.get('LOGNAME',''), r='{r}2'.format(r=self.range_name_F),
                    s=self.get_input_sub_dirs())
        return input_dir
    
    def make_input_name(self, input_dir, file_name):
        if input_dir is None:
            input_name = file_name
        else:
            input_name = join(input_dir, file_name)
        #input_name = raw_input_name
        return input_name 
    
    def get_actual_input_name(self, input_name):
        (input_dir, file_name) = self.get_dir_and_base_name(input_name)
        return self.make_input_name(input_dir, file_name)
    
    def import_to_db(self):
        #debug = False
        #debug = not debug
        method_name = '%s.%s()' % (base_import_verification_dir.MODULE_NAME, 'import_to_db')
        input_name = self.get_input_name_argument()
        #if debug:
        #    print('%s is called, input_name: [%s]' % (method_name, input_name))
        if input_name is None:
            print('{m}: input_name is missing'.format(m=method_name))
        else:
            if '' != input_name and isfile(input_name):
                self.import_file_to_db(input_name)
            else:
                self.import_dir_to_db(input_name)

    def import_dir_to_db(self, input_dir=None):
        debug = False
        #debug = not debug
        method_name = '%s.%s()' % (base_import_verification_dir.MODULE_NAME, 'import_dir_to_db')
        (input_dir, processed_dir) = self.get_and_check_directories(input_dir)
        if debug:
            print('%s: input_dir: [%s], processed_dir[%s]' % (
                    method_name, input_dir, processed_dir))
        file_list = get_file_list(input_dir)
        processed_file_count = self.import_files_to_db(file_list, input_dir, processed_dir)

        if 0 < processed_file_count:
            if 1 == processed_file_count:
                info_str = 'file'
            else:
                info_str = 'files'
            info_message = '     [INFO] processed %d %s' % (processed_file_count, info_str)
        else:
            info_message = '     [INFO] No updated verification files'
        print(info_message)

    def import_files_to_db(self, file_list, input_dir=None, processed_dir=None):
        #method_name = '%s.%s()' % (base_import_verification_dir.MODULE_NAME, 'import_files_to_db')

        skip_file_count = 0
        processed_file_count = 0
        #print(' input_dir: %s' % input_dir)
        for file_name in file_list:
            #print(' file: %s' % file_name)
            in_fullname = self.make_input_name(input_dir, file_name)
            processed = self.import_file_to_db(in_fullname, processed_dir)
            if processed:
                processed_file_count += 1
            else:
                skip_file_count += 1
        return processed_file_count
    
    def import_files_to_db_org(self, file_list, input_dir=None, processed_dir=None):
        #method_name = '%s.%s()' % (base_import_verification_dir.MODULE_NAME, 'import_files_to_db')

        skip_file_count = 0
        processed_file_count = 0
        #print(' input_dir: %s' % input_dir)
        for file_name in file_list:
            #print(' file: %s' % file_name)
            in_fullname = self.make_input_name(input_dir, file_name)
            #print('%s raw: %s, in_name: %s' % (method_name,raw_input_name, in_fullname))
            #mtime = getmtime(in_fullname)
            #file_size = getsize(in_fullname)
            if processed_dir is not None:
                processed_file = join(processed_dir,file_name)
                input_modified = self.is_modified(in_fullname, processed_file)
                if not input_modified:
                    skip_file_count += 1
                    print('     - Skipped file: %s' % file_name)
                    continue
            
                print('     - Processing file: %s' % file_name)
            
            self.import_to_db_preprocess(input_dir, file_name)
            
            real_in_fullname = self.get_processed_input_name(input_dir, file_name)
            if not isfile(real_in_fullname):
                skip_file_count += 1
                print('     - Does not exist input file: %s' % in_fullname)
                continue
            
            skip_record_count = 0
            processed_record_count = 0
            #cycle_time = file_name.split('_')[0].strip()
            #cycle_time_sql = DateUtil.convert_to_mysql_date(cycle_time.strip())
            with open(real_in_fullname) as file_handle:
                for line in file_handle:
                    sql_update = self.get_update_sql_string(line.strip())
                    #is_processed = self.import_a_line_to_db(line.strip())
                    if sql_update is None or 1 > len(sql_update):
                        skip_record_count += 1
                    else:
                        self.cursor.execute(sql_update)
                        processed_record_count += 1
                
                if 0 < processed_record_count:
                    self.db_conn.commit()
            
            if processed_dir is not None:
                mtime = getmtime(in_fullname)
                file_size = getsize(in_fullname)
                write_processed(processed_file, file_size, mtime)
                
            info_message = '     [INFO] processed %d records' % (processed_record_count)
            if 1 < skip_record_count:
                info_message= '%s and skipped %d records' % (info_message, skip_record_count)
            print(info_message)

            self.import_to_db_postprocess(input_dir, file_name)
            processed_file_count += 1
        return processed_file_count
    
    def is_modified(self, in_fullname, processed_file):
        input_modified = True
        if isfile(in_fullname) and exists(processed_file):
            in_mtime = getmtime(in_fullname)
            in_file_size = getsize(in_fullname)
            (processed_file_size, processed_mtime) = read_processed(processed_file)
            if 0.00001 > math.fabs(in_mtime - processed_mtime) \
                    and in_file_size == processed_file_size:
                input_modified = False
            else:
                if in_file_size == processed_file_size:
                    print('      File size is changed %d to %d' %(
                            processed_file_size, in_file_size))
                else:
                    print('      Modification timestamp is changed: %r [%s] to %r [%s]' %(
                        processed_mtime, time.ctime(processed_mtime), in_mtime, time.ctime(in_mtime)))
        return input_modified
    
class verification_pair_record(object):

    DATE_FORMAT="%4d-%02d-%02d %02d:%02d"
    FORMAT_STRING_ORG="%4d-%02d-%02d %02d:%02d, %f, %f, %d, %s, %f, %f, %d, %f, %f, %d, %f, %f, %f, %f, %d, %f, %f, %d, %f, %f, %d, %f, %f, %d"
    FORMAT_STRING='%4d-%02d-%02d %02d:%02d:00,%.2f,%.2f,%d,%s,%.2f,%.2f,%d,%.2f,%.2f,%d,%.2f,%.2f,%.2f,%.2f,%d,%.2f,%.2f,%d,%.2f,%.2f,%d,%.2f,%.2f,%d'
    QC_FORMAT_STRING = 'psfc:%2d, slp: %2d, t: %2d, q: %2d, ws: %2d, wd: %2d'
    NON_SAMS_PLATFORMS = ['METR','SHIP','SPEC','SYNP']

    sThreshold = 2
    
    def __init__(self, byte_buffer):
        #BaseObject.__init__(self)
        if base_verification_pair.VERIFICATION_RAW_DATA_LENGTH == len(byte_buffer):
            self.obs_time  = None
            self.year      = get_short_int_from_bytes(byte_buffer, 0)
            month_day      = get_short_int_from_bytes(byte_buffer, 2)
            hour_min       = get_short_int_from_bytes(byte_buffer, 4)
            self.month     = month_day/100
            self.day       = month_day%100
            self.hour      = hour_min/100
            self.min       = hour_min%100
            self.lat       = get_short_int_from_bytes(byte_buffer, 6)/100.0
            self.lon       = get_short_int_from_bytes(byte_buffer, 8)/100.0
            self.domain_id = get_short_int_from_bytes(byte_buffer,10)
            self.platform  = byte_buffer[12:16].decode("utf-8")
            self.psfc_m    = get_short_int_from_bytes(byte_buffer,16)/10.0
            self.psfc_o    = get_short_int_from_bytes(byte_buffer,18)/10.0
            self.psfc_qc   = get_short_int_from_bytes(byte_buffer,20)
            self.slp_m     = get_short_int_from_bytes(byte_buffer,22)/100.0
            self.slp_o     = get_short_int_from_bytes(byte_buffer,24)/100.0
            self.slp_qc    = get_short_int_from_bytes(byte_buffer,26)
            self.ter_m     = get_short_int_from_bytes(byte_buffer,28)/100.0
            self.ter_o     = get_short_int_from_bytes(byte_buffer,30)/100.0
            self.t_m       = get_short_int_from_bytes(byte_buffer,32)/100.0
            self.t_o       = get_short_int_from_bytes(byte_buffer,34)/100.0
            self.t_qc      = get_short_int_from_bytes(byte_buffer,36)
            self.q_m       = get_short_int_from_bytes(byte_buffer,38)/100.0
            self.q_o       = get_short_int_from_bytes(byte_buffer,40)/100.0
            self.q_qc      = get_short_int_from_bytes(byte_buffer,42)
            self.ws_m      = get_short_int_from_bytes(byte_buffer,44)/100.0
            self.ws_o      = get_short_int_from_bytes(byte_buffer,46)/100.0
            self.ws_qc     = get_short_int_from_bytes(byte_buffer,48)
            self.wd_m      = get_short_int_from_bytes(byte_buffer,50)/100.0
            self.wd_o      = get_short_int_from_bytes(byte_buffer,52)/100.0
            self.wd_qc     = get_short_int_from_bytes(byte_buffer,54)
            
        else:
            #Date,Lat,Lon,Domain,Platform,psfc_m,psfc_o,psfc_qc,slp_m,slp_o,slp_qc,ter_m,ter_o,t_m,t_o,t_qc,q_m,q_o,q_qc,ws_m,ws_o,ws_qc,wd_m,wd_o,wd_qc
            if isinstance(byte_buffer, bytes):
                byte_array = byte_buffer.decode("utf-8")
            else:
                byte_array = byte_buffer
            (obs_time,lat,lon,domain_id,platform,psfc_m,psfc_o,psfc_qc,slp_m, slp_o,slp_qc,
                ter_m,ter_o,t_m,t_o,t_qc,q_m,q_o,q_qc,ws_m,ws_o,ws_qc,wd_m,wd_o,wd_qc) = byte_array.split(',')

            self.obs_time  = obs_time.strip()
            self.lat       = float(lat)
            self.lon       = float(lon)
            self.domain_id = int(domain_id)
            self.platform  = platform.strip()
            self.psfc_m    = float(psfc_m)
            self.psfc_o    = float(psfc_o)
            self.psfc_qc   = int(psfc_qc)
            self.slp_m     = float(slp_m)
            self.slp_o     = float(slp_o)
            self.slp_qc    = int(slp_qc)
            self.ter_m     = float(ter_m)
            self.ter_o     = float(ter_o)
            self.t_m       = float(t_m)
            self.t_o       = float(t_o)
            self.t_qc      = int(t_qc)
            self.q_m       = float(q_m)
            self.q_o       = float(q_o)
            self.q_qc      = int(q_qc)
            self.ws_m      = float(ws_m)
            self.ws_o      = float(ws_o)
            self.ws_qc     = int(ws_qc)
            self.wd_m      = float(wd_m)
            self.wd_o      = float(wd_o)
            self.wd_qc     = int(wd_qc)

        missing_by_10  = base_verification_pair.PAIR_DATA_MISSING_VALUE / 10.0
        missing_by_100 = base_verification_pair.PAIR_DATA_MISSING_VALUE / 100.0
        if self.psfc_o == missing_by_10:
            self.psfc_o = base_verification_pair.MISSING_VALUE
        if self.slp_o == missing_by_100:
            self.slp_o = base_verification_pair.MISSING_VALUE
        if self.ter_o == missing_by_100:
            self.ter_o = base_verification_pair.MISSING_VALUE
        if self.t_o == missing_by_100:
            self.t_o = base_verification_pair.MISSING_VALUE
        if self.q_o == missing_by_100:
            self.q_o = base_verification_pair.MISSING_VALUE
        if self.ws_o == missing_by_100:
            self.ws_o = base_verification_pair.MISSING_VALUE
        if self.wd_o == missing_by_100:
            self.wd_o = base_verification_pair.MISSING_VALUE
            
    
    @staticmethod
    def get_header():
        return "Date,Lat,Lon,Domain,Platform,psfc_m,psfc_o,psfc_qc,slp_m,slp_o,slp_qc,ter_m,ter_o,t_m,t_o,t_qc,q_m,q_o,q_qc,ws_m,ws_o,ws_qc,wd_m,wd_o,wd_qc"
    
    @staticmethod
    def get_threshold():
        return verification_pair_record.sThreshold
    
    @staticmethod
    def set_threshold(threshold):
        verification_pair_record.sThreshold = threshold

    def get_obs_time(self):
        if self.obs_time is None:
            obs_time = verification_pair_record.DATE_FORMAT % (
                    self.year,self.month,self.day,self.hour,self.min)
        else:
            obs_time = self.obs_time
        return obs_time
    
    def get_sams_id(self):
        sams_id = None
        #debug = not debug
        sams_platform = True
        if self.platform[0:2].upper() == 'SM':
            if self.platform[2:].isdigit():
                sams_id = self.platform[2:]
        elif self.platform[0] == 'S':
            #for a_platform in verification_pair_record.NON_SAMS_PLATFORMS:
            #    if self.platform == a_platform:
            #        sams_platform = False
            #        break
            sams_platform = self.platform[1].isdigit()
            if sams_platform:
                if self.platform[1:].isdigit():
                    if '0' == self.platform[1]:
                        sams_id = self.platform[2:]
                    else:
                        sams_id = self.platform[1:]
                    
        #if sams_id is not None:
        #    sams_id = '%02d' % int(sams_id)

        return sams_id
    
    def is_same_float_value(self, value1, value2):
        return (math.fabs(value1 - value2) < 0.000000001)
    
    def is_sams_platform(self):
        method_name = '{m1}.{m2}'.format(m1='verification_pair_record', m2='is_sams_platform')
        debug = False
        #debug = not debug
        sams_platform = False
        this_platform = self.platform.upper()
        if this_platform[0:2].upper() == 'SM':
            sams_platform = self.platform[2:].isdigit()
        elif this_platform[0] == 'S':
            #for a_platform in verification_pair_record.NON_SAMS_PLATFORMS:
            #    if self.platform == a_platform:
            #        sams_platform = False
            #        break
            #if sams_platform:
            #    sams_platform = self.platform[1:].isdigit()
            sams_platform = self.platform[1:].isdigit()
        #else:
        #    sams_platform = False
        #if debug:   print(' %s [%s] [%s] ==> %r' % (self.platform, self.platform[0:2], self.platform[0], sams_platform))
        #print(' %s %s [%s]' % (
        #    method_name, self.platform, self.platform[0]))
        if debug and sams_platform:   print(' %s %s [%s] [%s] ==> %r' % (
            method_name, self.platform, self.platform[0:2], self.platform[0], sams_platform))
        return sams_platform
    
    def is_valid_qc(self):
        threshold = verification_pair_record.get_threshold()
        is_valid = (threshold <= self.psfc_qc and threshold <= self.slp_qc
                and threshold <= self.t_qc    and threshold <= self.q_qc
                and threshold <= self.ws_qc   and threshold <= self.wd_qc)
        
        return is_valid
    
    def qc_values(self):
        return verification_pair_record.QC_FORMAT_STRING % (
                self.psfc_qc,self.slp_qc,self.t_qc,self.q_qc,self.ws_qc,self.wd_qc)
    def toString(self):
        return verification_pair_record.FORMAT_STRING % (
                self.year,self.month,self.day,self.hour,self.min,
                self.lat,self.lon,self.domain_id,self.platform,
                self.psfc_m,self.psfc_o,self.psfc_qc,self.slp_m, self.slp_o,self.slp_qc,
                self.ter_m,self.ter_o,self.t_m,self.t_o,self.t_qc,
                self.q_m,self.q_o,self.q_qc,self.ws_m,self.ws_o,self.ws_qc,
                self.wd_m,self.wd_o,self.wd_qc)

class verification_pair_reader(base_verification_pair):
    '''
    classdocs
    '''

    MODULE_NAME = 'verification_pair_reader'

    
    #@abstractmethod  
    def pre_process(self):
        print("%s" % verification_pair_record.get_header())
    
    #@abstractmethod  
    def handle_record(self, verification_record, index):
        print("%s" % (verification_record.toString()))
    


