#!/usr/bin/env python

from atec.util.DateUtil import DateUtil, LogTime
from atec.veri_pair.verification_pair import base_import_verification_dir

MODULE_NAME = 'import_verification_model_data'

MODULE_VERSION="Version 1.0"
DEFAULT_RANGE_NAME = 'CRTC'
#DEFAULT_RANGE_NAME = 'ATC'

class import_model_data(base_import_verification_dir):

    #INPUT_SUB_DIRS='sams_sites/fcst'
    INPUT_SUB_DIRS='fcst'
    
    TABLE_NAME = 'surface_model_data'
    COLUMNS = 'cycle_time,valid_time,unique_site_id,model_id,domain_id,temp,q2,pres,wspd_10m,wdir_10m,model_version_id,lead_time'

    def __init__(self, job_user = 'atecuser'):
        super(import_model_data, self).__init__(job_user)
        self.model_id = int(self.options.model_id)
        self.model_version_id = self.options.model_version_id
        self.missing_range_codes = []

    #@abstractmethod
    def get_input_sub_dirs(self):
        return import_model_data.INPUT_SUB_DIRS
    
    #@abstractmethod
    def get_table_name(self):
        return import_model_data.TABLE_NAME
    
    #@abstractmethod
    def get_update_sql_string(self, line_input):
        sql_insert = None
        #YYYYMMDDHHMN, FCST_HR, GRID,      STTN, PSFC_mb, PMSL_mb,     T_C,  Q_g/kg, SPD_m/s,  DIR
        (valid_time,lead_time,domain_id,range_code,pres,pmsl,temp,q2,wspd_10m,wdir_10m) = line_input.split(',')
        
        # Skip header
        if valid_time is not None and valid_time.isdigit():
            range_code = range_code.strip()
            if not range_code in self.missing_range_codes:
                unique_site_id = self.stations.get_unique_site_id(range_code)
                if unique_site_id is None:
                    print("     ERROR can't find unique_site_id from range_code [{r}]".format(r=range_code))
                    self.missing_range_codes.append(range_code)
                else:
                    valid_time_sql = DateUtil.convert_to_mysql_date(valid_time.strip())
                    time_diff = DateUtil.convert_to_datetime(valid_time.strip()) - self.cycle_time
                    try:
                        lead_time = int(time_diff.total_seconds() / 3600)
                    except:
                        lead_time = int((time_diff.days * 24 * 60 * 60 + time_diff.seconds) / 3600)
                    #print('time_diff: {t}, lead_time: {l}'.format(t=time_diff, l=lead_time))
                    values = "'{c}','{v}','{s}',{m},{d},{t},{q},{p},{ws},{wd},'{mv}',{l}".format(
                            c=self.cycle_time_sql,v=valid_time_sql,s=unique_site_id.strip(),
                            m=self.model_id,d=domain_id,t=temp,q=q2,p=pres,ws=wspd_10m,wd=wdir_10m,
                            mv=self.model_version_id,l=lead_time)
                    sql_insert = base_import_verification_dir.INSERT_TEMPLATE % (
                            import_model_data.TABLE_NAME, import_model_data.COLUMNS,values)

        return sql_insert
        
    #@abstractmethod
    def import_to_db_preprocess(self, input_full_name):
        #file = self.get_basename(input_full_name)
        cycle_time_str = self.get_basename(input_full_name).split('_')[0].strip()
        self.cycle_time = DateUtil.convert_to_datetime(cycle_time_str)
        self.cycle_time_sql = DateUtil.convert_to_mysql_date(self.cycle_time)

    #@abstractmethod
    def import_to_db_postprocess(self, input_full_name):
        pass
        
def main():
    #model_id = 0
    #model_version_id = ''
    job_user = 'atecuser'
    actor = import_model_data(job_user)
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