#!/usr/bin/env python

try:
    from AbstractRecord import AbstractRecord
    from AbstractTable import AbstractTable
except ImportError:
    from atec.dao.AbstractRecord import AbstractRecord
    from atec.dao.AbstractTable import AbstractTable
    
MODULE_NAME = 'Table_SurfaceObsData'

def get_singleton(db_actor):
    if not Table_SurfaceObsData.singleton:
        Table_SurfaceObsData.singleton = Table_SurfaceObsData(db_actor)
        #Table_SurfaceObsData.singleton.initialize_map_and_list()
    return Table_SurfaceObsData.singleton
  
def get_table_name():
    table_name = Table_SurfaceObsData.TABLE_NAME
    if Table_SurfaceObsData.POSTFIX:
        table_name = '%s%s' % (table_name, Table_SurfaceObsData.POSTFIX)
    #return Table_SurfaceObsData.TABLE_NAME + Table_SurfaceObsData.POSTFIX
    return table_name
  
  
class SurfaceObsData(AbstractRecord):
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
    TEMP_QC2        = 1 + WDIR_QC
    Q2_QC2          = 1 + TEMP_QC2 
    PRES_QC2        = 1 + Q2_QC2
    WSPD_QC2        = 1 + PRES_QC2
    WDIR_QC2        = 1 + WSPD_QC2
    MAX_INDEX       = WDIR_QC2
    
    DATA_COLUMNS = ['temp','temp_qc','q2','q2_qc','pres','pres_qc','wspd_10m','wspd_10m_qc','wdir_10m','wdir_10m_qc',
                    'temp_qc2','q2_qc2','wspd_10m_qc2','wdir_10m_qc2','pres_qc2' ]

    VAR_START_INDEX = TEMP
    VAR_LAST_INDEX  = WDIR_QC2
    
    FILTERED_MARK = -9998
    FIELD_IDS = [1,2,3,4,5]
    COLUMN_INDEXES = [TEMP, Q2, PRES, WSPD, WDIR ]
    FIELD_ID_2_COLUMN_INDEX = dict(zip(FIELD_IDS, COLUMN_INDEXES))
    
    def __init__(self, row):
        super(SurfaceObsData, self).__init__(row)
        self.modified = False
        self.modified_fields = [False for y in range(self.VAR_LAST_INDEX - self.VAR_START_INDEX + 1)]
    
    def get_table_name(self):
        return Table_SurfaceObsData.TABLE_NAME
    
    def get_last_update(self):
        return self.row[self.LAST_UPDATE]
        
    def get_last_update_sql(self):
        return self.convert_datetime_to_string(self.get_last_update())
    
    def get_obs_time(self):
        return self.row[self.OBS_TIME]
    
    def get_obs_time_as_seconds(self):
        return self.convert_datetime_to_seconds(self.get_obs_time())
    
    def get_obs_time_sql(self):
        return self.convert_datetime_to_string(self.get_obs_time())
        
    def get_unique_site_id(self):
        return self.row[self.UNIQUE_SITE_ID]
    def get_pres(self, filter_by_qc=False):
        value = self.row[self.PRES]
            
        return value 
    def get_pres_qc(self):
        return self.row[self.PRES_QC]
    def get_pres_qc2(self):
        return self.row[self.PRES_QC2]
    def get_q2(self, filter_by_qc=False):
        return self.row[self.Q2]
    def get_q2_qc(self):
        return self.row[self.Q2_QC]
    def get_q2_qc2(self):
        return self.row[self.Q2_QC2]
    def get_temp(self, filter_by_qc=False):
        return self.row[self.TEMP]
    def get_temp_qc(self):
        return self.row[self.TEMP_QC]
    def get_temp_qc2(self):
        return self.row[self.TEMP_QC2]
    def get_wdir(self, filter_by_qc=False):
        return self.row[self.WDIR]
    def get_wdir_qc(self):
        return self.row[self.WDIR_QC]
    def get_wdir_qc2(self):
        return self.row[self.WDIR_QC2]
    def get_wspd(self, with_qc=False):
        return self.row[self.WSPD]
    def get_wspd_qc(self):
        return self.row[self.WSPD_QC]
    def get_wspd_qc2(self):
        return self.row[self.WSPD_QC2]
    
    def get_value_with_qc_score(self, var_name):
        getter_name='self.get_{v}'.format(v=var_name)
        qc_getter_name='self.get_{v}_qc2'.format(v=var_name)
        value = getter_name()
        qc_value = qc_getter_name()
        return (value, qc_value)
    
    def get_column_index(self, field_id):
        index = self.FIELD_ID_2_COLUMN_INDEX.get(field_id, None)
        if index is None:
            index = -1
        return index
  
    def get_column_index_old(self, field_id):
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
        if index >= 0 and index < self.MAX_INDEX:
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
        if not self.is_equal_number(self.row[index], value):
            self.row[index] = value
            self.modified = True
            self.modified_fields[index - self.VAR_START_INDEX] = True

    def set_pres_qc(self, value):
        index = self.PRES_QC
        if not self.is_equal_number(self.row[index], value):
            self.row[index] = value
            self.modified = True
            self.modified_fields[index - self.VAR_START_INDEX] = True

    def set_pres_qc2(self, value):
        index = self.PRES_QC2
        if not self.is_equal_number(self.row[index], value):
            self.row[index] = value
            self.modified = True
            self.modified_fields[index - self.VAR_START_INDEX] = True

    def set_q2(self, value):
        index = self.Q2
        if not self.is_equal_number(self.row[index], value):
            self.row[index] = value
            self.modified = True
            self.modified_fields[index - self.VAR_START_INDEX] = True

    def set_q2_qc(self, value):
        index = self.Q2_QC
        if not self.is_equal_number(self.row[index], value):
            self.row[index] = value
            self.modified = True
            self.modified_fields[index - self.VAR_START_INDEX] = True

    def set_q2_qc2(self, value):
        index = self.Q2_QC2
        if not self.is_equal_number(self.row[index], value):
            self.row[index] = value
            self.modified = True
            self.modified_fields[index - self.VAR_START_INDEX] = True

    def set_temp(self, value):
        index = self.TEMP
        if not self.is_equal_number(self.row[index], value):
            self.row[index] = value
            self.modified = True
            self.modified_fields[index - self.VAR_START_INDEX] = True

    def set_temp_qc(self, value):
        index = self.TEMP_QC
        if not self.is_equal_number(self.row[index], value):
            self.row[index] = value
            self.modified = True
            self.modified_fields[index - self.VAR_START_INDEX] = True

    def set_temp_qc2(self, value):
        index = self.TEMP_QC2
        if not self.is_equal_number(self.row[index], value):
            self.row[index] = value
            self.modified = True
            self.modified_fields[index - self.VAR_START_INDEX] = True

    def set_wdir(self, value):
        index = self.WDIR
        if not self.is_equal_number(self.row[index], value):
            self.row[index] = value
            self.modified = True
            self.modified_fields[index - self.VAR_START_INDEX] = True

    def set_wdir_qc(self, value):
        index = self.WDIR_QC
        if not self.is_equal_number(self.row[index], value):
            self.row[index] = value
            self.modified = True
            self.modified_fields[index - self.VAR_START_INDEX] = True

    def set_wdir_qc2(self, value):
        index = self.WDIR_QC2
        if not self.is_equal_number(self.row[index], value):
            self.row[index] = value
            self.modified = True
            self.modified_fields[index - self.VAR_START_INDEX] = True

    def set_wspd(self, value):
        index = self.WSPD
        if not self.is_equal_number(self.row[index], value):
            self.row[index] = value
            self.modified = True
            self.modified_fields[index - self.VAR_START_INDEX] = True

    def set_wspd_qc(self, value):
        index = self.WSPD_QC
        if not self.is_equal_number(self.row[index], value):
            self.row[index] = value
            self.modified = True
            self.modified_fields[index - self.VAR_START_INDEX] = True

    def set_wspd_qc2(self, value):
        index = self.WSPD_QC2
        if not self.is_equal_number(self.row[index], value):
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
    
class Table_SurfaceObsData(AbstractTable):
    TABLE_NAME    = "surface_obs_data"
    POSTFIX       = ""
    DISTINCT_KEYS = "DISTINCT obs_time,unique_site_id,report_type"
    
    debug = True
    singleton = None
    
    #@staticmethod  
    #def get_table_name():
    #  return Table_SurfaceObsData.TABLE_NAME
    
    def __init__(self, db_actor):
        super(Table_SurfaceObsData, self).__init__(db_actor)
        #self.initialize_map_and_list()
    
    def get_data(self, db_row):
        return SurfaceObsData(db_row)
    
    def get_distinct_columns(self):
        return Table_SurfaceObsData.DISTINCT_KEYS
    
    # Override the base
    def get_records(self, sql_columns, sql_where, group_by='', order_by=''):
        fname = '%s.%s' % (MODULE_NAME, 'get_records')
        self.debug_print_called(fname)
        
        #table_name = self.get_table_name_with_alias()
        table_name = self.get_table_name()
        
        rows = self.db_actor.get_records(table_name, sql_columns, sql_where, group_by, order_by)
        return rows
    
    # sql_template should have two %s:
    #  first %s is for the table name
  
    def get_surface_data_by_key(self, key_record):
        debug = False
        fname = '%s.%s' % (MODULE_NAME, 'get_surface_data_by_key')
        data = None
        sql_where = "report_type=%d AND unique_site_id='%s' AND obs_time='%s'" % (
                key_record.get_report_type(), key_record.get_unique_site_id(),key_record.get_obs_time())
        
        rows = self.db_actor.get_records(self.get_table_name(), sql_where=sql_where)
        if rows:
            data = self.get_data(rows[0])
            if debug:
                message = " %s: row: " % (fname), data
                self.debug_message(message)
        else:
            self.error_message(" %s: no rows for [%s] sql: %s" % (
                    fname, key_record.to_string(), sql_where))
        return data

    def get_table_name(self):
        return Table_SurfaceObsData.TABLE_NAME

    # Map with station_id which is map by obs_time in seconds
#     def load_and_entity_map_del(self, target_obs_time):
#         my_sql_where = "DATE(obs_time)='{d}'".format(d=target_obs_time.strftime(self.SQL_DATE_FORMAT_YMD))
#         rows = self.get_records('*', sql_where=my_sql_where, order_by='unique_site_id, obs_time')
#         
#         dao_entity_map = {}
#         entity_list = {}
#         prev_site_id = '___dummy_site___'
#         if rows is not None:
#             unique_site_id = None
#             for row in rows:
#                 dao_entity = self.get_data(row)
#                 unique_site_id = dao_entity.get_unique_site_id()
#                 if prev_site_id != unique_site_id:
#                     if prev_site_id != '___dummy_site___':
#                         dao_entity_map[str(prev_site_id)] = entity_list
#                     entity_list = dao_entity_map.get(unique_site_id, None)
#                     if entity_list is None:
#                         entity_list = {}
#                     prev_site_id = unique_site_id
#                 entity_list[dao_entity.get_obs_time_as_seconds()] = dao_entity
#             if unique_site_id is not None:
#                 dao_entity_map[str(unique_site_id)] = entity_list
#         return dao_entity_map

    def load_and_entity_map(self, target_obs_time, include_previous=False):
        my_sql_where = "DATE(obs_time)='{d}'".format(d=target_obs_time.strftime(self.SQL_DATE_FORMAT_YMD))
        if include_previous:
            my_sql_where = "{s} OR (obs_time)<'{d}' AND obs_time>=DATE_SUB('{d}', INTERVAL 1 HOUR)".format(
                        s=my_sql_where, d=target_obs_time.strftime(self.SQL_DATE_FORMAT_YMD))
        return self.load_and_entity_map_with_where(my_sql_where)

    def load_and_entity_map_with_where(self, sql_where):
        rows = self.get_records('*', sql_where=sql_where, order_by='unique_site_id, obs_time')
        
        dao_entity_map = {}
        entity_list = {}
        prev_site_id = '___dummy_site___'
        if rows is not None:
            unique_site_id = None
            for row in rows:
                dao_entity = self.get_data(row)
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

    def load_entity_map_not_QCed(self):
        #sql_min_date = 'SELECT MIN(obs_time) FROM {t} WHERE temp_qc2=-1 OR wspd_10m_qc2=-1 OR wdir_10m_qc2=-1 OR pres_qc2=-1'.format(
        #        t=self.get_table_name())
        #sql_max_date = 'SELECT MAX(obs_time) FROM {t} WHERE temp_qc2=-1 OR wspd_10m_qc2=-1 OR wdir_10m_qc2=-1 OR pres_qc2=-1'.format(
        #        t=self.get_table_name())
        dao_entity_map = {}

        table_name  = self.get_table_name()
        sql_min_date = None
        sql_max_date = None
        my_sql_where = 'temp_qc2=-1 OR wspd_10m_qc2=-1 OR wdir_10m_qc2=-1 OR pres_qc2=-1'
        rows = self.get_records('MIN(obs_time)', sql_where=my_sql_where)
        if rows is not None and len(rows) > 0 and rows[0][0] is not None:
            sql_min_date = rows[0][0]
            rows = self.get_records('MAX(obs_time)', sql_where=my_sql_where)
            if rows is not None and len(rows) > 0:
                sql_max_date = rows[0][0]
            my_sql_where = "obs_time<'{t}'".format(t=sql_min_date)
            rows = self.get_records('MAX(obs_time)', sql_where=my_sql_where)
            if rows is not None and len(rows) > 0 and rows[0][0] is not None:
                sql_min_date = rows[0][0]
                my_sql_where = "obs_time >= '{mint}' AND obs_time <= '{maxt}'".format(
                        mint=sql_min_date, maxt=sql_max_date)
            elif sql_max_date is not None:
                my_sql_where = "obs_time <= '{maxt}'".format(maxt=sql_max_date)
            else:
                my_sql_where = "obs_time > NOW()"
            dao_entity_map = self.load_and_entity_map_with_where(my_sql_where)
        return dao_entity_map


