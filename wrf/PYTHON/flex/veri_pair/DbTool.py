#!/usr/bin/env python

'''
Created on Apr 10, 2015

@author: hsoh
'''

import sys
import datetime

from Constants import Constants
try:
    from BaseObject import debug_message, error_message
    from DbActor  import DbActor, create_db_actor
except ImportError:
    from atec.dao.BaseObject import debug_message, error_message
    from atec.dao.DbActor  import DbActor, create_db_actor
try:
    from DbConfig import create_db_config
except ImportError:
    from atec.config.DbConfig import create_db_config
    
DEF_RANGE_ID    = 3
DEF_REPORT_TYPE = 3


class DbTool(object):
    '''
    classdocs
    '''
    
    CLASS_NAME = 'DbTool'

    TBL_NAME_column_availability = 'qc_column_availability'
    TBL_NAME_field_table_map     = 'qc_field_table_map'
    TBL_NAME_surface_data        = 'qc_surface_data'
    #TBL_NAME_surface_data        = 'sams_surface_data'
    TBL_NAME_range               = 'qc_range'
    TBL_NAME_qc_score_summary    = 'qc_score_summary'
    
    db_actor = None
    db_actors = {}
    range_ids = None
    
    def __init__(self, params):
        '''
        Constructor
        '''
        
    @staticmethod
    def build_sql_string(self, sql_map):
        return 'not implemented'
    
    @staticmethod
    def initialize_db(db_name):
        db_actor = DbTool.db_actors.get(db_name)
        if db_actor is None:
            db_config = create_db_config()
            db_actor = create_db_actor(db_config, db_name)
            db_actor.db_config = db_config
            DbTool.db_actors[db_name] = db_actor
            DbTool.db_actor = db_actor
        return (db_actor)
    
    @staticmethod
    def get_active_column_id_list(report_type, range_id):
        method_name = '   %s@%s()' % (DbTool.CLASS_NAME, 'get_active_column_id_list')
        column_id_array = None
        if DbTool.db_actor is not None:
            column_id_array = []
            columns    = 'column_id'
            sql_where_common  = "enabled!=0"
            table_name = DbTool.TBL_NAME_column_availability
            sql_where  = "range_id=%d AND %s" % (range_id, sql_where_common)
            if report_type is not None:
                sql_where  = "report_type=%d AND %s" % (report_type, sql_where)
            rows = DbTool.db_actor.get_records(table_name, columns, sql_where, order_by=columns)
            if rows is None or 0 == len(rows):
                if DEF_REPORT_TYPE != report_type:
                    #if report_type is not None and 14 == report_type:
                    sql_where  = "report_type=%d AND range_id=%d AND %s" % (
                            DEF_REPORT_TYPE, range_id, sql_where_common)
                    rows = DbTool.db_actor.get_records(table_name, columns, sql_where, order_by=columns)
            if rows is None or 0 == len(rows):
                if DEF_RANGE_ID != range_id:
                    sql_where  = "range_id=%d AND %s" % (DEF_RANGE_ID, sql_where_common)
                    if report_type is not None:
                        sql_where  = "report_type=%d AND %s" % (report_type, sql_where)
                        rows = DbTool.db_actor.get_records(table_name, columns, sql_where, order_by=columns)
            if rows is None or 0 == len(rows):
                #if report_type is not None and 14 == report_type:
                sql_where  = "report_type=%d AND range_id=%d AND %s" % (
                        DEF_REPORT_TYPE, DEF_RANGE_ID, sql_where_common)
                rows = DbTool.db_actor.get_records(table_name, columns, sql_where, order_by=columns)
            if rows is not None and 0 < len(rows):
                for row in rows:
                    column_id_array.append(row[0])
            else:
                error_message('{m} Can not find QC enabled columns (Check {t})'.format(
                        m=method_name, t=table_name))
        else:
            error_message('%s Call DbTool.initialize_db(db_name) first' % (method_name))
        return column_id_array
    
    @staticmethod
    def get_disabled_column_id_list(report_type, range_id):
        method_name = '   %s@%s()' % (DbTool.CLASS_NAME, 'get_available_column_id_list')
        column_id_array = None
        if DbTool.db_actor is not None:
            column_id_array = []
            columns    = 'column_id'
            table_name = DbTool.TBL_NAME_column_availability
            sql_where_common  = "range_id=%d AND enabled=0" % (range_id)
            if report_type is not None:
                sql_where  = "report_type=%d AND %s" % (report_type, sql_where_common)
            rows = DbTool.db_actor.get_records(table_name, columns, sql_where, order_by=columns)
            if rows is None or 0 == len(rows):
                if report_type is not None:
                    sql_where  = "report_type=%d AND %s" % (DEF_REPORT_TYPE, sql_where_common)
                    rows = DbTool.db_actor.get_records(table_name, columns, sql_where, order_by=columns)
            if rows is not None and 0 < len(rows):
                for row in rows:
                    column_id_array.append(row[0])
        else:
            error_message('{} Call DbTool.initialize_db(db_name) first'.format(method_name))
        return column_id_array
    
    @staticmethod
    def get_obs_table_name():
        method_name = '   %s@%s()' % (DbTool.CLASS_NAME, 'get_obs_table_name')
        obs_table_name = None
        if DbTool.db_actor is not None:
            columns    = 'table_name'
            table_name = DbTool.TBL_NAME_field_table_map
            sql_where  = "id=1"                 # Table name for temp
            rows = DbTool.db_actor.get_records(table_name, columns, sql_where)
            if rows is not None and 0 < len(rows):
                obs_table_name = rows[0][0]
        else:
            error_message('{} Call DbTool.initialize_db(db_name) first'.format(method_name))
        return obs_table_name
    
    @staticmethod
    def get_available_range_id_list():
        method_name = '   %s@%s()' % (DbTool.CLASS_NAME, 'get_available_range_id_list')
        if DbTool.range_ids is None:
            range_ids = None
            if DbTool.db_actor is not None:
                range_ids = []
                columns    = 'DISTINCT range_id'
                table_name = DbTool.TBL_NAME_surface_data
                sql_where  = "obs_time > DATE_ADD((SELECT MAX(obs_time) FROM %s), INTERVAL -1 HOUR)" % (
                        DbTool.TBL_NAME_qc_score_summary)
                rows = DbTool.db_actor.get_records(table_name, columns, sql_where)
                if rows is None or 0 == len(rows):
                    rows = DbTool.db_actor.get_records(
                            DbTool.TBL_NAME_column_availability, columns)
                if rows is None or 0 == len(rows):
                    rows = DbTool.db_actor.get_records(table_name, columns)
                if rows is not None and 0 < len(rows):
                    for row in rows:
                        range_ids.append(row[0])
                else:
                    error_message(' {m} Can not find range_id from {t1} and {t2}'.format(
                            m=method_name, t1=DbTool.TBL_NAME_surface_data,
                            t2=DbTool.TBL_NAME_column_availability))
                    
                DbTool.range_ids = range_ids
            else:
                error_message('{} Call DbTool.initialize_db(db_name) first'.format(method_name))
    
            debug_message("%s  available range id list: %r " % (method_name, range_ids))
        return DbTool.range_ids
    
    @staticmethod
    def get_range_id_list_from_db():
        method_name = '   %s@%s()' % (DbTool.CLASS_NAME, 'get_range_id_list_from_db')
        if DbTool.range_ids is None:
            range_ids = None
            if DbTool.db_actor is not None:
                range_ids = []
                columns    = 'range_id'
                table_name = DbTool.TBL_NAME_range
                rows = DbTool.db_actor.get_records(table_name, columns, sql_where='range_id != 999')
                if rows is not None and 0 < len(rows):
                    for row in rows:
                        range_ids.append(row[0])
            else:
                error_message ('{} Call DbTool.initialize_db(db_name) first'.format(method_name))
            
            debug_message("%s  available range id list from db: %r " % (method_name, range_ids))
            DbTool.range_ids = range_ids
        return DbTool.range_ids

    
    @staticmethod
    def get_available_report_types(range_id):
        method_name = '   %s@%s()' % (DbTool.CLASS_NAME, 'get_available_report_types')
        report_type_array = []
        if DbTool.db_actor is not None:
            table_name = DbTool.get_obs_table_name()
            if table_name is not None:
                columns    = 'DISTINCT report_type'
                rows = DbTool.db_actor.get_records(table_name, columns)
                if rows is None or 0 == len(rows):
                    tmp_table_name = '%s_%s' % (table_name, DbTool.get_historic_cur_month())
                    rows = DbTool.db_actor.get_records(tmp_table_name, columns)
                if rows is not None and 0 < len(rows):
                    for row in rows:
                        report_type_array.append(row[0])
                #else:
                #    report_type_array.append(3)
            else:
                error_message('{} Can not find obs table name'.format(method_name))
                #report_type_array.append(3)
        else:
            error_message('{} Call DbTool.initialize_db(db_name) first'.format(method_name))
        if 0 == len(report_type_array):
            report_type_array.append(3)
            if 3 == range_id:
                report_type_array.append(15)
            elif 1 == range_id or 6 == range_id or 8 == range_id:
                report_type_array.append(14)
            
        return report_type_array

    @staticmethod
    def get_historic_cur_month():
        #tmp_now = datetime.datetime.now()
        #yyyymm = '%4d%02d' % (tmp_now.year, tmp_now.month)
        today = datetime.date.today()
        return today.strftime("%Y%m")

    @staticmethod
    def set_DbActor_debug_options(command, debug):
        sdd_command = command.lower()
        if   sdd_command == "debug":
            DbActor.debug = debug
            DbActor.debug_delete = debug
            DbActor.debug_execute = debug
            DbActor.debug_insert = debug
            DbActor.debug_select = debug
            DbActor.debug_update = debug
        elif sdd_command == "delete":
            DbActor.debug_delete = debug
        elif sdd_command == "execute":
            DbActor.debug_execute = debug
        elif sdd_command == "insert":
            DbActor.debug_insert = debug
        elif sdd_command == "select":
            DbActor.debug_select = debug
        elif sdd_command == "update":
            DbActor.debug_update = debug
    
    
def main(argv):
    db_name = Constants.DB_NAME
    range_id = 3
    DbTool.initialize_db(db_name)
    report_types = DbTool.get_available_report_types(range_id)
    if report_types is not None and 0 < len(report_types):
        print (' ==== report_types: %r' % (report_types))
        for report_type in report_types:
            column_id_list = DbTool.get_active_column_id_list(report_type, range_id)
            print ('      report_type: %d,   available column_id_list: %r' % (report_type, column_id_list))
            column_id_list = DbTool.get_disabled_column_id_list(report_type, range_id)
            print ('      report_type: %d,    disabled column_id_list: %r' % (report_type, column_id_list))

if __name__ == "__main__":
    print (' DbTool is called' )
    main(sys.argv)
