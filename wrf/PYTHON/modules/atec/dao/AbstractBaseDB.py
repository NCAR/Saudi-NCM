#!/usr/bin/env python

from abc import ABCMeta, abstractmethod

try:
    from BaseObject import BaseObject, convert_datetime_to_seconds, is_equal_number
except ImportError:
    from atec.dao.BaseObject import BaseObject, convert_datetime_to_seconds, is_equal_number

def convert_datetime_to_sql_string(datetime_obj):
    cdts_date_string = None
    if datetime_obj and datetime_obj != "NULL" and datetime_obj != "null":
        cdts_date_string = datetime_obj.strftime(AbstractBaseDB.SQL_DATE_FORMAT)
    return cdts_date_string
  
class AbstractBaseDB(BaseObject):
    __metaclass__ = ABCMeta

    SQL_DATE_FORMAT = '%Y-%m-%d %H:%M:%S'
    SQL_DATE_FORMAT_YMD = "%Y-%m-%d"
    
    COLUMNS_TABLE_NAME = "information_schema.columns"
    TABLES_TABLE_NAME  = "information_schema.tables"
  

    def __init__(self):
        super(AbstractBaseDB, self).__init__()
    
    def convert_datetime_to_seconds(self, datetime_obj):
        return convert_datetime_to_seconds(datetime_obj)
    
    def convert_datetime_to_string(self, datetime_obj):
        datetime_str = None
        if datetime_obj is not None:
            datetime_str = datetime_obj.strftime(AbstractBaseDB.SQL_DATE_FORMAT)
        return datetime_str
    
    def get_variable_columns(self):
        gvc_column_list = []
        gvc_table_name = self.get_table_name()
        if gvc_table_name is not None:
            # "select column_name from information_schema.columns where TABLE_SCHEMA='mir' and TABLE_NAME='surface_data' and DATA_TYPE='double';"
            gvc_sql_columns = "column_name"
            gvc_sql_where = "TABLE_SCHEMA='%s' and TABLE_NAME='%s' and DATA_TYPE='double'" \
                % (self.db_actor.db_name, gvc_table_name)
        
            gvc_rows = self.db_actor.get_records(
                    AbstractBaseDB.COLUMNS_TABLE_NAME,gvc_sql_columns, gvc_sql_where)
            for gvc_row in gvc_rows:
                gvc_column_list.append(gvc_row[0])
    
        return gvc_column_list
    
    @abstractmethod
    def get_table_name(self):
        pass
  
    def is_equal_number(self, value1, value2):
        return is_equal_number(value1, value2)
    
