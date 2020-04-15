#!/usr/bin/env python

from abc import ABCMeta, abstractmethod

try:
    from AbstractBaseDB import AbstractBaseDB
except ImportError:
    from atec.dao.AbstractBaseDB import AbstractBaseDB
    
MODULE_NAME = 'AbstractTable'

class AbstractTable(AbstractBaseDB):
    __metaclass__ = ABCMeta

    ALIAS_MAIN_TABLE = "M"
    
    def __init__(self, db_actor):
        AbstractBaseDB.__init__(self)
        self.db_actor = db_actor
        
        self.initialization_time = 0
        self.insert_time = 0
        self.table_read_time = 0
        self.modified_count = 0
    
    def append_alias(self, name, alias):
        return "%s %s" % (name, alias)
    
    def commit(self):
        self.db_actor.commit()
            
    def execute_sql(self, sql_string, commit_flag=False):
        #fname = '%s.%s' % (MODULE_NAME, 'execute_sql')
        result = self.db_actor.execute_sql(sql_string,commit_flag)
        return result
      
    def execute_sql2(self, sql_string, parameters, commit_flag=False):
        #fname = '%s.%s' % (MODULE_NAME, 'execute_sql')
        result = self.db_actor.execute_sql2(sql_string,parameters,commit_flag)
        return result
      
    def exist_column(self, db_name, table_name, column_name):
        return self.db_actor.exist_column(table_name, column_name)
      
    def exist_table(self, table_name):
        return self.db_actor.exist_table(table_name)
      
    def get_column_names_by_data_type(self, data_type):
        table_name = self.get_table_name()
        # "select column_name from information_schema.columns where TABLE_SCHEMA='mir' and TABLE_NAME='surface_data' and DATA_TYPE='double';"  
        sql_columns = "column_name"
        sql_where = "TABLE_SCHEMA='%s' and TABLE_NAME='%s' and DATA_TYPE='%s'" % (
                self.db_actor.db_name, table_name, data_type)
        
        column_list = []    
        rows = self.db_actor.get_records(AbstractTable.COLUMNS_TABLE_NAME,
                sql_columns, sql_where)
        for row in rows:
            column_list.append(row[0])
        
        return column_list
    
    def get_column_names_from_query(self, cursor):
        #num_fields = len(cursor.description)
        column_names = [i[0] for i in cursor.description]  
        return column_names
    
    def get_column_names_by_double(self):
        return self.get_column_names_by_data_type("double")
    
    def get_count(self, sql_where='', group_by=''):
        #fname = '%s.%s' % (MODULE_NAME, 'get_count()')
        count = self.get_count_any(self.get_table_name(), sql_where, group_by)
        return count
      
    def get_count_any(self, table_name, sql_where='', group_by=''):
        #fname = '%s.%s' % (MODULE_NAME, 'get_count_any()')
        count = self.db_actor.get_count(table_name, '%s %s' % (sql_where, group_by))
        return count
      
    def get_records(self, sql_columns='*', sql_where='', group_by='', order_by=''):
        #fname = '%s.%s' % (MODULE_NAME, 'get_records()')
        rows = self.get_records_any(self.get_table_name(), sql_columns, sql_where, group_by, order_by)
        return rows
      
    def get_records_any(self, table_name, sql_columns='*', sql_where='', group_by='', order_by=''):
        #fname = '%s.%s' % (MODULE_NAME, 'get_records_any()')
        rows = self.db_actor.get_records(table_name, sql_columns, sql_where, group_by, order_by)
        return rows
    
    @abstractmethod  
    def get_table_name(self):
        pass
    
    def get_table_name_with_alias(self):
        return self.append_alias(self.get_table_name(), self.ALIAS_MAIN_TABLE)
    
    def get_variable_columns(self):
        return self.get_column_names_by_double()
    
    def add_modified_count(self, count):
        self.modified_count += count
      
    def get_modified_count(self):
        return self.modified_count
      
    def reset_modified_count(self):
        self.modified_count = 0
      
    def set_modified_count(self, count):
        self.modified_count = count
      
    def add_initialization_time(self, duration):
        self.initialization_time += duration
    
    def get_initialization_time(self):
        return self.initialization_time
    
    #def set_initialization_time(self, duration):
    #   self.initialization_time = duration
    
    def add_insert_time(self, duration):
        self.insert_time += duration
    
    def get_insert_time(self):
        return self.insert_time
    
    #def set_insert_time(self, duration):
    #    self.insert_time = duration
    
    #def add_qc_time(self, duration):
    #    self.qc_time += duration
    
    #def get_qc_time(self):
    #    return self.qc_time
    
    #def set_qc_time(self, duration):
    #    self.qc_time = duration
    
    def add_table_read_time(self, duration):
        self.table_read_time += duration
    
    def get_table_read_time(self):
        return self.table_read_time
      
    #def set_table_read_time(self, duration):
    #  self.table_read_time = duration
    