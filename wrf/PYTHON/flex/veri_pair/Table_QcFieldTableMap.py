#!/usr/bin/env python

try:
    from AbstractMapTable import AbstractMapTable
    from AbstractRecord import Record_Id_Name
except ImportError:
    from atec.dao.AbstractMapTable import AbstractMapTable
    from atec.dao.AbstractRecord import Record_Id_Name

from Table_QcFieldStaticData import Table_QcFieldStaticData

MODULE_NAME = 'Table_QcFieldTableMap'

def build_map(db_actor):
    debug = False
    #debug = True
    
    fname = '%s:%s' % (MODULE_NAME, 'build_map')
    if debug:    print("  > {f} is called".format(f=fname))
    if not Table_QcFieldTableMap.table_map:
        if debug:    print("  > {f} Getting qc_field_map".format(f=fname))
        Table_QcFieldTableMap(db_actor)
    return (Table_QcFieldTableMap.table_map, Table_QcFieldTableMap.table_map_by_id)

def build_min_max_map(db_actor):
    debug = False
    #debug = not debug
    
    fname = '%s:%s' % (MODULE_NAME, 'get_min_max_map')
    if debug:    print ("   {f} is called".format(f=fname))
    if not Table_QcFieldTableMap.staticMinMaxMap:
        if debug:    print("  > {f} Getting qc_field_map".format(f=fname))
        Table_QcFieldTableMap(db_actor)
    return Table_QcFieldTableMap.staticMinMaxMap

def make_key_by_name(field_map_data):
    return field_map_data.get_obs_column_name()
    
def make_key_by_id(field_map_data):
    return field_map_data.get_id()

class ObsMinMaxData(Record_Id_Name):
    #def __init__(self, row):
    #  self.row = row
    
    def get_table_name(self):
        return Table_QcFieldStaticData.TABLE_NAME
    
    def get_obd_field_id(self):
        return self.get_id()
    
    def get_obs_column_name(self):
        return self.get_name()
    
    def get_qc_field_id(self):
        return self.row[2]
        
    def get_min_value(self):
        return self.row[3]
    
    def get_max_value(self):
        return self.row[4]
        
    #def get_unit(self):
    #  return self.row[5]
    
    #def is_disabled_qc(self):
    #  return self.row[6]
    
    #def get_comments(self):
    #  return self.row[7]
    
    def to_string(self):
        return ' %s [%d/%d] %f %f' % (self.get_name(), self.get_id(),
                self.get_qc_field_id(), self.get_min_value(), self.get_max_value())
  
class QcFieldTableMap(Record_Id_Name):
    #def __init__(self, row):
    #  self.row = row
    
    JOINED_COLUMNS = "M.id,M.obs_field_name,qc_field_id,J.field_name,M.table_name,J.disable_qc"
    
    def __init__(self, row):
        super(self.__class__, self).__init__(row)
        #self.set_call_stack(True)
        self.table_postfix = None
    
    def get_table_name(self):
        return Table_QcFieldTableMap.TABLE_NAME
    
    #def get_id(self):
    #  return self.row[0]
    
    def get_obs_column_name(self):
        return self.get_name()
    
    def get_qc_field_id(self):
        return self.row[2]
        
    def get_qc_field_name(self):
        return self.row[3]
    
    def get_obs_table_name(self):
        gotn_obs_table_name = self.row[4]
        if self.table_postfix:
            gotn_obs_table_name = '%s%s' % (gotn_obs_table_name, self.table_postfix)
        return gotn_obs_table_name
    
    def is_qc_disabled(self):
        return self.row[5]
        
    #def get_obs_field_unit(self):
    
    def set_table_name_postfix(self, table_postfix):
        self.table_postfix = table_postfix
    
    def toString(self):
        return "%d -> %d, %s.%s" % (self.get_id(), self.get_qc_field_id(),
                self.get_obs_table_name(), self.get_obs_column_name())
  
class Table_QcFieldTableMap(AbstractMapTable):
    debug = False
    #debug = True
    
    TABLE_NAME      = "qc_field_table_map"                # M
    JOIN_TABLE_NAME = Table_QcFieldStaticData.TABLE_NAME  # J
    
    JOINED_COLUMNS = "M.id,M.obs_field_name,qc_field_id,J.field_name,M.table_name,J.disable_qc"
    
    table_map = None
    table_map_by_id  = None
    map_by_qc_id = {}
    
    #qc_field_static_data
    #SELECT F.id,F.obs_field_name name,qc_field_id,min_value,max_value FROM qc_field_table_map F JOIN qc_field_static_data S ON S.id=F.qc_field_id WHERE disable_qc=0
    SQL_FIELD_MIN_MAX_TEMPLATE = 'SELECT F.id,F.obs_field_name name,qc_field_id,min_value, max_value FROM qc_field_table_map F JOIN %s S ON S.id=F.qc_field_id WHERE disable_qc=0'
    SQL_FIELD_MIN_MAX_COLUMN = 'F.id,F.obs_field_name name,qc_field_id,min_value,max_value'
    
    staticMinMaxMap = None
#    climaticMinMaxMap = None
    
    
    def __init__(self, db_actor):
        super(self.__class__, self).__init__(db_actor)
        #self.set_call_stack(True)
        if not Table_QcFieldTableMap.table_map:
            self.initialize()
        self.initialize_min_max_map()

    def initialize_min_max_map(self):
        debug = False
        #debug = not debug
        method_name = '%s.%s' % (MODULE_NAME, 'initialize_min_max_map')
        if not Table_QcFieldTableMap.staticMinMaxMap:
            table_map       = {}
            table_map_by_id = {}
            'qc_field_table_map F JOIN %s S ON S.id=F.qc_field_id'
            sql_where = 'disable_qc=0'
            table_name = '%s F JOIN %s S ON F.qc_field_id=S.id' % (self.get_table_name(), 'qc_field_static_data')
            if debug: print('  %s SELECT %s FROM %s WHERE %s' % (method_name,
                    Table_QcFieldTableMap.SQL_FIELD_MIN_MAX_COLUMN, table_name, sql_where))
            rows = self.db_actor.get_records(table_name,
                    Table_QcFieldTableMap.SQL_FIELD_MIN_MAX_COLUMN, sql_where)
            for row in rows:
                if debug:
                    debug_msg = "  %s  row: " % method_name, row
                    self.debug_message(debug_msg)
                min_max_data = ObsMinMaxData(row)
                key = self.make_key(min_max_data)
                table_map[key] = min_max_data
                table_map_by_id[make_key_by_id(min_max_data)] = min_max_data
            
            Table_QcFieldTableMap.staticMinMaxMap = table_map
            Table_QcFieldTableMap.table_map_by_id = table_map_by_id

    def build_map_postprocess(self, record):
        qc_variable_id = record.get_qc_field_id()
        obs_ids = Table_QcFieldTableMap.map_by_qc_id.get(
                qc_variable_id)
        if obs_ids is None: obs_ids = []
        obs_ids.append(record.get_id())
        Table_QcFieldTableMap.map_by_qc_id[qc_variable_id] = obs_ids
    
    #@abstractmethod  
    def get_record_from_row(self, row):
        #record_data = QcFieldStaticData(row)
        #return record_data
        return QcFieldTableMap(row)

    def get_rows_for_map(self):
        table_name = self.get_table_name_with_alias()
        join_tbl_name = "%s J" % Table_QcFieldTableMap.JOIN_TABLE_NAME
        join_on = "J.id=M.qc_field_id"
        sql_columns = Table_QcFieldTableMap.JOINED_COLUMNS
        combined_table_name = "%s JOIN %s ON %s" % (table_name, join_tbl_name, join_on)
        #SELECT M.id,M.obs_field_name,qc_field_id,J.field_name FROM qc_field_map M JOIN qc_field_info J on J.id=M.qc_field_id;
        sql_where = "enabled>0"

        return self.db_actor.get_records(combined_table_name, sql_columns, sql_where)


    def get_map(self):
        return Table_QcFieldTableMap.table_map
    
    def set_map(self, table_map):
        Table_QcFieldTableMap.table_map = table_map
  
    def get_table_name(self):
        return Table_QcFieldTableMap.TABLE_NAME
    
    def make_key(self, record):
        return make_key_by_name(record)

def test():
    from DbActor  import create_db_actor
    from DbConfig import create_db_config
    
    #debug = True
    method_name = '%s@%s' % (MODULE_NAME, 'test()') 
    db_name = 'mir'
    _db_config = create_db_config()
    _db_actor = create_db_actor(_db_config, db_name)
    
    tmp_map = build_min_max_map(_db_actor)
    for a_map in tmp_map.values():
        print('  %s %s' % (method_name, a_map.to_string()))

if __name__ == "__main__":
    test()