#!/usr/bin/env python

try:
    from AbstractMapTable import AbstractMapTable
    from AbstractRecord import Record_Id_Name
except ImportError:
    from atec.dao.AbstractMapTable import AbstractMapTable
    from atec.dao.AbstractRecord import Record_Id_Name

MODULE_NAME = 'Table_QcFieldStaticData'

def build_map(db_actor):
  gfl_debug = False
  #gfl_debug = True
  
  gfl_fname = '%s:%s' % (MODULE_NAME,'build_map')
  if gfl_debug:    print("  > %s is called" % gfl_fname)
  
  if not Table_QcFieldStaticData.map:
    if gfl_debug:    print("  > %s, Getting qc_field_min_max_data_map" % gfl_fname)
    Table_QcFieldStaticData(db_actor)
  return Table_QcFieldStaticData.map;
  
def make_key(field_id):
  return str(field_id)

def make_key_2(record):
  return make_key(record.get_id())

class QcFieldStaticData(Record_Id_Name):
  #def __init__(self, row):
  #  self.row = row

  def get_table_name(self):
    return Table_QcFieldStaticData.TABLE_NAME
  
  def get_field_name(self):
    return self.get_name()
    
  def get_full_name(self):
    return self.row[2]
  
  def get_min_value(self):
    return self.row[3]
  
  def get_max_value(self):
    return self.row[4]
    
  def get_unit(self):
    return self.row[5]
  
  def is_disabled_qc(self):
    return self.row[6]
  
  def get_comments(self):
    return self.row[7]

class Table_QcFieldStaticData(AbstractMapTable):
    debug = False
  #debug = True
  
    TABLE_NAME      = "qc_field_static_data"
    COLUMNS         = "id,field_name,full_name,min_value,max_value,unit,disable_qc,comments"

    map = None

    def __init__(self, db_actor):
        super(self.__class__, self).__init__(db_actor)
        #self.set_call_stack(True)
        if not Table_QcFieldStaticData.map:
            self.initialize()

    #@staticmethod      
    #def get_map():
    #  return Table_QcFieldStaticData.map
  
    def get_map(self):
        return Table_QcFieldStaticData.map
  
    def set_map(self, a_map):
        Table_QcFieldStaticData.map = a_map
  
    #@abstractmethod  
    def get_record_from_row(self, row):
        return QcFieldStaticData(row)

    def get_rows_for_map(self):
        return self.get_records(Table_QcFieldStaticData.COLUMNS)

    
    #def get_record_list(self):
    #    grl_debug = False
    #    #grl_debug = True
    #    grl_fname = '%s.%s' % (MODULE_NAME, 'get_record_list')
    #    grl_record_list = []
    #
    #    if grl_debug:   self.debug_message("  %s  table: %s columns: %s" % (grl_fname, self.get_table_name_with_alias(), Table_QcFieldStaticData.COLUMNS))
    #    grl_rows = self.get_records(Table_QcFieldStaticData.COLUMNS)
    #    if grl_debug:
    #        grl_debug_msg = "  %s  rows: " % grl_fname, grl_rows
    #        self.debug_message(grl_debug_msg)
    #    for grl_row in grl_rows:
    #        if grl_debug:
    #            grl_debug_msg = "  %s  row: " % grl_fname, grl_row
    #            self.debug_message(grl_debug_msg)
    #        grl_record_list.append(QcFieldStaticData(grl_row))
    #
    #    if grl_debug:   self.debug_message("  %s   found %d field list" % (grl_fname, len(grl_record_list)))
    #
    #    return grl_record_list
  
    def get_table_name(self):
        return Table_QcFieldStaticData.TABLE_NAME

    def make_key(self, record):
        return make_key_2(record)
