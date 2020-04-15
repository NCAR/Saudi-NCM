#!/usr/bin/env python

try:
    from AbstractMapTable import AbstractMapTable
    from AbstractRecord import Record_Id
except ImportError:
    from atec.dao.AbstractMapTable import AbstractMapTable
    from atec.dao.AbstractRecord import Record_Id

MODULE_NAME = 'Table_QcFieldClimaticData'

def build_map(db_actor):
  debug = False
  #debug = True
  
  fname = '%s:%s' % (MODULE_NAME,'build_map')
  if debug:    print("  > {f} is called".format(f=fname))
  
  if not Table_QcFieldClimaticData.table_map:
    if debug:    print("  > {f} Getting qc_field_climatic_data_map".format(f=fname))
    #qc_field_info_table = Table_QcFieldClimaticData(db_actor)
    Table_QcFieldClimaticData(db_actor)
  return Table_QcFieldClimaticData.table_map;
  
def make_key(range_id, field_id, month):
  return '%d_%d_%d' % (range_id, field_id, month)

def make_key_2(record):
  return make_key(record.get_range_id(), record.get_field_id(), record.get_month())

class QcFieldClimaticData(Record_Id):
  #def __init__(self, row):
  #  self.row = row

  def get_table_name(self):
    return Table_QcFieldClimaticData.TABLE_NAME
  
  def get_range_id(self):
    return self.row[1]
    
  def get_field_id(self):
    return self.row[2]
  
  def get_month(self):
    return self.row[3]
  
  def get_min_value(self):
    return self.row[4]
  
  def get_max_value(self):
    return self.row[5]

  def toString(self):
    return "%d domain: %d field_id %d, month:%d min:%f, max:%f" % (
        self.get_id(), self.get_range_id(), self.get_field_id(),
        self.get_month(), self.get_min_value(), self.get_max_value())
    

class Table_QcFieldClimaticData(AbstractMapTable):
  debug = False
  #debug = True
  
  TABLE_NAME      = "qc_field_climatic_data"
  COLUMNS         = "id,range_id,qc_field_id,month,min_value,max_value"
  
  table_map = None

  def __init__(self, db_actor):
    super(self.__class__, self).__init__(db_actor)
    #self.set_call_stack(True)
    if not Table_QcFieldClimaticData.table_map:
      self.initialize()
  
  def get_map(self):
    return Table_QcFieldClimaticData.table_map
  
  def set_map(self, table_map):
    Table_QcFieldClimaticData.table_map = table_map
  
  #@abstractmethod  
  def get_record_from_row(self, row):
        return QcFieldClimaticData(row)

  def get_rows_for_map(self):
        return self.get_records(Table_QcFieldClimaticData.COLUMNS)

  #def get_record_list(self):
  #  fname = '%s.%s' % (MODULE_NAME, 'get_record_list')
  #  debug = Table_QcFieldClimaticData.debug
  #  record_list = []
  #  
  #  if debug:   self.debug_message("  %s  table: %s columns: %s" % (fname, self.get_table_name_with_alias(), Table_QcFieldClimaticData.COLUMNS))
  #  rows = self.get_records(Table_QcFieldClimaticData.COLUMNS)
  #  if debug:
  #    debug_msg = "  %s  rows: " % fname, rows
  #    self.debug_message(debug_msg)
  #  for row in rows:
  #    if debug:
  #      debug_msg = "  %s  row: " % fname, row
  #      self.debug_message(debug_msg)
  #    
  #    field_data = QcFieldClimaticData(row)
  #    record_list.append(field_data)
  #  if debug:   self.debug_message("  %s   found %d field list" % (fname, len(record_list)))
  #  
  #  return record_list
  
  def get_table_name(self):
    return Table_QcFieldClimaticData.TABLE_NAME

  def make_key(self, record):
    return make_key_2(record)
  