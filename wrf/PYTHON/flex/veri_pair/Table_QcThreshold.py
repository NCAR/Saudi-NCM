#!/usr/bin/env python

from abc import ABCMeta, abstractmethod

try:
    from AbstractMapTable import AbstractMapTable
    from AbstractRecord import Record_Id
except ImportError:
    from atec.dao.AbstractMapTable import AbstractMapTable
    from atec.dao.AbstractRecord import Record_Id

from Constants import Constants

MODULE_NAME = 'Table_QcThreshold'

def get_1min_map(db_actor):
  if not Table_QcTemporalThreshold_1Min.singleton:
    Table_QcTemporalThreshold_1Min.singleton = Table_QcTemporalThreshold_1Min(db_actor)
  return Table_QcTemporalThreshold_1Min.singleton

def get_5min_map(db_actor):
  if not Table_QcTemporalThreshold_5Min.singleton:
    Table_QcTemporalThreshold_5Min.singleton = Table_QcTemporalThreshold_5Min(db_actor)
  return Table_QcTemporalThreshold_5Min.singleton

def get_15min_map(db_actor):
  if not Table_QcTemporalThreshold_15Min.singleton:
    Table_QcTemporalThreshold_15Min.singleton = Table_QcTemporalThreshold_15Min(db_actor)
  return Table_QcTemporalThreshold_15Min.singleton

#def build_map_1min(db_actor):
#  debug = False
#  #debug = True
#  
#  fname = '%s:%s' % (MODULE_NAME,'build_map_1min')
#  if debug:    print "  > %s is called" % fname
#  
#  if not Table_QcTemporalThreshold_1Min.map:
#    if debug:    print "  > %s, Getting qc_temporal_diff_1min" % fname
#    Table_QcTemporalThreshold_1Min(db_actor)
#  return Table_QcTemporalThreshold_1Min.map;
#  
#def build_map_5min(db_actor):
#  debug = False
#  #debug = True
#  
#  fname = '%s:%s' % (MODULE_NAME,'build_map_5min')
#  if debug:    print "  > %s is called" % fname
#  
#  if not Table_QcTemporalThreshold_5Min.map:
#    if debug:    print "  > %s, Getting qc_temporal_diff_5min" % fname
#    Table_QcTemporalThreshold_5Min(db_actor)
#  return Table_QcTemporalThreshold_5Min.map;
#  
#def build_map_15min(db_actor):
#  debug = False
#  #debug = True
#  
#  fname = '%s:%s' % (MODULE_NAME,'build_map_15min')
#  if debug:    print "  > %s is called" % fname
#  
#  if not Table_QcTemporalThreshold_15Min.map:
#    if debug:    print "  > %s, Getting qc_temporal_diff_15min" % fname
#    Table_QcTemporalThreshold_15Min(db_actor)
#  return Table_QcTemporalThreshold_15Min.map;
  
def make_key(range_id, field_id):
  return '%d_%d' % (range_id, field_id)

def make_key_2(record):
  return make_key(record.get_range_id(), record.get_qc_field_id())

class QcThreshold(Record_Id):
  #def __init__(self, row):
  #  self.row = row

  def get_table_name(self):
    return 'Dummy_Not_Defined'
  
  def get_range_id(self):
    return self.row[1]
    
  def get_qc_field_id(self):
    return self.row[2]
  
  def get_threshold(self):
    return self.row[3]
  
  def get_threshold_max(self):
    return self.row[4]
  

class Table_QcThreshold(AbstractMapTable):
  __metaclass__ = ABCMeta
  
  debug = False
  #debug = True
  
  COLUMNS         = "id,range_id,qc_field_id,threshold,threshold_max"
  
  def __init__(self, db_actor):
    super(Table_QcThreshold, self).__init__(db_actor)
    #self.set_call_stack(True)
  
  def get(self, key, def_value=None):
    value = def_value
    map_by_key = self.get_map_by_key()
    if map_by_key is not None:
      value = map_by_key.get(key, def_value)
    return value
  
  def get_by_key_map(self, key_map, def_value=None):
    value = def_value
    map_by_key = self.get_map_by_key()
    if map_by_key:
      t_range_id = key_map.get("range_id", Constants.DEF_DOMAIN_ID)
      t_field_id = key_map.get("qc_field_id", -1)
      key = make_key(t_range_id, t_field_id)
      value = map_by_key.get(key, def_value)
    return value
  
  def keys(self):
    k_keys = None
    map_by_key = self.get_map_by_key()
    if map_by_key:
      k_keys = map_by_key.keys()
    return k_keys
  
  #@abstractmethod  
  def get_record_from_row(self, row):
        return QcThreshold(row)

  def get_rows_for_map(self):
        return self.get_records(Table_QcThreshold.COLUMNS)
    
  #def get_map(self):
  #  return self.map
  
  #def set_map(self, a_map):
  #  self.map = a_map
  
  #def get_record_list(self):
  #  fname = '%s.%s' % (MODULE_NAME, 'get_record_list')
  #  debug = Table_QcThreshold.debug
  #  record_list = [] 
  #  if debug:   self.debug_message("  %s  table: %s columns: %s" % (
  #      fname, self.get_table_name_with_alias(), Table_QcThreshold.COLUMNS))
  #  rows = self.get_records(Table_QcThreshold.COLUMNS)
  #  if debug:
  #    debug_msg = "  %s  rows: " % fname, rows
  #    self.debug_message(debug_msg)
  #  for row in rows:
  #    if debug:
  #      debug_msg = "  %s  row: " % fname, row
  #      self.debug_message(debug_msg)
  #    
  #    field_data = QcThreshold(row)
  #    record_list.append(field_data)
  #  if debug:   self.debug_message("  %s   found %d field list" % (fname, len(record_list)))
  #  
  #  return record_list
  
  @abstractmethod  
  def get_table_name(self):
    pass

  def make_key(self, record):
    return make_key_2(record)
   

class Table_QcTemporalThreshold_1Min(Table_QcThreshold):
  
  debug = False
  #debug = True
  singleton = None
  
  TABLE_NAME      = "qc_temporal_diff_1min"
  
  #def __init__(self, db_actor):
  #  super(self.__class__, self).__init__(db_actor)
  #  #self.set_call_stack(True)
  #  #if not Table_QcTemporalThreshold_1Min.map:
  #  #  self.initialize()
  
  def get_table_name(self):
    return Table_QcTemporalThreshold_1Min.TABLE_NAME

class Table_QcTemporalThreshold_5Min(Table_QcThreshold):
  
  debug = False
  #debug = True
  singleton = None
  #map = None
  
  TABLE_NAME      = "qc_temporal_diff_5min"
  
  #def __init__(self, db_actor):
  #  super(self.__class__, self).__init__(db_actor)
  #  #self.set_call_stack(True)
  #  #if not Table_QcTemporalThreshold_5Min.map:
  #  #  self.initialize()
  
  #def get_map(self):
  #  return Table_QcTemporalThreshold_5Min.map
  
  #def set_map(self, map):
  #  Table_QcTemporalThreshold_5Min.map = map
  
  def get_table_name(self):
    return Table_QcTemporalThreshold_5Min.TABLE_NAME

class Table_QcTemporalThreshold_15Min(Table_QcThreshold):
  
  debug = False
  #debug = True
  singleton = None
  #map = None
  
  TABLE_NAME      = "qc_temporal_diff_15min"
  
  #def __init__(self, db_actor):
  #  super(self.__class__, self).__init__(db_actor)
  #  #self.set_call_stack(True)
  #  #if not Table_QcTemporalThreshold_15Min.map:
  #  #  self.initialize()
  
  #def get_map(self):
  #  return Table_QcTemporalThreshold_15Min.map
  
  #def set_map(self, map):
  #  Table_QcTemporalThreshold_15Min.map = map
  
  def get_table_name(self):
    return Table_QcTemporalThreshold_15Min.TABLE_NAME


