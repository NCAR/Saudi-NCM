#!/usr/bin/env python


try:
    from AbstractRecord import Record_Id_Name
    from AbstractMapTable import AbstractMapTable
except ImportError:
    from atec.dao.AbstractRecord import Record_Id_Name
    from atec.dao.AbstractMapTable import AbstractMapTable

MODULE_NAME = 'Table_QcRange'

class Record_QcRange(Record_Id_Name):
    debug = False
    #debug = True
  
    def get_description(self):
        return self.row[2]
  
    def get_table_name(self):
        return Table_QcRange.TABLE_NAME

class Table_QcRange(AbstractMapTable):
    debug = False
    #debug = True
    
    range_map = None
  
    TABLE_NAME             = "qc_range"

    def get_table_name(self):
        return Table_QcRange.TABLE_NAME
  
    #@abstractmethod  
    def make_key(self, record):
        #method_name = '%s.%s' % (MODULE_NAME, 'make_key')
        a_key = '%s' % record.get_name()
        #print ('   %s is called, key: [%s]' % (method_name, a_key))
        return a_key

    #@abstractmethod  
    def get_record_from_row(self, row):
        return Record_QcRange(row)

    def get_map(self):
        return Table_QcRange.range_map
  
    
    #def get_record_list(self):
    #    fname = '%s.%s' % (MODULE_NAME, 'get_record_list')
    #    debug = Table_QcRange.debug
    #    record_list = [] 
    #    if debug:   self.debug_message("  %s  table: %s columns: *" % (
    #            fname, self.get_table_name_with_alias()))
    #    rows = self.get_records()
    #    if debug:
    #        debug_msg = "  %s  rows: " % fname, rows
    #        self.debug_message(debug_msg)
    #    for row in rows:
    #        if debug:
    #            debug_msg = "  %s  row: " % fname, row
    #            self.debug_message(debug_msg)
    #  
    #        record_data = Record_QcRange(row)
    #        record_list.append(record_data)
    #    if debug:   self.debug_message("  %s   found %d field list" % (fname, len(record_list)))
    #
    #    return record_list
  
    #@abstractmethod  
    def set_map(self, a_map):
        Table_QcRange.range_map = a_map
