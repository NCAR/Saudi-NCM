#!/usr/bin/env python

from abc import ABCMeta, abstractmethod

try:
    from AbstractRecord import Record_Id
    from AbstractTable import AbstractTable
except ImportError:
    from atec.dao.AbstractRecord import Record_Id
    from atec.dao.AbstractTable import AbstractTable
    
MODULE_NAME = 'AbstractMapTable'

class AbstractMapTable(AbstractTable):
    __metaclass__ = ABCMeta
    
    def __init__(self, db_actor):
        super(AbstractMapTable, self).__init__(db_actor)
        #self.initialized = False
        # Derived calss should call 'self.initialize()'
        self.map_by_key = None
        self.map_by_id = None
        self.initialize()
        
    def initialize(self):
        self.map_by_key = self.build_map()
        self.set_map(self.map_by_key)
    
    def build_map(self):
        debug = False
        #debug = True
        method_name = '%s.%s' % (MODULE_NAME, 'build_map')
        map_by_key = {}
        map_by_id = {}
        rows = self.get_rows_for_map()
        for row in rows:
            record = self.get_record_from_row(row)
            key = self.make_key(record)
            map_by_key[key] = record
            if type(record) is Record_Id:
                map_by_id[record.get_id()] = record
            if debug: self.debug_message('%s key: %s, %r' % (method_name, key, record))
            self.build_map_postprocess(record)
        
        if 0 < len(map_by_id):  self.map_by_id = map_by_id
        return map_by_key
    
    def build_map_postprocess(self, record ):
        pass
    
    @abstractmethod  
    def get_record_from_row(self, row):
        pass
    
    
    def get_rows_for_map(self):
        return self.get_records()
        
    @abstractmethod  
    def make_key(self, record):
        pass

    def get_map(self):
        return self.get_map_by_key()
    
    def get_map_by_key(self):
        return self.map_by_key
    
    def set_map(self, a_map):
        pass
