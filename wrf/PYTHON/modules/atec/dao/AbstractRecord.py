#!/usr/bin/env python

from abc import ABCMeta, abstractmethod

try:
    from AbstractBaseDB import AbstractBaseDB
except ImportError:
    from atec.dao.AbstractBaseDB import AbstractBaseDB

class AbstractRecord(AbstractBaseDB):
    __metaclass__ = ABCMeta

    def __init__(self, row):
        AbstractBaseDB.__init__(self)
        self.row = list(row)
        self.table_object = None
    
    @abstractmethod
    def get_table_name(self):
        pass
  
    def get_table_object(self):
        return self.table_object

    def set_table_object(self, table_object):
        self.table_object = table_object
    
class Record_Id(AbstractRecord):

    def get_id(self):
        return self.row[0]
  
class Record_Name(AbstractRecord):

    def get_name(self):
        return self.row[0]
  
class Record_Id_Name(Record_Id):
    def __init__(self, row):
        self.row = row

    def get_name(self):
        return self.row[1]
  
