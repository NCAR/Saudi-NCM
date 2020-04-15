'''
Created on Jan 16, 2015

@author: hsoh
'''
from atec.obs2nc.BaseConfig import BaseConfig

class ConfigReader(BaseConfig):
#class ConfigReader():
    '''
    classdocs
    '''

    #def __init__(self, params):
    #    '''
    #    Constructor
    #    '''
        
    #def extract_value(self, line):
    #    (param, value) = super(ConfigReader, self).extract_value(line)
    #    #return (param, value)
    #    value = value.split('/')
    #    return (param, value)

    def get_report_type(self, default_value):
        value = self.get_value('report_type')
        if None != value:
            value = int(value)
        else:
            value = default_value
        return value


