#!/usr/bin/env python

import sys

#from AbstractTable import AbstractTable
try:
    from BaseObject import BaseObject, debug_message
    #except ModuleNotFoundError:
except ImportError:
    from atec.dao.BaseObject import BaseObject, debug_message
#from Properties.Properties import Properties
#from Properties import Properties
from Table_QcFieldClimaticData import build_map as build_field_climatic_data_map
from Table_QcFieldTableMap import build_map as build_map_field_table_map
from Table_QcFieldTableMap import build_min_max_map as build_field_static_min_max_map
from Table_QcRange import Table_QcRange

DO_RANGE_OBS_QC = True
try:
    from Table_QcFieldStaticData import build_map as build_field_min_max_data_map
except:
    DO_RANGE_OBS_QC = False
    pass

MODULE_NAME = 'MapHandler'

#def get_climatic_data_map():  
#  return MapHandler.climatic_data_map
  
#def get_range_map():
#  return MapHandler.range_map

#def get_field_table_map():
#  return MapHandler.qc_field_table_map

def get_obs_table_name_by_column(obs_column_name, postfix):
    table_name = None
    if MapHandler.qc_field_table_map is not None:
        qc_field_table_map = MapHandler.qc_field_table_map
        table_name = '%s%s' % (
                qc_field_table_map.get(obs_column_name).get_obs_table_name(),
                postfix)
    else:
        print("%s@%s Can not get the table name from column name %s" % \
                (MODULE_NAME, 'get_obs_table_name_by_column',obs_column_name))
    return table_name
  
def get_singleton(db_actor):
    debug = False
    #debug = True
    fname = '%s:%s' % (MODULE_NAME, 'get_singleton')
    if debug:  debug_message("%s is called " % fname)
    if not MapHandler.singleton:
        MapHandler.singleton = MapHandler(db_actor)
    return MapHandler.singleton

#def get_qc_field_map_by_id():
#    return MapHandler.qc_field_map_by_id

#def get_key_4_min_max_data_map():
#    return MapHandler.min_max_data_map

def get_min_max_data_map():  
    return MapHandler.min_max_data_map
  
  
#class MapHandler(AbstractTable):
class MapHandler(BaseObject):

    debug = True
    singleton = None
    
    qc_field_table_map       = None
    qc_field_table_map_by_id = None
    #qc_field_map_by_id = None
    climatic_data_map = None
    min_max_data_map = None
    static_min_max_map = None
    range_map = None
    #temporal_threshold_1min_map = None
    #temporal_threshold_5min_map = None
    #temporal_threshold_15min_map = None


    def __init__(self, db_actor):
        #i_fname = 'MapHandler.__init__'
        #print('   %s is called, ' % (i_fname), MapHandler.DB_CONFIG_FILE_NAME)
        #if not MapHandler.DB_PROPERTIES:
        #  #print('   %s: MapHandler.DB_PROPERTIES does not exists' % (i_fname))
        #  i_config_name = get_config_name()
        #  if os.path.isfile(i_config_name):
        #    MapHandler.DB_PROPERTIES = get_properties(i_config_name)
        #  else:
        #    print('   %s: config file [%s] does not exist' % (i_fname, DB_CONFIG_FILE_NAME))
        super(self.__class__, self).__init__()
        self.db_actor = db_actor
        #self.call_stack = True
        self.initialize_map_and_list()
    
    def initialize_map_and_list(self):
        debug = False
        #debug = True
        fname = '%s.%s' % (MODULE_NAME, 'initialize_map_and_list')
        self.debug_print_called(fname)
        if MapHandler.qc_field_table_map is None:
            (tmp_map, tmp_map_by_id)= build_map_field_table_map(self.db_actor)
            MapHandler.qc_field_table_map = tmp_map
            MapHandler.qc_field_table_map_by_id = tmp_map_by_id
            if debug:
                #debug_msg = "%s map: " % (fname), tmp_map
                #self.debug_message(debug_msg)
                for key in tmp_map.keys():
                    debug_msg = "%s key: %s value: %s" % (fname,key, tmp_map.get(key).toString())
                    self.debug_message(debug_msg)
        min_max_data_map = MapHandler.min_max_data_map
        if DO_RANGE_OBS_QC and min_max_data_map is None:
            tmp_map = build_field_min_max_data_map(self.db_actor)
            MapHandler.min_max_data_map = tmp_map
            if debug:
                #debug_msg = "%s field: " % (fname) , tmp_map
                #self.debug_message(debug_msg)
                for key in tmp_map.keys():
                    data = tmp_map.get(key, None)
                    #self.debug_message('%s min_max_data_map %s: %r' % (fname, key, data))
                    if data:
                        debug_msg = "%s min_max_data_map %s -> field: %s, min: %f, max: %f" % (
                                fname, key, data.get_field_name(),
                                data.get_min_value(), data.get_max_value())
                        self.debug_message(debug_msg)
              
        if MapHandler.static_min_max_map is None:
            MapHandler.static_min_max_map = build_field_static_min_max_map(self.db_actor)
        
        if MapHandler.climatic_data_map is None:
            tmp_map = build_field_climatic_data_map(self.db_actor)
            MapHandler.climatic_data_map = tmp_map
            if debug:
                #debug_msg = "%s field: " % (fname), tmp_map
                #self.debug_message(debug_msg)
                for key in tmp_map.keys():
                    data = tmp_map.get(key, None)
                    if data:
                        static_data = min_max_data_map.get(str(data.get_field_id()), None)
                        if static_data is not None:
                            field_name = static_data.get_field_name()
                        else:
                            field_name = "N/A"
                        debug_msg = "%s climatic_data_map domain: %d field: %s, month: %d, min: %f, max %f" % (
                                fname, data.get_range_id(), field_name,
                                data.get_month(), data.get_min_value(),
                                data.get_max_value())
                        self.debug_message(debug_msg)
        
        if MapHandler.range_map is None:
            tmp_map = Table_QcRange(self.db_actor).get_map()
            MapHandler.range_map = tmp_map
            if debug:
                #debug_msg = "%s field: " % (fname), tmp_map
                #self.debug_message(debug_msg)
                for key in tmp_map.keys():
                    data = tmp_map.get(key, None)
                    if data:
                        static_data = min_max_data_map.get(str(data.get_field_id()), None)
                        if static_data:
                            field_name = static_data.get_field_name()
                        else:
                            field_name = "N/A"
                        debug_msg = "%s climatic_data_map domain: %d field: %s, month: %d, min: %f, max %f" % (
                                fname, data.get_range_id(), field_name,
                                data.get_month(), data.get_min_value(),
                                data.get_max_value())
                        self.debug_message(debug_msg)
        
        #if not MapHandler.temporal_threshold_1min_map:
        #  MapHandler.temporal_threshold_1min_map = get_temporal_threshold_map_1min(self.db_actor)
        #if not MapHandler.temporal_threshold_5min_map:
        #  MapHandler.temporal_threshold_5min_map = get_temporal_threshold_map_5min(self.db_actor)
        #  if debug:
        #    tmp_map = MapHandler.temporal_threshold_5min_map
        #    for key in tmp_map.keys():
        #      data = tmp_map.get(key, None)
        #      if data:
        #        static_data = min_max_data_map.get(str(data.get_qc_field_id()), None)
        #        if static_data:
        #          field_name = static_data.get_field_name()
        #        else:
        #          field_name = "N/A"
        #        debug_msg = "%s temporal_threshold_5min_map domain: %d field: %s (%d), threshold: %f, threshold_max: %f" % (
        #            fname, data.get_range_id(), field_name,
        #            data.get_qc_field_id(),
        #            data.get_threshold(), data.get_threshold_max())
        #        self.debug_message(debug_msg)
        #if not MapHandler.temporal_threshold_15min_map:
        #  MapHandler.temporal_threshold_15min_map = get_temporal_threshold_map_15min(self.db_actor)
        
        self.initialize_map_and_list_extra()
    
    def initialize_map_and_list_extra(self):
        #debug = False
        #debug = True
        #fname = '%s.%s' % (MODULE_NAME, 'initialize_map_and_list_extra')
        #self.debug_print_called(fname)
        pass
    
    def get_climatic_data_map(self):
        return MapHandler.climatic_data_map
    
    #def get_range_map():
    #    return MapHandler.range_map
    def get_column_name_by_id(self, variable_id):
        column_name = None
        field_data = MapHandler.qc_field_table_map_by_id.get(variable_id)
        if field_data is not None:
            column_name = field_data.get_obs_column_name()
        return column_name
    
    def get_field_data_by_variable_id(self, variable_id):
        return MapHandler.qc_field_table_map_by_id.get(variable_id)
    
    def get_field_id_list(self):
        debug = False
        debug = not debug
        field_id_list = []
        qc_field_table_map = self.get_field_table_map()
        for qc_field_table_data in qc_field_table_map.values():
            if qc_field_table_data:
                field_id_list.append(qc_field_table_data.get_id())
        
        self.set_start_time()
        if field_id_list:  field_id_list.sort()
        return field_id_list
    
    def get_field_min_max_map(self):
        return MapHandler.static_min_max_map
    
    def get_field_table_data_values(self):
        return MapHandler.qc_field_table_map.values()
    
    def get_field_table_map(self):
        return MapHandler.qc_field_table_map
    
    def get_min_max_data_map(self):  
        return MapHandler.min_max_data_map
    
    def get_obs_table_name_by_column(self, obs_column_name):
        from Table_SurfaceData import Table_SurfaceData
        
        table_name = None
        if MapHandler.qc_field_table_map:
            qc_field_table_map = MapHandler.qc_field_table_map
            table_name = '%s%s' % (
                    qc_field_table_map.get(obs_column_name).get_obs_table_name(),
                    Table_SurfaceData.POSTFIX)
        else:
            print("%s@%s Can not get the table name from column name %s" % (
                    MODULE_NAME, 'get_obs_table_name_by_column',obs_column_name))
        return table_name
    
    def get_qc_field_id_by_id(self, variable_id):
        qc_field_id = None
        field_data = MapHandler.qc_field_table_map_by_id.get(variable_id)
        if field_data is not None:
            qc_field_id = field_data.get_obs_column_name()
        return qc_field_id
    
    def get_range_map(self):
        return MapHandler.range_map
    
    #def get_temporal_threshold(self, interval_in_sec, range_id, qc_field_id):
    #  gtt_map = self.get_temporal_threshold_map(interval_in_sec)
    #  key_map = {}
    #  key_map["range_id"] = range_id
    #  key_map["qc_field_id"] = qc_field_id
    #  return gtt_map.get_by_key_map(key_map)
    
    #def get_temporal_threshold_map(self, interval_in_sec):
    #  gttm_map = None
    #  if interval_in_sec <= 60:
    #    gttm_map = MapHandler.temporal_threshold_1min_map
    #  elif interval_in_sec <= 300:
    #    gttm_map = MapHandler.temporal_threshold_5min_map
    #  elif interval_in_sec <= 900:
    #    gttm_map = MapHandler.temporal_threshold_15min_map
    #  return gttm_map
    
    #def get_temporal_threshold_map_1min(self):
    #  return MapHandler.temporal_threshold_1min_map
    
    #def get_temporal_threshold_map_5min(self):
    #  return MapHandler.temporal_threshold_5min_map
    
    #def get_temporal_threshold_map_15min(self):
    #  return MapHandler.temporal_threshold_15min_map

def main(argv):
    pass

if __name__ == "__main__":
    main(sys.argv)
