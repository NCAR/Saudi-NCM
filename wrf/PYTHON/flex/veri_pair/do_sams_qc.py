#!/usr/bin/env python

#*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*
# * Copyright (c) 2018 UCAR
# * University Corporation for Atmospheric Research (UCAR)
# * National Center for Atmospheric Research (NCAR)
# * Research Applications Laboratory (RAL)
# * P.O. Box 3000, Boulder, Colorado, 80307-3000 USA
# * All rights reserved. Licenced use only.
# * Do not copy or distribute without authorization.
# *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*
#
############################################################

'''
Created on March 16, 2017
'''

from optparse import OptionParser
#import os
import sys

from atec.util.DateUtil import LogTime
try:
    from Table_SurfaceObsData import Table_SurfaceObsData
except ImportError:
    from atec.veri_pair.Table_SurfaceObsData import Table_SurfaceObsData
from sams_qc import sams_qc, sams_qc_tools

MODULE_NAME = 'sams_qc'
MODULE_VERSION = '1.0.0.0001'

def create_parser():
    usage_str = "%prog [options] "
    parser = OptionParser(usage = usage_str, version="%prog " + MODULE_VERSION)
    parser.add_option('-r', '--range', dest='range',            default='', help=" Range name - required")
    parser.add_option('-d', '--debug', dest='debug', action='store_true', default=False,
            help=" Enable debug - optional")
    parser.add_option('-D', '--debug_sql', dest='debug_sql', action='store_true', default=False,
            help=" Enable debug for SQL - optional")
    parser.add_option('-t', '--test',  dest='opt_test', action='store_true', default=False,
            help=" Enable opt_test - optional")
    return parser

class do_sams_qc(sams_qc):

    #SCORE_MAX            =    100
    #SCORE_MISSING        =  -1000   # -2
    #SCORE_NOT_IN_RANGE   =  -2000   # -3
    #SCORE_NOT_IN_RANGE_C =  -4000   # -4
    MISSING_VALUE = -9999.
    UPDATE_COUNT_FOR_COMMIT = 10000

    def __init__(self, range_name):
        
        super(do_sams_qc, self).__init__(range_name)
        
    def do_sams_qc(self):
        method_name = '%s.%s()' % (MODULE_NAME, 'do_sams_qc')
        debug = False
        
        count_updated     = 0
        count_not_updated = 0
        skip_record_count = 0
        processed_record_count = 0

        dao_table = Table_SurfaceObsData(self.db_actor)
        dao_entities = dao_table.load_entity_map_not_QCed()
        self.dao_entities = dao_entities
        
        prev_climo_month = -1
        range_id = None
        
        for station_id in dao_entities.keys():
            station_dao_entities =  dao_entities.get(station_id, None)
            print('   Processing station_id {s} for {c} records'.format(s=station_id, c=len(station_dao_entities)))
            if station_dao_entities is not None:
                if range_id is None:
                    range_id = sams_qc_tools.get_range_id_from_site_id(
                            station_id, self.map_handler.get_range_map())
                    time_interval = self.get_time_interval(sorted(station_dao_entities.keys()))
                    self.time_interval = time_interval
                    self.setup_qc_data_temporal(range_id, time_interval)

                p_time = None
                p_temp = self.MISSING_VALUE
                p_pres = self.MISSING_VALUE
                p_wspd = self.MISSING_VALUE
                p_wdir = self.MISSING_VALUE
                for obs_time_key in sorted(station_dao_entities.keys()):
                    obs_data_entity = station_dao_entities.get(obs_time_key, None)
                    if obs_data_entity is None:
                        continue
                    
                    if debug or self.options.debug:
                        if station_id != obs_data_entity.get_unique_site_id():
                            print('   station_id: {si1}, obs_data_entity: {si2}'.format(
                                si1= station_id, si2=obs_data_entity.get_unique_site_id()))
                        print('   obs_data_entity: {o}, temp: {t} qc: {tqc}'.format(
                                o=obs_data_entity.get_obs_time(), 
                                t=obs_data_entity.get_temp(), tqc=obs_data_entity.get_temp_qc2()))
                    
                    climo_month = obs_data_entity.get_obs_time().month
                    if climo_month != prev_climo_month:
                        print('{n} climatic manth was changed to {c} from {o} '.format(
                            n=method_name, c=climo_month, o=obs_data_entity.get_obs_time()))
                        self.setup_qc_data_climatic(range_id, climo_month)
                        prev_climo_month = climo_month

                    missing_var_count = 0
                    temp = obs_data_entity.get_temp()
                    pres = obs_data_entity.get_pres()
                    wspd = obs_data_entity.get_wspd()
                    wdir = obs_data_entity.get_wdir()
                    if obs_data_entity.get_temp_qc2() == sams_qc.SCORE_NOT_DONE \
                            or obs_data_entity.get_pres_qc2() == sams_qc.SCORE_NOT_DONE \
                            or obs_data_entity.get_wspd_qc2() == sams_qc.SCORE_NOT_DONE \
                            or obs_data_entity.get_wdir_qc2() == sams_qc.SCORE_NOT_DONE:
                        if temp is self.MISSING_VALUE:
                            temp_qc = sams_qc.SCORE_MISSING
                            missing_var_count += 1
                        if pres is self.MISSING_VALUE:
                            pres_qc = sams_qc.SCORE_MISSING
                            missing_var_count += 1
                        if wspd is self.MISSING_VALUE:
                            wspd_qc = sams_qc.SCORE_MISSING
                            missing_var_count += 1
                        if wdir is self.MISSING_VALUE:
                            wdir_qc = sams_qc.SCORE_MISSING
                            missing_var_count += 1
                        
                        if missing_var_count < 4:
                            if p_time is None:
                                time_delta = 0
                            else:
                                time_delta = obs_time_key - p_time
                            (temp_qc,pres_qc,wspd_qc,wdir_qc) = self.do_QC(
                                    time_delta,temp,pres,wspd,wdir,p_temp,p_pres,p_wspd,p_wdir, self.MISSING_VALUE)
                            self.debug_message("temp: {t}({tq}), pres: {p}({pq}), wspd: {ws}({wsq}), wdir: {wd}({wdq})".format(
                                   t=temp,tq=temp_qc,p=pres,pq=pres_qc,ws=wspd,wsq=wspd_qc,wd=wdir,wdq=wdir_qc), debug)
                        if time_delta > self.time_interval:
                        #if time_delta > sams_qc.TEMPORAL_TIME_DIFF_MAX:
                            if obs_data_entity.get_temp_qc2() <= 0: obs_data_entity.set_temp_qc2(temp_qc);
                            if obs_data_entity.get_pres_qc2() <= 0: obs_data_entity.set_pres_qc2(pres_qc);
                            if obs_data_entity.get_wspd_qc2() <= 0: obs_data_entity.set_wspd_qc2(wspd_qc);
                            if obs_data_entity.get_wdir_qc2() <= 0: obs_data_entity.set_wdir_qc2(wdir_qc);
                        else:
                            obs_data_entity.set_temp_qc2(temp_qc);
                            obs_data_entity.set_pres_qc2(pres_qc);
                            obs_data_entity.set_wspd_qc2(wspd_qc);
                            obs_data_entity.set_wdir_qc2(wdir_qc);
                        if obs_data_entity.is_modified():
                            update_sql_string = obs_data_entity.update_sql()
                            count_updated += 1
                            self.debug_message(" {i} sql_string: {s}".format(
                                    i='update', s=update_sql_string), debug)
                            if update_sql_string is not None:
                                self.cursor.execute(update_sql_string)
                            else:
                                self.warn_message("Check why SQL string was not generated")
                        else:
                            count_not_updated += 1
                        processed_record_count += 1
                        if 0 == (count_updated % self.UPDATE_COUNT_FOR_COMMIT):
                            self.db_conn.commit()
                    
                    p_time = obs_time_key
                    p_temp = temp
                    p_pres = pres
                    p_wspd = wspd
                    p_wdir = wdir
                
                if 0 < processed_record_count:
                    self.db_conn.commit()
    
    #@abstractmethod
    def get_table_name(self):
        return do_sams_qc.TABLE_NAME
    
#     def get_time_interval(self, time_offset_list):
#         debug = False
#         debug = not debug
#         count_01min = 0
#         count_05min = 0
#         count_15min = 0
#         for t_idx in range(0, len(time_offset_list)-2):
#             time_diff = time_offset_list[t_idx+1] - time_offset_list[t_idx]
#             if time_diff <= 60:
#                 count_01min += 1
#             elif time_diff <= 300:
#                 count_05min += 1
#             else:
#                 count_15min += 1
#         if count_05min > count_15min:
#             time_interval = 300 if (count_05min > count_01min) else 60
#         else:
#             time_interval = 900 if (count_15min > count_01min) else 60
#         self.debug_message("Time interval: {t}".format(t=time_offset_list), debug)
#         return time_interval
        
        
def main(argv = None):
    sys.argv_back = None
    #if argv is not None:
    #    sys.argv_back = sys.argv
    #    sys.argv = []
    #    sys.argv.extend(argv)
    
    job_user = 'atecuser'
    option_parser = create_parser()
    (options, args) = option_parser.parse_args()
        
    
    actor = do_sams_qc(options.range)
    actor.options = options
    actor.args = args
    
    #print(' input_name = %s' % actor.options.input_name)
    actor.do_sams_qc()
        
    # Cleanup
    actor.cursor.close()
    actor.db_conn.close()
    #if argv is not None and sys.argv_back is not None:
    #    sys.argv = sys.argv_back

if __name__ == '__main__':
    log_time = LogTime()
    main()
    (duration_wall, duration_process) = log_time.get_durations()
    print ("      === Done {m}, took {w:.3} [{w1}], process time: {p:.3} [{p1}]".format(
            m=MODULE_NAME, w=duration_wall, w1=log_time.format(duration_wall),
            p=duration_process, p1=log_time.format(duration_process)))
    
