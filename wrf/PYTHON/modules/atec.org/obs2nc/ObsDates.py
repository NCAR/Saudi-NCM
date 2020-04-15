'''
Created on Feb 4, 2015

@author: hsoh
'''

import os
from abc import ABCMeta, abstractmethod
from atec.util.DateUtil import DateUtil

OPT_DATES_FROM_DB = True
OPT_USE_MAX_TIME = False
OPT_USE_MAX_TIME = True

BAD_INDEX = -1
DEF_LEAD_TIMES = 24
MISSING_VALUE     = -9999
MIR_MISSING_VALUE = -999

MODULE_NAME = 'ObsDates'
debug = False

def get_db_records(cursor, sql_select):
    if debug:  print "SQL request ",sql_select+"\n"
    cursor.execute( sql_select )
    return cursor.fetchall()



class BaseDates(object):
    '''
    classdocs
    '''
    
    __metaclass__ = ABCMeta
    
    
    SQL_SELECT_MAX_CYCLE = "SELECT MAX({c}) FROM `{t}` WHERE {w}"
    SQL_SELECT_MIN_CYCLE = "SELECT MIN({c}) FROM `{t}` WHERE {w}"
    SQL_SELECT_TEMPLATE_MIN_MAX_DATE = "SELECT DATE(min(%s)),DATE(max(%s)) FROM `%s` WHERE %s BETWEEN %s"
    #SQL_SELECT_TEMPLATE = "SELECT DATE(min(%s)),DATE(max(%s)) FROM %s WHERE %s BETWEEN %s"
    #SQL_SELECT_TEMPLATE = "SELECT DATE(%s) FROM %s WHERE %s GROUP BY DATE(%s) ORDER BY 1"
    SQL_SELECT_TEMPLATE = "SELECT DISTINCT DATE({d}) FROM {t} WHERE {w} ORDER BY 1"
    
    COL_TIME_OBS = "obs_time"
    COL_TIME_MODEL = "cycle_time"
    
    #lead_times = DEF_LEAD_TIMES
    
    #SQL_ANALOG_BETWEEN = "BETWEEN DATE_ADD(now(), interval -1 YEAR) AND NOW()"
    #SQL_ANALOG_STRAT_DATE = "DATE_ADD(now(), interval -5 YEAR)"
    #SQL_ANALOG_BETWEEN = "%s AND NOW()" % SQL_ANALOG_STRAT_DATE
    SQL_ANALOG_RECENT_YEAR = 5
    
    #SQL_SELECT_MODEL = SQL_SELECT_TEMPLATE % (
    #    BaseDates.COL_TIME_MODEL, BaseDates.COL_TIME_MODEL, "surface_model_data",
    #    BaseDates.COL_TIME_MODEL, BaseDates.SQL_ANALOG_BETWEEN)
    #SQL_SELECT_OBS = SQL_SELECT_TEMPLATE % (
    #    BaseDates.COL_TIME_OBS, BaseDates.COL_TIME_OBS, "surface_obs_data",
    #    BaseDates.COL_TIME_OBS, BaseDates.SQL_ANALOG_BETWEEN)
    
    def __init__(self, db_cursor, range_name):
        '''
        Constructor
        '''
        #BaseObject.__init__(self)
        self.cursor = db_cursor
        self.range_name = range_name
        self.max_recent_days = None
        self.end_time        = 'NOW()'
    
        if not OPT_DATES_FROM_DB:
            self.generate_cycle_dates()
        #else:
        #    self.get_cycle_dates_from_db()
    
    @staticmethod
    def get_key(site):
        return "%s" % (site)
        
    #@staticmethod
    #def get_singleton(site, db_cursor):
    #    key = BaseDates.get_key(site)
    #    a_singleton = BaseDates.singletons.get(key, None)
    #    if a_singleton is None:
    #        a_singleton = BaseDates(db_cursor, range)
    #        BaseDates.singletons[key] = a_singleton
    #    return a_singleton
    
    @abstractmethod
    def get_obs_time_column_name(self):
        pass
    
    @abstractmethod
    def get_sql_where(self):
        pass
    
    @abstractmethod
    def get_table_name(self):
        pass
    
    def get_date_count(self):
        return len(self.dates)
    
    def get_date_index(self, cycle_date):
        index = BAD_INDEX
        count = self.get_date_count()
        tmp_index = 0
        while tmp_index < count:
            if self.dates[tmp_index] == cycle_date:
                index = tmp_index
                break
            tmp_index += 1
        #print " get_date_index(): cycle_date: %s ==> %d" % (cycle_date, index)
        return index

    
    def get_sql_date_between(self, sql_extra_where=None):
        debug = False
        debug = debug or os.environ.get("OPT_DEBUG") is not None
        #debug = not debug
        debug_sql = False
        debug_sql = debug_sql or os.environ.get("OPT_DEBUG_SQL") is not None

        sql_end_time = "NOW()"
        if self.end_time is not None and not self.end_time.startswith("NOW"):
            sql_end_time = "'%s'" % DateUtil.mysql_date_from_yyyymmddmm(self.end_time, to_end_of_day=True)
        
        if self.max_recent_days is None or self.max_recent_days == 0:
            sql_date_between = "DATE_ADD(now(), INTERVAL -%d YEAR) AND %s" % (
                    BaseDates.SQL_ANALOG_RECENT_YEAR, sql_end_time)
        else:
            time_column_name = self.get_obs_time_column_name()
            sql_max_time = "SELECT MAX({c}) FROM {t}".format(
                    c=time_column_name, t=self.get_table_name())
            sql_where = "{c} > DATE_ADD(NOW(), INTERVAL -45 DAY)".format(
                    c=time_column_name)
            if sql_extra_where is not None:
                sql_where = "%s AND %s" % (sql_where, sql_extra_where)
            sql_max_time = "%s WHERE %s" % (sql_max_time, sql_where)
            sql_date_between = "DATE_ADD((%s), INTERVAL -%d DAY) AND %s" % (
                    sql_max_time, self.max_recent_days, sql_end_time)
        return sql_date_between;


class VerificationDates(BaseDates):
    singletons = {}
    MODULE_NAME = 'VerificationDates'
    
    @staticmethod
    def get_singleton(site, db_cursor):
        key = BaseDates.get_key(site)
        a_singleton = VerificationDates.singletons.get(key, None)
        if a_singleton is None:
            a_singleton = VerificationDates(db_cursor, range)
            VerificationDates.singletons[key] = a_singleton
        return a_singleton
    
    def set_argument(self, arg_name, arg_value):
        setattr(self, arg_name, arg_value)
    
    def set_arguments(self, model_id, cycle_hour=2, domain_id=3, max_recent_days=0):
        self.model_id        = model_id
        self.domain_id       = domain_id
        self.cycle_hour      = cycle_hour
        self.max_recent_days = max_recent_days
    
    def generate_cycle_dates(self):
        debug = False
        debug = debug or os.environ.get("OPT_DEBUG", False)
        #debug = not debug
        debug_sql = False
        debug_sql = debug_sql or os.environ.get("OPT_DEBUG_SQL", False)
        
        #debug_sql = not debug_sql
        self.dates = []
        
        #print "   ", BaseDates.SQL_SELECT_TEMPLATE
        # "SELECT DATE(min(%s)),DATE(max(%s)) FROM %s WHERE %s BETWEEN %s"
        obs_time_column = self.get_obs_time_column_name()
        sql_date_between = self.get_sql_date_between()
        select_sql = self.SQL_SELECT_TEMPLATE_MIN_MAX_DATE % (
            obs_time_column, obs_time_column,
            self.get_table_name(), obs_time_column, sql_date_between)
    
        if debug_sql:    print "SQL for Min/Max date: %s" % select_sql
        dates = get_db_records(self.cursor, select_sql)
        if dates and dates[0]:
            (min_date, max_date) = dates[0]
        else:
            print " ====================== Should not happen ==================="
            print "   get_min_max_dates_from_db()"
            print " ====================== Not Implemented Yet ==================="
            min_date = '2013-01-01'
            max_date = DateUtil.get_today()
        
        if debug:
            print " date: min: %s, max: %s (type: min: %s, max: %s)" % (
                    min_date, max_date, type(min_date), type(max_date))
    
        tmp_y = min_date.year
        tmp_m = min_date.month
        tmp_d = min_date.day
    
        start_date = DateUtil.get_date_yyyymmdd_int(tmp_y, tmp_m, tmp_d)
        end_date   = DateUtil.get_date_yyyymmdd_int(max_date.year, max_date.month, max_date.day)
        
        tmp_date   = start_date
        
        while (end_date >= tmp_date):
            if debug:  print "  cycle_date: %d" % (tmp_date)
            self.dates.append(tmp_date)
            (tmp_y, tmp_m, tmp_d) = DateUtil.get_next_day(tmp_y, tmp_m, tmp_d)
            tmp_date = DateUtil.get_date_yyyymmdd_int(tmp_y, tmp_m, tmp_d)
        
    def get_cycle_dates_from_db(self):
        debug = False
        debug = debug or os.environ.get("OPT_DEBUG", False)
        #debug = not debug
        debug_sql = False
        debug_sql = debug_sql or os.environ.get("OPT_DEBUG_SQL", False)
        #debug_sql = not debug_sql
        self.dates = []
        
        method_name = '%s.%s()' % (MODULE_NAME, 'get_cycle_dates_from_db')
        #print "   ", BaseDates.SQL_SELECT_TEMPLATE
        #print  " type of options.cycle_hour: ", type(cycle_hour)
        #select_sql = BaseDates.SQL_SELECT_TEMPLATE % (
        #    BaseDates.COL_TIME_MODEL, "surface_model_data", options.model,
        #    options.domain, BaseDates.COL_TIME_MODEL, SQL_ANALOG_BETWEEN,
        #    BaseDates.COL_TIME_MODEL, options.cycle_hour, BaseDates.COL_TIME_MODEL)
        obs_time_column = self.get_obs_time_column_name()
        sql_where = self.get_sql_where()
        select_sql = self.SQL_SELECT_TEMPLATE.format(d=obs_time_column,
                t=self.get_table_name(), w=sql_where)
    
        if debug_sql:    print "SQL for dates: %s" % select_sql
        dates = get_db_records(self.cursor, select_sql)
        
        if dates and dates[0]:
            for date in dates:
                sql_yyyymmdd = date[0];
                tmp_date = DateUtil.get_date_yyyymmdd_from_sql_date(sql_yyyymmdd)
                self.dates.append(tmp_date)
                #print " tmp_date: %d, sql_date: %s" % (tmp_date, sql_yyyymmdd)

        if 0 == len(self.dates):
            print('%s Empty dates, sql: %s' % (method_name, select_sql))
        
        
    def get_max_cycle_time(self):
        debug = False
        debug = debug or os.environ.get("OPT_DEBUG") is not None
        #debug = not debug
        debug_sql = False
        debug_sql = debug_sql or os.environ.get("OPT_DEBUG_SQL") is not None
        method_name = '%s.%s()' % (VerificationDates.MODULE_NAME, 'get_max_cycle_time')
        
        max_cycle_time = None
        sql_where = self.get_sql_where_for_max_cycle_time()
        select_sql = self.SQL_SELECT_MAX_CYCLE.format(c=self.get_obs_time_column_name(),
                t=self.get_table_name(), w=sql_where)
    
        if debug_sql:    print("%s SQL for dates: %s" % (method_name, select_sql))
        dates = get_db_records(self.cursor, select_sql)
        
        if dates and dates[0]:
            max_cycle_time = dates[0][0];
            #print " tmp_date: %d, sql_date: %s" % (tmp_date, sql_yyyymmdd)

        return max_cycle_time
    
    def get_min_cycle_time(self):
        debug = False
        debug = debug or os.environ.get("OPT_DEBUG") is not None
        #debug = not debug
        debug_sql = False
        debug_sql = debug_sql or os.environ.get("OPT_DEBUG_SQL") is not None
        method_name = '%s.%s()' % (VerificationDates.MODULE_NAME, 'get_min_cycle_time')
        
        max_cycle_time = None
        sql_where = self.get_sql_where()
        select_sql = self.SQL_SELECT_MIN_CYCLE.format(c=self.get_obs_time_column_name(),
                t=self.get_table_name(), w=sql_where)
    
        if debug_sql:    print("%s SQL for dates: %s" % (method_name, select_sql))
        dates = get_db_records(self.cursor, select_sql)
        
        if dates and dates[0]:
            max_cycle_time = dates[0][0];
            #print " tmp_date: %d, sql_date: %s" % (tmp_date, sql_yyyymmdd)

        return max_cycle_time
    
    def get_max_recent_days(self):
        max_recent_days = 0
        if self.max_recent_days is not None:
            max_recent_days = self.max_recent_days
        return max_recent_days

    def get_obs_time_column_name(self):
        return BaseDates.COL_TIME_MODEL
    
    def get_sql_date_between(self, sql_extra_where=None):
        debug = False
        debug = debug or os.environ.get("OPT_DEBUG") is not None
        #debug = not debug
        debug_sql = False
        debug_sql = debug_sql or os.environ.get("OPT_DEBUG_SQL") is not None

        sql_end_time = "NOW()"
        if self.end_time is not None and not self.end_time.startswith("NOW"):
            sql_end_time = "'%s'" % DateUtil.mysql_date_from_yyyymmddmm(self.end_time, to_end_of_day=True)
        
        if self.max_recent_days is None or self.max_recent_days == 0:
            sql_date_between = "DATE_ADD(NOW(), INTERVAL -{y} YEAR) AND {e} AND HOUR({c})={h}".format(
                    y=BaseDates.SQL_ANALOG_RECENT_YEAR, e=sql_end_time,
                    c=self.get_obs_time_column_name(),h=self.cycle_hour)
        else:
            time_column_name = self.get_obs_time_column_name()
            sql_max_time = "SELECT MAX({c}) FROM {t}".format(
                    c=time_column_name, t=self.get_table_name())
            sql_where = "{c} > DATE_ADD(NOW(), INTERVAL -45 DAY)".format(
                c=time_column_name)
            if sql_extra_where is not None:
                sql_where = "%s AND %s" % (sql_where, sql_extra_where)
            sql_max_time = "{s} WHERE {w} AND HOUR({c})={h}".format(
                    s=sql_max_time, w=sql_where, c=self.get_obs_time_column_name(),h=self.cycle_hour)
            sql_date_between = "DATE_ADD((%s), INTERVAL -%d HOUR) AND %s" % (
                    sql_max_time, (self.max_recent_days*24-1), sql_end_time)
            #sql_date_between = "DATE_ADD(NOW(), INTERVAL -%d DAY) AND %s" % (
            #        self.max_recent_days, sql_end_time)
        return sql_date_between;


    #@abstractmethod
    def get_sql_where(self):
        obs_time_column = self.get_obs_time_column_name()
        sql_extra_where = None
        if self.model_id is not None:
            sql_extra_where = "model_id=%d" % self.model_id
        sql_date_between = self.get_sql_date_between(sql_extra_where)
        sql_where = "model_id={m} AND domain_id={d} AND {o} BETWEEN {b} AND HOUR({o})={h}".format(
                m=self.model_id, d=self.domain_id, o=obs_time_column,
                b=sql_date_between, h=self.cycle_hour)
        return sql_where
    
    def get_sql_where_for_max_cycle_time(self):
        obs_time_column = self.get_obs_time_column_name()
        sql_where = "model_id={m} AND domain_id={d} AND {o} > DATE_ADD(NOW(), INTERVAL -45 DAY) AND HOUR({o})={h}".format(
                m=self.model_id, d=self.domain_id, o=obs_time_column, h=self.cycle_hour)
        return sql_where
    
    def get_table_name(self):
        return 'surface_model_data'
    
