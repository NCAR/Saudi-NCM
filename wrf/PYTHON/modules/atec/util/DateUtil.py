'''
Created on Jan 22, 2015

@author: hsoh
'''

import calendar
import datetime
from datetime import timedelta
import time
import platform

MODULE_NAME = 'DateUtil'

class LogTime(object):
    def __init__(self, message=None):
        self.message = message
        self.reset()
        self.duration_wall = 0
        self.duration_processor = 0
        self.timers = {}
    
    @staticmethod
    def get_current_time_string():
        return time.strftime('%Y-%d-%d %X %Z')
    
    def reset(self):
        self.start_time_wall      = time.time()
        self.start_time_processor = time.clock()
    
    def stop(self, print_cur_time=False):
        self.duration_wall      = time.time()  - self.start_time_wall
        self.duration_processor = time.clock() - self.start_time_processor
        if print_cur_time:
            print('   Timer stopped at %s' % (LogTime.get_current_time_string()))
    
    def get_current_duration(self):
        duration_wall      = time.time()  - self.start_time_wall
        return duration_wall
    
    def get_durations(self, latest = False):
        if latest or self.duration_wall == 0:
            self.stop()
        duration_wall      = self.duration_wall
        duration_processor = self.duration_processor
        return (duration_wall, duration_processor)

    def get_durations_by_name(self, name):
        return self.timers.get(name, None)

    def format(self, duration):
        t_seconds = duration
        if 60 <= t_seconds:
            t_minutes = int(t_seconds / 60)
            t_seconds = t_seconds % 60
            if 60 <= t_minutes:
                t_hours = int(t_minutes / 60)
                t_minutes = t_minutes % 60
                str_duration = '%d:%02d:%02d' % (t_hours, t_minutes, t_seconds)
            else:
                str_duration = '%d:%02d' % (t_minutes, t_seconds)
        else:
            str_duration = '{d}'.format(d=t_seconds)
        return str_duration
    
    def get_duration_strings(self, name=None):
        durations = None
        duration_wall_str = ""
        duration_process_str = ""
        if name is not None:
            durations = self.get_durations_by_name(name)
        else:
            durations  = self.get_durations()
        if durations is not None:
            duration_wall = durations[0]
            duration_process = durations[1]
            duration_wall_str = '{d}'.format(d=duration_wall)
            if 60 <= duration_wall:
                duration_wall_str = "%s [%s]" % (duration_wall_str, self.format(duration_wall))
            duration_process_str = '{d}'.format(d=duration_process)
            if 60 <= duration_process:
                duration_process_str = "%s [%s]" % (duration_process_str, self.format(duration_process))
        return (duration_wall_str, duration_process_str)

    def save(self, name, durations=None):
        if durations is None:
            durations = self.get_durations()
        self.timers[name] = durations
        
class DateUtil(object):
    '''
    classdocs
    '''
      
    # Input: time object or datetime.date object : convert to seconds
    # Input: interval_time object or datetime.date object : convert to seconds
    # Returns date object
    @staticmethod
    def add_time_to_date( a_date_or_time, time_interval_in_seconds):
        method_name = '     %s.%s' % (MODULE_NAME, 'add_time_to_date()')
        a_time = None
        if isinstance(a_date_or_time, datetime.time):
            a_time = a_date_or_time
        elif isinstance(a_date_or_time, datetime.datetime):
            a_time = DateUtil.convert_to_time( a_date_or_time )
        elif isinstance(a_date_or_time, str) or ('Windows' != platform.system() and isinstance(a_date_or_time, unicode)):
            a_time = DateUtil.convert_to_time( a_date_or_time )
        else:
            input_data_type = type(a_date_or_time).__name__
            print ('%s Unknown data type %s' % (method_name, input_data_type))
        #print ('    add_offset(): datatype: %s, %s => %f' % (type(a_date_or_time).__name__, type(a_date_or_time), a_time))
        new_date = None
        if None != a_time:
            new_date = datetime.datetime.fromtimestamp( a_time + time_interval_in_seconds)
        return new_date
      
    # Input: time object or datetime.date object : convert to seconds
    @staticmethod
    def add_offset_for_ymdhm( a_date_or_time, time_interval_in_seconds):
        new_date = DateUtil.add_time_to_date( a_date_or_time, time_interval_in_seconds)
        return datetime.datetime.strftime(new_date, "%Y%m%d%H%M")
      
    @staticmethod
    def add_offset_for_ymdhms( a_date_or_time, time_interval_in_seconds):
        new_date = DateUtil.add_time_to_date( a_date_or_time, time_interval_in_seconds)
        return datetime.datetime.strftime(new_date, "%Y%m%d%H%M%S")
      
    @staticmethod
    def conv_mysqldatetime( a_date ):
        return DateUtil.convert_date_to_time( a_date )
      
    @staticmethod
    def convert_to_mysql_date( a_date ):
        mysql_date = None
        #input_data_type = type(a_date)
        if   isinstance(a_date, str):
            mysql_date = DateUtil.mysql_date_from_yyyymmddmm(a_date)
        elif isinstance(a_date, datetime.datetime):
            #mysql_date = DateUtil.mysql_date(a_date.date.year,a_date.date.month,a_date.date.day,
            #        a_date.time.hour,a_date.time.minute,a_date.time.second)
            mysql_date = DateUtil.mysql_date(a_date.year,a_date.month,a_date.day,
                    a_date.hour,a_date.minute,a_date.second)
        else:
            print('  == ERROR == at convert_to_mysql_date. Not supported data type {t}'.format(t=type(a_date)))
        return mysql_date
      
    # Input: date or string ('yyyymmdd' / 'yyyymmddhhmm')
    @staticmethod
    def convert_to_time( time_input ):
        debug = False
        #input_data_type = type(time_input)
        #if debug: print ('   convert_to_time: input data type: %s' % (input_data_type))
        #if   'time' ==  input_data_type:
        #    a_time = time_input
        #elif 'datetime' == input_data_type:
        #    a_time = time.mktime( time_input.timetuple() )
        #elif 'str' == input_data_type:
        #    str_input = time_input.replace(':',"").replace('-','').replace(' ','')
        #    a_date = DateUtil.convert_to_datetime( str_input )
        #    a_time = time.mktime( a_date.timetuple() )
        #else:
        #    a_time = None
        if   isinstance(time_input, datetime.time):
            a_time = time_input
        elif isinstance(time_input, datetime.datetime):
            a_time = time.mktime( time_input.timetuple() )
        elif isinstance(time_input,str) or 'Windows' != platform.system() and isinstance(time_input, unicode):
            str_input = time_input.replace(':',"").replace('-','').replace(' ','')
            a_date = DateUtil.convert_to_datetime( str_input )
            a_time = time.mktime( a_date.timetuple() )
        else:
            a_time = None
        return a_time
      
    # Input: time or string ('yyyymmdd' / 'yyyymmddhhmm')
    @staticmethod
    def convert_to_datetime( date_input ):
        if isinstance(date_input, datetime.datetime):
            a_date = date_input
        elif isinstance(date_input, datetime.time):
            a_date = datetime.date.fromtimestamp(date_input)
        elif isinstance(date_input, str) or ('Windows' != platform.system() and isinstance(date_input, unicode)):
            (yr, mth, day, hr, minute, second) = DateUtil.split_date( date_input )
            a_date = datetime.datetime( yr, mth, day, hr, minute, second )
        else:
            a_date = None
        return a_date
      
    @staticmethod
    def convert_datetime_to_seconds(datetime_obj):
        #if isinstance(datetime_obj, str):
        #    datetime_obj = DateUtil.convert_to_datetime(datetime_obj)
        return time.mktime(datetime_obj.timetuple()) if datetime_obj is not None else None

    @staticmethod
    def format_to_ymd(a_date):
        return datetime.datetime.strftime(a_date, "%Y%m%d")
      
    @staticmethod
    def format_to_ymdh(a_date):
        return datetime.datetime.strftime(a_date, "%Y%m%d%H")
      
    @staticmethod
    def format_to_ymdhm(a_date):
        return datetime.datetime.strftime(a_date, "%Y%m%d%H%M")
      
    @staticmethod
    def format_to_ymdhms(a_date):
        return datetime.datetime.strftime(a_date, "%Y%m%d%H%M%S")
      
    @staticmethod
    def format_to_mysql_date(a_date):
        return datetime.datetime.strftime(a_date, "%Y-%m-%d %H:%M:%S")
    
    @staticmethod
    def get_date_yyyymmdd_int(a_year, a_month, a_day):
        return (a_year * 10000 + a_month * 100 + a_day)
      
    @staticmethod
    def get_date_yyyymmdd_from_sql_date(sql_date):
        return DateUtil.get_date_yyyymmdd_int(sql_date.year, sql_date.month, sql_date.day)
      
    @staticmethod
    def get_date_yyyymmddhh_from_sql_date(sql_date):
        return (DateUtil.get_date_yyyymmdd_int(sql_date.year, sql_date.month, sql_date.day) * 100) + sql_date.hour
      
    @staticmethod
    def get_next_day(a_year, a_month, a_day):
        n_year  = a_year
        n_month = a_month
        
        gnd_last_day = calendar.monthrange(a_year,a_month)[1]
        if gnd_last_day > a_day:
            n_day = a_day + 1
        else:
            n_day = 1
            if 12 > n_month:
                n_month = n_month + 1
            else:
                n_month = 1
                n_year  = n_year + 1
        return (n_year, n_month, n_day)
      
    @staticmethod
    def get_now():
        return datetime.datetime.now()
      
    # Returns datetime.date object
    @staticmethod
    def get_today():
        return datetime.date.today()
  
    # Returns datetime.date object
    @staticmethod
    def get_yesterday():
        t = DateUtil.get_today()
        y = t - timedelta(1) 
        return y
    
    # takes year month day hour min and returns a
    # mysql formatted datetime yyyy-mm-dd hh:nn:00
    @staticmethod
    def mysql_date(year,month,day,hour,minute,second='00'):
        return_date = "%4d-%02d-%02d %02d:%02d:%02d" % (int(year),int(month),int(day),int(hour),int(minute),int(second))
        return return_date
  
    @staticmethod
    def mysql_date_from_yyyymmddmm(yyyymmddmmss, to_end_of_day=False):
        (year,month,day,hour,minute,second) = DateUtil.split_date( yyyymmddmmss, to_end_of_day )
        #mysql_date = DateUtil.mysql_date(year,month,day,hour,minute)
        mysql_date = "%4d-%02d-%02d %02d:%02d:%02d" % (year,month,day,hour,minute,second)
        return mysql_date
  
    @staticmethod
    def split_date( date_ymd_hm, to_end_of_day=False ):
        if date_ymd_hm[0] == date_ymd_hm[-1]:
            if date_ymd_hm[0] == "'" or date_ymd_hm[0] == '"':
                date_ymd_hm = date_ymd_hm[1:-1]
        t_date_ymd_hm = date_ymd_hm.replace('-','').replace(':','').replace(' ','')
        yr  = t_date_ymd_hm[0:4]
        mth = t_date_ymd_hm[4:6]
        day = t_date_ymd_hm[6:8]
        if to_end_of_day:
            hr = "23"
            mm = "59"
            ss = "59"
        else:
            hr = "00"
            mm = "00"
            ss = "00"
        if len(t_date_ymd_hm) > 8:
            hr = t_date_ymd_hm[8:10]
            if len(t_date_ymd_hm) > 10:
                mm = t_date_ymd_hm[10:12]
                if len(t_date_ymd_hm) > 12:
                    ss = t_date_ymd_hm[12:14]
   
        #return (yr,mth,day,hr,nn)
        return (int(yr),int(mth),int(day),int(hr),int(mm),int(ss))
  
#
# End of the class
#
  
def test_add_offset():
    d_today = DateUtil.get_today()
    t_today = DateUtil.convert_to_time( d_today )
    intervals = [1, 2, 60, 100, 110, 120, 200, 300, 3600, 5900, 230000] 
    print ('     intervals: %r' % (intervals))
    for interval in intervals: 
        tmp_date_ymdhm  = DateUtil.add_offset_for_ymdhm( t_today, interval)
        tmp_date_ymdhms = DateUtil.add_offset_for_ymdhms( t_today, interval)
        print ('     date after adding %8d: %r, %r' % (interval, tmp_date_ymdhm, tmp_date_ymdhms ))
    print ('')

def test():
    dt_now = DateUtil.get_now()
    d_today = DateUtil.get_today()
    d_yesterday = DateUtil.get_yesterday()
    print ('      now: %r' % (dt_now))
    print ('    today: %r, yesterday: %r' % (d_today, d_yesterday))
    t_today = DateUtil.convert_to_time( d_today )
    print ('  t_today: %r' % (t_today))
    print ('')
    
    test_add_offset()
    
    i_yyyymmddhhss = '2015012205'
    t_time = DateUtil.convert_to_time(i_yyyymmddhhss)
    #print (' %s to time %r' % (i_yyyymmddhhss, t_time))
    print (' %s to time %r (%f)' % (i_yyyymmddhhss, t_time, t_time ))
    
if __name__ == '__main__':
    test()
    print ("  === Done %s ===" % MODULE_NAME)