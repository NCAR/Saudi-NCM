#!/usr/bin/env python

import sys
import math
import time
import traceback

def convert_datetime_to_seconds(datetime_obj):
    return time.mktime(datetime_obj.timetuple())

def debug_message(message):
    print("{p} {m}".format(p=BaseObject.DEBUG_P, m=message))

def error_message(message):
    #print("\n ===========================================")
    print("\n{p} {m}\n".format(p=BaseObject.ERROR_P, m=message))
    #print(" ===========================================\n")

def info_message(message):
    print("{p} {m}".format(p=BaseObject.INFO_P,m=message))

def is_equal_number(value1, value2):
    result = False
    if value1 is not None and value2 is not None:
        result = (math.fabs((value1 - value2)) < BaseObject.EPSILON)
    return result

def perf_message(message):
    #print("\n   >>>>> >>>>> >>>>> >>>>> >>>>>> <<<<< <<<<< <<<<< <<<<<")
    print("{p} {m}".format(p=BaseObject.PERFORMANCE_P, m=message))
    #print("   >>>>> >>>>> >>>>> >>>>> >>>>>> <<<<< <<<<< <<<<< <<<<<\n")

def performance_message(message):
    perf_message(message)

def progress_message(message):
    print("{p} {m}\n".format(p=BaseObject.PROGRESS_P,m=message))

def warn_message(message):
    #print("\n -------------------------------------------")
    print("{p} {m}".format(p=BaseObject.WARN_P,m=message))
    #print(" -------------------------------------------\n")


class BaseObject(object):

    CALLSTACK_P   = "     _call_stack_"
    DEBUG_P       = "     [DEBUG]"
    ERROR_P       = "     == ERROR =="
    INFO_P        = "      [INFO]"
    WARN_P        = "     -- WARN --"
    PERFORMANCE_P = "     >> PERFORMANCE << "
    PROGRESS_P    = "     ..PROGRESS.. "

    EPSILON = 0.0001
    
    def __init__(self):
        #self.db_actor = db_actor
        self.start_time = 0
        self.end_time = 0
        self.call_stack = False
        self.debug = False
    
    def blank_line(self):
        print("")
        
    def display_exception(self, message=''):
        print("\n{m}\n".format(m=message))
        traceback.print_stack()
        traceback.print_exc(file=sys.stdout)
        print("\n\n")
        print(sys.exc_info())
        print("\n")
        
    def get_current_time(self):
        return time.time()
        
    def get_duration(self):
        return (self.end_time - self.start_time)
        
    def compute_duration(self, start_time):
        return (self.get_current_time() - start_time)
        
    def is_call_stack(self):
        return self.call_stack
        
    def set_call_stack(self, value):
        self.call_stack = value
        
    def is_debug_enabled(self):
        return self.debug
        
    def set_debug(self, value):
        self.debug = value
        
    def set_start_time(self, new_start_time=None):
        if new_start_time is None:
            self.start_time = time.time()
        else:
            self.start_time = new_start_time
    def set_end_time(self, new_end_time=None):
        if new_end_time is None:
            self.end_time = time.time()
        else:
            self.end_time = new_end_time
        
    def debug_message(self, message, debug=False):
        if debug or self.is_debug_enabled(): debug_message(message)
    
    def debug_print_called(self, module_name):
        if self.call_stack: print("{p} {m]} is called".format(p=BaseObject.CALLSTACK_P,m=module_name))
    
    def error_message(self, message):
        error_message(message)
    
    def info_message(self, message):
        info_message(message)
    
    def perf_message(self, message):
        perf_message(message)
    
    def performance_message(self, message):
        perf_message(message)
    
    def progress_message(self, message):
        progress_message(message)
    
    def warn_message(self, message):
        warn_message(message)

