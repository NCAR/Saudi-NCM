'''
Created on Feb 10, 2015

@author: hsoh
'''

import sys
import traceback

def handle_exception(exit_message = None, method_name=None):
    ex = sys.exc_info()[0]
    if None != method_name:
        print(' Exception happened at: %s' % (method_name))
    print(' except: %r' % (ex))
    traceback.print_exc()
    if None != exit_message:
        sys.exit( exit_message )

def handle_sql_exception(sql_string, exit_message = None, method_name=None):
    ex = sys.exc_info()[0]
    if None != method_name:
        print(' Exception happened at: %s SQL: %s' % (method_name, sql_string))
    print(' except: %r' % (ex))
    traceback.print_exc()
    if None != exit_message:
        sys.exit( exit_message )
