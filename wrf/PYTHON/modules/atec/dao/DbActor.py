#!/usr/bin/env python
import sys
import time
import traceback

USE_MYSQL_CONNECTOR = False

try:
    import mysql.connector
    USE_MYSQL_CONNECTOR = True
except ImportError:
    import MySQLdb

MODULE_NAME = 'DbActor'

LONG_EXECUTION_TIME      = 10    # seconds
VERY_LONG_EXECUTION_TIME = 30    # seconds

def check_long_query(start_time, sql_string):
    end_time = time.time()
    duration = end_time - start_time
    if LONG_EXECUTION_TIME < duration:
        indent_str = "       ###"
        if VERY_LONG_EXECUTION_TIME < duration:
            indent_str = "     #####"
        print("{i} LONG Query ({d}): {s}".format(i=indent_str, d=duration, s=sql_string))

def connect_db(user, password, db_name, db_host='localhost'):
    debug= False
    fname = '%s:%s' % (MODULE_NAME, 'connect_db()')
    if debug: print('   %s: u=[%s] p=[%s] d=[%s] h=[%s]' % (fname, user, password, db_name, db_host))
    connection = None
    try:
        if USE_MYSQL_CONNECTOR:
            connection = mysql.connector.connect(user=user, passwd=password,
                    host=db_host, db=db_name)
        else:
            connection = MySQLdb.connect(
                    host=db_host, # your host, usually localhost
                    user=user,
                    passwd=password, # your password
                    db=db_name) # name of the data base
    except Exception as e:
        print('   == ERROR === Failed to connect %s: u=[%s] d=[%s] h=[%s]' % (
                fname, user, db_name, db_host))
        raise e
    return connection

def create_db_actor(db_config, db_key):
    #fname = '%s:%s' % (MODULE_NAME, 'create_db_actor()')
    db_actor  = DbActor()
    (user, password, host, db_name) = db_config.get_db_credentials(db_key)
    db_actor.setup_db(user, password, db_name, host)
    return db_actor

def execute_sql(dbConn, sql_string, commit_flag=True):
    fname = '%s:%s' % (MODULE_NAME, 'execute_sql()')
  
    debug = DbActor.debug
    if not debug:
        if   sql_string.startswith('insert') or sql_string.startswith('INSERT') \
                or sql_string.startswith('replace') or sql_string.startswith('REPLACE'):
            debug = DbActor.debug_insert or DbActor.debug_execute
        elif sql_string.startswith('delete') or sql_string.startswith('DELETE'):
            debug = DbActor.debug_delete or DbActor.debug_execute
        elif sql_string.startswith('select') or sql_string.startswith('SELECT'):
            debug = DbActor.debug_select
        elif sql_string.startswith('update') or sql_string.startswith('UPDATE'):
            debug = DbActor.debug_update or DbActor.debug_execute
    
    if debug:  print(' === %s is called, SQL: %s' % (fname, sql_string))
  
    if dbConn:
        try:
            start_time = time.time()
      
            cur = dbConn.cursor() 
            cur.execute(sql_string)
            check_long_query(start_time, sql_string)
        except:
            except_message = " === %s exception sql =[%s]" % (fname, sql_string)
            print(except_message)
            sys.stderr.write(except_message + "\n")
            handle_exception(-2)
            #sys.exit(-2)
        finally:
            if cur:        cur.close()
            if commit_flag:   dbConn.commit()
    else:
        print(' === ERROR === %s %s ' % (fname, 'db connection is missing'))

def execute_sql2(dbConn, sql_string, parameters, commit_flag=True):
    fname = '%s:%s' % (MODULE_NAME, 'execute_sql2()')
  
    debug = DbActor.debug
    if not debug:
        if   sql_string.startswith('insert') or sql_string.startswith('INSERT') \
                or sql_string.startswith('replace') or sql_string.startswith('REPLACE'):
            debug = DbActor.debug_insert or DbActor.debug_execute
        elif sql_string.startswith('delete') or sql_string.startswith('DELETE'):
            debug = DbActor.debug_delete or DbActor.debug_execute
        elif sql_string.startswith('select') or sql_string.startswith('SELECT'):
            debug = DbActor.debug_select
        elif sql_string.startswith('update') or sql_string.startswith('UPDATE'):
            debug = DbActor.debug_update or DbActor.debug_execute
    
    if debug:  print(' === %s is called, SQL: %s' % (fname, sql_string))
  
    if dbConn:
        try:
            start_time = time.time()
            cur = dbConn.cursor() 
            cur.execute(sql_string, parameters)
            check_long_query(start_time, sql_string)
        except:
            except_message = " === %s exception sql =[%s]" % (fname, sql_string)
            print(except_message)
            sys.stderr.write(except_message + "\n")
            handle_exception(-2)
            #sys.exit(-2)
        finally:
            if commit_flag: dbConn.commit()
            if cur:         cur.close()
    else:
        print(' === ERROR === %s %s ' % (fname, 'db connection is missing'))

def exist_column(dbConn, db_name, table_name, column_name):
    fname = '%s:%s' % (MODULE_NAME, 'exist_column()')
    #print(' === %s is called %s' % (fname, sql_string))
    exist = False
  
    if dbConn:
        table_name = "information_schema.COLUMNS"
        sql_where  = "TABLE_SCHEMA='%s' AND TABLE_NAME='%s' AND COLUMN_NAME='%s'" % (
                db_name, table_name, column_name)
        exist = (0 < get_count(dbConn,table_name,sql_where=sql_where))
    else:
        print(' === ERROR === %s %s ' % (fname, 'db connection is missing'))
    return exist

def exist_table(dbConn, db_name, table_name):
    fname = '%s:%s' % (MODULE_NAME, 'exist_table()')
    #print(' === %s is called %s' % (fname, sql_string))
    exist = False
  
    if dbConn:
        table_name = "information_schema.TABLES"
        sql_where  = "TABLE_SCHEMA='%s' AND TABLE_NAME='%s'" % (
                db_name, table_name)
        exist = (0 < get_count(dbConn,table_name,sql_where=sql_where))
    else:
        print(' === ERROR === %s %s ' % (fname, 'db connection is missing'))
    return exist

def get_count(dbConn, table_name, sql_where=''):
    fname = '%s:%s' % (MODULE_NAME, 'get_count()')
    #print(' === %s is called' % fname)
    
    cur = None
    count = 0
    if dbConn:
        try:
            # you must create a Cursor object. It will let
            #  you execute all the queries you need
            start_time = time.time()
            cur = dbConn.cursor() 
            
            # Use all the SQL you like
            sql_string = "SELECT count(*) FROM %s" % (table_name)
            if sql_where:
                sql_string += " WHERE %s" % sql_where
            #if group_by:
            #    sql_string += " GROUP BY %s" % group_by
            #print('  %s: sql: %s' % (fname, sql_string))
            #dbConn.query("SELECT VERSION()")
            if DbActor.debug_select:  print('  %s: sql: %s' % (fname, sql_string))
            cur.execute(sql_string)
            
            rows = cur.fetchall()
            count = rows[0][0]
            check_long_query(start_time, sql_string)
            #for row in rows:
            #  print('  %s: ' % fname, row)
        except:
            print('  %s: sql: %s' % (fname, sql_string))
            handle_exception(-3)
            #sys.exit(-3)
        finally:
            if cur:
                cur.close()
    else:
        print(' === ERROR === %s %s ' % (fname, 'db connection is missing'))
    return count

def get_records(dbConn, table_name, sql_columns='*', sql_where='', group_by='', order_by=''):
    fname = '%s:%s' % (MODULE_NAME, 'get_records()')
    #print(' === %s is called' % fname)
    
    cur = None
    rows = []
    if dbConn:
        try:
            # you must create a Cursor object. It will let
            #  you execute all the queries you need
            start_time = time.time()
            cur = dbConn.cursor() 
            
            # Use all the SQL you like
            sql_string = "SELECT %s FROM %s" % (sql_columns, table_name)
            if sql_where is not None and 0 < len(sql_where):
                sql_string += " WHERE %s" % sql_where
            if group_by is not None and 0 < len(group_by):
                sql_string += " GROUP BY %s" % group_by
            if order_by is not None and 0 < len(order_by):
                sql_string += " ORDER BY %s" % order_by
            #dbConn.query("SELECT VERSION()")
            if DbActor.debug_select:  print('  %s: sql: %s' % (fname, sql_string))
            cur.execute(sql_string)
            
            rows = cur.fetchall()
            
            check_long_query(start_time, sql_string)
            #print('  %s: sql: %s' % (fname, sql_string))
            #for row in rows:
            #  print('  %s: ' % fname, row)
        except:
            print('  %s: sql: %s' % (fname, sql_string))
            handle_exception(-4)
        finally:
            if cur:  cur.close()
    else:
        print(' === ERROR === %s %s ' % (fname, 'db connection is missing'))
    return rows

def handle_exception(exit_code=-1):
    print("\n ============= Exception at %s " % MODULE_NAME)
    traceback.print_stack()
    traceback.print_exc(file=sys.stdout)
    traceback.print_exc(file=sys.stderr)
    print("\n\n")
    print(sys.exc_info())
    sys.exit(exit_code)
   
   
class DbActor(object):

    VERSION = "1.0 build_0001"
      
    debug         = False
    debug_delete  = False
    debug_insert  = False
    debug_execute = False
    debug_select  = False
    debug_update  = False
    
    #debug = True
    #db_config = None
    
    #def get_config_filename():
    
    def __init__(self):
        self.dbConn = None
        #if not DbActor.db_config:
        #  DbActor.db_config = DbActor()
          
    def commit(self):
        if self.dbConn:     self.dbConn.commit()
        
    def rollback(self):
        if self.dbConn:     self.dbConn.rollback()
        
        
    def isConnected(self):
        return (self.dbConn != None)
      
    def get_name_id_map(self, table_name):
        #fname = '%s.%s' % (MODULE_NAME, 'get_name_id_map()')
        rows = get_records(self.dbConn, table_name,'id,name')
        a_map = {}
        for row in rows:
            a_map[row[1]] = row[0]
        #print('  DEBUG] %s: ' % fname, map)
        return a_map
        
    def execute_sql(self, sql_string, commit_flag=True):
        execute_sql(self.dbConn, sql_string, commit_flag)
        
    def execute_sql2(self, sql_string, parameters, commit_flag=True):
        execute_sql2(self.dbConn, sql_string, parameters, commit_flag)
        
    def exist_column(self, table_name, column_name):
        #fname = '%s.%s' % (MODULE_NAME, 'exist_column()')
        #print(' === %s is called %s' % (et_fname, sql_string))
        return exist_column(self.dbConn, self.db_name, table_name, column_name)
      
    def exist_table(self, table_name):
        #fname = '%s.%s' % (MODULE_NAME, 'exist_table()')
        #print(' === %s is called %s' % (fname, sql_string))
        return exist_table(self.dbConn, self.db_name, table_name)
      
    def get_count(self, table_name, sql_where=''):
        #fname = '%s.%s' % (MODULE_NAME, 'get_records()')
        count = get_count(self.dbConn, table_name, sql_where)
        return count
        
    def get_records(self, table_name, sql_columns='*', sql_where='', group_by='', order_by=''):
        #fname = '%s.%s' % (MODULE_NAME, 'get_records()')
        rows = get_records(self.dbConn, table_name,  sql_columns, sql_where, group_by, order_by)
        return rows
        
    def release(self):
        if self.dbConn and self.is_dbConn_closed:
            self.dbConn.close()
            self.is_dbConn_closed = True
      
    def setup_db(self, username, password, db_name, db_host='localhost'):
        self.username = username
        self.password = password
        self.db_name = db_name
        self.dbConn = connect_db(username, password, db_name, db_host)
        self.is_dbConn_closed = False
      
