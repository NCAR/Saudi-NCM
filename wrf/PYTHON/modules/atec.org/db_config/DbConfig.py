#!/usr/bin/env python
import os
import sys

USE_MYSQL_CONNECTOR = False

if USE_MYSQL_CONNECTOR:
    import mysql.connector
else:
    import MySQLdb

#from Properties.Properties import Properties
from atec.db_config.Properties import Properties

MODULE_NAME = 'DbConfig'

def create_db_config():
    cdc_db_config = DbConfig()
    return cdc_db_config
 
def get_config_name():
    #fname = '%s@%s' % (MODULE_NAME, 'get_config_name')
    fullname = DbConfig.DB_CONFIG_FILE_NAME
    if fullname is None or not os.path.exists(fullname):
        home_dir= os.getenv('HOME')
        if not home_dir:
            home_dir= os.getenv('USERPROFILE')  # For Windows
        #print ( 'XXX: %s [%s]' % (home_dir, DbConfig.DB_CONFIG_DIR_NAME))
        fullname = os.path.join(home_dir, DbConfig.DB_CONFIG_DIR_NAME, DbConfig.DB_CONFIG_FILE_NAME)
        if not os.path.isfile(fullname):
            username = get_user_name()
            if '4dwx' != username:
                fullname = fullname.replace('4dwx',username)
    return fullname


def get_user_name():
    #fname = 'DbConfig.get_user_name'
    #print ' %s is called' %  fname
    #env_key = 'OSTYPE'
    #print ' %s ==> %s ' % (env_key, os.getenv(env_key,None))
    #env_key = 'OSTYPE_'
    #print ' %s ==> %s ' % (env_key, os.getenv(env_key,None))
    username = None
    env_user_key = 'LOGNAME'
    env_user_value = os.getenv(env_user_key,None)
    if env_user_value is None:
        env_user_key = 'USER'
        env_user_value = os.getenv(env_user_key,None)
        if env_user_value is None:
            env_user_key = 'USERNAME'
            env_user_value = os.getenv(env_user_key,None)
    if env_user_value is not None:
        username = env_user_value
        
    return username

  
class DbConfig(Properties):

    VERSION = " 1.1 build 0001"
    
    DB_CONFIG_DIR_NAME = 'config'
    DB_CONFIG_FILE_NAME = 'db.config'
    DB_CONFIG_FILE_FULLNAME = None
    DB_PROPERTIES = None
    debug = False
    
    def __init__(self):
        #fname = 'DbConfig.__init__'
    
        config_name = get_config_name()    
        #print '   %s: atec file [%s]' % (fname, DbConfig.DB_CONFIG_FILE_FULLNAME)
        super(DbConfig, self).__init__(config_name)
    
    def get_db_credentials(self, db_name_key):
        debug = False
        debug = debug or DbConfig.debug
        
        fname = '%s.%s' % (MODULE_NAME, 'get_db_credentials')
        user           = self.get(db_name_key + '_user')
        password       = self.get(db_name_key + '_pass')
        host           = self.get(db_name_key + '_host')
        actual_db_name = self.get(db_name_key + '_name', db_name_key)
        if debug:   self.debug_message('%s u:%s, p:%s, h:%s, db: %s' % (fname,user,password,host, actual_db_name))
        
        return (user, password, host, actual_db_name)
    
    def get_db_connection(self, db_name_key):
        debug = False
        debug = debug or DbConfig.debug
        method_name = '%s.%s()' % (MODULE_NAME,'get_db_connection')
        conn = None
        try:
            (user, password, host, actual_db_name) = self.get_db_credentials(db_name_key)
            if debug:   print (db_name_key, user, password, host)
            if USE_MYSQL_CONNECTOR:
                conn = mysql.connector.connect(user=user, passwd=password,
                        host=host, db=actual_db_name)
            else:
                conn = MySQLdb.connect (host=host, user=user, passwd=password, db=actual_db_name)
            
        #except Error, e:
        except Exception as e:
            if 1 < len(e.args):
                print ("%s Error %d: %s (%r)" % (method_name, e.args[0], e.args[1], e.args))
            else:
                print ("%s Error %r, (%r)" % (method_name, e, e.args))
            sys.exit ("Can't Connect to Database\n")
    
        return conn
    
    def get_db_cursor(self, db_conn):
        return db_conn.cursor()

    @staticmethod
    def set_debug_option(enabled):
        DbConfig.debug = enabled
