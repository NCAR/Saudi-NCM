#!/usr/bin/env python

#import os
import os.path
import subprocess
import platform

FILE_NAME = 'Properties'

CALLSTACK_P   = "     _call_stack_"
DEBUG_P       = "     [DEBUG]"
ERROR_P       = "   === ERROR ==="
INFO_P        = "     [INFO]"
WARN_P        = "    -- WARN --"
PERFORMANCE_P = "     >> PERFORMANCE <<  "
PROGRESS_P    = "     ...PROGRESS... "

opt_printed_decrypt_error          =  False
opt_printed_find_command_with_path =  False

def debug_message(message):
    print ("%s %s" % (DEBUG_P,message))



def decrypt(encrypted_value):
    global opt_printed_decrypt_error
    fname = '%s:%s' % (FILE_NAME, 'decrypt')
    decrypted_value = encrypted_value
    command_name = 'decrypt.sh'
    command_full_name = find_command_with_path(command_name)
    if None != command_full_name and 0 < len(command_full_name):
        process = subprocess.Popen('%s %s' % (command_full_name, encrypted_value),
            shell=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
        decrypted_value = process.stdout.read().strip()
        #print ('  DEBUG] %s decrypted_value = %s ' % (fname, decrypted_value))
    elif not opt_printed_decrypt_error:
        print ("  === ERROR === ERROR === ERROR === ERROR === ERROR ===")
        print ("     %s Can't not find %s" % (fname, command_name))
        print ("  === ERROR === ERROR === ERROR === ERROR === ERROR ===")
        opt_printed_decrypt_error = not opt_printed_decrypt_error
        tmp_offset = encrypted_value.find('_encrypted')
        if 0 < tmp_offset:  decrypted_value = encrypted_value[0:tmp_offset]

    return decrypted_value

def find_command_with_path(command_name):
    debug = False
    #debug = not debug
    fname = '%s:%s' % (FILE_NAME, 'find_command_with_path')
    full_command = None
    if os.path.isfile(command_name):
        full_command = command_name
    else:
        global opt_printed_find_command_with_path
        if platform.system() == "Linux":
            base_command = os.path.basename(command_name)
            process = subprocess.Popen('which %s' % (base_command),
                shell=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
            output = process.stdout.read().strip()
            if debug:
                #print ("  DEBUG] %s stderr: [%r]" % (fname, process.stderr.read().strip()))
                print ("  DEBUG] %s output: [%s]" % (fname, output))
            if ("no %s in" % base_command) in output:
                tmp_full_command = '%s/bin/%s' % (os.environ['HOME'], base_command)
                if os.path.isfile(tmp_full_command):
                    full_command = tmp_full_command
                else:
                    tmp_full_command = '%s/datbin/%s' % (os.environ['HOME'], base_command)
                    if os.path.isfile(tmp_full_command):
                        full_command = tmp_full_command
            else:
                full_command = output
        elif not opt_printed_find_command_with_path:
            print ("  === ERROR === ERROR === ERROR === ERROR === ERROR ===")
            print ("     %s is not implemented for Other OS than Linux" % (fname))
            print ("  === ERROR === ERROR === ERROR === ERROR === ERROR ===")
            opt_printed_find_command_with_path = not opt_printed_find_command_with_path
    return full_command

def get_basename(file_name):
    return os.path.splitext(os.path.basename(file_name))[:-1]
  
def get_extension(file_name):
    return os.path.splitext(os.path.basename(file_name))[-1]


class Properties(object):
    map = []
    
    def __init__(self, properties_filename):
        self.properties_filename = properties_filename
        #i_basename = get_basename(properties_filename)
        #i_file = open(properties_filename, 'r')
        self.properties = Properties._read_properties(properties_filename)
    
    @staticmethod
    def _read_properties(property_name):
        debug = False
        fname = '%s.%s' % (FILE_NAME, '_read_properties')
        properties = {}
        with open(property_name, 'r') as prop_file:
            for line_buf in prop_file.readlines():
                if debug:    print ('  DEBUG] %s input line: [%s]' % (fname, line_buf.strip()))
                if 0 == len(line_buf.strip()) or line_buf.startswith('#') or line_buf.startswith('='):
                    continue
                
                index = line_buf.find('=')
                #print ('  DEBUG] %s index for = [%d]' % (fname, index))
                if 0 < index:
                    key = line_buf[:index].strip()
                    index_comment = line_buf.find('#',index)
                    if 0 > index_comment:
                        value = line_buf[(index+1):].strip()
                    else:
                        value = line_buf[(index+1):(index_comment-1)].strip()
                    if 0 < key.find('_pass'):
                        enc_value = value
                        value = decrypt(value)
                        if (enc_value == value):
                            tmp_offset = enc_value.find('_encrypted')
                            if 0 < tmp_offset:  value = enc_value[0:tmp_offset]
                    if debug:    print ('  DEBUG] %s  key:value= %s ==> %s ' % (fname, key,value))
                    #print ('  DEBUG] %s  key:value= %s ==> %s ' % (fname, key,value))
                    properties[key] = value
                else:
                    print ('  === ERROR === %s: [%s] %s' % (fname,line_buf,'Invalid key value pair'))
                
        if debug:
            print ('  DEBUG] %s properties: ' % (fname), properties)
        return properties
    
        
    @staticmethod
    def get_properties(properties_filename):
        basename = get_basename(properties_filename)
        properties = Properties.map.get(basename, None)
        if not properties:
            properties = Properties(properties_filename)
            Properties.map[basename] = properties
        return properties
    
    def get(self, key, def_value=None):
        #fname = '%s.%s' % (FILE_NAME, 'get')
        value = self.properties.get(key, def_value)
        #print ('  DEBUG] %s  key : value = %s ==> %s ' % (fname, key,value))
        return value
    
    def debug_message(self, message):
        debug_message(message)



def test():
    print (platform.system())
    print (platform.platform())
    
    cmd_name = 'which_'
    print (" === %s --> %s" % (cmd_name, find_command_with_path(cmd_name)))
    cmd_name = 'which'
    print (" === %s --> %s" % (cmd_name, find_command_with_path(cmd_name)))
    cmd_name = 'decrypt.sh'
    print (" === %s --> %s" % (cmd_name, find_command_with_path(cmd_name)))
    
    encrypted_value = '384585ACCE83ABD3'
    print (" === decrypt: ", decrypt(encrypted_value))

  
if __name__ == "__main__":
    test()
