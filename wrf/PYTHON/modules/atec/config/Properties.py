#!/usr/bin/env python

import sys
import os.path
import subprocess
import platform
import base64
from os.path import expanduser
from cryptography.fernet import Fernet

MODULE_NAME = 'Properties'

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
    fname = '%s:%s' % (MODULE_NAME, 'decrypt')
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
    fname = '%s:%s' % (MODULE_NAME, 'find_command_with_path')
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
            if "no {c} in".format(c=base_command) in output:
                home_dir = get_home_dir()
                tmp_full_command = '{h}/bin/{c}'.format(h=home_dir, c=base_command)
                if os.path.isfile(tmp_full_command):
                    full_command = tmp_full_command
                else:
                    tmp_full_command = '{h}/datbin/{c}'.format(h=home_dir, c=base_command)
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

def get_home_dir():
    #home_dir = os.getenv('HOME')
    ##home_dir = os.environ.get('HOME')
    #if home_dir is None:
    #    home_dir = os.getenv('HOME')
    home_dir = expanduser("~")
    return home_dir


class myCipher(object):
    #JUNK_STRING = 'HowToGenerateKeyForEncryption!!!'
    KEY = b'SG93VG9HZW5lcmF0ZUtleUZvckVuY3J5cHRpb24hISE='

    def __init__(self, key = None):
        debug = False
        if key is None:
            key = myCipher.update_key()
            if key is None:
                key = myCipher.KEY
        if debug:   print('cipher key: {k}'.format(k=key))
        self.key = key
        self.cipher_suite = Fernet(key)

    def decrypt(self, text):
        try:
            if isinstance(text, bytes):
                byte_text = text
            else:
                byte_text = str.encode(text)
            uncipher_text = (self.cipher_suite.decrypt(byte_text))
            decrypt_text = bytes(uncipher_text).decode("utf-8") 
        except:
            print('Fail to decrypt [{e}] by {k}'.format(e=text, k=self.key))
            decrypt_text = None
        return decrypt_text 

    def encrypt(self, text):
        if isinstance(text, bytes):
            byte_text = text
        else:
            byte_text = str.encode(text)
        return self.cipher_suite.encrypt(byte_text)

    @staticmethod
    def update_key():
        debug = False
        #debug = not debug
        key = None
        key_file = os.path.join(get_home_dir(),'.ssh', 'id_rsa.pub')
        #if debug:   print(' key file: {f}'.format(f=key_file))
        if os.path.exists(key_file):
            if debug:   print(' found key file')
            with open(key_file, 'r') as file_object:
                try:
                    rsa_key = '\n'.join(file_object.readlines())
                    if debug:   print(' rsa key: {k}'.format(k=rsa_key))
                    keys = rsa_key.split(' ')
                    if 1 < len(keys):
                        encoded_key = keys[1]
                    else:
                        encoded_key = rsa_key
                    decoded_key = base64.urlsafe_b64decode(encoded_key)
                    #print(' rsa key: {k} encoded: {e}  decoded: {d}'.format(k=rsa_key, e=encoded_key, d=decoded_key))
                    key = base64.urlsafe_b64encode(decoded_key[:32])
                except:
                    traceback.print_exc()
                    pass
        return key


class Properties(object):
    map = []
    cipher = myCipher()
    
    def __init__(self, properties_filename):
        self.properties_filename = properties_filename
        #i_basename = get_basename(properties_filename)
        #i_file = open(properties_filename, 'r')
        self.properties = Properties._read_properties(properties_filename)
    
    @staticmethod
    def _read_properties(property_name):
        debug = False
        #debug = not debug
        fname = '%s.%s' % (MODULE_NAME, '_read_properties')
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
                    if key.endswith('_pass'):
                        dec_value = Properties.cipher.decrypt(value)
                        if dec_value is not None:
                            value = dec_value
                        else:
                            enc_value = value
                            value = decrypt(value)
                            if (enc_value == value):
                                tmp_offset = enc_value.find('_encrypted')
                                if 0 < tmp_offset:  value = enc_value[0:tmp_offset]
                    elif key.endswith('_pass2') or key.endswith('_pass_python'):
                        print('  decrypt by Python for {v}'.format(v=value))
                        dec_value = Properties.cipher.decrypt(value)
                        if debug:    print ('  DEBUG] %s  key:value= %s ==> %s ' % (fname, key,dec_value))
                        if dec_value is not None:
                            if key.endswith('_pass2'):
                                trunc_count = 1
                            elif key.endswith('_pass_python'):
                                trunc_count = len('_python')
                            key = key[:-trunc_count]
                            value = dec_value
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
        #fname = '%s.%s' % (MODULE_NAME, 'get')
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
    
    plain_text = 'test'
    print('  {p} ==> [{e}]'.format(p=plain_text, e=myCipher().encrypt(plain_text)))

  
if __name__ == "__main__":
    if 2 > len(sys.argv):
        test()
    else:
        text = None
        action = 'decrypt'
        for arg in sys.argv[1:]:
            if arg == "encrypt" or arg == "encrypt" or arg == "both":
                action = arg
            elif arg == "encode":
                action = "encrypt"
            elif arg == "decode":
                action = "decrypt"
            else:
                text = arg
        
        if text is not None:
            cipher = Properties.cipher
            if cipher is None:
                cipher = myCipher()
            if (action == "both"):
                encrypted = cipher.encrypt(text)
                print("{t} ==> {e} ==> {d}".format(t=text, e=encrypted, d=cipher.decrypt(encrypted)))
            else:
                print(cipher.encrypt(text) if action== "encrypt" else cipher.decrypt(text))
    
