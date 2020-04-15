import os 
import subprocess

def whereis(program):
    # check if a program on the PATH
    # http://jimmyg.org/blog/2009/working-with-python-subprocess.html#introducing-subprocess
    # whereis('echo')
    for path in os.environ.get('PATH', '').split(':'):
        if os.path.exists(os.path.join(path, program)) and \
           not os.path.isdir(os.path.join(path, program)):
            return os.path.join(path, program)
    return None

def runcmd(mycmd):
    p = subprocess.Popen(mycmd, shell=True,
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT)
    return p.communicate()
