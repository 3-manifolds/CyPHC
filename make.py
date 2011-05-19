from os import environ
import sys
from subprocess import Popen, PIPE, call

def which(executable):
    execpath, errs = Popen(['which', executable],
                           stdout=PIPE, stderr=PIPE).communicate()
    if execpath:
        return execpath.strip()

# Find gnats.
gnatmake = which('gnatmake')
gnatbind = which('gnatbind')

# Customize fallbacks for your environments.
if sys.platform == 'darwin':
    if not gnatmake:
        gnatmake = '/usr/local/ada-4.3/bin/gnatmake'
    if not gnatbind:
        gnatbind = '/usr/local/ada-4.3/bin/gnatbind'
    
if gnatmake is None or gnatbind is None:
    print "gnat is not in your path"
    sys.exit()

# Set make variables according to platform.

args = [
    'make',
    'GNATMAKE=%s'%gnatmake,
    'GNATBIND=%s'%gnatbind,
    ]

if sys.platform == 'darwin':
    args.append('GNATFLAGS=-gnat95 -gnatv -O3 -gnatp -gnatf ')
elif sys.platform == 'linux2':
    args += ['GNATFLAGS=-gnat95 -gnatv -O3 -gnatp -gnatf -fPIC ',
             'GCCFLAGS=-fPIC' 
             ]
else:
    print 'Unknown platform'
    sys.exit()
    
call(args)
