from os import environ
from sys import platform
from subprocess import Popen, PIPE, call

if platform == 'darwin':
    gnatmake, errors = Popen(['which', 'gnatmake'],
                             stdin=PIPE, stderr=PIPE).communicate()
    gnatmake = gnatmake.strip()
    if not gnatmake:
        gnatmake = '/usr/local/ada-4.3/bin/gnatmake'
    gnatlink, errors = Popen(['which', 'gnatlink'],
                             stdin=PIPE, stderr=PIPE).communicate()
    gnatlink = gnatlink.strip()
    if not gnatlink:
        gnatlik = '/usr/local/ada-4.3/bin/gnatlink'
        
    args = [
        'make',
        'GNATMAKE=%s'%gnatlink,
        'GNATBIND=%s'%gnatbind,
        'GNATFLAGS=-gnat95 -gnatv -O3 -gnatp -gnatf '
        ]
elif platform == 'linux2':
    args = [
        'make',
        'GNATMAKE=gnatmake',
        'GNATBIND=gnatbind',
        'GNATFLAGS=-gnat95 -gnatv -O3 -gnatp -gnatf -fPIC ',
        'GCCFLAGS=-fPIC' 
        ]
else:
    print 'Unknown platform'
    
call(args)
    
