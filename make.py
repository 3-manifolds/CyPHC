from os import environ
from sys import platform
from subprocess import call

if platform == 'darwin':
    args = [
        'make',
        'GNATMAKE=/usr/local/ada-4.3/bin/gnatmake',
        'GNATBIND=/usr/local/ada-4.3/bin/gnatbind',
        ('GNATFLAGS=-gnat95 -gnatv -O3 -gnatp -gnatf '+
         '--GNATBIND="gnatbind -static"')
        ]
elif platform == 'linux2':
    args = [
        'make',
        'GNATMAKE=gnatmake',
        'GNATBIND=gnatbind',
        ('GNATFLAGS=-gnat95 -gnatv -O3 -gnatp -gnatf -fPIC '+
         '--GNATBIND="gnatbind -static"')
        ]
else:
    print 'Unknown platform'
    
call(args)
    
