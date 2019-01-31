darwin_gnat_fallback_dir = '/usr/local/gnat/bin'
#darwin_gnat_fallback_dir = '/pkgs/gnat/bin'

from os import environ
import sys, subprocess

def which(executable):
    ans = subprocess.check_output(['which', executable])
    return ans.decode('ascii').strip()

if __name__ == '__main__':
    # Find gnats.
    gnatmake = which('gnatmake')
    gnatbind = which('gnatbind')

    # Customize fallbacks for your environments.
    if sys.platform == 'darwin':
        if not gnatmake:
            gnatmake = darwin_gnat_fallback_dir + '/gnatmake'
        if not gnatbind:
            gnatbind = darwin_gnat_fallback_dir + '/gnatbind'
    
    if gnatmake is None or gnatbind is None:
        print('gnat is not in your path')
        sys.exit()

    # Set make variables according to platform.

    args = [
        'make',
        'GNATMAKE=%s'%gnatmake,
        'GNATBIND=%s -shared'%gnatbind,
        'PYTHON=%s'%sys.executable, 
        ] + sys.argv[1:]

    if sys.platform == 'darwin':
        args.append('GNATFLAGS=-gnat05 -gnatv -O3 -gnatp -gnatf ')
    elif sys.platform == 'linux2':
        args += ['GNATFLAGS=-gnat05 -gnatv -O3 -gnatp -gnatf -fPIC ',
                 'GCCFLAGS=-fPIC' 
                 ]
    else:
        print('Unknown platform')
        sys.exit()
    
    subprocess.call(args)
