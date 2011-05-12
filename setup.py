from distutils.core import setup
from distutils.extension import Extension
from Cython.Distutils import build_ext
import os.path as path
import subprocess
from os import environ
import sys
import subprocess

Adaobjs = [path.join('Ada_build', 'cy2ada')]

# Use the gnatlink command in place of the gcc linker
gnatlink_cmd = 'gnatlink'
if subprocess.call(['which', 'gnatlink']):
    gnatlink_cmd= '/usr/local/ada-4.3/bin/gnatlink'
    
if sys.platform == 'darwin':
    Adaobjs += ['/Library/Frameworks/Python.framework/Versions/2.7/lib/libpython2.7.dylib']
environ['LDSHARED'] = gnatlink_cmd
environ['LDFLAGS'] = '-C -shared'
#

src = ['phc.pyx']
               
setup(
    name = 'phc',
    version = '1.0',
    description = 'Python interface to PHC',
    author = 'Marc Culler',
    cmdclass = {'build_ext': build_ext},
    ext_modules = [Extension('phc', sources=src,
                             extra_objects=Adaobjs)]
)
