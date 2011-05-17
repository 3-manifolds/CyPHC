from distutils.core import setup
from distutils.extension import Extension
from Cython.Distutils import build_ext
import os.path as path
import subprocess
from os import environ
import sys
from sys import platform

Adaobjs = [path.join('Ada_build', 'cy2ada'),
           path.join('Ada_build', 'mv_glue.o'),
           path.join('Ada_build', 'cell_stack.o'),
           path.join('Ada_build', 'form_lp.o'),
           path.join('Ada_build', 'index_tree_lp.o'),
           path.join('Ada_build', 'zero_index_tree.o'),
           path.join('Ada_build', 'one_level_lp.o'),
           path.join('Ada_build', 'mixed_volume.o'),
           path.join('Ada_build', 'relation_table.o'),
           path.join('Ada_build', 'prepare_for_mv.o')]
gnatlink_cmd = 'gnatlink'

# Use the gnatlink command in place of the gcc linker
if platform == 'darwin':
    gnatlink_cmd= '/usr/local/ada-4.3/bin/gnatlink'
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
