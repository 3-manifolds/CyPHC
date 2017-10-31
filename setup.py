from distutils.core import setup
from distutils.extension import Extension
from Cython.Distutils import build_ext
import os.path as path
from subprocess import Popen, PIPE
from os import environ
import sys, site

def which(executable):
    execpath, errs = Popen(['which', executable],
                           stdout=PIPE, stderr=PIPE).communicate()
    if execpath:
        return execpath.strip()

gnatlink = which('gnatlink')

Adaobjs = [path.join('PHCbuild', 'cy2ada')]
#           path.join('PHCbuild', 'mv_glue.o'),
#
#           path.join('PHCbuild', 'cell_stack.o'),
#           path.join('PHCbuild', 'form_lp.o'),
#           path.join('PHCbuild', 'index_tree_lp.o'),
#           path.join('PHCbuild', 'zero_index_tree.o'),
#           path.join('PHCbuild', 'one_level_lp.o'),
#           path.join('PHCbuild', 'mixed_volume.o'),
#           path.join('PHCbuild', 'relation_table.o'),
#           path.join('PHCbuild', 'prepare_for_mv.o')
#           ]

if sys.platform == 'darwin':
    if not gnatlink:
        from make import darwin_gnat_fallback_dir
        gnatlink= darwin_gnat_fallback_dir + '/gnatlink'
    Adaobjs += [site.PREFIXES[0] + '/lib/libpython2.7.dylib']
# Use the gnatlink command in place of the gcc linker
# NOTE: newer versions of gnatlink do not accept the -C flag.
environ['LDSHARED'] = gnatlink
#environ['LDFLAGS'] = '-C -shared'
environ['LDFLAGS'] = '-shared'

src = ['phc.pyx']
inc = [path.join('PHCsource','src','Ada','Root_Counts','MixedVol')]

setup(
    name = 'phc',
    version = '1.0',
    description = 'Python interface to PHC',
    author = 'Marc Culler',
    cmdclass = {'build_ext': build_ext},
    ext_modules = [Extension('phc',
                             sources=src,
                             include_dirs=inc,
                             extra_objects=Adaobjs)]
    )
