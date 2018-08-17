Build Instructions
==================

If you have a copy of the PHC source files, create a symbolic link
named PHCroot pointing to the PHC root directory. Otherwise, run the
script getPHC.sh to download the source and create the symbolic link.

Install takes two steps at the moment::

   python make.py
   python setup.py install

WARNING: Requires Cython >= 13.0

HINT: On OS X, if gnat is not in your path, edit the first line of make.py

License
=======

Copyright 2011-present by Marc Culler and others.

All parts of this package are released under the `GNU General Public
License, version 2 <http://www.gnu.org/licenses/gpl-2.0.txt>`_ or (at
your discretion) any later version as published by the Free Software
Foundation.