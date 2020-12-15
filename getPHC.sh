#!/bin/bash
PHCDIR=PHCpack-2.4.82
PHCTAR=v2.4.82.tar.gz
PHCURL=https://github.com/janverschelde/PHCpack/archive/$PHCTAR
curl -L -O $PHCURL 
tar xvfz $PHCTAR
rm $PHCTAR
ln -s $PHCDIR PHCsource
