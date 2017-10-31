#!/bin/bash
https://github.com/janverschelde/PHCpack/archive/v2.3.97.tar.gz
PHCDIR=PHCpack-2.3.97
PHCTAR=v2.3.97.tar.gz
PHCURL=https://github.com/janverschelde/PHCpack/archive/$PHCTAR
curl -L -O $PHCURL 
tar xvfz $PHCTAR
rm $PHCTAR
ln -s $PHCDIR PHCsource