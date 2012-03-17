#!/bin/bash
export PHCDIR=PHCv2_3p
export PHCTAR=$PHCDIR.tar.gz
curl -O http://homepages.math.uic.edu/%7Ejan/$PHCTAR
tar xvfz $PHCTAR
rm $PHCTAR
ln -s $PHCDIR PHCsource