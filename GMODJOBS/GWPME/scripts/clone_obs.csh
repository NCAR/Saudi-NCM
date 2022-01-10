#!/bin/csh


echo $1

set cycle = $1
set RUNDIR  = $2

# Configure for the source of the MM5-RTFDDA qc-obs data...
set range='fddasys@gs-c2:' 
set CLONEDIR = "/raid/cycles/GMWSMR/WSMR"

cd $RUNDIR/$cycle
set echo

scp $range$CLONEDIR/$cycle/\*_s .


exit(0)

