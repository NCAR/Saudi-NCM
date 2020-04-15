#!/bin/bash

/home/x_fisherh/bin/snuff -l rsync
cd /lustre/project/k1206/x_fisherh/distrib/GWPME/
ls
/usr/bin/rsync -avC --delete cycles/ cfo-kaust@94.97.82.134:/data/cycles>& /home/x_fisherh/datlog/cycles_distrib.log
/usr/bin/rsync -avC --delete grib/ cfo-kaust@94.97.82.134:/data/grib >& /home/x_fisherh/datlog/grib_distrib.log
/usr/bin/rsync -avC --delete --exclude '*d03*' netcdf/ cfo-kaust@94.97.82.134:/data/netcdf >& /home/x_fisherh/datlog/netcdf_distrib.log
