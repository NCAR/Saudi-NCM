#!/bin/bash

/home/x_fisherh/bin/snuff -l rsync
cd /lustre/project/k1206/x_fisherh/distrib/GWPME/
ls
#/usr/bin/rsync -e 'ssh -i /home/x_fisherh/.ssh/saudi_ssh_key' -avC --exclude '*d03*' --delete mobile_grib/ opc@158.101.229.65:/data/latest >& /home/x_fisherh/datlog/grib_web_distrib.log
/usr/bin/rsync -e 'ssh -i /home/x_fisherh/.ssh/saudi_ssh_key' -avC --delete mobile_grib/ opc@158.101.229.65:/data/latest >& /home/x_fisherh/datlog/grib_web_distrib.log