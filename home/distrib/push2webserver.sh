#!/bin/bash

/lustre/project/k1206/x_fisherh/bin/snuff -l rsync
cd /lustre/project/k1206/x_fisherh/distrib/GWPME/
ls
/usr/bin/rsync -e 'ssh -i /home/x_fisherh/.ssh/saudi_ssh_key' -avC --exclude '*d03*' --delete mobile_grib/ opc@158.101.229.65:/data/latest >& /lustre/project/k1206/x_fisherh/datlog/grib_web_distrib.log
#/usr/bin/rsync -e 'ssh -i /home/x_fisherh/.ssh/saudi_ssh_key' -avC --delete mobile_grib/ opc@158.101.229.65:/data/latest >& /home/x_fisherh/datlog/grib_web_distrib.log
/usr/bin/rsync -e 'ssh -i /home/x_fisherh/.ssh/saudi_ssh_key' -avCL --delete cycles opc@158.101.229.65:/var/www/html/projects/GWPME/web >& /lustre/project/k1206/x_fisherh/datlog/images_web_distrib.log
/usr/bin/rsync -e 'ssh -i /home/x_fisherh/.ssh/saudi_ssh_key' -avCL --delete DUST_plots opc@158.101.229.65:/var/www/html/projects/GWPME >& /lustre/project/k1206/x_fisherh/datlog/images_web_distrib.log
/usr/bin/rsync -e 'ssh -i /home/x_fisherh/.ssh/saudi_ssh_key' -avCL --delete gifs opc@158.101.229.65:/var/www/html/projects/GWPME/web >& /lustre/project/k1206/x_fisherh/datlog/images_web_distrib.log
cd /scratch/x_fisherh/cycles/GWPME/GRM
ls
/usr/bin/rsync -e 'ssh -i /home/x_fisherh/.ssh/saudi_ssh_key' -avCL --delete meteograms opc@158.101.229.65:/var/www/html/projects/GWPME >& /lustre/project/k1206/x_fisherh/datlog/meteograms_web_distrib.log
/usr/bin/rsync -e 'ssh -i /home/x_fisherh/.ssh/saudi_ssh_key' -avCL --delete timesgraphs opc@158.101.229.65:/var/www/html/projects/GWPME >& /lustre/project/k1206/x_fisherh/datlog/timegraphs_web_distrib.log
