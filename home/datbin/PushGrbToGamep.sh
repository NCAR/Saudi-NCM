#!/usr/bin/sh
 #/home/x_fisherh/sshpass/sshpass-1.06/sshpass -f /home/x_fisherh/datbin/ingest/ecmwfp sftp -o Port=36979 shaheen@37.216.246.197:/incoming/GRIB1/ <<< $'put d03_2018-12-13_*:00:00.grb'

gribfile=$1
echo $gribfile
echo $1
 /home/x_fisherh/sshpass/sshpass-1.06/sshpass -f /home/x_fisherh/datbin/ingest/ecmwfp sftp -o Port=36979 shaheen@37.216.246.197:/incoming/GRIB1/ <<< $'put '$gribfile' '
