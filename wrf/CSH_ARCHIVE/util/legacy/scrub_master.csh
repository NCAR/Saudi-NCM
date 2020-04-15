#!/bin/csh
#
# Replace <RANGE> with name of run

# scrub input data
#

scrub -d 6 /raid/input

# scrub job logs
#

scrub -d 7 /raid/fdda-atec/GMODJOBS/GW<RANGE>/logs
scrub -d 7 /raid/fdda-atec/GMODJOBS/GW<RANGE>/tmp

# scrub cycle directories
#

scrub -d 3 /raid/fdda-atec/cycles/GW<RANGE>/<RANGE>/web
scrub -d 2 /raid/fdda-atec/cycles/GW<RANGE>/<RANGE>/restart_files
scrub -d 2 /raid/fdda-atec/cycles/GW<RANGE>/<RANGE>/merged_gbc/wrf*

# scrub archive files
#

scrub -d 20 /raid/fdda-atec/archive/GW<RANGE>/*fcst*

