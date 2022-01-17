#This script calls both d1 and d2 12Z dust plotting perl scripts
#We need to do this in a shell because we need to load ncl first 
module load ncl
# module load does not work from cron so we export and hard code
export PATH=$PATH:/sw/xc40cle7/ncl/6.6.2/sles15_binary/bin
export NCARG_ROOT=/sw/xc40cle7/ncl/6.6.2/sles15_binary
#We can't put the DUST_color colormap in NCARG root directory so need to point to the directory it is in
cp /lustre/project/k1206/x_fisherh/dust_plots/plot_DUST_all.ncl /lustre/project/k1206/x_fisherh/distrib/GWPME/DUST_plots
cp /lustre/project/k1206/x_fisherh/dust_plots/plot_AOD_600nm.ncl /lustre/project/k1206/x_fisherh/distrib/GWPME/DUST_plots
cp /lustre/project/k1206/x_fisherh/dust_plots/plot_DUST_surface.ncl /lustre/project/k1206/x_fisherh/distrib/GWPME/DUST_plots
export NCARG_COLORMAPS=/lustre/project/k1206/x_fisherh/dust_plots:$NCARG_ROOT/lib/ncarg/colormaps
/usr/bin/perl /lustre/project/k1206/x_fisherh/dust_plots/plot_DUST_all_d112Z.pl
/usr/bin/perl /lustre/project/k1206/x_fisherh/dust_plots/plot_DUST_all_d212Z.pl
