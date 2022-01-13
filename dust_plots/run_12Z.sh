#This script calls both d1 and d2 12Z dust plotting perl scripts
#We need to do this in a shell because we need to load ncl first 
module load ncl
/usr/bin/perl /lustre/project/k1206/x_fisherh/dust_plots/plot_DUST_all_d112Z.pl
/usr/bin/perl /lustre/project/k1206/x_fisherh/dust_plots/plot_DUST_all_d212Z.pl
