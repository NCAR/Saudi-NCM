
sub do_plots_ncl {
my ($work_dir, $hourly_file, $ndom, $cycle, $valid_time, $dest, $dest_small) = @_;

$DOIT = 1;

print "\n";
print 
"=============================================================================";
print "do_plots_ncl (work_dir, $hourly_file, $ndom, $cycle, $valid_time, $dest, $dest_small)\n";
print "\nPlot WRF output with NCL.\n\n";

# Can only process WRF output files with NCL
if (! $IS_WRF) {
    print "NCL plots cannot process MM5 data, skip...\n"; 
#   return -1;
} 

# Make sure we are using the write NCAR Graphic library
$ENV{'NCARG_ROOT'} = "/opt/ncarg";
$ENV{'NCARG_LIB'} = "/opt/ncarg/lib";
$ENV{'NCARG_RANGS'} = $NCARG_RANGS_DIR;

# Need to know the season for the color tables.
@SEASONS=( 'winter', 'winter', 'winter', 'summer', 'summer', 'summer', 'summer', 'summer', 'summer', 'summer', 'winter', 'winter');

# Where the list of stations with coordinates resides
$ENV{'STATIONLIST'} = "/raid/cycles/WRFTEST/RIP4/stationlist";

# Where the high-resolution maps reside
$ENV{'HIRESMAP'} = "$RIP_ROOT/${RANGE}_map.ascii";
$ENV{'RANGEMAP'} = "$RIP_ROOT/${RANGE}_map.ascii";
$ENV{'stationg3'} = "$RIP_ROOT/stationg3";
$ENV{'stationg4'} = "$RIP_ROOT/stationg4";
#

# Working directory
system("$MustHaveDir $PLOT_NCL_DIR");
chdir "$PLOT_NCL_DIR";

# Clean-up before working
system("rm -f $PLOTS_NCL_DIR/*.ps");
system("rm -f $PLOTS_NCL_DIR/*.gif");


# Extract the root and the date stamp of the WRF output file name
@parts = split("d0",$hourly_file);
$filen = @parts[0];
$wrf_date_string = substr(@parts[1],2);

# Figure out the season from the date
$month = substr( $valid_time, 4, 2);
$season = @SEASONS[$month-1];

# NCL will parse the environment to get those variables 
# The variables on the righ-hand side are defined in the calling script 
# do_output.pl and passed as global variable

@PS_FILETYPE = ("maps","other","skewT","surface","upperAir");

#------------------------------------------------------------
# Format $cycle time to match the times in the WRF file names
#------------------------------------------------------------
$year = substr($cycle,0,4);
$month = substr($cycle,4,2);
$day = substr($cycle,6,2);
$hour = substr($cycle,8,2);

$wrf_date = join "-", $year, $month, $day ;
$wrf_time = join ":", $hour, '00', '00' ;
$wrf_cycle = join "_", $wrf_date, $wrf_time ;


$ENV{'RANGE'}        = $RANGE;
$ENV{'NDOMAINS'}     = "$ndom";
$ENV{'INDATA_DIR'}   = $work_dir;
$ENV{'PLOTS_DIR'}    = "$PLOT_NCL_DIR";
$ENV{'NCL_CODE_DIR'} = "$POSTPROCS_DIR/NCL_WRF";
$ENV{'NML_DIR'}      = "$ENV{'NCL_CODE_DIR'}/seasonal_info";
$ENV{'CYCLE_TIME'}     = "$wrf_cycle";

print "setenv RANGE $ENV{'RANGE'}\n";
print "setenv NDOMAINS $ENV{'NDOMAINS'}\n";
print "setenv INDATA_DIR $ENV{'INDATA_DIR'}\n";
print "setenv PLOTS_DIR $ENV{'PLOTS_DIR'}\n";
print "setenv NCL_CODE_DIR $ENV{'NCL_CODE_DIR'}\n";
print "setenv NML_DIR $ENV{'NML_DIR'}\n";
print "setenv CYCLE_TIME $ENV{'wrf_cycle'}\n";

$NCL_CODE_DIR = $ENV{'NCL_CODE_DIR'};

# Need file psadilookup.dat

system ("cp -f $NCL_CODE_DIR/psadilookup.dat .");

# Loop over the domains
foreach $d ( 1..$ndom) {

   print "\n".
"-----------------------------------------------------------------------------";
   print "\nCreating plots for domain $d\n";

   # Domain/time dependant variable passed to NCL
   $ENV{'ND'} = $d;
   # Expect file name: wrfout_d01
   $ENV{'WRF_FILE_NAME'} = substr ($hourly_file,0,9)."$d";
   $ENV{'PREV_WRF_FILE_NAME'} = "$ENV{'WRF_FILE_NAME'}"."-1";
   $ENV{'season'} = $season;

   print "setenv ND $ENV{'ND'}\n";
   print "setenv WRF_FILE_NAME $ENV{'WRF_FILE_NAME'}\n";
   print "setenv PREV_WRF_FILE_NAME $ENV{'PREV_WRF_FILE_NAME'}\n";
   print "setenv season $ENV{'season'}\n";

   # Generate surface plots
   print "\n".
"-----------------------------------------------------------------------------";
   print "\nCreating surface plots for domain $d\n";

   $command = "$ENV{'NCARG_ROOT'}/bin/ncl $NCL_CODE_DIR/wrf_surface.ncl";
   print "\n$command\n\n";
   system  ($command) if ($DOIT);

   # Convert postscript output files into gifs

   # Upper-air plots
   print "\n".
"-----------------------------------------------------------------------------";
   print "\nCreating upper air plots for domain $d\n";

   $command = "$ENV{'NCARG_ROOT'}/bin/ncl $NCL_CODE_DIR/wrf_upperAir.ncl";
   print "\n$command\n\n";
   system  ($command) if ($DOIT);

   # Maps plots
   print "\n".
"-----------------------------------------------------------------------------";
   print "\nCreating maps plots for domain $d\n";

   $command = "$ENV{'NCARG_ROOT'}/bin/ncl $NCL_CODE_DIR/wrf_maps.ncl";
   print "\n$command\n\n";
   system  ($command) if ($DOIT);

   # Skewt
   print "\n".
"-----------------------------------------------------------------------------";
   print "\nCreating skewt plots for domain $d\n";

   $command = "$ENV{'NCARG_ROOT'}/bin/ncl $NCL_CODE_DIR/wrf_skewT.ncl";
   print "\n$command\n\n";
   system  ($command) if ($DOIT);

   # Others
   print "\n".
"-----------------------------------------------------------------------------";
   print "\nCreating other plots for domain $d\n";

   $command = "$ENV{'NCARG_ROOT'}/bin/ncl $NCL_CODE_DIR/wrf_other.ncl";
   print "\n$command\n\n";
   system  ($command) if ($DOIT);

   # Check for postscript files
   @PS = `find . -name "\*.ps" -print `;

   if ($#PS < 0) {
       print "\nERROR: Cannot find any postscript files, check NCL scripts\n";
       next;
   }

   # Converting to gif format
   print "\n".
"-----------------------------------------------------------------------------";
   print "\nConverting the plots into gif format for domain $d\n";


   foreach $ps_type (@PS_FILETYPE) { 

      $ND = $d;
      $GIF_DIR = ".";
      $PS_DIR = ".";
#-------------------------------------------------------------------------
# +adjion splits the frames into seperate gif files
#-------------------------------------------------------------------------

      $command = 
      "convert +adjoin -size 800x800 -crop 0x0".
      "${PS_DIR}/wrf_${ps_type}_d0${ND}.ps ".
      "${GIF_DIR}/wrf_${ps_type}_d0${ND}.gif";

      print "\n$command\n";
      system  ($command);

    }

   #-------------------------------------------------------------------------
   # Surface files
   #-------------------------------------------------------------------------
   system ("mv ${GIF_DIR}/wrf_surface_d0${ND}.gif.0 ${GIF_DIR}/d${ND}_2mtempA10mwnd.gif");
   system ("mv ${GIF_DIR}/wrf_surface_d0${ND}.gif.1 ${GIF_DIR}/d${ND}_ms500h.gif");
   system ("mv ${GIF_DIR}/wrf_surface_d0${ND}.gif.2 ${GIF_DIR}/d${ND}_ms925h.gif");
   system ("mv ${GIF_DIR}/wrf_surface_d0${ND}.gif.3 ${GIF_DIR}/d${ND}_pblhgt.gif");
   system ("mv ${GIF_DIR}/wrf_surface_d0${ND}.gif.4 ${GIF_DIR}/d${ND}_2mmxraA10mwnd.gif");
   system ("mv ${GIF_DIR}/wrf_surface_d0${ND}.gif.5 ${GIF_DIR}/d${ND}_lsigrhA10mwnd.gif");
   system ("mv ${GIF_DIR}/wrf_surface_d0${ND}.gif.6 ${GIF_DIR}/d${ND}_lsigheA10mwnd.gif");
   system ("mv ${GIF_DIR}/wrf_surface_d0${ND}.gif.7 ${GIF_DIR}/d${ND}_1hrpre.gif");
   system ("mv ${GIF_DIR}/wrf_surface_d0${ND}.gif.8 ${GIF_DIR}/d${ND}_radar.gif");
   system ("mv ${GIF_DIR}/wrf_surface_d0${ND}.gif.9 ${GIF_DIR}/d${ND}_visibility.gif");
   system ("mv ${GIF_DIR}/wrf_surface_d0${ND}.gif.10 ${GIF_DIR}/d${ND}_sfcstrm.gif");
   system ("mv ${GIF_DIR}/wrf_surface_d0${ND}.gif.11 ${GIF_DIR}/d${ND}_cape.gif");
   system ("mv ${GIF_DIR}/wrf_surface_d0${ND}.gif.12 ${GIF_DIR}/d${ND}_cin.gif");
   system ("mv ${GIF_DIR}/wrf_surface_d0${ND}.gif.13 ${GIF_DIR}/d${ND}_snowcv.gif");

   #-------------------------------------------------------------------------
   # Upper air
   #-------------------------------------------------------------------------
   system ("mv ${GIF_DIR}/wrf_upperAir_d0${ND}.gif.0 ${GIF_DIR}/d${ND}_850htt.gif");
   system ("mv ${GIF_DIR}/wrf_upperAir_d0${ND}.gif.1 ${GIF_DIR}/d${ND}_850rhw.gif");
   system ("mv ${GIF_DIR}/wrf_upperAir_d0${ND}.gif.2 ${GIF_DIR}/d${ND}_700tem.gif");
   system ("mv ${GIF_DIR}/wrf_upperAir_d0${ND}.gif.3 ${GIF_DIR}/d${ND}_700rhw.gif");
   system ("mv ${GIF_DIR}/wrf_upperAir_d0${ND}.gif.4 ${GIF_DIR}/d${ND}_500wnd.gif");
   system ("mv ${GIF_DIR}/wrf_upperAir_d0${ND}.gif.5 ${GIF_DIR}/d${ND}_500rhw.gif");
   system ("mv ${GIF_DIR}/wrf_upperAir_d0${ND}.gif.6 ${GIF_DIR}/d${ND}_600mWV.gif");
   system ("mv ${GIF_DIR}/wrf_upperAir_d0${ND}.gif.7 ${GIF_DIR}/d${ND}_300wnd.gif");

   #-------------------------------------------------------------------------
   # Sounding, skewT files (number of soundings differ for each domain)
   #-------------------------------------------------------------------------
   system ("mv ${GIF_DIR}/wrf_skewT_d0${ND}.gif.0 ${GIF_DIR}/d${ND}_sndg01.gif")
       if (-e "${GIF_DIR}/wrf_skewT_d0${ND}.gif.0");

   system ("mv ${GIF_DIR}/wrf_skewT_d0${ND}.gif.1 ${GIF_DIR}/d${ND}_sndg02.gif")
       if (-e "${GIF_DIR}/wrf_skewT_d0${ND}.gif.1");

   system ("mv ${GIF_DIR}/wrf_skewT_d0${ND}.gif.2 ${GIF_DIR}/d${ND}_sndg03.gif")
       if (-e "${GIF_DIR}/wrf_skewT_d0${ND}.gif.2");

   system ("mv ${GIF_DIR}/wrf_skewT_d0${ND}.gif.3 ${GIF_DIR}/d${ND}_sndg04.gif")
       if (-e "${GIF_DIR}/wrf_skewT_d0${ND}.gif.3");

   #-------------------------------------------------------------------------
   # "Other" files
   #-------------------------------------------------------------------------
   system ("mv ${GIF_DIR}/wrf_other_d0${ND}.gif.0 ${GIF_DIR}/d${ND}_vissat.gif");
   system ("mv ${GIF_DIR}/wrf_other_d0${ND}.gif.1 ${GIF_DIR}/d${ND}_slhflux.gif");
   system ("mv ${GIF_DIR}/wrf_other_d0${ND}.gif.2 ${GIF_DIR}/d${ND}_sshflux.gif");
   system ("mv ${GIF_DIR}/wrf_other_d0${ND}.gif.3 ${GIF_DIR}/d${ND}_soilt1.gif");
   system ("mv ${GIF_DIR}/wrf_other_d0${ND}.gif.4 ${GIF_DIR}/d${ND}_soilma.gif");

   #-------------------------------------------------------------------------
   # Map files
   #-------------------------------------------------------------------------
   system ("mv ${GIF_DIR}/wrf_maps_d0${ND}.gif.0 ${GIF_DIR}/d${ND}_g1lvw.gif");
   system ("mv ${GIF_DIR}/wrf_maps_d0${ND}.gif.1 ${GIF_DIR}/d${ND}_g1luse.gif");
   system ("mv ${GIF_DIR}/wrf_maps_d0${ND}.gif.2 ${GIF_DIR}/d${ND}_g1terr.gif");


}

# Check for any gif file to transfer

@GIFS = `find . -name "\*.gif" -print `;

if ($#GIFS >= 0) { 

    # Copy over the gifs files to the server.

   print "\n".
"-----------------------------------------------------------------------------";
    print "Copy over images at carson\@atec-server:/d1/www/htdocs/images/epg/rtfdda/gifs/$valid_time/.\n\n";

    foreach $f (`ls -1 *.gif`) {

      chomp($f);

      $command = "rsync -e 'ssh -i $KEY' -avzC $f $dest/$valid_time/.";
      print  "$command\n";
      system("$command") if ($DOIT);
    }

}

}
1

