
sub do_grid_plots_gcat
{
my ($hourly_file,$ndom, $cycle, $valid_time, $dest) = @_;

$ENV{'NCARG_ROOT'} = "/opt/ncarg";
$ENV{'NCARG_LIB'} = "/opt/ncarg/lib";
$ENV{'STATIONLIST'} = "$MM5HOME/cycle_code/CONSTANT_FILES/RIP/stationlist";
$ENV{'HIRESMAP'} = "$MM5HOME/cycle_code/CONSTANT_FILES/RIP/${RANGE}_map.ascii";
$ENV{'RANGEMAP'} = "$MM5HOME/cycle_code/CONSTANT_FILES/RIP/${RANGE}_map.ascii";
$ENV{'stationg3'} = "$MM5HOME/cycle_code/CONSTANT_FILES/RIP/stationg3";
$ENV{'stationg4'} = "$MM5HOME/cycle_code/CONSTANT_FILES/RIP/stationg4";
#$ENV{'RIP_ROOT'} = "$MM5HOME/cycle_code/CONSTANT_FILES/RIP";
#$RIP_ROOT = "$MM5HOME/cycle_code/CONSTANT_FILES/RIP";
$ENV{'RIP_ROOT'} = "/raid/GMODJOBS/GCAT/postprocs";

@MONTHS=( '01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12');
@SEASONS=( 'winter', 'winter', 'winter', 'winter', 'summer', 'summer', 'summer', 'summer', 'summer', 'summer', 'summer', 'winter', 'winter');

$RIPDP_EXE = "$MM5HOME/cycle_code/EXECUTABLE_ARCHIVE/ripdp_new.exe";
$RIP_EXE = "$MM5HOME/cycle_code/EXECUTABLE_ARCHIVE/rip_new.exe";
$RIPDP_OBS = "$MM5HOME/cycle_code/EXECUTABLE_ARCHIVE/ripdp_obs.exe";
$RIP_OBS = "$MM5HOME/cycle_code/EXECUTABLE_ARCHIVE/rip_obs.exe";

system("$MustHaveDir $PLOTS_DIR");
system("$MustHaveDir $PLOTS_DIR/ripscr_g");
system("$MustHaveDir $PLOTS_DIR/ripgrid");
chdir $PLOTS_DIR;

system("scrub 1/6 $PLOTS_DIR/ripscr_g");
system("rm -f $PLOTS_DIR/ripgrid/*");

@parts = split("DOMAIN",$hourly_file);
$filen = @parts[0];
$range = substr( @parts[1], 2, 10);

foreach $d ( 1..$ndom) {
   $fn = $filen . "DOMAIN" . $d . "." . $range;

   chdir $PLOTS_DIR;
   chdir ripscr_g;
   system("$RIPDP_EXE Domain_$d $fn > $PLOTS_DIR/ripdp_g$d.log 2>&1");

   open(XTIMES, "Domain_$d.xtimes");
   $ntimes = <XTIMES>; 
   while ( $xtin = <XTIMES>) {
      $xt = $xtin;
   }
   close(XTIMES);
   chomp($xt);

   chdir $PLOTS_DIR;
   chdir ripgrid;

   system("cat $RIP_ROOT/config_GRM/domain$d.in | sed s/plttime/$xt/g > domain$d.in");

   system("$RIP_EXE -f ../ripscr_g/Domain_$d domain$d  > $PLOTS_DIR/rip_g$d.log 2>&1");
     &conv2gif_grid( "domain$d.cgm", $d);

     chdir $PLOTS_DIR;
     $mkdir_command = "mkdir $JOB_LOC/config";
##   system( "ssh 4dwx\@128.117.200.214 $mkdir_command" );
     system( "ssh $DEST_SERVER $mkdir_command" );
##   system("scp -pr gifs_ugui/config/g$d\* 4dwx\@128.117.200.214:$JOB_LOC/config" );
     system("scp -pr gifs_ugui/config/g$d\* $DEST_SERVER:$JOB_LOC/config" );

}

}
1;
