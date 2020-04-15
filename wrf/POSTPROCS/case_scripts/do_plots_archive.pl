
sub do_plots
{
my ($hourly_file,$ndom, $cycle, $valid_time, $dest, $dest_small) = @_;

##
## Special version to do EPG plots based on WSMR Domain2...
##
$DO_EPG = 0;

$ENV{'NCARG_ROOT'} = "/opt/ncarg";
$ENV{'NCARG_LIB'} = "/opt/ncarg/lib";
$ENV{'STATIONLIST'} = "$MM5HOME/cycle_code/CONSTANT_FILES/RIP/stationlist";
$ENV{'HIRESMAP'} = "$MM5HOME/cycle_code/CONSTANT_FILES/RIP/${RANGE}_map.ascii";
$ENV{'RANGEMAP'} = "$MM5HOME/cycle_code/CONSTANT_FILES/RIP/${RANGE}_map.ascii";
$ENV{'stationg3'} = "$MM5HOME/cycle_code/CONSTANT_FILES/RIP/stationg3";
$ENV{'stationg4'} = "$MM5HOME/cycle_code/CONSTANT_FILES/RIP/stationg4";
$ENV{'RIP_ROOT'} = "$MM5HOME/cycle_code/CONSTANT_FILES/RIP";
$RIP_ROOT = "$MM5HOME/cycle_code/CONSTANT_FILES/RIP";

@SEASONS=( 'winter', 'winter', 'winter', 'summer', 'summer', 'summer', 'summer', 'summer', 'summer', 'summer', 'winter', 'winter');

$RIPDP_EXE = "$MM5HOME/cycle_code/EXECUTABLE_ARCHIVE/ripdp_new.exe";
$RIP_EXE = "$MM5HOME/cycle_code/EXECUTABLE_ARCHIVE/rip_new.exe";
$RIPDP_OBS = "$MM5HOME/cycle_code/EXECUTABLE_ARCHIVE/ripdp_obs.exe";
$RIP_OBS = "$MM5HOME/cycle_code/EXECUTABLE_ARCHIVE/rip_obs.exe";

system("$MustHaveDir $PLOTS_DIR");
system("$MustHaveDir $PLOTS_DIR/ripscr");
system("$MustHaveDir $PLOTS_DIR/ripscr_obs");
system("$MustHaveDir $PLOTS_DIR/riprun");
system("$MustHaveDir $PLOTS_DIR/riprun_obs");
chdir $PLOTS_DIR;

system("scrub 1/6 $PLOTS_DIR/ripscr");
system("scrub -d 1/6 $PLOTS_DIR/gifs_ugui");
system("scrub -d 1/6 $PLOTS_DIR/gifs_small");
system("rm -f $PLOTS_DIR/riprun/*");
system("rm -f $PLOTS_DIR/riprun_obs/*");
system("rm -f $PLOTS_DIR/ripscr_obs/*");

@parts = split("DOMAIN",$hourly_file);
$filen = @parts[0];
$range = substr( @parts[1], 2, 10);
$month = substr( $valid_time, 4, 2);
$season = @SEASONS[$month-1];

$do_tiles = $DO_TILES;
if ( $range == "GRM" && $do_tiles == 1 ) {

   foreach $tile (1..6) {
     $fn = $WORK_DIR . "/MMOUTPUT_D2" . $tile;
     chdir $PLOTS_DIR;
     chdir ripscr;
     system("$RIPDP_EXE Domain_$tile $fn > $PLOTS_DIR/ripdp$tile.log 2>&1");

     open(XTIMES, "Domain_$tile.xtimes");
     $ntimes = <XTIMES>; 
     while ( $xtin = <XTIMES>) {
        $xt = $xtin;
     }
     close(XTIMES);
     chomp($xt);
     $newfile = sprintf( "%s/ripscr/Domain_%s.xtimes", $PLOTS_DIR, $tile);

     if ( $xt >= 1.0 ) { 
       open(OUTFILE, ">$newfile") || do {
          warn "Cannot open output file: $newfile\n$!\n";
       };
       print OUTFILE "2\n";
       $xtprev = $xt - 1;
       $xtout = sprintf("%09.5f\n",$xtprev);
       print OUTFILE "$xtout";
       print OUTFILE "$xt";
       close(OUTFILE);
     }

     chdir $PLOTS_DIR;
     chdir riprun;

     system("cat $RIP_ROOT/namelists/Mdomain2.$season.$range | sed s/plttime/$xt/g > domain$tile.1");
     system("cat domain$tile.1 | sed s/rangename/$range/g > domain$tile.2");
     system("cat domain$tile.2 | sed s/cycle/$cycle/g > domain$tile.in");

     system("$RIP_EXE -f ../ripscr/Domain_$tile domain$tile  > $PLOTS_DIR/rip$tile.log 2>&1");
     &conv2gif( "domain$tile.cgm", $tile, $valid_time);

#    Now do the observation plots --- using a custom version of RIP
     chdir $PLOTS_DIR;
     chdir ripscr_obs;
     system("rm MMOUT.in");
     system("ln -s $fn MMOUT.in");
     system("$RIPDP_OBS Domain_$tile MMOUT.in > $PLOTS_DIR/ripdp_obs$tile.log 2>&1");

     chdir $PLOTS_DIR;
     $valid_m1 = &hh_advan_date( $valid_time, -1);

     chdir riprun_obs;
     system("rm -f indata");
     $obs_f1 = $WORK_DIR . "/" . $valid_m1 . "_qc_obs_for_assimilation_s";
     $obs_f2 = $WORK_DIR . "/" . substr($valid_time,0,10) . "_qc_obs_for_assimilation_s";
     system("cat $obs_f1 $obs_f2 > indata");

     system("cat $RIP_ROOT/namelists/Mdomain2.obs.$range | sed s/plttime/$xt/g > domain$tile.1");
     system("cat domain$tile.1 | sed s/rangename/$range/g > domain$tile.2");
     system("cat domain$tile.2 | sed s/cycle/$cycle/g > domain$tile.in");

     system("$RIP_OBS -f ../ripscr_obs/Domain_$tile domain$tile  > $PLOTS_DIR/rip_obs$tile.log 2>&1");
     &conv2gif_obs( "domain$tile.cgm", $tile, $valid_time);

     chdir $PLOTS_DIR;
     if ( $tile == 1 ) {
        $mkdir_command = "mkdir $JOB_LOC/ugui/gifs/$valid_time";
        system( "ssh 4dwx\@atec-server $mkdir_command" );
     }
     chdir $PLOTS_DIR;
     system("scp -pr gifs_ugui/$valid_time/d$tile\* $dest/$valid_time" );
   }

} else {


foreach $d ( 1..$ndom) {
   $fn = $filen . "DOMAIN" . $d . "." . $range;

  if ( -e $fn ) {
   chdir $PLOTS_DIR;
   chdir ripscr;
   system("$RIPDP_EXE Domain_$d $fn > $PLOTS_DIR/ripdp$d.log 2>&1");

   open(XTIMES, "Domain_$d.xtimes");
   $ntimes = <XTIMES>; 
   while ( $xtin = <XTIMES>) {
      $xt = $xtin;
   }
   close(XTIMES);
   chomp($xt);
     $newfile = sprintf( "%s/ripscr/Domain_%s.xtimes", $PLOTS_DIR, $d);

     if ( $xt >= 1.0 ) {
       $xtprev = $xt - 1;
       $xtout = sprintf("%09.5f\n",$xtprev);
       $xtoute = sprintf("%09.5f",$xtprev);
       $checkfile = sprintf("%s/ripscr/Domain_2_%s_tmk",$PLOTS_DIR,$xtoute);
       if ( -e "$checkfile" ) {
       open(OUTFILE, ">$newfile") || do {
          warn "Cannot open output file: $newfile\n$!\n";
       };
       print OUTFILE "2\n";
       print OUTFILE "$xtout";
       print OUTFILE "$xt";
       close(OUTFILE);
       }
     }

   chdir $PLOTS_DIR;
   chdir riprun;

   if ( $d == 1 ) {
     $xt_d1 = $xt
   }
   if ( $xt != $xt_d1 ) {
#  If this domain doesn't run as long as D1...
   system("rm -f domain$d.cgm");
   } else {
   system("cat $RIP_ROOT/namelists/Mdomain$d.$season.$range | sed s/plttime/$xt/g > domain$d.1");
   system("cat domain$d.1 | sed s/rangename/$range/g > domain$d.2");
   system("cat domain$d.2 | sed s/cycle/$cycle/g > domain$d.3");
   if ( -e  "/data/GMODJOBS/$JOB_ID/sloc$d.in" ) {
      system("cat domain$d.3 /data/GMODJOBS/$JOB_ID/sloc$d.in > domain$d.in");
   } else {
      system("cat domain$d.3 $RIP_ROOT/namelists/Mdomain$d.sloc.$range > domain$d.in");
   }


   system("$RIP_EXE -f ../ripscr/Domain_$d domain$d  > $PLOTS_DIR/rip$d.log 2>&1");

## For EPG
   if ( $d == 2 && $DO_EPG == 1 ) {
   system("cat $RIP_ROOT/namelists/Mdomain4.$season.$range | sed s/plttime/$xt/g > domain4.1");
   system("cat domain4.1 | sed s/rangename/$range/g > domain4.2");
   system("cat domain4.2 | sed s/cycle/$cycle/g > domain4.3");
   if ( -e  "/data/GMODJOBS/$JOB_ID/sloc4.in" ) {
      system("cat domain4.3 /data/GMODJOBS/$JOB_ID/sloc4.in > domain4.in");
   } else {
      system("cat domain4.3 $RIP_ROOT/namelists/Mdomain4.sloc.$range > domain4.in");
   }


   system("$RIP_EXE -f ../ripscr/Domain_$d domain4  > $PLOTS_DIR/rip4.log 2>&1");
## End for EPG
   }

   }
     &conv2gif( "domain$d.cgm", $d, $valid_time);
## For EPG
   if ( $d == 2 && $DO_EPG == 1 ) {
     &conv2gif( "domain4.cgm", 4, $valid_time);
   }

#    Now do the observation plots --- using a custom version of RIP
     chdir $PLOTS_DIR;
     chdir ripscr_obs;
     system("rm MMOUT.in");
     system("ln -s $fn MMOUT.in");
     system("$RIPDP_OBS Domain_$d MMOUT.in > $PLOTS_DIR/ripdp_obs$d.log 2>&1");

     chdir $PLOTS_DIR;
     $valid_m1 = &hh_advan_date( $valid_time, -1);

     chdir riprun_obs;
     system("rm -f indata");
     $obs_f1 = $WORK_DIR . "/" . $valid_m1 . "_qc_obs_for_assimilation_s";
     $obs_f2 = $WORK_DIR . "/" . substr($valid_time,0,10) . "_qc_obs_for_assimilation_s";
     system("cat $obs_f1 $obs_f2 > indata");

     system("cat $RIP_ROOT/namelists/Mdomain$d.obs.$range | sed s/plttime/$xt/g > domain$d.1");
     system("cat domain$d.1 | sed s/rangename/$range/g > domain$d.2");
     system("cat domain$d.2 | sed s/cycle/$cycle/g > domain$d.in");

     system("$RIP_OBS -f ../ripscr_obs/Domain_$d domain$d  > $PLOTS_DIR/rip_obs$d.log 2>&1");
     &conv2gif_obs( "domain$d.cgm", $d, $valid_time);

## For EPG
   if ( $d == 2 && $DO_EPG == 1 ) {
    system("cat $RIP_ROOT/namelists/Mdomain4.obs.$range | sed s/plttime/$xt/g > domain4.1");
     system("cat domain4.1 | sed s/rangename/$range/g > domain4.2");
     system("cat domain4.2 | sed s/cycle/$cycle/g > domain4.in");

     system("$RIP_OBS -f ../ripscr_obs/Domain_$d domain4  > $PLOTS_DIR/rip_obs4.log 2>&1");
     &conv2gif_obs( "domain4.cgm", 4, $valid_time);
##   End for EPG
     }


     chdir $PLOTS_DIR;
#LPC     $mkdir_command = "mkdir $JOB_LOC/gifs/$valid_time";
#LPC     system( "ssh 4dwx\@atec-server $mkdir_command" );
#LPC     system("scp -pr gifs_ugui/$valid_time/d$d\* $dest/$valid_time" );

     chdir gifs_ugui;
     system("rsync -e 'ssh -i /home/fddasys/.ssh/rtfdda' -avzC $valid_time $dest");
#LPC     if ( $d == 2 ) {
#LPC     system("scp -pr gifs_ugui/$valid_time/d4\* $dest/$valid_time" );
#LPC     }
## For EPG
     if ( $DO_EPG == 1 ) {
#LPC     $mkdir_command = "mkdir /EPG/rtfdda/gifs/$valid_time";
#LPC     system( "ssh 4dwx\@atec-server $mkdir_command" );
#LPC     system("scp -pr gifs_ugui/$valid_time/d$d\* 4dwx\@atec-server:/EPG/rtfdda/gifs/$valid_time" );
     system("rsync -e 'ssh -i /home/fddasys/.ssh/rtfdda' -avzC $valid_time carson\@atec-server:/d1/www/htdocs/images/epg/rtfdda/gifs");
#LPC     if ( $d == 2 ) {
#LPC     system("scp -pr gifs_ugui/$valid_time/d4\* 4dwx\@atec-server:/EPG/rtfdda/gifs/$valid_time" );
#LPC     }
     }
     chdir $PLOTS_DIR;
     if ( $DO_PLOTS_SMALL ) {
       $mkdir_command = "mkdir $JOB_LOC/gifs_small/$valid_time";
       system( "ssh 4dwx\@atec-server $mkdir_command" );
       system("scp -pr gifs_small/$valid_time/d$d\* $dest_small/$valid_time" );
       $scandir_command = "/www/cgi-bin/model/gmod/pda_gif_lister range=\"$USER/$JOB_ID\"";
       system( "ssh 4dwx\@atec-server $scandir_command" );
     }
   }

}

}

}
1;
