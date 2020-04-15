
sub do_plots
{
my ($hourly_file,$ndom, $cycle, $valid_time, $dest) = @_;

$ENV{'NCARG_ROOT'} = "/opt/ncarg";
$ENV{'NCARG_LIB'} = "/opt/ncarg/lib";
$ENV{'STATIONLIST'} = "$MM5HOME/cycle_code/CONSTANT_FILES/RIP/stationlist";
$ENV{'HIRESMAP'} = "$MM5HOME/cycle_code/CONSTANT_FILES/RIP/${RANGE}_map.ascii";
$ENV{'RANGEMAP'} = "$MM5HOME/cycle_code/CONSTANT_FILES/RIP/${RANGE}_map.ascii";
$ENV{'RIP_ROOT'} = "$MM5HOME/cycle_code/CONSTANT_FILES/RIP";
$RIP_ROOT = "$MM5HOME/cycle_code/CONSTANT_FILES/RIP";

@MONTHS=( '01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12');
@SEASONS=( 'winter', 'winter', 'winter', 'winter', 'summer', 'summer', 'summer', 'summer', 'summer', 'summer', 'summer', 'winter', 'winter');

$RIPDP_EXE = "$MM5HOME/cycle_code/EXECUTABLE_ARCHIVE/ripdp_new.exe";
$RIP_EXE = "$MM5HOME/cycle_code/EXECUTABLE_ARCHIVE/rip_new.exe";

system("$MustHaveDir $PLOTS_DIR");
system("$MustHaveDir $PLOTS_DIR/ripscr");
system("$MustHaveDir $PLOTS_DIR/riprun");
chdir $PLOTS_DIR;

system("scrub 1/4 $PLOTS_DIR/ripscr");
system("scrub -d 1/6 $PLOTS_DIR/gifs_ugui");
system("rm -f $PLOTS_DIR/riprun/*");

@parts = split("DOMAIN",$hourly_file);
$filen = @parts[0];
$range = substr( @parts[1], 2, 10);

$do_tiles = 1;
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
       $xtout = sprintf("%03d.00000\n",$xtprev);
       print OUTFILE "$xtout";
       print OUTFILE "$xt";
       close(OUTFILE);
     }

     chdir $PLOTS_DIR;
     chdir riprun;

     system("cat $RIP_ROOT/namelists/Mdomain2.winter.$range | sed s/plttime/$xt/g > domain$tile.1");
     system("cat domain$tile.1 | sed s/rangename/$range/g > domain$tile.2");
     system("cat domain$tile.2 | sed s/cycle/$cycle/g > domain$tile.in");

     system("$RIP_EXE -f ../ripscr/Domain_$tile domain$tile  > $PLOTS_DIR/rip$tile.log 2>&1");
     &conv2gif( "domain$tile.cgm", $tile, $valid_time);
     chdir $PLOTS_DIR;
     if ( $tile == 1 ) {
        $mkdir_command = "mkdir $JOB_LOC/gifs/$valid_time";
        system( "ssh 4dwx\@128.117.200.214 $mkdir_command" );
     }
     system("scp -pr gifs_ugui/$valid_time/d$tile\* $dest/$valid_time" );
##     system("scp -pr gifs_ugui/$valid_time $dest" );
   }

} else {


foreach $d ( 1..$ndom) {
   $fn = $filen . "DOMAIN" . $d . "." . $range;

   chdir ripscr;
   system("$RIPDP_EXE Domain_$d $fn > $PLOTS_DIR/ripdp$d.log 2>&1");

   open(XTIMES, "Domain_$d.xtimes");
   $ntimes = <XTIMES>; 
   while ( $xtin = <XTIMES>) {
      $xt = $xtin;
   }
   close(XTIMES);
   chomp($xt);


   chdir $PLOTS_DIR;
   chdir riprun;

   system("cat $RIP_ROOT/namelists/Mdomain$d.winter.$range | sed s/plttime/$xt/g > domain$d.1");
   system("cat domain$d.1 | sed s/rangename/$range/g > domain$d.2");
   system("cat domain$d.2 | sed s/cycle/$cycle/g > domain$d.in");

   system("$RIP_EXE -f ../ripscr/Domain_$d domain$d  > $PLOTS_DIR/rip$d.log 2>&1");

}

}

}
1;
