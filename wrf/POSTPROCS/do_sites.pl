
sub do_sites
{
    my ($hourly_file,$ndom, $cycletag, $valid_time, $valid_time_m1, $dest) = @_;

#
# Executable files
#
print "\nJOB_ID = $JOB_ID\n";
if ($IS_WRF) {
  $SITES_EXE = "$MM5HOME/cycle_code/EXECUTABLE_ARCHIVE/readWRF.exe" ;
}else{
  $SITES_EXE = "$MM5HOME/cycle_code/EXECUTABLE_ARCHIVE/sites_new.exe";
}
print "Will use this executable: $SITES_EXE\n";

system("$MustHaveDir $SITES_DIR");
system("$MustHaveDir $SITES_ARCHIVE_DIR");
chdir $SITES_DIR;
system("rm site*.dat");
system("rm site*.bias");
system("rm wrfout*");

system("rm stationmap_new.list");
if ( -e "$GSJOBDIR/postprocs/stationmap_new.list" ) {
  system("ln -s $GSJOBDIR/postprocs/stationmap_new.list stationmap_new.list");
} else {
  system("ln -s $MM5HOME/cycle_code/POSTPROCS/stationmap_new.list stationmap_new.list");
}


if ( $IS_WRF ) {
  @parts = split("_",$hourly_file);
  $input_file_root = @parts[0];
# valid_time and valid_time_m1 come from do_output_gmod.pl.....
  $yr = substr( $valid_time_m1, 0, 4);
  $mo = substr( $valid_time_m1, 4, 2);
  $dy = substr( $valid_time_m1, 6, 2);
  $hr = substr( $valid_time_m1, 8, 2);
  $WRF_TIME_M1 = "${yr}-${mo}-${dy}_${hr}:00:00";
  $yr = substr( $valid_time, 0, 4);
  $mo = substr( $valid_time, 4, 2);
  $dy = substr( $valid_time, 6, 2);
  $hr = substr( $valid_time, 8, 2);
  $WRF_TIME = "${yr}-${mo}-${dy}_${hr}:00:00";
  @lines = split( " ", `wc -l stationmap_new.list`);
  $lc = @lines[0];
  system("cat  $MM5HOME/cycle_code/POSTPROCS/sites_wrf.nml | sed s/NLINES/$lc/ > pobs_input.1");
  system("cat  pobs_input.1  | sed s/TAG/$cycletag/ > pobs_input.2");
  foreach $d ( 1..$ndom) {
#  reconstruct the "WRF-style" filename for input to the pobs-program...
   $filename_m1 = sprintf("wrfout_d%02d_%s",$d,$WRF_TIME_M1);
   $input_file_m1 = sprintf("%s_d%02d-1",$input_file_root,$d);
   if ( -e $input_file_m1 ) {
   system("ln -s $input_file_m1 $filename_m1");
   }
   $filename = sprintf("wrfout_d%02d_%s",$d,$WRF_TIME);
   $input_file = sprintf("%s_d%02d",$input_file_root,$d);
   system("ln -s $input_file $filename");
   system("cat pobs_input.2 | sed s/FILENAME/$filename/g > pobs_input.nml");
   system("$SITES_EXE");
  }

} else { 

 @parts = split("DOMAIN",$hourly_file);
 $filen = @parts[0];
 $range = substr( @parts[1], 2, 10);

 foreach $d ( 1..$ndom) {
   system("ln -s $GSJOBDIR/TERRAIN/TERRAIN_DOMAIN$d TERRAIN_DOMAIN$d");
   $fn = "DOMAIN" . $d . "." . $range;
   $fn_dom = $filen . "DOMAIN" . $d . "." . $range;
   system("rm -f $fn");
   system("ln -s $fn_dom $fn");
   open(INPUTS, ">${SITES_DIR}/input");
   print( INPUTS "./\n");
   print( INPUTS $fn, "\n" ); 
   print( INPUTS $cycletag, "\n" ); 
   close(INPUTS);
   system("$SITES_EXE < input");
 }

}

foreach $f (`ls *.dat`) {

  chomp($f);
  $archfile = "$SITES_ARCHIVE_DIR/$f";
  if ( -e $archfile ) {
     system("cat $archfile $f > tmpfile");
     system("tail -250 tmpfile > $archfile");
  } else {
     system("mv $f $SITES_ARCHIVE_DIR");
  }
}

system("chmod g+w $SITES_ARCHIVE_DIR/*.dat ");
system("rsync -e 'ssh -i $KEY' -avzC $SITES_ARCHIVE_DIR/\*.dat $SITES_DEST_DIR");


}
1;
