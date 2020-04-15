
sub do_sites_gcat
{
    my ($hourly_file,$ndom, $cycletag, $dest) = @_;

#
# Executable files
#
$SITES_EXE = "$MM5HOME/cycle_code/EXECUTABLE_ARCHIVE/sites_new.exe";

system("$MustHaveDir $SITES_DIR");
system("$MustHaveDir $SITES_ARCHIVE_DIR");
chdir $SITES_DIR;

#system("ln -s $MM5HOME/cycle_code/SOURCE/RT_SITES/mkstation/stationmap_new.list stationmap_new.list");
#system("ln -s $MM5HOME/cycle_code/POSTPROCS/stationmap_new.list stationmap_new.list");
system("ln -s /data/GMODJOBS/$JOB_ID/postprocs/stationmap.list stationmap_new.list");


@parts = split("DOMAIN",$hourly_file);
$filen = @parts[0];
$range = substr( @parts[1], 2, 10);

foreach $d ( 1..$ndom) {
   system("ln -s /data/GMODJOBS/$JOB_ID/TERRAIN/TERRAIN_DOMAIN$d TERRAIN_DOMAIN$d");
   $fn = "DOMAIN" . $d . "." . $range;
   $fn_dom = $filen . "DOMAIN" . $d . "." . $range;
   system("rm -f $fn");
   system("ln -s $fn_dom $fn");
   open(INPUTS, ">${SITES_DIR}/input");
   print( INPUTS "./\n");
   print( INPUTS $fn, "\n" ); 
   print( INPUTS $cycletag, "\n" ); 
   close(INPUTS);
   if (-s $fn_dom){
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

#LPCsystem("scp $SITES_ARCHIVE_DIR/\*.dat $SITES_DEST_DIR");
system("rsync -e 'ssh -i /data/GMODJOBS/$JOB_ID/rtfdda.key' -avzC $SITES_ARCHIVE_DIR/\*.dat $SITES_DEST_DIR");


}
1;
