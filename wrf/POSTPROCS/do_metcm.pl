#use Sys::Hostname;

sub do_metcm
{
###
### Note: Variables that are passed in as env vars through "require" $FLEXINPUT
### in 'do_output_gmod.pl' include:
### $MM5HOME
### Variables that are pased in from postprocinput.pl include:
### $METCM_DEST_ROOT
    my ($hourly_file, $this_cycle) = @_;

   #
   # Executable files
   #
   print "\nin do_metcm $hourly_file, $this_cycle\n\n";
   print "\nJOB_ID = $JOB_ID\n";
   $METCM_EXE = "$MM5HOME/cycle_code/EXECUTABLE_ARCHIVE/readWRF.exe";
   
   print "METCM_EXE Will use this executable: $METCM_EXE\n";
   
   system("mkdir -p $METCM_DEST_ROOT");
   
   if ( -e "$GSJOBDIR/postprocs/pobs_input.nml" ) {
      system("cp $GSJOBDIR/postprocs/pobs_input.nml .");
   } else {
      die "File $GSJOBDIR/postprocs/pobs_input.nml does not exist!";
   }

   if ( -e "$GSJOBDIR/postprocs/metcm_locations.txt" ) {
      system("ln -s $GSJOBDIR/postprocs/metcm_locations.txt .");
   } else {
      die "File $GSJOBDIR/postprocs/metcm_locations.txt does not exist!";
   }

   open(NML_IN,"pobs_input.nml");
   open(NML_OUT,">pobs_input.nml_tmp");
   while (<NML_IN>) {
      s/INPUT_WRF_FILE/$hourly_file/;
      print NML_OUT $_;
   }
   close(NML_IN);
   close(NML_OUT);
   system("mv pobs_input.nml_tmp pobs_input.nml");

   system("$METCM_EXE");

   if ( $AT_IAF ) {
      system("chmod g+w METCM*.txt");
      system("mv METCM*.txt $METCM_DEST_ROOT/.");
   } else {
      system("mkdir -p $METCM_DEST_ROOT/$this_cycle");
      foreach $txt (<*.txt>) {
         if ($txt =~ /^[A-Z]\d{9,}\.txt/) {
            system("chmod g+w $txt");
            system("mv $txt $METCM_DEST_ROOT/$this_cycle/.");
         }
      }
      if ($DO_DISTRIB) {
# proceed only if DO_TAR_SUM_FOR_DISTRIB is turned on
        if ($DO_TAR_SUM_FOR_DISTRIB) {
           chdir $METCM_DEST_ROOT;
           if  (-e $this_cycle.tgz) {
              print "$this_cycle.tgz exists - another process is working on it\n";
           } else {
              print "tarring and summing $this_cycle\n";
              system("tar -czvf $this_cycle.tgz $this_cycle");
              system("sha256sum $this_cycle.tgz > $this_cycle.tgz.sum");
              system("mv $this_cycle.tgz* $DISTRIB_DIR/metcm");
           }
         }
       }
   }
}
1;
