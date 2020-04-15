#use Sys::Hostname;

our $AnEn;
our $DO_VERI_PAIR_DATABASE;

sub do_sams_sites
{
###
### Note: Variables that are passed in as env vars through "require" $FLEXINPUT
### in 'do_output_gmod.pl' include:
### $MM5HOME
### $RANGE
### $DATADIR
###
    my ($hourly_file,$ndom, $cycletag, $this_cycle) = @_;
    my $stage;

   #
   # Executable files
   #
   print "\nin do_sams_sites $hourly_file,$ndom, $cycletag, $this_cycle\n\n";
   print "\nJOB_ID = $JOB_ID\n";
   $SAMS_SITES_EXE = "$MM5HOME/cycle_code/EXECUTABLE_ARCHIVE/v_wrf_sfc_interp_model_only.exe";
   
   print "SAMS_SITES_EXE Will use this executable: $SAMS_SITES_EXE\n";
   
   system("mkdir -p $SAMS_SITES_DIR/final");
   system("mkdir -p $SAMS_SITES_DIR/fcst");
   
   my $stage;
   if ($CYCLE_TAG=~  /final/i) {
      $stage="F";
   } else {
      $stage="P+FCST";
   }
   
   my $outputFile;
   if ($CYCLE_TAG=~ /final/i) {
      $outputFile = "$SAMS_SITES_DIR/final/${this_cycle}_wrfVars_samsSites_$stage";
   } else {
      $outputFile = "$SAMS_SITES_DIR/fcst/${this_cycle}_wrfVars_samsSites_$stage";
   }
   
   my $samsNcdFile;
   my $range_lc = $RANGE;
   $range_lc =~ tr/A-Z/a-z/;
   
   my $ncd=`ls -1tr $DATADIR/$range_lc/sams/sams*.cdf | tail -1`;
   if ($ncd) {
      chomp($ncd);
      $samsNcdFile = $ncd;
   } else {
      $samsNcdFile = $SAMSFILE; ##?????
   }
   
   print "SAMS NetCDF file used for $SAMS_SITES_EXE is: $samsNcdFile\n";
   
   system("$SAMS_SITES_EXE -f $hourly_file -c $samsNcdFile -o $outputFile");
   
   system("chmod g+w $outputFile");

   #
   # Import the model data (verification pair) to database
   # AnEn process is moved to perl/flex/post_process_clean.pl
   unless ($CYCLE_TAG=~ /final/i) {
      my $import_veri_pair = 0;
      if (defined $DO_VERI_PAIR_DATABASE) {
         $import_veri_pair = $DO_VERI_PAIR_TO_DB;
      }
      if (defined $AnEn) {
         $import_veri_pair = 1 if ($AnEn);
      }
      if ($import_veri_pair) {
         print "  do_sams_sites.pl imports the verification pair to database\n";
         if (! defined $PYTHONPATH) {
            $PYTHONPATH = "$MM5HOME/cycle_code/PYTHON/modules" if ( defined ($MM5HOME));
         }
         if (defined ($PYTHONPATH)) {
            if ( defined ($ENV{PYTHONPATH}) ) {
               $ENV{PYTHONPATH} = "$PYTHONPATH:$ENV{PYTHONPATH}" ;
            } else {
               $ENV{PYTHONPATH} = $PYTHONPATH;
            }
         }
         if ( defined ($ENV{PYTHONPATH}) ) {
            my $model_id = 0;
            my $do_at_post_process = 1;    # AnEn for model is moved to post_process_clean job
            $model_id = 3 if ($ENV{GSJOBID} eq "GWATC2" || $JOB_ID eq "GWATC2" || $ENV{PBS_JOBID} =~ /bigmacb/i);
         
            my $my_cmd = "$PYTHON_ARCHIVE/flex/veri_pair/import_verification_model_data.py -r $range_lc -m $model_id -i $outputFile";
            #system("pwd");
            print ("     $my_cmd\n");
            my $systat = system("$my_cmd");
            print ("=== ERROR === Unable to import verification model data into database for cycle $this_cycle\n") if ($systat != 0);
            if (defined $AnEn && $AnEn && 0 == $do_at_post_process) {
               my $lead_hours = 24;
               if (defined $VERI_PAIR_LEAD_HOURS) {
                  $lead_hours = $VERI_PAIR_LEAD_HOURS;
               }
               $my_cmd = "$MM5HOME/cycle_code/CSH_ARCHIVE/verif/run_veri2anen.sh $this_cycle $range_lc $PYTHON_ARCHIVE/flex/veri_pair lead_hours=$lead_hours";
               print ("     $my_cmd\n");
               my $systat = system("$my_cmd");
               print ("=== ERROR === Unable to export verification pair and process AnEn for cycle $this_cycle\n") if ($systat != 0);
            }
         }
         else {
            print ("  do_sams_sites.pl ENV{PYTHONPATH} is not defined!!!\n");
         }
      }
      else {
         print "  do_sams_sites. import_veri_pair is disabled !!!\n";
         if (defined ($DO_VERI_PAIR_DATABASE)) {
            print ("  DO_VERI_PAIR_TO_DB [$DO_VERI_PAIR_TO_DB] is defined\n");
         }
      }
   }

print "\nexiting do_sams_sites $hourly_file,$ndom, $cycletag, $this_cycle\n\n";
}
1;
