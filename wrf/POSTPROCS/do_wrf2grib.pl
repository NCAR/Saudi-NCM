#
# Process one hour of WRF OUTPUT to grib files and scp to the GRIB ingest host
#
#
sub do_wrf2grib
{
    my ($hourly_file, $dest, $time_step) = @_;

print "\nin do_wrf2grib $hourly_file, $dest, $time_step\n\n";

$in_file = basename $hourly_file;
@parts = split("_",$in_file);
$this_domain= substr( @parts[1], 2, 1);
$valid_date = substr( @parts[2], 0, 10);
$valid_hour = substr( @parts[3], 0, 8);
$valid_t = $valid_date . "_" . $valid_hour;
$yr = substr( $valid_date,0,4);
$mo = substr( $valid_date,5,2);
$day= substr( $valid_date,8,2);
$hr = substr( $valid_hour,0,2);
$min= substr( $valid_hour,3,2);
$output_file= sprintf("wrf_d%02d_%04d%02d%02d%02d%02d.grb",$this_domain,$yr,$mo,$day,$hr,$min);

system("$MustHaveDir $WRFPOST_DIR");
chdir $WRFPOST_DIR;

# pick up Ferrier's microphysic's table and the wrf post control file

system("ln -fs $GSJOBDIR/wrfrun/ETAMPNEW_DATA eta_micro_lookup.dat");
# Cp this file - since we may need to modify it...
if ( -e "$GSJOBDIR/postprocs/wrf2grib.param" ) {
    print "getting $GSJOBDIR/postprocs/wrf2grib.param  \n";
    system("cp -f $GSJOBDIR/postprocs/wrf2grib.param wrf_cntrl.parm");
} else {
    print "getting $POSTPROCS_DIR/wrf2grib.param \n";
    system("cp -f $POSTPROCS_DIR/wrf2grib.param wrf_cntrl.parm");
}


# specify the directory of wrf post executables
# wrf post is used to read native wrf model output and put out isobaric
# state fields and derived fields  
# copygb is used to horizontally interpolate from one domain to another
# it is necessary to run this step for wrf nmm (but not for wrf arw) because
# wrf nmm's computational domain is on rotated Arakawa-E grid

$POSTEXEC = "$MM5HOME/cycle_code/EXECUTABLE_ARCHIVE/unipost.exe";

$ENV{'MP_SHARED_MEMORY'} = "yes";

$ENV{'MP_LABELIO'} = "yes";

system("rm -f wrfout.file");
system("ln -fs $hourly_file wrfout.file");
system("cat $POSTPROCS_DIR/wrf2grib.input | sed s/time/$valid_t/g > input.1");
# 
# $time_max is getting changed too quickly between Analysis and Forecast
# we preserve it in do_output_gmod but need to add the correct $time_step
# for setting the "anl" tag in the grib file
#
if ( $CYCLE_TAG eq "final" ) {
  $fcoffset = int($time_max / 60.)+$time_step;
} else {
  $fcoffset = int($time_max / 60.);
}
print "INFO:  fcoffset $fcoffset $time_max $time_step\n";
system("cat input.1 | sed s/OFFSET/$fcoffset/g > input.file");

$GRIB_SYSTEM_TAG = '00089'; # overridden below if $AT_IAF is ture

print "AT_IAF = $AT_IAF\n"; # required from postprocintpu.pl
if ($AT_IAF) {

   if ($BCS=~ /AVN/ || $BCS=~ /GFS/) {
      $MODEL = 'G';
   } elsif ($BCS=~ /UKM/) {
      $MODEL = 'U';
   } elsif ($BCS=~ /DWD/) {
      $MODEL = 'D';
   } elsif ($BCS=~ /ECM/) {
      $MODEL = 'E';
   }

   $user = $ENV{LOGNAME};

   $GSJOBID =~ /GWMAGEN_(\d+)/;
   $tile_number = $1;
   $tile_number = '01' if ( $GSJOBID =~ /OPA_HR/ );

# Fill in the model-ID in the input control file 
# $model_ids is a key-value-pair list in postprocinput.pl
#  $MODEL_STRING = sprintf "%s%s", $MODEL,$user ;
#  $GRIB_SYSTEM_TAG = $model_ids{$MODEL_STRING};

   $GRIB_SYSTEM_TAG = ($this_domain-1)*$nModels+$model{$MODEL}+$uid_start{$user};
   $GRIB_SYSTEM_TAG = sprintf "%05d", $GRIB_SYSTEM_TAG;

   print "INFO: GRIB_SYSTEM_TAG = $GRIB_SYSTEM_TAG\n";

#  $lead_time is calculated in do_output_gmod.pl

   if ($lead_time < 0) {  # analysis
      $fcst_hour = sprintf "0%03d",abs($lead_time);
   } else {  # fcst
      $fcst_hour = sprintf "1%03d",$lead_time;
   }

# $THIS_CYCLE is in do_output_gmod.pl

   $cycle_yr = substr($THIS_CYCLE,0,4);
   $cycle_mm = substr($THIS_CYCLE,4,2);
   $cycle_dd = substr($THIS_CYCLE,6,2);
   $cycle_hr = substr($THIS_CYCLE,8,2);

   if ($this_domain < 2) {  # D1
      $output_file = sprintf "model-WRF_%s_%d-%04d-%4d%02d%02d_%02d0000.gb",
        $MODEL,$this_domain,$fcst_hour,$cycle_yr,$cycle_mm,$cycle_dd,
        $cycle_hr;
   } else {  # D2 and D3
      $output_file = sprintf "model-WRF_%s_%d-%02d-%04d-%4d%02d%02d_%02d0000.gb",
        $MODEL,$this_domain,$tile_number,$fcst_hour,$cycle_yr,$cycle_mm,$cycle_dd,
        $cycle_hr;
   }

}

system("cp wrf_cntrl.parm wrf_cntrl.parm.input");
system("cat wrf_cntrl.parm.input  | sed s/SYSTEM/$GRIB_SYSTEM_TAG/g > wrf_cntrl.parm");

#   Run wrfpost
system("rm fort.*");

system("ln -sf wrf_cntrl.parm fort.14");
system("ln -sf griddef.out fort.110");
system("rm WRFPRS*");
system("rm WRFSRS*");
# The latest version of post (UPP) requires the input file to be "itag"
system("rm -f itag");
system("ln -s input.file itag");
# This will not affect the earlier versions reading from stdin
print "\nstarting $POSTEXEC\n\n";
#system("$POSTEXEC < input.file");
system("$POSTEXEC < input.file > ../../output_wrfpost_d$this_domain.log ");
print "\nending $POSTEXEC\n\n";
@ofiles = qx / ls WRFPRS* / ;
chop(@ofiles[0]);
$post_out = @ofiles[0];
if ( -e "$post_out") {
  if ($AT_IAF) {
     if ($GRIB_DEST_TMP) {  # staging

        system("mkdir -p $GRIB_DEST_TMP/$user");
        print "mv $post_out $GRIB_DEST_TMP/$user/$output_file\n";
        system("mv $post_out $GRIB_DEST_TMP/$user/$output_file");

        if($IAF_SLEEP) {
          # wait for end of cycle to ingest
        } else {   # immediate ingest
          print "mv $GRIB_DEST_TMP/$user/$output_file $GRIB_DEST_ROOT/$user/$output_file\n";
          system("mv $GRIB_DEST_TMP/$user/$output_file $GRIB_DEST_ROOT/$user/$output_file");
        }

     } else {  # no staging; immediate ingest by default

        system("mkdir -p $GRIB_DEST_ROOT/$user");
        print "mv $post_out $GRIB_DEST_ROOT/$user/$output_file\n";
        system("mv $post_out $GRIB_DEST_ROOT/$user/$output_file");
     }
          
  } else {
  system("mv $post_out $output_file");
  system("chmod 777 $output_file");
  #Stripped down grib output
  $small_out = $post_out;
  substr($small_out,3,1) = "S";
  $small_output_file = $output_file;
  substr($small_output_file,0,3) = "mob";
  system("mv $small_out $small_output_file");
  system("chmod 777 $small_output_file");

  if ($DO_DISTRIB ) {
    $work_dir = "$DISTRIB_DIR/grib/$this_cycle";
    system( "mkdir -p $work_dir" );
    print "preparing for distrib $output_file to $work_dir\n";
      if ($DO_TAR_SUM_FOR_DISTRIB ) {
        print "sha256sum $output_file > ${output_file}.sum\n";
        system("sha256sum $output_file > ${output_file}.sum");
      }
    system("cp $output_file* $work_dir"); 
    system("cp $small_output_file* $work_dir"); 
  }

  system("mv $output_file ../../");
  }
} else {
  print (" No grib file generated! \n");
}

print "\nexiting do_wrf2grib $hourly_file, $dest, $time_step\n\n";

#   End of wrf post job

}
1;
