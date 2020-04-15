#!/usr/bin/perl
use Time::gmtime;

sub processData
{
  print " this_cycle = $this_cycle\n";
  print " last_cycle = $last_cycle\n";
  print " BCS_date = $BCS_date\n";
  print " RUNDIR = $RUNDIR\n";
  print " DATA_ICBC_DIR = $DATA_ICBC_DIR\n";
  print " DATA_DIR = $DATA_DIR\n";
  print " MODEL = $MODEL\n";
  print " RAP_start = $RAP_start\n";
  print " RAP_end = $RAP_end\n";
  print " normal = $normal\n";
  print " NODE = $NODE\n";
  print " GSJOBID = $GSJOBID\n";
  print " CYC_INT = $CYC_INT\n";
  print "COLD_0012 = $COLD_0012\n"; 

  print "\n";
  print gmctime(), "Removing the following old symbolic links:\n";
  system ("find $DATA_DIR -type l -print");
  system ("find $DATA_DIR -type l -exec rm -f \{\} \\;");
  print "\n";
  print gmctime(), "$DATA_DIR now contains only:\n";
  system ("ls $DATA_DIR");
  print "\n";

#exit;

  # OLD: %BCSDT=("ETA",3,"AVN",6,"AVNFTP",3);
  # OLD: $BCSDT{$BCS} = 3;

  # NEW:
  $step_size = 3;

  $BCS_CHANGE = 0;
  $BCS_CHANGE_hh = 0;
  $BCS_date_L = 0;

  $nshift_p=0;

  if (-e "$RUNDIR/${last_cycle}/nambcdate"){
  open(BCSDATEPRE,"$RUNDIR/${last_cycle}/nambcdate");
  $BCS_date_L = <BCSDATEPRE>;
  chomp($BCS_date_L);
  close(BCSDATEPRE);
  }else{
   print "Cannot find file $RUNDIR/${last_cycle}/nambcdate\n";
  }

  # cycle hour hh: 5 <= $hh < 5+$CYC_INT or 17 <= $hh < 17+$CYC_INT  or  not $normal
  if ( ($hh >= 5 && $hh-5 < $CYC_INT) || ($hh >= 17 && $hh-17 < $CYC_INT)
       || $normal > 1)
  {
    $BCS_CHANGE = 1;
    $BCS_CHANGE_hh = $hh;
  }

  if (-e "$RUNDIR/critic.time"){
  open (CRITIC, "$RUNDIR/critic.time");
  $time_max = <CRITIC>;
  close (CRITIC);
  }else{
   print "Cannot find file $RUNDIR/critic.time\n";
  }

  print "BCS_CHANGE = $BCS_CHANGE\n";
  print "BCS_date_L = $BCS_date_L\n";
  print "BCS_date   = $BCS_date\n";
  print "time_max = $time_max\n";
  print "\n";

# if($BCS_CHANGE == 1 || ($BCS_CHANGE != 1 && $BCS_date == $BCS_date_L))
# {
    if($hh == $BCS_CHANGE_hh+$CYC_INT && $time_max > 420)
    {
      $pre_last_cycle = &hh_advan_date($last_cycle, -$CYC_INT);
      if (-e "$RUNDIR/${pre_last_cycle}/nambcdate"){
      open(BCSDATEPRE,"$RUNDIR/${pre_last_cycle}/nambcdate") ;
      $BCS_date_L = <BCSDATEPRE>; chomp($BCS_date_L); close(BCSDATEPRE);
      }else{
       print "Cannot find file $RUNDIR/${pre_last_cycle}/nambcdate\n";
      }
    }

    for ($i = 0; $i <= 138; $i+=$step_size) # Good enough for 5-day forecasts
    {
      $ii = sprintf("%02d", $i);
      $BCS_file = &constructGFSfilename( $BCS_date, $ii);
      $bcs_file_from = $DATA_ICBC_DIR."/".$BCS_file; #$DATA_AVN_DIR
      $bcs_file_to   = ${DATA_DIR}."/".$BCS_file;
#
    if ($COLD_0012 == 2) {
      $i_ = $i % 12;
      $ntime = int($i/12);
        if ($ntime >= 1 )
       {
         $nshift=$ntime * 12;
         $BCS_date_=&hh_advan_date($BCS_date,$nshift);
         $ii_ = sprintf("%02d", $i_);
         $BCS_file_ = &constructGFSfilename( $BCS_date_, $ii_);
         $bcs_file_from_ = $DATA_ICBC_DIR."/".$BCS_file_; #$DATA_AVN_DIR
	 if (! -e  "${bcs_file_from_}" && $nshift_p > 0)  {
           $BCS_date_=&hh_advan_date($BCS_date,$nshift_p);
	   $i_=$i-$nshift_p;
           $ii_ = sprintf("%02d", $i_);
           $BCS_file_ = &constructGFSfilename( $BCS_date_, $ii_);
           $bcs_file_from_ = $DATA_ICBC_DIR."/".$BCS_file_; #$DATA_AVN_DIR
	   $nshift=$nshift_p;
         }
        }
       }
#
      # Exceptions in order to correct ICBC fcst error in 05Z and 17Z cylce
      if(($ii == 3 || $ii == 0) && $normal == 1 && $time_max > 420)
      {
        if($BCS_date == &hh_advan_date($BCS_date_L,12))
        {
          $BCS_file1 = &constructGFSfilename( $BCS_date_L, "12");
          $bcs_file_from_1_12 = $DATA_ICBC_DIR."/".$BCS_file1; #$DATA_AVN_DIR
          $BCS_file1 = &constructGFSfilename( $BCS_date_L, "15");
          $bcs_file_from_1_15 = $DATA_ICBC_DIR."/".$BCS_file1; #$DATA_AVN_DIR
          if ($ii == 0)
          {
            $bcs_file_from = $bcs_file_from_1_12;
          }
          elsif ($ii == 3)
          {
            $bcs_file_from = $bcs_file_from_1_15;
          }
        }
        elsif ($BCS_date == &hh_advan_date($BCS_date_L,24))
        {
          $BCS_file1 = &constructGFSfilename( $BCS_date_L, "24");
          $bcs_file_from_1_24 = $DATA_ICBC_DIR."/".$BCS_file1; #$DATA_AVN_DIR
          $BCS_file1 = &constructGFSfilename( $BCS_date_L, "27");
          $bcs_file_from_1_27 = $DATA_ICBC_DIR."/".$BCS_file1; #$DATA_AVN_DIR
          if ($ii == 0)
          {
            $bcs_file_from = $bcs_file_from_1_24;
          }
          elsif ($ii == 3 )
          {
            $bcs_file_from = $bcs_file_from_1_27;
          }
        }
      }  # if $hh exception

      print FILEL "\n    $BCS:   $bcs_file_from  \n     --> $bcs_file_to \n";
	print("bcs_file_from = ${bcs_file_from}; bcs_file_from_ = ${bcs_file_from_}\n");
      if((-e "${bcs_file_from_}" && ! -e "${bcs_file_to}") &&  $COLD_0012 == 2 ){
         print  (" ln -s -f $bcs_file_from_ ${bcs_file_to}\n") ;
         system (" ln -s -f $bcs_file_from_ ${bcs_file_to}"); 
	 $nshift_p=$nshift;
	 print "nshift_p= $nshift_p\n";
      }
      if(-e "${bcs_file_from}" && ! -e "${bcs_file_to}"){
         print  (" ln -s -f $bcs_file_from ${bcs_file_to}\n");
         system (" ln -s -f $bcs_file_from ${bcs_file_to}");
      }
      if(! -e "$bcs_file_to")
      {
        print       " Missing file $bcs_file_from \n";
        print FILEL "    missing $bcs_file_to \n";
      }
    } # for $i

    open(ETADATE,">$RUNDIR/$this_cycle/nambcdate");  # nambcdate is used for either ETA or AVN
    print ETADATE $BCS_date;
    print ETADATE "\nThis is fresh $BCS. 0 - 72 h fcst is used";
    close(ETADATE);
    $ETA_STATUS = 0;
    print FILEL "   Finish $BCS data collection at ", gmctime();

    # Do interpolation of NAM / GFS onto Domain 1 grid and terrain

    if($MODEL eq "MM5")
    {
       print FILEL "\n   starting GFS regrid.csh at ", gmctime();
       system("$CSH_ARCHIVE/Forecast/RT_A_regrid_gfs.csh $BCS_date $this_cycle $RAP_start $RAP_end $normal $NODE");
       print FILEL "\n   ending GFS regrid.csh at ", gmctime();

    }
    elsif ($MODEL eq "WRF")
    {
      print FILEL "\n   starting $BCS wps at ", gmctime();
      $BCSINT=10800;
      system("$CSH_ARCHIVE/Forecast/RT_A_wrf_wps.csh $this_cycle $RAP_start $RAP_end $NUM_PROCS $NODE $BCS $BCSINT $NUM_DOMS");
      print FILEL "\n   ending  $BCS wps at ", gmctime();
    }
# }
############## END REGRID - 1st Attempt ########################################

  if($MODEL eq "MM5")
  {
    $ONE_REGRID =   4600000; # 84x98            size=4910000
    $ONE_REGRID =   1600000; #preset 3.3 50*56  size=1780000
    $ONE_REGRID =   1300000; #preset 4.0 40*50  size=1325870
    $ONE_REGRID *= 1.2 if ($BCS eq 'ETA');
    #$BCS_SIZE_C = 100000000;
    #$BCS_SIZE_C =  71020196 ;
    #$BCS_SIZE_C = 20000000 if ($BCS eq 'AVN' || $BCS eq 'AVNFTP');
    #$BCS_SIZE_C = 20000000 if ($BCS eq 'AVN' || $BCS eq 'AVNFTP');
    $BCS_SIZE_C = $ONE_REGRID * (2+ ($CYC_INT+$FCST_LENGTH+$FIN_END)/$step_size);
    #     160*110: 10000000, 84*98: 5000000

    $bcs_size = -s "${this_cycle}_REGRIDv3.${RANGE}";
    $tmp = "$GEAPSTMP/${this_cycle}_REGRIDv3.${RANGE}";
    $bcs_size = -s "$tmp" if( -s "$tmp");
    if ( $bcs_size < $BCS_SIZE_C) {
       $secondAttempt = 1;
    } else {
       $secondAttempt = 0;
    }
  }
  else
  {
    $secondAttempt = determine($RAP_start,$RAP_end,$step_size);
  }

  #########Check if the file size of REGRIDv3 is big enough#######################
  if ( $secondAttempt )
  {
    $ETA_STATUS = -1;
    print FILEL "\n   -- Not find enough data for BC, IC and 1ST Guess. \n";
    print FILEL "\n   starting recreating 1ST guess from 12 hour earlier $BCS at ", gmctime();
    system("rm $RUNDIR/$this_cycle/*REGRIDv3*");
    system("rm ${DATA_DIR}/eta*");

    $BCS_date_L = &hh_advan_date($BCS_date,-12);

    system ("find $DATA_DIR -type l -exec rm -f \{\} \\;");

    for ($i = 12; $i <= 150; $i+=$step_size) # Good enough for 5-day forecasts
    {
      $ii = sprintf("%02d", $i);

      $BCS_file = &constructGFSfilename( $BCS_date_L, $ii);
      $bcs_file_from = $DATA_ICBC_DIR."/".$BCS_file;
      $bcs_file_to   = ${DATA_DIR}."/".$BCS_file;
#
    if ($COLD_0012 == 2) {
      $i_ = $i % 12;
      $ntime = int($i/12);
        if ($ntime >= 1 )
       {
         $nshift=$ntime * 12;
         $BCS_date_=&hh_advan_date($BCS_date,$nshift);
         $ii_ = sprintf("%02d", $i_);
         $BCS_file_ = &constructGFSfilename( $BCS_date_, $ii_);
         $bcs_file_from_ = $DATA_ICBC_DIR."/".$BCS_file_; #$DATA_AVN_DIR
	 if (! -e  "${bcs_file_from_}" && $nshift_p > 0)  {
           $BCS_date_=&hh_advan_date($BCS_date,$nshift_p);
	   $i_=$i-$nshift_p;
           $ii_ = sprintf("%02d", $i_);
           $BCS_file_ = &constructGFSfilename( $BCS_date_, $ii_);
           $bcs_file_from_ = $DATA_ICBC_DIR."/".$BCS_file_; #$DATA_AVN_DIR
	   $nshift=$nshift_p
         }
        }
      }
#
      print FILEL "     $BCS:      $bcs_file_from_  \n     --> $bcs_file_to \n";
	print("bcs_file_from = ${bcs_file_from}; bcs_file_from_ = ${bcs_file_from_}\n");
      if((-e "${bcs_file_from_}" && ! -e "${bcs_file_to}") &&  $COLD_0012 == 2 ){
         print  (" ln -s -f $bcs_file_from_ ${bcs_file_to}\n") ;
         system (" ln -s -f $bcs_file_from_ ${bcs_file_to}"); 
	 $nshift_p=$nshift;
	 print "nshift_p= $nshift_p\n";
      }
      if(-e "${bcs_file_from}" && ! -e "${bcs_file_to}"){
         print  (" ln -s -f $bcs_file_from ${bcs_file_to}\n") ;
         system (" ln -s -f $bcs_file_from ${bcs_file_to}"); 
      }
      if (! -e "$bcs_file_to")
      {
        print       " Missing file $bcs_file_from \n";
        print FILEL "\n    missing $bcs_file_to";
      }
    } #for $i

    print FILEL "   Finish 2nd $BCS data collection at ", gmctime();

    # Do interpolation of NAM / GFS onto Domain 1 grid and terrain
    if ( $MODEL eq "MM5")
    {
      print FILEL "\n   starting GFS regrid.csh at ", gmctime();
      system("$CSH_ARCHIVE/Forecast/RT_A_regrid_gfs.csh $BCS_date_L $this_cycle $RAP_start $RAP_end $normal $NODE");
      print FILEL "\n   ending GFS regrid.csh at ", gmctime();

      # Fill in any missing time periods
      print FILEL "\n   starting missing_times.csh at ", gmctime();
      system("$CSH_ARCHIVE/Forecast/RT_B_missing_times.csh $BCS_date $this_cycle");
      print FILEL "   ending   missing_times.csh at ", gmctime();
    }
    elsif ($MODEL eq "WRF")
    {
      print FILEL "\n   starting $BCS wps at ", gmctime();
      $BCSINT=10800;
      system("$CSH_ARCHIVE/Forecast/RT_A_wrf_wps.csh $this_cycle $RAP_start $RAP_end $NUM_PROCS $NODE $BCS $BCSINT $NUM_DOMS");
      print FILEL "\n   ending   $BCS wps at ", gmctime();
    }

    open(ETADATE,">$RUNDIR/$this_cycle/nambcdate");
    print ETADATE $BCS_date_L;
    print ETADATE "\nThis is 12-hour older $BCS. 12 - 72 h fcst is used";
    close(ETADATE);
    print FILEL "\n   ending recreating 1ST guess from 12 hour earlier ICBC data at ", gmctime();
  }
} #sub
sub constructGFSfilename
{
  (my $DATE, my $fcst_hr) = @_ ;

      $file_name = $ICBC_NAME_TEMPLATE;
      $BCS_CYC = substr($DATE,8,2);
      $file_name =~ s/CYCLE/$BCS_CYC/;

#     count how many digits of $fcst_hour to include in the file-name
      my $c = $file_name =~ tr/F//;
      $fcststring = sprintf("%0${c}d", $fcst_hr);
      $F = "F";
      $stringF = "";
      for ($count=0; $count<$c; $count++) {
        $stringF = $stringF . sprintf("%s", $F);
      }
      $file_name =~ s/${stringF}/${fcststring}/;

      $BCS_CC = substr($DATE,0,2);
      $BCS_YY = substr($DATE,2,2);
      $BCS_MM = substr($DATE,4,2);
      $BCS_DD = substr($DATE,6,2);
      $BCS_HH = substr($DATE,8,2);
      $file_name =~ s/CC/$BCS_CC/;
      $file_name =~ s/YY/$BCS_YY/;
      $file_name =~ s/MM/$BCS_MM/;
      $file_name =~ s/DD/$BCS_DD/;
      $file_name =~ s/HH/$BCS_HH/;
      return($file_name);
}

sub determine {

  my ($start_time,$end_time,$interval) = @_; # YYYYMMDDHH,YYYYMMDDHH,HRS
  my ($yyyy,$mm,$dd,$hh);
  my $FILE;
  my $size;
  my $max_size = 0;
  my $min_size = 999999999;

  my $valid_time = $start_time;
  while ($valid_time <= $end_time) {

     $yyyy = substr($valid_time,0,4);
     $mm = substr($valid_time,4,2);
     $dd = substr($valid_time,6,2);
     $hh = substr($valid_time,8,2);

     $FILE = "${RUNDIR}/${this_cycle}/WRF_WPS/FILE:${yyyy}-${mm}-${dd}_${hh}";

     if ( -s $FILE ) {
        $size = -s $FILE;
        $max_size = $size if ($size >= $max_size);
        $min_size = $size if ($size <= $min_size);
     } else {
        print "File $FILE missing or empty, needs 2nd attempt!\n";
        return 1; # needs 2nd attempt
     }

     $valid_time = hh_advan_date($valid_time,$interval);
  }

  if ($max_size != $min_size) {
     print "Metgrid file sizes inconsistent throughout forecast length; needs 2nd attempt!\n";
     return 1; # some files are incomplete; needs 2nd attempt
  } else {
     return 0; # completed without problems; no need for 2nd attempt
  }

}

1;
