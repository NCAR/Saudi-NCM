#!/usr/bin/perl

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

  print "\n";
  print &ctime(time), "Removing the following old symbolic links:\n";
  system ("find $DATA_DIR -type l -print");
  system ("find $DATA_DIR -type l -exec rm -f \{\} \\;");
  print "\n";
  print &ctime(time), "$DATA_DIR now contains only:\n";
  system ("ls $DATA_DIR");
  print "\n";

  # OLD: %BCSDT=("ETA",3,"AVN",6,"AVNFTP",3);
  # OLD: $BCSDT{$BCS} = 3;

  # NEW:
  $step_size = 3;

  $BCS_date_L = 0;
  open(BCSDATEPRE,"$RUNDIR/${last_cycle}/etabcdate");
  $BCS_date_L = <BCSDATEPRE>;
  chomp($BCS_date_L);
  close(BCSDATEPRE);

  $BCS_CHANGE = 0;
  $BCS_CHANGE_hh = 0;

  # cycle hour hh: 5 <= $hh < 5+$CYC_INT or 17 <= $hh < 17+$CYC_INT  or  not $normal
  if ( ($hh >= 5 && $hh-5 < $CYC_INT) || ($hh >= 17 && $hh-17 < $CYC_INT)
       || $normal > 1)
  {
    $BCS_CHANGE = 1;
    $BCS_CHANGE_hh = $hh;
  }

  open (CRITIC, "$RUNDIR/critic.time");
  $time_max = <CRITIC>;
  close (CRITIC);

  if($BCS_CHANGE == 1 || ($BCS_CHANGE != 1 && $BCS_date == $BCS_date_L))
  {
    if($hh == $BCS_CHANGE_hh+$CYC_INT && $time_max > 420)
    {
      $pre_last_cycle = &hh_advan_date($last_cycle, -$CYC_INT);
      open(BCSDATEPRE,"$RUNDIR/${pre_last_cycle}/etabcdate") ;
      $BCS_date_L = <BCSDATEPRE>; chomp($BCS_date_L); close(BCSDATEPRE);
    }

    for ($i = 0; $i <= 72; $i+=$step_size)
    {
      $ii = sprintf("%02d", $i);
      $BCS_file = $BCS_date."_fh.00";
      $bcs_file_from = $DATA_ICBC_DIR."/".$BCS_file.$ii.'_tl.press_gr.onedeg'; #$DATA_AVN_DIR
      $bcs_file_to   = ${DATA_DIR}."/".$BCS_file.$ii.'_tl.press_gr.onedeg';

      # Exceptions in order to correct eta fcst error in 05Z and 17Z cylce
      if(($ii == 3 || $ii == 0) && $normal == 1 && $time_max > 420)
      {
        if($BCS_date == &hh_advan_date($BCS_date_L,12))
        {
          $BCS_file1 = $BCS_date_L."_fh.00";
          $bcs_file_from_1_12 = $DATA_ICBC_DIR."/".$BCS_file1."12".'_tl.press_gr.onedeg'; #$DATA_AVN_DIR
          $bcs_file_from_1_15 = $DATA_ICBC_DIR."/".$BCS_file1."15".'_tl.press_gr.onedeg'; #$DATA_AVN_DIR
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
          $BCS_file1 = $BCS_date_L."_fh.00";
          $bcs_file_from_1_24 = $DATA_ICBC_DIR."/".$BCS_file1."24".'_tl.press_gr.onedeg'; #$DATA_AVN_DIR
          $bcs_file_from_1_27 = $DATA_ICBC_DIR."/".$BCS_file1."27".'_tl.press_gr.onedeg'; #$DATA_AVN_DIR
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
      system (" ln -s -f $bcs_file_from ${bcs_file_to}") if(! -e ${bcs_file_to});
      if(! -e "$bcs_file_to")
      {
        print FILEL "    missing $bcs_file_to \n";
      }
    } # for $i

    open(ETADATE,">$RUNDIR/$this_cycle/etabcdate");  # etabcdate is used for either ETA or AVN
    print ETADATE $BCS_date;
    print ETADATE "\nThis is fresh $BCS. 0 - 72 h fcst is used";
    close(ETADATE);
    $ETA_STATUS = 0;
    print FILEL "   Finish $BCS data collection at ", &ctime(time);

    # Do interpolation of ETA / AVN onto Domain 1 grid and terrain

    if($MODEL eq "MM5")
    {
       print FILEL "\n   starting AVNFTP regrid.csh at ", &ctime(time);
       system("$CSH_ARCHIVE/Forecast/RT_A_regrid_avn_ftp_en.csh $BCS_date $this_cycle $RAP_start $RAP_end $normal $NODE");
       print FILEL "\n   ending AVNFTP regrid.csh at ", &ctime(time);

       # Fill in any missing time periods
       # print FILEL "\n   starting missing_times.csh at ", &ctime(time);
       # system("$CSH_ARCHIVE/Forecast/RT_B_missing_times.csh $BCS_date $this_cycle");
       # print FILEL "   ending   missing_times.csh at ", &ctime(time);
    }
    elsif ($MODEL eq "WRF")
    {
      print FILEL "\n   starting $BCS wps at ", &ctime(time);
      $BCSINT=10800;
      #system("$CSH_ARCHIVE/Forecast/RT_A_wrfsi_test.csh $this_cycle $RAP_start $RAP_end $GSJOBID $NODE $BCS $BCSINT $RUNDIR_ROOT");
      system("$CSH_ARCHIVE/Forecast/RT_A_wrf_wps.csh $this_cycle $RAP_start $RAP_end $NUM_PROCS $NODE $BCS $BCSINT $NUM_DOMS");
      print FILEL "\n   ending  $BCS wps at ", &ctime(time);
    }
  }
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
  }
  else
  {
    $bcs_size = 5;  #need work here!!
    $BCS_SIZE_C=1;
  }

  #########Check if the file size of REGRIDv3 is big enough#######################
  if ( $bcs_size < $BCS_SIZE_C)
  {
    $ETA_STATUS = -1;
    print FILEL "\n   -- Not find enough data for BC, IC and 1ST Guess. \n";
    print FILEL "\n   starting recreating 1ST guess from 12 hour earlier $BCS at ", &ctime(time);
    system("rm $RUNDIR/$this_cycle/*REGRIDv3*");
    system("rm ${DATA_DIR}/eta*");

    $BCS_date_L = &hh_advan_date($BCS_date,-12);

    for ($i = 12; $i <= 72; $i+=$step_size)
    {
      $ii = sprintf("%02d", $i);
      $iii = sprintf("%02d", $i-12);

      $BCS_file = $BCS_date_L."_fh.00";
      $bcs_file_from = $DATA_ICBC_DIR."/".$BCS_file.$ii."_tl.press_gr.onedeg"; #$DATA_AVN_DIR
      $bcs_file_to   = ${DATA_DIR}."/".$BCS_file.$ii."_tl.press_gr.onedeg";
      print FILEL "     $BCS:      $bcs_file_from  \n     --> $bcs_file_to \n";
      system (" ln -s -f $bcs_file_from ${bcs_file_to}") if(! -e ${bcs_file_to});
      if (! -e "$bcs_file_to")
      {
        print FILEL "\n    missing $bcs_file_to";
      }
    } #for $i

    print FILEL "   Finish 2nd $BCS data collection at ", &ctime(time);

    # Do interpolation of ETA / AVN onto Domain 1 grid and terrain
    if ( $MODEL eq "MM5")
    {
      print FILEL "\n   starting AVNFTP regrid.csh at ", &ctime(time);
      system("$CSH_ARCHIVE/Forecast/RT_A_regrid_avn_ftp_en.csh $BCS_date_L $this_cycle $RAP_start $RAP_end $normal $NODE");
      print FILEL "\n   ending   regrid.csh at ", &ctime(time);

      # Fill in any missing time periods
      print FILEL "\n   starting missing_times.csh at ", &ctime(time);
      system("$CSH_ARCHIVE/Forecast/RT_B_missing_times.csh $BCS_date $this_cycle");
      print FILEL "   ending   missing_times.csh at ", &ctime(time);
    }
    elsif ($MODEL eq "WRF")
    {
      print FILEL "\n   starting $BCS wps at ", &ctime(time);
      $BCSINT=10800;
      #system("$CSH_ARCHIVE/Forecast/RT_A_wrfsi_test.csh $this_cycle $RAP_start $BCS_end $GSJOBID $NODE $BCS $BCSINT $RUNDIR_ROOT");
      system("$CSH_ARCHIVE/Forecast/RT_A_wrf_wps.csh $this_cycle $RAP_start $BCS_end $NUM_PROCS $NODE $BCS $BCSINT $NUM_DOMS");
      print FILEL "\n   ending   $BCS wps at ", &ctime(time);
    }

    open(ETADATE,">$RUNDIR/$this_cycle/etabcdate");
    print ETADATE $BCS_date_L;
    print ETADATE "\nThis is 12-hour older $BCS. 12 - 72 h fcst is used";
    close(ETADATE);
    print FILEL "\n   ending recreating 1ST guess from 12 hour earlier eta data at ", &ctime(time);
  }
} #sub
1;
