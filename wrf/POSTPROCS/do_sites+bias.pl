
sub do_sites
{
    my ($hourly_file,$ndom, $cycletag, $dest) = @_;

#
# Executable files
#
$SITES_EXE = "$MM5HOME/cycle_code/EXECUTABLE_ARCHIVE/sites_new.exe";

system("$MustHaveDir $SITES_DIR");
system("$MustHaveDir $SITES_ARCHIVE_DIR");
if ( $DO_SITES_BIAS ) {
  system("$MustHaveDir $SITES_ARCHIVE_BIAS_DIR");
}
chdir $SITES_DIR;
system("rm site*.dat");
system("rm site*.bias");
system("rm stationmap_new.list");

if ( -e "$MM5HOME/cycle_code/POSTPROCS/stationmap_new.$range.list" ) {
  system("ln -s $MM5HOME/cycle_code/POSTPROCS/stationmap_new.$range.list stationmap_new.list");
} else {
  system("ln -s $MM5HOME/cycle_code/POSTPROCS/stationmap_new.list stationmap_new.list");
}


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

foreach $f (`ls *.dat`) {

  chomp($f);
  $archfile = "$SITES_ARCHIVE_DIR/$f";
  if ( -e $archfile ) {
     system("cat $archfile $f > tmpfile");
     system("tail -250 tmpfile > $archfile");
  } else {
     system("mv $f $SITES_ARCHIVE_DIR");
  }
  if ( $DO_SITES_BIAS ) {
    $stn = substr($f,4,3);
    $biasfile = "$SITES_BIAS_DATA_DIR/${stn}_bias_correction.dat";
    if ( -e $biasfile ) {
    open (STN_DATA,$f);
    $aline=<STN_DATA>;
    $city=" ";
    $state=" ";
    if ( (split(/ +/, $aline) < 22) ) {
      ($sta_name, $lat, $long, $height, $yy, $mm, $dd, $hhmm, $cycle_tag, $mslp,
      $t2, $td, $rh, $wdir10, $wspd10, $cc, $precip, $pbl,$q,$stnp,$ter) = split(/ +/, $aline)
    } else {
      ($city, $state, $sta_name, $lat, $long, $height, $yy, $mm, $dd, $hhmm, $cycle_tag, $mslp,
      $t2, $td, $rh, $wdir10, $wspd10, $cc, $precip, $pbl,$q,$stnp,$ter) = split(/ +/, $aline)
    }
    $timekey = $yy.$mm.$dd;
    if ( $cycle_tag eq "prelim"  || $cycle_tag eq "fcst") {
      $cycle_tag_use = "fcst";
      } else {
      $cycle_tag_use = "final";
      }
    # read through the bias correction file to see if there's a valid bias for this time/fcst
    open (STN_BIAS,$biasfile);
    # skip the two lines of header info
    $aline=<STN_BIAS>;
    $aline=<STN_BIAS>;
    $match = 0;
    while (<STN_BIAS>) {
      my ( $station,$valid_date,$utc,$fcst_type,$temp_bc,$spec_hum_bc,$spd_bc,$u_bc,$v_bc, $count_temp,$count_spec_hum,$count_spd,$count_uv ) = split;
    # if so, subtract the bias from the computed values, & write the new data to the SITES_BIAS_DIR files
    # if not, skip this time/date/field
      if ( $cycle_tag_use eq $fcst_type && $hhmm eq $utc ) {
	$match = 1;
	# Loop through all of the biases in the file for this valid hour & use the most recent non-missing one
        if ( $temp_bc != -8888.00 ) {
	   $temp_bc_use = $temp_bc;
        } else {
	   $temp_bc_use = "MISSING";
        }
        if ( $spec_hum_bc != -8888.00 ) {
	   $spec_hum_bc_use = $spec_hum_bc;
        } else {
	   $spec_hum_bc_use = "MISSING";
        }
        if ( $u_bc != -8888.00 && $v_bc != -8888.00 ) {
	   $u_bc_use = $u_bc;
	   $v_bc_use = $v_bc;
        } else {
	   $u_bc_use = "MISSING";
	   $v_bc_use = "MISSING";
        }
        if ( $spd_bc != -8888.00 ) {
	   $spd_bc_use = $spd_bc;
        } else {
	   $spd_bc_use = "MISSING";
        }
       }
     }
     if ( $match == 0 ) {
       $t2_bc="MISSING";
       $wdir10_bc="MISSING";
       $wspd10_bc="MISSING";
       $rh_bc="MISSING";
       $mslp_bc = "MISSING";
     } else {
        if ( $temp_bc_use ne "MISSING") {
	   $t2_bc = $t2 - $temp_bc_use;
        } else {
	   $t2_bc = "MISSING";
        }
        if ( $spec_hum_bc_use ne "MISSING") {
	   $q_bc = $q - $spec_hum_bc_use;
	   if ( $t2_bc ne "MISSING" ) {
	     $rh_bc = rhcalc($t2_bc, $q_bc, $stnp);
	     $td_bc = tdcalc($t2_bc, $rh_bc );
	   } else {
	     $rh_bc = "MISSING";
	     $td_bc = "MISSING";
	   }
        } else {
	   $q_bc = "MISSING";
	   $rh_bc = "MISSING";
	   $td_bc = "MISSING";
        }
        if ( $q_bc ne "MISSING"&& $t2_bc ne "MISSING") {
           $mslp_bc = slpcal($stnp, $t2_bc, $ter, $q_bc);
	} else {
           $mslp_bc = "MISSING";
        }
        if ( $u_bc_use ne "MISSING"&& $v_bc_use ne "MISSING") {
	   $u = uwind($wspd10,$wdir10);
	   $v = vwind($wspd10,$wdir10);
	   $u = $u - $u_bc_use;
	   $v = $v - $v_bc_use;
	   $wdir10_bc = (wdir( $u, $v) % 360);
        } else {
	   $wdir10_bc = "MISSING";
        }
        if ( $spd_bc_use ne "MISSING") {
	   $wspd10_bc = $wspd10 * $spd_bc_use;
        } else {
	   $wspd10_bc = "MISSING";
        }
     }
     $newfile = "$SITES_DIR/site${stn}.bias";
     open (STN_NEW,">${newfile}");
         $label = $city . " " . $state;
          printf STN_NEW "%s   %3s%7.3f%9.3f%7.1f %4s %2s %2s %4s %5s",
          $label, $sta_name, $lat, $long, $height, $yy, $mm, $dd, $hhmm, $cycle_tag;
#	  if ( $mslp_bc eq "MISSING" ) {
#	    printf STN_NEW " %7s", $mslp_bc;
#	  } else {
#	    printf STN_NEW " %6.1f", $mslp_bc;
#	  }
#	  For now... always output the uncorrected MSLP...
	    printf STN_NEW " %6.1f", $mslp;
	  if ( $t2_bc eq "MISSING" ) {
	    printf STN_NEW " %7s", $t2_bc;
	  } else {
	    printf STN_NEW " %5.1f", $t2_bc;
	  }
	  if ( $rh_bc eq "MISSING" || $td_bc eq "MISSING" ) {
	    printf STN_NEW " MISSING MISSING";
	  } else {
	    printf STN_NEW " %5.1f %4.0f", $td_bc, $rh_bc;
	  }
	  if ( $wdir10_bc eq "MISSING" ) {
	    printf STN_NEW " %7s", $wdir10_bc;
	  } else {
	    printf STN_NEW " %3d", $wdir10_bc;
	  }
	  if ( $wspd10_bc eq "MISSING" ) {
	    printf STN_NEW " %7s %3s %6.3f %6.0f\n", $wspd10_bc, $cc, $precip, $pbl;
	  } else {
	    printf STN_NEW " %5.1f %3s %6.3f %6.0f\n", $wspd10_bc, $cc, $precip, $pbl;
	  }
     close(STN_NEW);
     $archfile = "$SITES_ARCHIVE_BIAS_DIR/$f";
     if ( -e $archfile ) {
        system("cat $archfile $newfile > tmpfile");
        system("tail -250 tmpfile > $archfile");
     } else {
        system("mv $newfile $SITES_ARCHIVE_BIAS_DIR/$f");
     }


    }
  }
}

#LPCsystem("scp $SITES_ARCHIVE_DIR/\*.dat $SITES_DEST_DIR");
system("rsync -e 'ssh -i $KEY' -avzC $SITES_ARCHIVE_DIR/\*.dat $SITES_DEST_DIR");
if ( $DO_SITES_BIAS ) {
  system("rsync -e 'ssh -i $KEY' -avzC $SITES_ARCHIVE_BIAS_DIR/\*.dat $SITES_DEST_BIAS_DIR");
}


}
#
##  sub deg_to_rad(x)
##
##  returns the value of x degrees in radians
##
sub deg_to_rad {
    ($x) = @_;

    return $x * 0.01745329251994372;
}


sub uwind {
    ($wspd, $wdir) = @_;

    return sprintf("%.2f", (-($wspd) * sin(deg_to_rad($wdir))));
}

sub vwind {
    ($wspd, $wdir) = @_;

    return sprintf("%.2f", (-($wspd) * cos(deg_to_rad($wdir))));
}
sub wdir {
    ($u, $v) = @_;
    $pi = 3.1415926;
    return sprintf("%.2f", (1.5*$pi-atan2($v,$u))*180./$pi);
}
#
sub rhcalc {
 ($t, $q, $p ) = @_;
 use POSIX qw(log10);
  $t = $t+273.15;
  # convert to kg/kg
  $qk = $q*.001;
# in mb
  $es=10**(-2937.4/$t-4.9283*log10($t)+23.5518);  
  if ($t < 233. ) {
    # saturation over ice
    $es=10**(10.5553-2667./$t);     
    }
  $re=$qk/(1.-$qk);
  $ee=$p*$re/(0.622+$re);
  $rh = $ee/$es;
  if(rhcalc > 1.) {
    $rh =1.0;
    }

 return sprintf("%.2f", $rh*100.0);
}
sub tdcalc {
 ($t, $rh ) = @_;
 my $x = abs(log ($rh*0.01));
 my $xr = 461.51;
 my $xlv = 2500000.0;
 my $td = ($t+273.15) / ($xr*$x*($t+273.15)/$xlv +1.0);
 return sprintf("%.2f", $td-273.15);
}

sub slpcal {
 ($psfc, $t2, $ter, $q ) = @_;
 my $rgas = 287.05;
 my $g = 9.8;

 my $qs = $q*0.001;
 my $tv = ($t2+273.15) * ( 1.0 + 0.61*$qs/(1.0+$qs));
 my $slp = $psfc * exp( $ter * $g / $rgas / $tv);

 return sprintf("%.1f", $slp);
}
1;

