#!/usr/bin/perl
use File::Basename;

# merge NCEP MRMS reflectivity data and map to RTFDDA grid
# each call process datasets in one hour for one domain 
# last modified: 02-10-2015 
#
$RANGE_NAME = shift(@ARGV);
$D_ID       = shift(@ARGV);
$current_time = shift(@ARGV);
 
$GSJOBDIR = $ENV{'GSJOBDIR'};
#$GSJOBDIR = "/modelrd/c4test/GMODJOBS/GW${RANGE_NAME}";
$src_dir = "$GSJOBDIR/rda/interp_ncepMRMS";
$out_dir = "/modelrd/c4test/radar_ingest/ncepMosaic/${RANGE_NAME}";
$data_dir = "/rdadata/input/mergedRefl";

$ter_file = "$GSJOBDIR/wps/geo_em.d0${D_ID}.nc";
$namelist = "$GSJOBDIR/rda/namelist.mosaic.ncep_${RANGE_NAME}${D_ID}";
$current_time = `date +%Y%m%d%H` if(! $current_time);
$current_time = substr($current_time,0,10);

print "   namelist: [$namelist]\n" ;
$_ops_sub_script_name = dirname($0) . "/crontab_job.pl";
if ( -e $_ops_sub_script_name) {
  print "  require $_ops_sub_script_name ==\n";
  require $_ops_sub_script_name;
  $data_dir = $tile_dir if ( defined $tile_dir );
}
print "$RANGE $D_ID  time = $current_time  output: [$out_dir], data_dir: [$data_dir]\n" ;

if ( ! -e $namelist ) {
  print " ***** ERROR namelist [$namelist] does not exist. No action *****\n" ;
}
else {
  $namelist_in = "namelist.mosaic.ncep";
  print "  namelist: [$namelist] ==> $namelist_in\n" ;
  system "rm $namelist_in" if ( -e $namelist_in);
  system "cp $namelist $namelist_in";

  # modify time - meixu 20101217
  my @mrmsSamples = ( 56, 34, 30, 26, 4, 0 );
  foreach $i (@mrmsSamples) {
    $start_time = advance_time($current_time, -${i}*60) ;
    $data_time = substr($start_time,0,12);
    print "   data_time: : $data_time\n";
    $this_date = substr($start_time,0,8);
    $hh = substr($start_time,8,2);
    $min = substr($start_time,10,2);
    system "mkdir -p $out_dir/$this_date" if ( ! -e "$out_dir/$this_date");
    #print "$src_dir/MERGTILE $out_dir $ter_file $data_time ${RANGE_NAME} ${D_ID} $tile_dir\n" ;
    #system"$src_dir/MERGTILE $out_dir/$this_date $ter_file $data_time ${RANGE_NAME} ${D_ID} $tile_dir" ;
    $this_min = "${hh}${min}";
    $tile_dir = `ls -d $data_dir/$this_date/${this_min}*`;
    chomp($tile_dir);
    #$tile_dir = "$data_dir/$this_date/$this_ss";
    if ( -s $tile_dir) {
      $system_cmd = "$src_dir/INTERP_ncepMRMS $out_dir/$this_date $ter_file $data_time ${RANGE_NAME} ${D_ID} $tile_dir";
      print "   $system_cmd\n" ;
      $system_output = `$system_cmd`;
      print "   $system_output\n" if ( defined $ENV{debug} );
    }
    else {
      print "   [$data_dir/$this_date/${this_min}*] does not exist\n";
    }
  }
}

sub advance_time {
   my ($s_date,$add_s)=@_;
   my ($yy,$mm,$dd,$hh,$mi,$ss);
   my @mdays=(31,31,28,31,30,31,30,31,31,30,31,30,31);
   my $time_new;
# if the advancement is more than 2 months, error may occur
   my $yy = substr($s_date,0,4);
   my $mm = substr($s_date,4,2);
   my $dd = substr($s_date,6,2);
   my $hh = substr($s_date,8,2);
   my $mi = substr($s_date,10,2);
   my $ss = substr($s_date,12,2);

  if($yy%400 == 0) { $mdays[2]=29; 
  } else { $mdays[2]=29 if(($yy%4 == 0) && ($yy%100 != 0)); }

	 $ss = $ss + $add_s;

     while($ss > 59) { $ss -= 60; $mi++; }
     while($mi > 59) { $mi -= 60; $hh++; }
     while($hh > 23) { $hh -= 24; $dd++; }
	 while($dd > $mdays[$mm+0]) { 
	   $dd = $dd - $mdays[$mm+0]; 
	   $mm++; 
       while($mm > 11) { $mm -= 12; $yy++; }
	 }
#
# need to use updated $mdays[$mm+0] if $yy is changed
# okay if $add_s is never > 2 months 
#
     while($ss < 0) { $ss += 60; $mi--; }
	 while($mi < 0) { $mi += 60; $hh--; }
	 while($hh < 0) { $hh += 24; $dd--; }
	 while($dd < 1) { 
       $mm--;
	   while($mm < 1) { $mm += 12; $yy--; }
	   $dd = $dd + $mdays[$mm+0];
     }

  $time_new=($yy*1000000+$mm*10000+$dd*100+$hh)*10000+$mi*100+$ss;
  return $time_new;
 }

sub advance_time_hh {
   my ($s_date,$add_h)=@_;
   my ($yy,$mm,$dd,$hh);
   my @mdays=(31,31,28,31,30,31,30,31,31,30,31,30,31);
   my $time_new;
# if the advancement is more than 2 months, error may occur
   my $yy = substr($s_date,0,4);
   my $mm = substr($s_date,4,2);
   my $dd = substr($s_date,6,2);
   my $hh = substr($s_date,8,2);

  if($yy%400 == 0) { $mdays[2]=29;
  } else { $mdays[2]=29 if(($yy%4 == 0) && ($yy%100 != 0)); }

	   $hh = $hh + $add_h;

       while($hh > 23) { $hh -= 24; $dd++; }
       while($dd > $mdays[$mm+0]) {
         $dd = $dd - $mdays[$mm+0];
         $mm++;
         while($mm > 11) { $mm -= 12; $yy++; }
    }
#
# need to use updated $mdays[$mm+0] if $yy is changed
# okay if $add_h is never > 2 months

     while($hh < 0) { $hh += 24; $dd--; }
     while($dd < 1) {
       $mm--;
	   while($mm < 1) { $mm += 12; $yy--; }
	   $dd = $dd + $mdays[$mm+0];
     }
    $time_new=$yy*1000000+$mm*10000+$dd*100+$hh;
    return $time_new;
}
1;
