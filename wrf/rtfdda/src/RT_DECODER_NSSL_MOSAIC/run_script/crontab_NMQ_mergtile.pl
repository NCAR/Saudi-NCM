#!/usr/bin/perl

$RANGE_NAME = shift(@ARGV);
$D_ID       = shift(@ARGV);
$src_dir = "/raid/radar/source/MM5_NEW";
$ter_file = "/raid/GMODJOBS/GM${RANGE_NAME}/TERRAIN/TERRAIN_DOMAIN${D_ID}";
$out_dir = "/raid/radar_ranges/${RANGE_NAME}";
#$data_dir = "/faa_ldmData/nsslMosaic";
$data_dir = "faa-c1:/raid/ldmData/nsslMosaic";
$tile_dir = "/raid/faa_ldmData/nsslMosaic";

chdir $src_dir;
system "rm namelist.mosaic";
system "cp   ${RANGE_NAME}/namelist.mosaic_${RANGE_NAME}${D_ID} namelist.mosaic";
print "$RANGE_NAME  $D_ID \n";
$current_time = `date +%Y%m%d%H`;

for($i = 65; $i >= 5;$i=$i-5) {
 $start_time = advance_time($current_time, -${i}*60) ;
 $data_time = substr($start_time,0,12);
 print "$data_time\n";
 
 $this_date = substr($start_time,0,8);
 $hh = substr($start_time,8,2);
 $min = substr($start_time,10,2);
 if ($D_ID == 2 && $tile_dir ne $data_dir ) {
 system"mkdir $tile_dir/tile5/mosaic3d_nc/$this_date" if ( ! -e "$tile_dir/tile5/mosaic3d_nc/$this_date");
 system"scp $data_dir/tile5/mosaic3d_nc/$this_date/${this_date}_$hh$min $tile_dir/tile5/mosaic3d_nc/$this_date/.";
 system"mkdir $tile_dir/tile6/mosaic3d_nc/$this_date" if ( ! -e "$tile_dir/tile6/mosaic3d_nc/$this_date");
 system"scp $data_dir/tile6/mosaic3d_nc/$this_date/${this_date}_$hh$min $tile_dir/tile6/mosaic3d_nc/$this_date/.";
 }
 print "$src_dir/MERGTILE $out_dir $ter_file $data_time ${RANGE_NAME} ${D_ID} $tile_dir\n" ;
 system"$src_dir/MERGTILE $out_dir $ter_file $data_time ${RANGE_NAME} ${D_ID} $tile_dir" ;
 if ($D_ID == 3 ) {
   system"rm $tile_dir/tile5/mosaic3d_nc/$this_date/${this_date}_$hh$min";
   system"rm $tile_dir/tile6/mosaic3d_nc/$this_date/${this_date}_$hh$min";
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
