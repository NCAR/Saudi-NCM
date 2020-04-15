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
