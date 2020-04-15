#!/usr/bin/perl

$orig = shift;

open(IN,"$orig");
open(TMP,">${orig}.tmp");

$cat_old = 0;
while (<IN>) {
  next if ( /^$/ );

  if ( /</ || /^TT/ || /^77/ || /^88/ || /^31313/ || /^41414/ ||
      /^51515/ || /^61616/ ) {
     $cat = 0; 
     if ($cat_old) {
        print TMP "\n$_";
     } else {
        print TMP;
     }
  } else {
     $cat = 1;
     chomp;
     s/\s+$//;
     if ($cat && !$cat_old ) {
        print TMP "$_";
     } else {
        if ( /^21212/ ) {
           print TMP "\n$_";
        } else {
           print TMP " $_";
        }
     }
  }

  $cat_old = $cat;

}

close(IN);
close(TMP);

exit;
