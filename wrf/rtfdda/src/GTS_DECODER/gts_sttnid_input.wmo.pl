#!/usr/bin/perl
 
open (IN,"gts_sttnid_input.wmo.txt_old");
open (OUT,">gts_sttnid_input.wmo.txt");
while($aline=<IN>) {
	$akey = substr($aline,0,28);
	$sta{$akey}=$aline;
}
foreach $aa (sort keys %sta) {
	print OUT $sta{$aa}
}
