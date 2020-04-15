#!/usr/bin/perl
#
$RANGE="DPG";
$QCOUT_EXE="/raid/fddahome_dev/cycle_code/EXECUTABLE_ARCHIVE/v_qcobs_from_sfcsta.exe";
$CRESS_EXE="/raid/fddahome_dev/cycle_code/EXECUTABLE_ARCHIVE/v_cress_sfc_interp.exe";
$BIASC_EXE="/raid/fddahome_dev/cycle_code/EXECUTABLE_ARCHIVE/v_sfc_pair_bias_remove.exe";

foreach $RANGE ("DPG","CRTC") {
 open(IN,"sfc_pair_$RANGE");
 while ($afile = <IN>) {
	chomp($afile);
	$bfile=$afile;
	$bfile =~ s/SAMS/BIASC/;
	$bfile =~ s/hold_run/bias_correct/;
	system("rm obs.dat; $QCOUT_EXE $afile");
	system("rm pairs_domain1; $BIASC_EXE TERRAIN4_$RANGE");
	system("dd bs=56 conv=swab if=pairs_domain1 of=pairs_swap");
	system("mv pairs_swap $bfile");
 }
 close(IN)
}
exit;

open(IN,"sfc_pair_$RANGE");
while ($afile = <IN>) {
	chomp($afile);
	$bfile=$afile;
	$bfile =~ s/SAMS/CRESS/;
	$bfile =~ s/hold_run/ana_cress/;
	system("rm obs.dat; $QCOUT_EXE $afile");
	system("rm pairs_domain1; $CRESS_EXE TERRAIN4_$RANGE");
	system("dd bs=56 conv=swab if=pairs_domain1 of=pairs_swap");
	system("mv pairs_swap $bfile");
}
#
