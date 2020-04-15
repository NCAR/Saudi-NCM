#
# scp the hourly files to the web server for interactive-naps
#
# @args - 0 - hourly file
# @args - 1 - valid time
# @args - 2 - this_domain 
# @args - 3 - Destination string for scp command
#
sub do_naps_default
{
    my ($hourly_file,$valid_time, $this_domain, $dest) = @_;
#
    chomp $hourly_file;
    $TOTAL_TIME_STEPS = 1;

$ENV{'NCARG_ROOT'} = "/opt/ncarg";
$ENV{'NCARG_LIB'} = "/opt/ncarg/lib";
$RIP_ROOT = "$MM5HOME/cycle_code/CONSTANT_FILES/RIP";


#
# Directory for naps files
#
system("$MustHaveDir $NAPS_DIR");
system("$MustHaveDir $NAPS_DIR/napstest");
chdir $NAPS_DIR;

#
# Executable files
#
$MM52SDG = "$MM5HOME/cycle_code/POSTPROCS/r_mm5_w_snd.job";
$SKEWT = "$MM5HOME/cycle_code/POSTPROCS/skewt_naps.csh";
$NAPSMM5 = "$MM5HOME/cycle_code/POSTPROCS/napsmm5.csh";


    # Convert the MM5 data to a sounding
    &mm5_2_sdg(${hourly_file}, 1);
    system("mv sndNAPS sndNAPS.txt");

    system("cp sndNAPS.txt ./napstest");

    &skewt($NAPS_DIR, "sndNAPS.txt");

    &run_naps($NAPS_DIR, "sndNAPS.txt");

    system("rsync -e 'ssh -i /home/fddasys/.ssh/rtfdda' -avzC snd*.txt $dest");
    system("rsync -e 'ssh -i /home/fddasys/.ssh/rtfdda' -avzC *.gif $dest");

}

#
# Convert this MM5 file to soundings
#
# args - 0 - directory
# args - 1 - MM5 file
#
sub mm5_2_sdg
{
    local($file) = @_[0];
    local($hours) = @_[1];
    if ( -e "param.txt" ) {
       system("rm param.txt");
    }
    open(PARAM, ">>./param.txt"); 
    print PARAM "Plate Range\n";
    print PARAM "Plate Range\n";
    print PARAM "Plate Range\n";
    print PARAM "39.447\n";
    print PARAM "-76.107\n";
    print PARAM $TOTAL_TIME_STEPS;
    close (PARAM);

    system( "$MM52SDG $file $hours" );
    $OUTFILENAME = "snd_D3_" . $valid_time . ".txt";
    system("cp sndNAPS $OUTFILENAME");
}



#
# Convert the soundings to skewt gifs
#
# args - 0 - directory
# args - 1 - ASCII sdg file
#
sub skewt
{
    local($dir) = @_[0];
    $file = @_[1];

    system( "$SKEWT $file" );

    if ( -e "med.tmp" ) {
       system("rm med.tmp");
    }
    open( MED, ">>./med.tmp");
    print MED "r gmeta\n";
    print MED "1w gpart\n";
    close (MED);

    system("/opt/ncarg/bin/med -f med.tmp");
    system("rm med.tmp");

    $OUTFILENAME = "skewt_D" . $this_domain . "_" . $valid_time . ".gif";
    $OUTFILENAME = "skewt_D3_" . $valid_time . ".gif";
    system("$RIP_ROOT/ncgm2gif gpart");
    system("mv gpart.gif $OUTFILENAME");

}

sub run_naps
{
    local($dir) = @_[0];
    $file = @_[1];

    system( "$NAPSMM5 $file" );

    if ( -e "med.tmp" ) {
       system("rm med.tmp");
    }
    open( MED, ">>./med.tmp");
    print MED "r gmeta\n";
    print MED "1w gpart\n";
    close (MED);

    system("/opt/ncarg/bin/med -f med.tmp");
    system("rm med.tmp");

    $OUTFILENAME = "naps_D" . $this_domain . "_" . $valid_time . ".gif";
    $OUTFILENAME = "naps_D3_" . $valid_time . ".gif";
    system("$RIP_ROOT/ncgm2gif gpart");
    system("mv gpart.gif $OUTFILENAME");

}

1;
