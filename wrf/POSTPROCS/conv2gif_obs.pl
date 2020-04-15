

sub conv2gif_obs
{
my ($cgm_file,$domain, $valid_time) = @_;
my $i;
my $iframe;
my $ppi = 72;
my $xmax = $ppi*8.5;
my $ymax = $ppi*11;
my $big;
my $density_string;

# Note: The first argument passed in is not used for WRF obs convert. It is
# kept for backward compatible with MM5 obs convert.

print "\nconv2gif_obs ($cgm_file,$domain, $valid_time)\n\n";

$ENV{'NCARG_ROOT'} = $NCARG_ROOT;
$ENV{'NCARG_LIB'} = $NCARG_LIB;
if ( $IS_RIP4 || $IS_WRF ) {
 $ENV{'STATIONLIST'} = "$MM5HOME/cycle_code/CONSTANT_FILES/RIP4/stationlist";
 $ENV{'HIRESMAP'} = "$MM5HOME/cycle_code/CONSTANT_FILES/RIP4/${RANGE}_map.ascii";
 $ENV{'RANGEMAP'} = "$MM5HOME/cycle_code/CONSTANT_FILES/RIP4/${RANGE}_map.ascii";
 $ENV{'RIP_ROOT'} = "$MM5HOME/cycle_code/CONSTANT_FILES/RIP4";
 $RIP_ROOT = "$MM5HOME/cycle_code/CONSTANT_FILES/RIP4";
} else {
 $ENV{'STATIONLIST'} = "$MM5HOME/cycle_code/CONSTANT_FILES/RIP/stationlist";
 $ENV{'HIRESMAP'} = "$MM5HOME/cycle_code/CONSTANT_FILES/RIP/${RANGE}_map.ascii";
 $ENV{'RANGEMAP'} = "$MM5HOME/cycle_code/CONSTANT_FILES/RIP/${RANGE}_map.ascii";
 $ENV{'RIP_ROOT'} = "$MM5HOME/cycle_code/CONSTANT_FILES/RIP";
 $RIP_ROOT = "$MM5HOME/cycle_code/CONSTANT_FILES/RIP";
}

#@MONTHS=( '01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12');
#@SEASONS=( 'winter', 'winter', 'winter', 'winter', 'summer', 'summer', 'summer', 'summer', 'summer', 'summer', 'summer', 'winter', 'winter');

system("mkdir -p $PLOTS_DIR/gifs_ugui");
system("mkdir -p $PLOTS_DIR/gifs_ugui/$valid_time");
chdir "$PLOTS_DIR";
chdir "gifs_ugui";

@fields = ( 'sfcobs', '850rpr', '700rpr', '500rpr', '300rpr', '0535sw', '3560sw', '60sfsw' );
# The following if-block commented out by Rong 6/1/2010
#if ( $DO_MAGEN4 ) {
#@rotateimg = ( -90, -90, 0, 0, 0);
#} else {
#@rotateimg = ( -90, -90, -90, 0, 0);
#}
#

$fnum = 0;

if ($IS_WRF) {  # WRF obs convert

  # convert sfc obs plot
  if ( -e "$PLOTS_DIR/SFCplot.eps" ) {
    $gif = "$valid_time/d${domain}_$fields[$fnum].gif";
    $tmp = `identify $PLOTS_DIR/SFCplot.eps`;
    $tmp =~ /(\d+)x(\d+)/;
    $xsize = $1;
    $ysize = $2;

    if ($xsize > $ysize) {
       $big = $xsize;
    } else {
       $big = $ysize;
    }

    if ($big*1.5 <= $ymax) {
       $density_string='-density 108';
    } elsif ($big*1.25 <= $ymax) {
       $density_string='-density 90';
    } else {
       $density_string="";
    }

    print "In conv2gif_obs, xsize = $xsize, ysize = $ysize, big = $big, density_string = $density_string\n";

    system("convert $density_string -flatten $PLOTS_DIR/SFCplot.eps $gif");

    unlink "$PLOTS_DIR/SFCplot.eps";

  } else {
    $gif = "$valid_time/d${domain}_$fields[$fnum].gif";
    print "$PLOTS_DIR/SFCplot.eps does not exist!\n";
    system("cp $RIP_ROOT/not_yet_avail.gif $gif");
  } 

  $fnum++;

  # convert upper-air obs plots
  if (-e "$PLOTS_DIR/RAOBplot.ps" ) {
    system("convert -trim +repage $density_string $PLOTS_DIR/RAOBplot.ps RAOBplot.gif");
    system("convert +adjoin -coalesce RAOBplot.gif frame%02d.gif");
    unlink "$PLOTS_DIR/RAOBplot.ps";
    unlink "RAOBplot.gif";
  } else {
    print "$PLOTS_DIR/RAOBplot.ps does not exist!\n";
    
  }

  foreach $i (0..3) {
    $iframe = sprintf("%02d",$i);
    $gif = "$valid_time/d${domain}_$fields[$fnum].gif";
    if (-e "frame${iframe}.gif") {
      system("mv frame${iframe}.gif $gif");
    } else {
      system("cp $RIP_ROOT/not_yet_avail.gif $gif");
    }

    $fnum++;
  }

  # convert satellite wind plots
  if (-e "$PLOTS_DIR/UPPplot.ps" ) {
    system("convert -trim +repage $density_string $PLOTS_DIR/UPPplot.ps UPPplot.gif");
    system("convert +adjoin -coalesce UPPplot.gif frame%02d.gif");
    unlink "$PLOTS_DIR/UPPplot.ps";
    unlink "UPPplot.gif";
  } else {
    print "$PLOTS_DIR/UPPplot.ps does not exist!\n";
  }

  foreach $i (0..2) {
    $iframe = sprintf("%02d",$i);
    $gif = "$valid_time/d${domain}_$fields[$fnum].gif";
    if (-e "frame${iframe}.gif") {
      system("mv frame${iframe}.gif $gif");
    } else {
      system("cp $RIP_ROOT/not_yet_avail.gif $gif");
    }

    $fnum++;
  }

} else {  ## MM5 obs convert

  foreach $fld (@fields) {

    $fnum++;
    system("rm filename");

    if ( -e "../riprun_obs/$cgm_file" ) {

      system("ln -sf ../riprun_obs/$cgm_file filename");
      system("cat $RIP_ROOT/mjfile | sed s/filenum/$fnum/  > jfile");

      system("$NCARG_ROOT/bin/med -f jfile");
      $newgif = $valid_time . "/d" . $domain . "_" . $fld . ".gif";
      print "$RIP_ROOT/ncgm2gif gpart\n";
      system("$RIP_ROOT/ncgm2gif gpart");
      print "mv gpart.gif $newgif\n";
      system("mv gpart.gif $newgif");
      system("ls -lR *");

    }else{
      $newgif = $valid_time . "/d" . $domain . "_" . $fld . ".gif";
      system("cp $RIP_ROOT/not_yet_avail.gif $newgif");
      print "\nError generating: $newgif:\n";
      print "Cannot find file:   $PLOTS_DIR/riprun_obs/$cgm_file\n";
      system ("ls -al $PLOTS_DIR/riprun_obs");
      print "Use $RIP_ROOT/not_yet_avail.gif instead\n";
    }
  }

} # end if $IS_WRF

}
1;
