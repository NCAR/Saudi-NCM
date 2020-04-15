

sub conv2gif
{
my ($cgm_file,$domain, $valid_time) = @_;

$ENV{'NCARG_ROOT'} = "/opt/ncarg";
$ENV{'NCARG_LIB'} = "/opt/ncarg/lib";
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

@MONTHS=( '01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12');
@SEASONS=( 'winter', 'winter', 'winter', 'winter', 'summer', 'summer', 'summer', 'summer', 'summer', 'summer', 'summer', 'winter', 'winter');

system("$MustHaveDir $PLOTS_DIR/gifs_ugui");
system("$MustHaveDir $PLOTS_DIR/gifs_ugui/$valid_time");
chdir $PLOTS_DIR;
chdir gifs_ugui;

@fields = ( '1hrpre', '850htt', '850wnd', '500htv', '500wnd', '300htt', '300wnd', '200wnd', '100wnd', 'vissat', 'lsigmrA10mwnd', '850rhw', '700rhw', '500rhw', 'pblhgt', 'lsigheA10mwnd', 'cape', 'cin', 'clghgt', 'lsigrhA10mwnd', '10mwst', 'ms500h', '2mtempA10mwnd', '2mmxraA10mwnd', 'snowcv', '700tem', 'radar', 'xs1loc', 'xs1thw', 'xs1rhw', 'xs1wwwAxs1nrv', '600htt', 'sndg01', 'sndg02', 'sndg03', 'sndg04');

$fnum = 0;

   foreach $fld (@fields) {

     $fnum++;
     system("rm filename");
     if ( -e "../riprun/$cgm_file" ) {
     system("ln -s ../riprun/$cgm_file filename");
     system("cat $RIP_ROOT/mjfile | sed s/filenum/$fnum/  > jfile");

     system("/opt/ncarg/bin/med -f jfile");
     system("$RIP_ROOT/ncgm2gif gpart");
     $newgif = $valid_time . "/d" . $domain . "_" . $fld . ".gif";
     system("mv gpart.gif $newgif");
     }else{
     $newgif = $valid_time . "/d" . $domain . "_" . $fld . ".gif";
     system("cp $RIP_ROOT/not_yet_avail.gif $newgif");
     }

   }

}
1;
