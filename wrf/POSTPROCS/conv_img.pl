

sub conv_img
{
my ($cgm_file,$domain, $valid_time, $from_img, $to_img, $field_file) = @_;
print "\nconv_img ($cgm_file,$domain, $valid_time, $from_img, $to_img, $field_file)\n\n";

# $from_img: 'cgm' or 'ps'
# $to_img: 'png' or 'gif'

# set environment variables

$ENV{'NCARG_ROOT'} = $NCARG_ROOT;
$ENV{'NCARG_LIB'} = $NCARG_LIB;
if ( $IS_RIP4 || $IS_WRF ) {
 $RIP_ROOT = "$MM5HOME/cycle_code/CONSTANT_FILES/RIP4";
 $ENV{'RIP_ROOT'} = "$RIP_ROOT";
} else {
 $RIP_ROOT = "$MM5HOME/cycle_code/CONSTANT_FILES/RIP";
}
$ENV{'RIP_ROOT'} = "$RIP_ROOT";

system("$MustHaveDir $PLOTS_DIR/gifs_ugui");
system("$MustHaveDir $PLOTS_DIR/gifs_ugui/$valid_time");
chdir $PLOTS_DIR;
chdir "gifs_ugui";

if ( -e "$field_file" ) {
  require "$field_file";
} elsif ( -e "$GSJOBDIR/postprocs/$field_file" ) {
  require "$GSJOBDIR/postprocs/$field_file";
} elsif ( -e "$GSJOBDIR/postprocs/image_fields.pl" ) {
  require "$GSJOBDIR/postprocs/image_fields.pl";
} else {
# Define field names for a generic job configuration
   @fields = ( '1hrpre', '850htt', '500htv', '500wnd', '300wnd', 'vissat', '1000rhw', '975rhw', '950rhw', '925rhw', '900rhw', '850rhw', '700rhw', '500rhw', 'pblhgt', 'lsigheA10mwnd', 'cape', 'cin', 'lsigrhA10mwnd', 'ms500h', '2mtempA10mwnd', '2mmxraA10mwnd', 'snowcv', '700tem', 'radar', '600mWV', 'sfcwind', 'clwint', 'pv320', 'pv330', 'te296', 'te308', 'te315', 'pv15hgt', 'cldhgt', 'xs1loc', 'xs1thw', 'xs1rhw', 'xs1tadvec', 'xs1div', 'xs1rhe', 'xs1omega', 'xs1pvu', 'sndg01', 'sndg02', 'sndg03' );

   @img_fields[0] = @fields;
   @img_fields[1] = @fields;
   @img_fields[2] = @fields;
   @img_fields[3] = @fields;
}

if ( $COLD_START_LABEL ) {
   if ( -e "$GSJOBDIR/postprocs/coldstart.ps" ) {
      $cold_start_ps = "$GSJOBDIR/postprocs/coldstart.ps";
   } else {
      die "Cold start label image $cold_start_ps does not exist!\n";
   }
}

# IMAGE_DENSITY is used by the "convert" command to set dots-per-inch.  This
# has the effect of changing the geometric size of the resulting png or gif,
# along with increasing image resolution/clarity
# 120 is a decent default value giving image sizes of about 900x900 pixels.
if ( $IMAGE_DENSITY <= 0 ) {
  $IMAGE_DENSITY = 120;
}
$rasfmt = "ps.color";

# if this is a cgm file from NCARGraphics, convert it to postscript with ctrans

$psfile = "file.ps";
if ( $from_img ne "ps") {
   if ( -e "../riprun/$cgm_file" ) {
     system("${NCARG_ROOT}/bin/ctrans -d $rasfmt -lscale 1.5  ../riprun/$cgm_file >! $psfile")
   }
# only cgm or ps are options 
} else {
   if ( -e "../riprun/$cgm_file" ) {
      system("rm -f $psfile");
      system("ln -s ../riprun/$cgm_file $psfile");
   }
}

# now, extract the frames using psplit ("plot" is the output filename root)
   if ( -e "${NCARG_ROOT}/bin/psplit" ) {
      system("${NCARG_ROOT}/bin/psplit $psfile plot ");
      print "${NCARG_ROOT}/bin/psplit $psfile plot \n";
   } else {
      system("/usr/bin/psplit $psfile plot ");
      print "/usr/bin/psplit $psfile plot \n";
   }

# Now loop through the individual frames, and remane them to "nice" field-name images
# while converting to png or gif for web output

$rhel = `uname -r`;

$fnum = 0;

   foreach $fld (@{ $img_fields[$domain-1]})  {

     $fnum++;
     $imgfile = sprintf("plot%04d.ps", $fnum);
     if ( $to_img eq "png" ) {
       $newimg = $valid_time . "/d" . $domain . "_" . $fld . ".png";
     } else {
       $newimg = $valid_time . "/d" . $domain . "_" . $fld . ".gif";
     }
     if ( -e "$imgfile" ) {
#
#     "convert" is an ImageMagick tool which is assumed to be in the user's $PATH
#     it is usually in /usr/bin
      if ( $ENV{COLD_START} && $CS_LABEL_OFFSET[$d] ) {
        my $d = $domain-1;
        if ($rhel =~ /el7/) {
           system("convert -background white -alpha remove -trim +repage -density $IMAGE_DENSITY -composite $imgfile $cold_start_ps -geometry $CS_LABEL_OFFSET[$d] $newimg") ;
        } else {
           system("/lustre/project/k1206/x_fisherh/ImageMagick/bin/convert -trim +repage -density $IMAGE_DENSITY -composite $imgfile $cold_start_ps -geometry $CS_LABEL_OFFSET[$d] $newimg") ;
           print "not redhat + cold convert -trim +repage -density $IMAGE_DENSITY -composite $imgfile $cold_start_ps -geometry $CS_LABEL_OFFSET[$d] $newimg";
        }
      } else {
        if ($rhel =~ /el7/) {
           system("convert -background white -alpha remove -trim +repage -density $IMAGE_DENSITY $imgfile $newimg") ;
        } else {
           system("/lustre/project/k1206/x_fisherh/ImageMagick/bin/convert -trim +repage -density $IMAGE_DENSITY $imgfile $newimg") ;
           print "not redhat  convert -trim +repage -density $IMAGE_DENSITY $imgfile $newimg\n" ;
        }
      }
     }else{
      system("cp $RIP_ROOT/not_yet_avail.gif $newimg");
      print "cp $RIP_ROOT/not_yet_avail.gif $newimg";
     }

   }

   if ( $from_img ne "ps") {
      system("mkdir -p ps_d$domain");
      system("mv plot*.ps ps_d$domain/.");
   }
}
1;
