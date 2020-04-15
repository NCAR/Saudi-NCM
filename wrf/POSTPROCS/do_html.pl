
sub do_html
{
my ($user, $job_id, $ndom, $dest) = @_;

chdir $WORK_DIR;

system("rm index.1");
system("cat $POSTPROCS_DIR/template_index.html | sed s/USERNAME/$user/g > index.1");
if ( $IS_VDRAS ) {
system("rm index.1");
system("cat $POSTPROCS_DIR/template_index_vdras.html | sed s/USERNAME/$user/g > index.1");
}
if ( $IS_GEAPS ) {
system("rm index.2");
system("cat index.1 | sed s/ugui/ugui_ens/g > index.2");
system("cat index.2 | sed s/<!--Z//g > index.3");
system("cat index.3 | sed s/Z-->//g > index.4");
system("cat index.4 | sed s/CASEID/$job_id/g > index.html");
} else {
system("cat index.1 | sed s/CASEID/$job_id/g > index.html");
}

system("rm medoc.1");
system("rm medoc.2");
system("cat $POSTPROCS_DIR/template_medoc.html | sed s/USERNAME/$user/g > medoc.1");
system("cat medoc.1 | sed s/DOMAINVAL/3/g > medoc.2");
system("cat medoc.2 | sed s/CASEID/$job_id/g > medoc_domain3.html");

system("rm medoc.1");
system("rm medoc.2");
system("cat $POSTPROCS_DIR/template_medoc.html | sed s/USERNAME/$user/g > medoc.1");
system("cat medoc.1 | sed s/DOMAINVAL/4/g > medoc.2");
system("cat medoc.2 | sed s/CASEID/$job_id/g > medoc_domain4.html");

if ( $IS_GEAPS ) {
  if ( $ndom == 1 ) {
  system("cat $POSTPROCS_DIR/js_ens.1 > js_file");
  } elsif ( $ndom == 2 ) {
  system("cat $POSTPROCS_DIR/js_ens.1 $POSTPROCS_DIR/js_ens.2 > js_file");
  } elsif ( $ndom == 3 ) {
  system("cat $POSTPROCS_DIR/js_ens.1 $POSTPROCS_DIR/js_ens.2 $POSTPROCS_DIR/js_ens.3 > js_file");
  } elsif ( $ndom == 4 ) {
  system("cat $POSTPROCS_DIR/js_ens.1 $POSTPROCS_DIR/js_ens.2 $POSTPROCS_DIR/js_ens.3 $POSTPROCS_DIR/js_ens.4 > js_file");
  }
} else { 
  if ( $ndom == 1 ) {
  system("cat $POSTPROCS_DIR/js.1 > js_file");
  } elsif ( $ndom == 2 ) {
  system("cat $POSTPROCS_DIR/js.1 $POSTPROCS_DIR/js.2 > js_file");
  } elsif ( $ndom == 3 ) {
  system("cat $POSTPROCS_DIR/js.1 $POSTPROCS_DIR/js.2 $POSTPROCS_DIR/js.3 > js_file");
  } elsif ( $ndom == 4 ) {
  system("cat $POSTPROCS_DIR/js.1 $POSTPROCS_DIR/js.2 $POSTPROCS_DIR/js.3 $POSTPROCS_DIR/js.4 > js_file");
  } elsif ( $ndom == 5 ) {
  system("cat $POSTPROCS_DIR/js.1 $POSTPROCS_DIR/js.2 $POSTPROCS_DIR/js.3 $POSTPROCS_DIR/js.4 $POSTPROCS_DIR/js.5 > js_file");
  }
}
#system("scp -p index.html $dest" );
#system("scp -p medoc*.html $dest" );
#system("scp -p js_file $dest" );
system("rsync -e 'ssh -i /home/fddasys/.ssh/rtfdda' -avzC *.html $dest");

if( -e "/data/GMODJOBS/$job_id/domain_file") {
#  system("scp -p /data/GMODJOBS/$job_id/domain_file $dest" );
  system("rsync -e 'ssh -i /home/fddasys/.ssh/rtfdda' -avzC /data/GMODJOBS/$job_id/domain_file $dest");
 } else {
#  system("scp -p $POSTPROCS_DIR/domain_file $dest" );
  system("rsync -e 'ssh -i /home/fddasys/.ssh/rtfdda' -avzC $POSTPROCS_DIR/domain_file $dest");
 }

}
1;
