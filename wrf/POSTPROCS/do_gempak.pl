#
# Convert this hour to gempak format, and scp to metbox machine
#

# @args - 0 - hourly file
# @args - 1 - Destination string for scp command
#
sub do_gempak
{

    my ($hourly_file,$dest,$valid_time,$domain) = @_;

    $gem_file = sprintf("%s_mm5_d%1d.gem", $valid_time,$domain);

    chdir $GEM_DIR;
    print "file is ${hourly_file}\n";
##  Run the gempak converter on this hour of data
    system( "/home/fddasys/bin/mm5togem -p -q -f ${hourly_file} -o $gem_file");
    print " scp ${gem_file} $dest\n";
    system( "scp ${gem_file} ${dest}");
    system( "scrub 4 $GEM_DIR");
}
1;
