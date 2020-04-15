#!/usr/bin/perl 
select STDOUT;
$|=1;
# This is a script to read binary VAD NIDS files and retrieve the wind 
# profiles 
# Usage: read_NIDSvad.pl dirname filename last/all 
#       last means just get last VAD profile
#       in the file. all means get all profiles in the file

#  Written by Greg Stossmeister
#             UCAR/JOSS
#              5/3/1997

# $ibytes is the current byte location in the NIDS file
# $j is the number of bytes to be read
# $k is the type of value to be unpacked
# $check - flag used to to alert script that next read is 1 byte long
# ascii character ($check=1) otherwise $check=0
# $units can equal "m" for metric or "e" for english
#
# ----------------------------------------------------------------------------
# From Peter Neilley email, dated 6-APR-1999...
# Andrew,  attached is Greg Stossmeister's perl VAD decoder.  I tried
# it with several of today's FTG VAD files and it seemed to work
# just fine.  Usage is:
#
#   read_NIDSvad.pl directory file [last]all]
#   
#where directory is the directory where the VAD file is, file is the
#name of the VAD file, and specifify either last or all indicating if
#you want just the last profile from the file or all.  I used only
#last since we store just one profile per file anyway.  Sample
#output from FTG's vad at 0355 UTC today:
#
#/home/neilley>read_NIDSvad.pl /nids/raw/nids/FTG/VAD/19990219 19990219.0355.VAD 
#last
# NIDS VAD data for:
#/nids/raw/nids/FTG/VAD/19990219/19990219.0355.VAD
# Julian Date : 2451229
# Calendar Date: 2 19 1999
# Time: 4:7:37
# Radar Latitude: 39.786
# Radar Longitude: -104.545
# Radar Altitude: 5610
# Radar Operating in Clear-Air Mode
# Radar Scan Coverage Pattern: 32
# Max Wind Speed (kts): 46
# Max Wind Direction (deg): 286
# Max Wind Altitude (ft): 22000
#
# Alt (m) DIR/SPD(m/s)  Time (gmt)
#
#   2743    189  17       358
#   3658    286  10       358
#   3963    291  14       358
#   4268    298  15       358
#   4573    300  16       358
#   4878    296  19       358
#   5182    296  19       358
#   5487    298  20       358
#   5792    297  22       358
#   6097    294  23       358
#   6707    286  23       358
#
#Peter Neilley, NCAR/RAP
#neilley@ucar.edu
#303-497-8446


# ----------------------------------------------------------------------------

$ibytes = -30;
$j = 2;
$k = "s";
$units="m"; 

$check = 0;
$ct = -1;
$last = " ";
$tlast = 0;
$read1 = 0;
$sttime = 9999;

   if($#ARGV != 2) {
      print "Invalid number of arguments \n";
      die "Usage: read_NIDSvad.pl dirname filename last/all \n";
   }
   $ddir = $ARGV[0];
   $name = $ARGV[1];
   $opt = substr($ARGV[2],0,1);

   $radar = substr($ddir, -16, 3); 
#  $date = substr($name,0,8);
#  $hhmm = substr($name,9,4);
#  print "$radar$date$hhmm\n";
# if in NCDC/NVW naming convention
   $datehhmm = substr($name,-12,12);
   print "$radar$datehhmm\n";

#chdir(MDIR,$ddir) || die "no $ddir?";
chdir $ddir || die "no $ddir?";
    open (IN, $name) || die "no $name?";

    while(sysread(IN, $buf, $j)) {
# Mei Xu 3-18-2003
            if ($k =~ "s") {
                $k="n";
            }
            elsif ($k =~ "l") {
                $k="N";
            }
# data is read and unpacked here
        $test = unpack($k,$buf);

# Begin series of tests to see where we are in the file and what type of
# data we are dealing with, if $ibytes < 120 then we are in the message 
# header. Print pertinent parameters.

#        if($ibytes <= 116) {
#            print $ibytes,"  ";
#            print $test, "\n";
#        }
        if($ibytes == 20) {
            $lat = $test/1000;
        } elsif($ibytes == 24) {
# Mei Xu  3-18-2003
# to get correct longitude values, compensate for 4294967296 = 65536x65536
            $test = $test -4294967296;
            $long = $test/1000;
        } elsif($ibytes == 28 ) {
            print "$lat $long $test\n";
        } elsif($ibytes == 96 ) {
            if ($units =~ /e/) {
                print "# Alt (k ft) ","DIR/SPD(kts)  ","Time (gmt)\n";
            } else {
                print "# Alt (m) ","DIR/SPD(m/s)  ","Time (gmt)\n";
            }
        } 

# Find the starting location of Symbology Block

        if($ibytes== 108) {
            $start = (2 * $test) + 16;
#            print "Start: ",$start,"\n";
#
# Find the starting location of Graphics Block (VAD does not have one)

        } elsif($ibytes == 112) {
            $grstart = 2 * $test;

# Find the starting location of the Tabular Block

        } elsif($ibytes == 116) {
            $alstart = (2 * $test) + 2;
            $stop = (2 * $test) + 2;
#            print "Stop: ",$stop,"\n";

# Check to see if we are within the Symbology Block

        } elsif($ibytes > 116 && $ibytes < $alstart) {
            if($ibytes >= $start && $ibytes <= $stop) {

# If we are at the beginning of a packet, Define the packet Code

                if($ibytes == $start) {
                    $packet =$test;
#                    print "***","Packet type:",$packet,"***","Bytes:",$ibytes,"\n";

# Define the length of the packet using the 2nd halfword

                } elsif($ibytes == $start + 2) {
                    $stop = $ibytes + $test;
#                    print "Stop: ",$stop,"\n";

#                    print $ibytes,"  ",$test,"\n";

# Packet Type 10 contains instructions for drawing the background grid

                } elsif($packet == 10 || $packet == 9) {
#                        print $ibytes,"  ";
                        if($ibytes == $start + 4) {
                            $color = $test;
#                            print "Color Level:";
                        }
#                        print $test,"\n";
 
# Packet type 4 contains actual wind data in bytes 12 - 15 
# need to translate time and height info from $x(time) and $y(Height)

                } elsif($packet == 4) {
#                        print $ibytes,"  ";
                        if($ibytes == $start + 6) {
                            $x = $test - 14;
                        } elsif($ibytes == $start + 8) {
                            $y = $test - 4;
                        } elsif($ibytes == $start + 10) {
                            $wdir = $test;
                        } elsif($ibytes == $start + 12) {
                            $wspd = $test;
                            $wind = join("/",$wdir,$wspd);
                            if($read1 == 0 && $opt =~ /l/i ) {
                               $sttime = $xlabel[$x];
                               $read1 = 1;
                            }
                            if($opt =~ /l/i && $xlabel[$x] != $sttime) {
#                               do not print anything
                                $work = "done";
                            } else { 
                                if($tlast != $xlabel[$x]) {
#                                   print "\n";
                                }
                                $tlast = $xlabel[$x];
                                if($units =~ /m/ ) {
                                    $ylabel[$y] = $ylabel[$y]*1000/3.28;
                                    $wspd = $wspd * .515;
                                }
                                printf "   %4d    %3d %5.2f      %4d\n",$ylabel[$y],$wdir,$wspd,$xlabel[$x];
                            }
                           
                        }
 
# Packet Type 8 contains graphic labeling instructions (Titles, axes labels)
#
                } elsif($packet == 8) {
#                        print $ibytes,"  ";
                        if($ibytes == $start + 6) {
                            $x = $test;
#                            print $test,"\n";
                        } elsif($ibytes == $start + 8) {
                            $y = $test;
#                            print $test,"\n";
                            $j = 1;
                            $ibytes = $ibytes - 1;
                            $k = "a";
                            $check = 1;
                        } elsif ($ibytes > $start + 8 && $ibytes < $stop) {
                            $ct++;
                            $word[$ct] = $test;
#                            print "test = ",$test,"\n";
                            $j = 1;
                            $ibytes = $ibytes - 1;
                            $k = "a";
                            $check = 1;
                        } elsif($ibytes == $stop) {
                            $ct++;
                            $word[$ct] = $test;
                            $label = join("",@word);
                            $ct = -1;
                            @word = "";
                            if($x < 50 && $x > 20 && $y > 15) {
                                $ylabel[$y] = $label;
#                                print $ylabel[$y]," ",$y,"\n";
                            } elsif($y > 460 && $x > 50) {
                                $xlabel[$x] = $label;
#                                print $xlabel[$x]," ",$x,"\n";
                            }
#                            print $test,"\n";
                        }
                } else {
#                        print $ibytes,"  ",$test,"\n";
                }

# If we are at the end of a packet, re-define $start and $stop to read next
# packet. If we are at the end of the product symbology block then arbitrarily
# define stop to be 50 bytes into Tabular Alpha block since length of that 
# block not known until we begin to read it. 

                if($ibytes == $stop) {
                    $start = $ibytes + 2;
#                    print "Start: ",$start,"\n";

                    $stop = $alstart;
#                    print "Stop: ",$stop,"\n";

                    if($start + 2 == $alstart) {
                        $stop = $alstart + 50;
#                        print "Stop: ",$stop,"\n";

                    }
                }


            }

# Print Tabular alphanumeric block, interpreting a 2 followed by blanks as a
# carriage return

        } elsif($ibytes > $alstart + 128) {
            $j = 1;
            $ibytes = $ibytes - 1;
            $k = "a";
            $check = 1;
            if($last =~ /2/ && $test =~ /\s/ ) {
#                print "\n";
            } else {
#                print $last;
            }
                $last = $test;
        }
       
        $ibytes = $ibytes + 2;
        if($check != 1 && $j == 4) {
            $ibytes = $ibytes + 2;
        }

# if next read is 1 byte ascii character then reset $check and go ahead and
# read the next byte. Otherwise depending on where we are in the file tell the
# script to read 4-byte signed longs, or 2-byte signed shorts.

        if($check != 1) {
            if ($ibytes == 4 || $ibytes == 8 || $ibytes == 20 || $ibytes == 24 || $ibytes == 42 || $ibytes == 48 || $ibytes == 108 || $ibytes == 112 || $ibytes == 116 || $ibytes == 124 || $ibytes == 132) {
                $j = 4;
                $k = "l";
            } else {
                $j = 2;
                $k = "s";
            }
         } else {
            $check = 0;
         }

    }
