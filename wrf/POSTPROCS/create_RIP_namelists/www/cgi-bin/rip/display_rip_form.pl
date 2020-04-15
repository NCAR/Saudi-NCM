#!/usr/bin/perl -wT

require "./ReadWriteDB.pm";
use GDBM_File;
use CGI;
#use CGI qw(:standard);
#autoEscape(undef);

use strict;

my $query = new CGI;

print $query->header;

print $query->start_html(-title=>'WRF Image Configuration Form - Edit Range',
                         -BGCOLOR=>"#DDDDDD");

print <<DONE;
<h1>WRF Image Configuration Form</h1>
<table border=1>
<tr>
<td><a href="#surface">Surface Plots</a></td>
<td><a href="#upper">Upper Air Plots</a></td>
<td><a href="#custom">Custom Plots</a></td>
<td><a href="#others">Other Plots</a></td>
<td><a href="#xs">Cross Sections</a></td>
<td><a href="#sounding">Soundings</a></td>
<td><a href="#overlays">Overlays</a></td>
<td><a href="#terrain">Terrain Heights</a></td>
<td><a href="#submit">Form Submission</a></td>
</tr>
</table>
<h3>Universal Configuration Information</h3>
DONE

print $query->start_form(-method =>'POST', -action =>'generate_for_ncl_auto.pl');

my $file = "/model/atecuser/web_extra/form_data/records.dbx";
my (%DATA,$key);
tie(%DATA, "GDBM_File", $file, GDBM_READER, 0666) || print $!;
my($Range) = $query->param('Range');
#my($Range) = "RTC";
my(%DEFAULT) = ReadWriteDB::GetData($DATA{$Range});
untie(%DATA);
my(%labels);
my $radios;

print <<DONE;
Configuration Options for $Range.<br>
Save under new name (optional):
DONE

print $query->textfield(-name=>'new_range',
                        -size=>10);
print <<DONE;
<p>
Number of Domains
DONE

   print $query->radio_group(-name=>'N_doms',
                             -values=>['3','4'],
                             -default=>$DEFAULT{'N_doms'},
                    );

print <<DONE;
<br>
One full wind barb equals
DONE
  %labels = (
    "5mps", "5 m/s",
    "10kts", "10 knots",
    );

   print $query->radio_group(-name=>'wind_barb',
                             -values=>['5mps','10kts'],
                             -default=>$DEFAULT{'wind_barb'},
                             -labels=>\%labels,
                    );

print <<DONE;
<br>
Hide fields below terrain level?
DONE
  %labels = (
    "hide", "hide",
    "visible", "visible",
    );

   print $query->radio_group(-name=>'underground',
                             -values=>['hide','visible'],
                             -default=>$DEFAULT{'underground'},
                             -labels=>\%labels,
                    );

print <<DONE;
<br>
Plot color bar on side or default?
DONE
  %labels = (
    "default", "Default: on whichever side maximizes the plot",
    "side", "Always on right hand side",
    );

   print $query->radio_group(-name=>'color_bar',
                             -values=>['default','side'],
                             -default=>$DEFAULT{'color_bar'},
                             -labels=>\%labels,
                    );

print <<DONE;
<br>
Frequency to plot domain 4 wind barbs:
DONE
  %labels = (
    "4,4,4,3", "Default: every 3rd",
    "4,4,4,2", "Every 2nd (creates more smaller barbs)",
    );

   print $query->radio_group(-name=>'wind_brb_freq',
                             -values=>['4,4,4,3','4,4,4,2'],
                             -default=>$DEFAULT{'wind_brb_freq'},
                             -labels=>\%labels,
                    );

print <<DONE;
<br>
Frequency to plot grid major tick marks:
DONE
  %labels = (
    "10", "Default: every 10",
    "20", "Every 20",
    );

   print $query->radio_group(-name=>'maj_tic_freq',
                             -values=>['10','20'],
                             -default=>$DEFAULT{'maj_tic_freq'},
                             -labels=>\%labels,
                    );

print <<DONE;
<br>
Frequency to plot grid minor tick marks:
DONE
  %labels = (
    "1", "Default: every 1",
    "2", "Every 2",
    "5", "Every 5",
    );

   print $query->radio_group(-name=>'mnr_tic_freq',
                             -values=>['1','2','5'],
                             -default=>$DEFAULT{'mnr_tic_freq'},
                             -labels=>\%labels,
                    );

print <<DONE;
<br>
Plot duplicates of each sounding with no Parcel Info box? (mainly useful for very cold locations)
DONE
  %labels = (
    "no", "Default: Do not plot a duplicate sounding",
    "yes", "Plot a duplicate sounding w/o a Parcel Info box",
    );

   print $query->radio_group(-name=>'dup_sounding',
                             -values=>['no','yes'],
                             -default=>$DEFAULT{'dup_sounding'},
                             -labels=>\%labels,
                    );

print <<DONE;
<p>
<a name="surface"><h3>Surface Plots</h3>
DONE

# T2mWnd plot definition
   print $query->checkbox(-name=>'T2mWnd_name',
                           -value=>'T2mWndC',
                           -checked=>$DEFAULT{'T2mWnd_name'},
                             -label=>'',
                    );
print <<DONE;
MSLP (contoured), 2-m Temperature (C, shaded) and 10-m Wind Barbs<br>
&nbsp;&nbsp;&nbsp;MSLP contour intervals for each domain
DONE
  %labels = (
    "4,3,2,2", "4, 3, 2, 2 hPa",
    "2,2,2,2", "2, 2, 2, 2 hPa",
    "none,none,none,none", "no MSLP field",
    );
   print $query->radio_group(-name=>'T2mWnd_ctr_int',
                             -values=>['4,3,2,2','2,2,2,2','none,none,none,none'],
                             -default=>$DEFAULT{'T2mWnd_ctr_int'},
                             -labels=>\%labels,
                    );
print <<DONE;
<br>
&nbsp;&nbsp;&nbsp;2-m Temperature contour intervals for each domain
DONE
  %labels = (
    "4,3,2,2", "4, 3, 2, 2 C",
    );
   print $query->radio_group(-name=>'T2mWnd_sha_int',
                             -values=>['4,3,2,2'],
                             -default=>$DEFAULT{'T2mWnd_sha_int'},
                             -labels=>\%labels,
                    );
print <<DONE;
<br>
&nbsp;&nbsp;&nbsp;2-m Temperature summer shading value range
DONE
  %labels = (
    "48;-12", "-12 to 48 C ",
    "48;0","0 to 48 C",
    );
   print $query->radio_group(-name=>'T2mWnd_rng_smr',
                             -values=>['48;-12','48;0'],
                             -default=>$DEFAULT{'T2mWnd_rng_smr'},
                             -labels=>\%labels,
                    );
print <<DONE;
<br>
&nbsp;&nbsp;&nbsp;2-m Temperature winter maximum shading value
DONE
  %labels = (
    "12;-60", "-60 to 12 C",
    "24;-48", "-48 to 24 C",
    "36;-36", "-36 to 36 C",
    "48;-12", "-12 to 48 C",
    );
   print $query->radio_group(-name=>'T2mWnd_rng_wtr',
                             -values=>['12;-60','24;-48','36;-36','48;-12'],
                             -default=>$DEFAULT{'T2mWnd_rng_wtr'},
                             -labels=>\%labels,
                    );
print <<DONE;
<br>
&nbsp;&nbsp;&nbsp;2-m Temperature Color Table<br>
DONE
  %labels = (
    "T5StepCBCust", "&nbsp;<img src=/rip/T4StepCBCust.jpg>&nbsp;&nbsp;&nbsp;4-Hue Colorblind Friendly (\"T4StepCBCust\")",
    "Temperature", "&nbsp;<img src=/rip/Temperature.jpg>&nbsp;&nbsp;&nbsp;Classic Color Scale (\"Temperature\")",
    );
   $radios = $query->radio_group(-name=>'T2mWnd_color',
                             -values=>['T5StepCBCust','Temperature'],
                             -default=>$DEFAULT{'T2mWnd_color'},
                             -labels=>\%labels,
                             -linebreak=>'true',
                    );
   $radios =~ s/\&lt;/\</g;
   $radios =~ s/\&gt;/\>/g;
   $radios =~ s/\&amp;/\&/g;
   print $radios;

print <<DONE;
<p>
DONE

# TF2mWnd plot definition
   print $query->checkbox(-name=>'TF2mWnd_name',
                           -value=>'T2mWndF',
                           -checked=>$DEFAULT{'TF2mWnd_name'},
                             -label=>'',
                    );
print <<DONE;
MSLP (contoured), 2-m Temperature (F, shaded) and 10-m Wind Barbs<br>
&nbsp;&nbsp;&nbsp;MSLP contour intervals for each domain
DONE
  %labels = (
    "4,3,2,2", "4, 3, 2, 2 hPa",
    "2,2,2,2", "2, 2, 2, 2 hPa",
    "none,none,none,none", "no MSLP field",
    );
   print $query->radio_group(-name=>'TF2mWnd_ctr_int',
                             -values=>['4,3,2,2','2,2,2,2','none,none,none,none'],
                             -default=>$DEFAULT{'TF2mWnd_ctr_int'},
                             -labels=>\%labels,
                    );
print <<DONE;
<br>
&nbsp;&nbsp;&nbsp;2-m F Temperature contour intervals for each domain
DONE
  %labels = (
    "4,4,4,4", "4, 4, 4, 4 F",
    );
   print $query->radio_group(-name=>'TF2mWnd_sha_int',
                             -values=>['4,4,4,4'],
                             -default=>$DEFAULT{'TF2mWnd_sha_int'},
                             -labels=>\%labels,
                    );
print <<DONE;
<br>
&nbsp;&nbsp;&nbsp;2-m Temperature summer shading value range
DONE
  %labels = (
    "108;-12", "-12 to 108 F",
    "128;8", "8 to 128 F",
    );
   print $query->radio_group(-name=>'TF2mWnd_rng_smr',
                             -values=>['108;-12','128;8'],
                             -default=>$DEFAULT{'TF2mWnd_rng_smr'},
                             -labels=>\%labels,
                    );
print <<DONE;
<br>
&nbsp;&nbsp;&nbsp;2-m Temperature winter maximum shading value
DONE
  %labels = (
    "68;-52", "-52 to 68 F",
    "88;-32", "-32 to 88 F",
    );
   print $query->radio_group(-name=>'TF2mWnd_rng_wtr',
                             -values=>['68;-52','88;-32'],
                             -default=>$DEFAULT{'TF2mWnd_rng_wtr'},
                             -labels=>\%labels,
                    );
print <<DONE;
<br>
&nbsp;&nbsp;&nbsp;2-m Temperature Color Table<br>
DONE
  %labels = (
    "TF5StepCBCust", "&nbsp;<img src=/rip/T4StepCBCust.jpg>&nbsp;&nbsp;&nbsp;5-Hue Colorblind Friendly (\"TF5StepCBCust\")",
    "Temperature", "&nbsp;<img src=/rip/Temperature.jpg>&nbsp;&nbsp;&nbsp;Classic Color Scale (\"Temperature\")",
    );
   $radios = $query->radio_group(-name=>'TF2mWnd_color',
                             -values=>['TF5StepCBCust','Temperature'],
                             -default=>$DEFAULT{'TF2mWnd_color'},
                             -labels=>\%labels,
                             -linebreak=>'true',
                    );
   $radios =~ s/\&lt;/\</g;
   $radios =~ s/\&gt;/\>/g;
   $radios =~ s/\&amp;/\&/g;
   print $radios;

print <<DONE;
<p>
DONE


# RHsfcWnd plot definition
   print $query->checkbox(-name=>'RHsfcWnd_name',
                           -value=>'RHsfcWnd',
                           -checked=>$DEFAULT{'RHsfcWnd_name'},
                           -label=>'',
                    );
print <<DONE;
Surface Humidity (shaded) and Winds Barbs<br>
&nbsp;&nbsp;&nbsp;RH contour interval: 10%<br>
DONE
  %labels = (
    "HumidityCB", "&nbsp;<img src=/rip/HumidityCB.jpg>&nbsp;&nbsp;&nbsp;2-Hue Colorblind Friendly (\"HumidityCB\")",
    "Humidity", "&nbsp;<img src=/rip/Humidity.jpg>&nbsp;&nbsp;&nbsp;Classic Color Scale (\"Humidity\")",
    );
   $radios = $query->radio_group(-name=>'RHsfcWnd_color',
                             -values=>['HumidityCB','Humidity'],
                             -default=>$DEFAULT{'RHsfcWnd_color'},
                             -labels=>\%labels,
                             -linebreak=>'true',
                    );
   $radios =~ s/\&lt;/\</g;
   $radios =~ s/\&gt;/\>/g;
   $radios =~ s/\&amp;/\&/g;
   print $radios;

print <<DONE;
<p>
DONE

# Td2mWnd plot definition
   print $query->checkbox(-name=>'Td2mWnd_name',
                           -value=>'Td2mWndC',
                           -checked=>$DEFAULT{'Td2mWnd_name'},
                           -label=>'',
                    );
print <<DONE;
2-m Dewpoint (C, shaded) and Winds Barbs<br>
&nbsp;&nbsp;&nbsp;2-m Dewpoint contour intervals for each domain
DONE
  %labels = (
    "4,4,2,2", "4, 4, 2, 2 C",
    );
   print $query->radio_group(-name=>'Td2mWnd_sha_int',
                             -values=>['4,4,2,2'],
                             -default=>$DEFAULT{'Td2mWnd_sha_int'},
                             -labels=>\%labels,
                    );

print <<DONE;
<br>
&nbsp;&nbsp;&nbsp;2-m Td summer shading value range
DONE
  %labels = (
    "32;-24", "-24 to 32 C",
    );
   print $query->radio_group(-name=>'Td2mWnd_rng_smr',
			     -values=>['32;-24'],
			     -default=>$DEFAULT{'Td2mWnd_rng_smr'},
			     -labels=>\%labels,
		    );
print <<DONE;
<br>
&nbsp;&nbsp;&nbsp;2-m Td winter shading value range
DONE
  %labels = (
    "32;-24", "-24 to 32 C",
    "24;-40", "-40 to 24 C",
    );
   print $query->radio_group(-name=>'Td2mWnd_rng_wtr',
			     -values=>['32;-24','24;-40'],
			     -default=>$DEFAULT{'Td2mWnd_rng_wtr'},
			     -labels=>\%labels,
		    );
print <<DONE;
<br>
DONE
  %labels = (
    "HumidityCB", "&nbsp;<img src=/rip/HumidityCB.jpg>&nbsp;&nbsp;&nbsp;2-Hue Colorblind Friendly (\"HumidityCB\")",
    "Humidity", "&nbsp;<img src=/rip/Humidity.jpg>&nbsp;&nbsp;&nbsp;Classic Color Scale (\"Humidity\")",
    );
   $radios = $query->radio_group(-name=>'Td2mWnd_color',
			     -values=>['HumidityCB','Humidity'],
			     -default=>$DEFAULT{'Td2mWnd_color'},
			     -labels=>\%labels,
			     -linebreak=>'true',
		    );
   $radios =~ s/\&lt;/\</g;
   $radios =~ s/\&gt;/\>/g;
   $radios =~ s/\&amp;/\&/g;
   print $radios;

print <<DONE;
<p>
DONE

# TdF2mWnd plot definition
   print $query->checkbox(-name=>'TdF2mWnd_name',
                           -value=>'Td2mWndF',
                           -checked=>$DEFAULT{'TdF2mWnd_name'},
                           -label=>'',
                    );
print <<DONE;
2-m Dewpoint (F, shaded) and Winds Barbs<br>
&nbsp;&nbsp;&nbsp;2-m Dewpoint contour intervals for each domain
DONE
  %labels = (
    "4,3,2,2", "4, 3, 2, 2 F",
    );
   print $query->radio_group(-name=>'TdF2mWnd_sha_int',
                             -values=>['4,3,2,2'],
                             -default=>$DEFAULT{'TdF2mWnd_sha_int'},
                             -labels=>\%labels,
                    );

print <<DONE;
<br>
&nbsp;&nbsp;&nbsp;2-m Td summer shading value range
DONE
  %labels = (
    "72;-24", "-24 to 72 F",
    "84;-12", "-12 to 84 F",
    );
   print $query->radio_group(-name=>'TdF2mWnd_rng_smr',
                             -values=>['72;-24','84;-12'],
                             -default=>$DEFAULT{'TdF2mWnd_rng_smr'},
                             -labels=>\%labels,
                    );
print <<DONE;
<br>
&nbsp;&nbsp;&nbsp;2-m Td winter shading value range
DONE
  %labels = (
    "48;-48", "-48 to 48 F",
    "72;-24", "-24 to 72 F",
    );
   print $query->radio_group(-name=>'TdF2mWnd_rng_wtr',
                             -values=>['48;-48','72;-24'],
                             -default=>$DEFAULT{'TdF2mWnd_rng_wtr'},
                             -labels=>\%labels,
                    );
print <<DONE;
<br>
DONE
  %labels = (
    "HumidityCB", "&nbsp;<img src=/rip/HumidityCB.jpg>&nbsp;&nbsp;&nbsp;2-Hue Colorblind Friendly (\"HumidityCB\")",
    "Humidity", "&nbsp;<img src=/rip/Humidity.jpg>&nbsp;&nbsp;&nbsp;Classic Color Scale (\"Humidity\")",
    );
   $radios = $query->radio_group(-name=>'TdF2mWnd_color',
                             -values=>['HumidityCB','Humidity'],
                             -default=>$DEFAULT{'TdF2mWnd_color'},
                             -labels=>\%labels,
                             -linebreak=>'true',
                    );
   $radios =~ s/\&lt;/\</g;
   $radios =~ s/\&gt;/\>/g;
   $radios =~ s/\&amp;/\&/g;
   print $radios;

print <<DONE;
<p>
DONE

# QvWnd plot definition
   print $query->checkbox(-name=>'QvWnd_name',
			   -value=>'QvWnd',
			   -checked=>$DEFAULT{'QvWnd_name'},
			   -label=>'',
		    );
print <<DONE;
Mixing Ratio on lowest model level (shaded) and Winds Barbs<br>
&nbsp;&nbsp;&nbsp;Qv contour interval: 2, 2, 1, 1 g/kg<br>
DONE
  %labels = (
    "HumidityCB", "&nbsp;<img src=/rip/HumidityCB.jpg>&nbsp;&nbsp;&nbsp;2-Hue Colorblind Friendly (\"HumidityCB\")",
    "Humidity", "&nbsp;<img src=/rip/Humidity.jpg>&nbsp;&nbsp;&nbsp;Classic Color Scale (\"Humidity\")",
    );
   $radios = $query->radio_group(-name=>'QvWnd_color',
			     -values=>['HumidityCB','Humidity'],
			     -default=>$DEFAULT{'QvWnd_color'},
                             -labels=>\%labels,
                             -linebreak=>'true',
		    );
   $radios =~ s/\&lt;/\</g;
   $radios =~ s/\&gt;/\>/g;
   $radios =~ s/\&amp;/\&/g;
   print $radios;

print <<DONE;
<p>
DONE

# ThEsfcWnd plot definition
   print $query->checkbox(-name=>'ThEsfcWnd_name',
                           -value=>'ThEsfcWnd',
                           -checked=>$DEFAULT{'ThEsfcWnd_name'},
                           -label=>'',
                    );
print <<DONE;
Theta-E (shaded) and Wind Barbs<br>
&nbsp;&nbsp;&nbsp;Theta-E contour interval: 5, 5, 2.5, 2.5 K<br>
&nbsp;&nbsp;&nbsp;Theta-E summer shading value range
DONE
  %labels = (
    "340;240", "240 to 340 K",
    "370;270", "270 to 370 K",
    );
   print $query->radio_group(-name=>'ThEsfcWnd_rng_smr',
			     -values=>['340;240','370;270'],
			     -default=>$DEFAULT{'ThEsfcWnd_rng_smr'},
			     -labels=>\%labels,
		    );
print <<DONE;
<br>
&nbsp;&nbsp;&nbsp;Theta-E winter shading value range
DONE
  %labels = (
    "340;240", "240 to 340 K",
    "370;270", "270 to 370 K",
    );
   print $query->radio_group(-name=>'ThEsfcWnd_rng_wtr',
			     -values=>['340;240','370;270'],
			     -default=>$DEFAULT{'ThEsfcWnd_rng_wtr'},
			     -labels=>\%labels,
		    );
print <<DONE;
<br>
DONE
  %labels = (
    "ThE4StepCBCust", "&nbsp;<img src=/rip/ThE4StepCBCust.jpg>&nbsp;&nbsp;&nbsp;4-Hue Colorblind Friendly (\"ThE4StepCBCust\")",
    "ThetaE", "&nbsp;<img src=/rip/ThetaE.jpg>&nbsp;&nbsp;&nbsp;Classic Color Scale (\"ThetaE\")",
    );
   $radios = $query->radio_group(-name=>'ThEsfcWnd_color',
			     -values=>['ThE4StepCBCust','ThetaE'],
			     -default=>$DEFAULT{'ThEsfcWnd_color'},
			     -labels=>\%labels,
			     -linebreak=>'true',
		    );
   $radios =~ s/\&lt;/\</g;
   $radios =~ s/\&gt;/\>/g;
   $radios =~ s/\&amp;/\&/g;
   print $radios;

print <<DONE;
<p>
DONE

# Strm10m plot definition
   print $query->checkbox(-name=>'Strm10m_name',
                           -value=>'Strm10m',
                           -checked=>$DEFAULT{'Strm10m_name'},
                           -label=>'',
                    );
print <<DONE;
Terrain (shaded) and 10-m Streamlines<br>
&nbsp;&nbsp;&nbsp;See Terrain Height section at bottom to define terrain contour mins and maxes 
DONE

print <<DONE;
<br>
DONE
  %labels = (
    "Grayscale", "&nbsp;<img src=/rip/Grayscale.jpg>&nbsp;&nbsp;&nbsp;Colorblind Friendly (\"Grayscale\")",
    "Terrain", "&nbsp;<img src=/rip/Terrain.jpg>&nbsp;&nbsp;&nbsp;Classic Color Scale (\"Terrain\")",
    );
   $radios = $query->radio_group(-name=>'Strm10m_color',
			     -values=>['Grayscale','Terrain'],
			     -default=>$DEFAULT{'Strm10m_color'},
			     -labels=>\%labels,
			     -linebreak=>'true',
		    );
   $radios =~ s/\&lt;/\</g;
   $radios =~ s/\&gt;/\>/g;
   $radios =~ s/\&amp;/\&/g;
   print $radios;

print <<DONE;
<p>
DONE

# sfcstrm plot definition
   print $query->checkbox(-name=>'sfcstrm_name',
                           -value=>'sfcstrm',
                           -checked=>$DEFAULT{'sfcstrm_name'},
                           -label=>'',
                    );
print <<DONE;
10-m Wind Speed and Streamlines<br>
&nbsp;&nbsp;&nbsp;Wind speed contour interval for each domain: 5 m/s (or 10 knots)<br>
DONE
  %labels = (
    "PurpleMonoCB", "&nbsp;<img src=/rip/PurpleMonoCB.jpg>&nbsp;&nbsp;&nbsp;1-Hue Colorblind Friendly (\"PurpleMonoCB\")",
    );
   $radios = $query->radio_group(-name=>'sfcstrm_color',
			     -values=>['PurpleMonoCB'],
			     -default=>$DEFAULT{'sfcstrm_color'},
			     -labels=>\%labels,
			     -linebreak=>'true',
		    );
   $radios =~ s/\&lt;/\</g;
   $radios =~ s/\&gt;/\>/g;
   $radios =~ s/\&amp;/\&/g;
   print $radios;

print <<DONE;
<p>
DONE

# WSPD10MAX plot definition
   print $query->checkbox(-name=>'WSPD10MAX_name',
                           -value=>'WSPD10MAX',
                           -checked=>$DEFAULT{'WSPD10MAX_name'},
                           -label=>'',
                    );
print <<DONE;
10-m Maximum Wind Speed<br>
&nbsp;&nbsp;&nbsp;Wind speed contour interval for each domain: 2 m/s (or 4 knots)<br>
DONE
  %labels = (
    "WspdCBCust", "&nbsp;<img src=/rip/WspdCBCust.jpg>&nbsp;&nbsp;&nbsp;4-Hue Colorblind Friendly (\"WspdCBCust\")",
    );
   $radios = $query->radio_group(-name=>'WSPD10MAX_color',
                             -values=>['WspdCBCust'],
                             -default=>$DEFAULT{'WSPD10MAX_color'},
                             -labels=>\%labels,
                             -linebreak=>'true',
                    );
   $radios =~ s/\&lt;/\</g;
   $radios =~ s/\&gt;/\>/g;
   $radios =~ s/\&amp;/\&/g;
   print $radios;

print <<DONE;
<p>
DONE

# Pre1hr plot definition
   print $query->checkbox(-name=>'Pre1hr_name',
                           -value=>'Pre1hr',
                           -checked=>$DEFAULT{'Pre1hr_name'},
                           -label=>'',
                    );
print <<DONE;
1-hour Precipitation (shaded). Contour interval: 2^n mm/hr<br>
DONE
  %labels = (
    "Rain3StepCust", "&nbsp;<img src=/rip/Rain3StepCust.jpg>&nbsp;&nbsp;&nbsp;3-Hue Colorblind Friendly (\"Rain3StepCust\")",
    "RainfallCust", "&nbsp;<img src=/rip/RainfallCust.jpg>&nbsp;&nbsp;&nbsp;Classic Color Scale (\"RainfallCust\")",
    );
   $radios = $query->radio_group(-name=>'Pre1hr_color',
			     -values=>['Rain3StepCust','RainfallCust'],
			     -default=>$DEFAULT{'Pre1hr_color'},
			     -labels=>\%labels,
			     -linebreak=>'true',
		    );
   $radios =~ s/\&lt;/\</g;
   $radios =~ s/\&gt;/\>/g;
   $radios =~ s/\&amp;/\&/g;
   print $radios;

print <<DONE;
<p>
DONE

# PreAcc plot definition
   print $query->checkbox(-name=>'PreAcc_name',
                           -value=>'Pref0ac',
                           -checked=>$DEFAULT{'PreAcc_name'},
                           -label=>'',
                    );
print <<DONE;
Accumulated Precipitation in forecast (shaded). Contour interval: 2^n mm/hr<br>
DONE
  %labels = (
    "Rain3StepCust", "&nbsp;<img src=/rip/Rain3StepCust.jpg>&nbsp;&nbsp;&nbsp;3-Hue Colorblind Friendly (\"Rain3StepCust\")",
    "RainfallCust", "&nbsp;<img src=/rip/RainfallCust.jpg>&nbsp;&nbsp;&nbsp;Classic Color Scale (\"RainfallCust\")",
    );
   $radios = $query->radio_group(-name=>'PreAcc_color',
                             -values=>['Rain3StepCust','RainfallCust'],
                             -default=>$DEFAULT{'PreAcc_color'},
                             -labels=>\%labels,
                             -linebreak=>'true',
                    );
   $radios =~ s/\&lt;/\</g;
   $radios =~ s/\&gt;/\>/g;
   $radios =~ s/\&amp;/\&/g;
   print $radios;

print <<DONE;
<p>
DONE

# Pre1hrZ plot definition
   print $query->checkbox(-name=>'Pre1hrZ_name',
                           -value=>'Pre1hrZ',
                           -checked=>$DEFAULT{'Pre1hrZ_name'},
                           -label=>'',
                    );
print <<DONE;
1-hr Precip (shaded), MSLP (contoured), Thickness (contoured) and 10-m Wind Barbs<br>
&nbsp;&nbsp;&nbsp;MSLP contour intervals for each domain:
DONE
  %labels = (
    "4,2,1,0.5", "4, 2, 1, 0.5 hPa",
    "none,none,none,none", "No MSLP contoured",
    );
   print $query->radio_group(-name=>'Pre1hrZ_ctr_int',
			     -values=>['4,2,1,0.5','none,none,none,none'],
			     -default=>$DEFAULT{'Pre1hrZ_ctr_int'},
			     -labels=>\%labels,
		    );
print <<DONE;
<br>
&nbsp;&nbsp;&nbsp;Precip Rate intervals: 2^n mm/hr<br>
DONE
  %labels = (
    "6,3,1,0.5", "1000 - 500 hPa (6, 3, 1, 0.5 hPa)",
    "3,2,1,0.5", "850 - 700 hPa (3, 2, 1, 0.5 hPa)",
    );
   print $query->radio_group(-name=>'Pre1hrZ_dsh_int',
			     -values=>['6,3,1,0.5','3,2,1,0.5'],
			     -default=>$DEFAULT{'Pre1hrZ_dsh_int'},
			     -labels=>\%labels,
		    );
print <<DONE;
<br>
&nbsp;&nbsp;&nbsp;1000-500 hPa Thickness highlight line:
DONE
  %labels = (
    "534", "534 dam",
    "540", "540 dam",
    );
   print $query->radio_group(-name=>'Pre1hrZ_ctr_hghlt',
                             -values=>['534','540'],
                             -default=>$DEFAULT{'Pre1hrZ_ctr_hghlt'},
                             -labels=>\%labels,
                    );
print <<DONE;
<br>
DONE
  %labels = (
    "Rain3StepCust", "&nbsp;<img src=/rip/Rain3StepCust.jpg>&nbsp;&nbsp;&nbsp;3-Hue Colorblind Friendly (\"Rain3StepCust\")",
    "RainfallCust", "&nbsp;<img src=/rip/RainfallCust.jpg>&nbsp;&nbsp;&nbsp;Classic Color Scale (\"RainfallCust\")",
    );
   $radios = $query->radio_group(-name=>'Pre1hrZ_color',
			     -values=>['Rain3StepCust','RainfallCust'],
			     -default=>$DEFAULT{'Pre1hrZ_color'},
			     -labels=>\%labels,
			     -linebreak=>'true',
		    );
   $radios =~ s/\&lt;/\</g;
   $radios =~ s/\&gt;/\>/g;
   $radios =~ s/\&amp;/\&/g;
   print $radios;

print <<DONE;
<p>
DONE

# Snow1hr plot definition
   print $query->checkbox(-name=>'Snow1hr_name',
                           -value=>'Snow1hr',
                           -checked=>$DEFAULT{'Snow1hr_name'},
                           -label=>'',
                    );     
print <<DONE;       
1-hour Snowfall (shaded). Contour interval: 2^n m/hr<br>
DONE
  %labels = (
    "Snow3StepCust", "&nbsp;<img src=/rip/Snow3StepCust.jpg>&nbsp;&nbsp;&nbsp;3-Hue Colorblind Friendly (\"Snow3StepCust\")",
    "SnowfallCust", "&nbsp;<img src=/rip/SnowfallCust.jpg>&nbsp;&nbsp;&nbsp;Classic Color Scale (\"SnowfallCust\")",
    );
   $radios = $query->radio_group(-name=>'Snow1hr_color',
                             -values=>['Snow3StepCust','SnowfallCust'],
                             -default=>$DEFAULT{'Snow1hr_color'},
                             -labels=>\%labels,
                             -linebreak=>'true',
                    );
   $radios =~ s/\&lt;/\</g;
   $radios =~ s/\&gt;/\>/g;
   $radios =~ s/\&amp;/\&/g;
   print $radios;

print <<DONE;
<p>
DONE

# SnowAcc plot definition
   print $query->checkbox(-name=>'SnowAcc_name',
                           -value=>'Snowf0ac',
                           -checked=>$DEFAULT{'SnowAcc_name'},
                           -label=>'',
                    );
print <<DONE;       
Accumulated Snowfall in forecast (shaded). Contour interval: 2^n m/hr<br>
DONE
  %labels = (
    "Snow3StepCust", "&nbsp;<img src=/rip/Snow3StepCust.jpg>&nbsp;&nbsp;&nbsp;3-Hue Colorblind Friendly (\"Snow3StepCust\")",
    "SnowfallCust", "&nbsp;<img src=/rip/SnowfallCust.jpg>&nbsp;&nbsp;&nbsp;Classic Color Scale (\"SnowfallCust\")",
    );
   $radios = $query->radio_group(-name=>'SnowAcc_color',
                             -values=>['Snow3StepCust','SnowfallCust'],
                             -default=>$DEFAULT{'SnowAcc_color'},
                             -labels=>\%labels,
                             -linebreak=>'true',
                    );
   $radios =~ s/\&lt;/\</g;
   $radios =~ s/\&gt;/\>/g;
   $radios =~ s/\&amp;/\&/g;
   print $radios;

print <<DONE;
<p>
DONE

# radar plot definition
   print $query->checkbox(-name=>'radar_name',
                           -value=>'radar',
                           -checked=>$DEFAULT{'radar_name'},
                           -label=>'',
                    );
print <<DONE;
Radar Reflectivity (shaded). Contour interval: 5 dBZ<br>
DONE
  %labels = (
    "dBZ3StepCust", "&nbsp;<img src=/rip/dBZ3StepCust.jpg>&nbsp;&nbsp;&nbsp;3-Hue Colorblind Friendly (\"dBZ3StepCust\")",
    "RadarCust", "&nbsp;<img src=/rip/RadarCust.jpg>&nbsp;&nbsp;&nbsp;Classic Color Scale (\"RadarCust\")",
    );
   $radios = $query->radio_group(-name=>'radar_color',
			     -values=>['dBZ3StepCust','RadarCust'],
			     -default=>$DEFAULT{'radar_color'},
			     -labels=>\%labels,
			     -linebreak=>'true',
		    );
   $radios =~ s/\&lt;/\</g;
   $radios =~ s/\&gt;/\>/g;
   $radios =~ s/\&amp;/\&/g;
   print $radios;

print <<DONE;
<p>
DONE

# pcpwtr plot definition
   print $query->checkbox(-name=>'pcpwtr_name',
                           -value=>'pcpwtr',
                           -checked=>$DEFAULT{'pcpwtr_name'},
                           -label=>'',
                    );
print <<DONE;
Precipitable water (hydrometeors+vapor - shaded). Contour interval: 4 mm<br>
DONE
  %labels = (
    "PcpWtr3StepCust", "&nbsp;<img src=/rip/PcpWtr3StepCust.jpg>&nbsp;&nbsp;&nbsp;3-Hue Colorblind Friendly (\"PcpWtr3StepCust\")",
    );
   $radios = $query->radio_group(-name=>'pcpwtr_color',
                             -values=>['PcpWtr3StepCust'],
                             -default=>$DEFAULT{'pcpwtr_color'},
                             -labels=>\%labels,
                             -linebreak=>'true',
                    );
   $radios =~ s/\&lt;/\</g;
   $radios =~ s/\&gt;/\>/g;
   $radios =~ s/\&amp;/\&/g;
   print $radios;

print <<DONE;
<p>
DONE

# pcpwv plot definition
   print $query->checkbox(-name=>'pcpwv_name',
                           -value=>'pcpwv',
                           -checked=>$DEFAULT{'pcpwv_name'},
                           -label=>'',
                    );
print <<DONE;
Precipitable water vapor. Contour interval: 4 mm<br>
DONE
  %labels = (
    "PcpWV3StepCust", "&nbsp;<img src=/rip/PcpWV3StepCust.jpg>&nbsp;&nbsp;&nbsp;3-Hue Colorblind Friendly (\"PcpWV3StepCust\")",
    );
   $radios = $query->radio_group(-name=>'pcpwv_color',
                             -values=>['PcpWV3StepCust'],
                             -default=>$DEFAULT{'pcpwv_color'},
                             -labels=>\%labels,
                             -linebreak=>'true',
                    );
   $radios =~ s/\&lt;/\</g;
   $radios =~ s/\&gt;/\>/g;
   $radios =~ s/\&amp;/\&/g;
   print $radios;

print <<DONE;
<p>
DONE

# cape plot definition
   print $query->checkbox(-name=>'cape_name',
                           -value=>'cape',
                           -checked=>$DEFAULT{'cape_name'},
                           -label=>'',
                    );
print <<DONE;
CAPE&nbsp;&nbsp;&nbsp; contour interval: 200 J/kg<br
DONE
  %labels = (
    "5StepCBCust", "&nbsp;<img src=/rip/5StepCBCust.jpg>&nbsp;&nbsp;&nbsp;5-Hue Colorblind Friendly (\"5StepCBCust\")",
    );
   $radios = $query->radio_group(-name=>'cape_color',
			     -values=>['5StepCBCust'],
			     -default=>$DEFAULT{'cape_color'},
			     -labels=>\%labels,
			     -linebreak=>'true',
		    );
   $radios =~ s/\&lt;/\</g;
   $radios =~ s/\&gt;/\>/g;
   $radios =~ s/\&amp;/\&/g;
   print $radios;

print <<DONE;
<p>
DONE

# cin plot definition
   print $query->checkbox(-name=>'cin_name',
                           -value=>'cin',
                           -checked=>$DEFAULT{'cin_name'},
                           -label=>'',
                    );
print <<DONE;
CIN&nbsp;&nbsp;&nbsp; contour interval: 50 J/kg<br>
DONE
  %labels = (
    "BlueMonoCB", "&nbsp;<img src=/rip/BlueMonoCB.jpg>&nbsp;&nbsp;&nbsp;1-Hue Colorblind Friendly (\"BlueMonoCB\")",
    );
   $radios = $query->radio_group(-name=>'cin_color',
			     -values=>['BlueMonoCB'],
			     -default=>$DEFAULT{'cin_color'},
			     -labels=>\%labels,
			     -linebreak=>'true',
		    );
   $radios =~ s/\&lt;/\</g;
   $radios =~ s/\&gt;/\>/g;
   $radios =~ s/\&amp;/\&/g;
   print $radios;

print <<DONE;
<p>
DONE

# pblhgt plot definition
   print $query->checkbox(-name=>'pblhgt_name',
                           -value=>'pblhgt',
                           -checked=>$DEFAULT{'pblhgt_name'},
                           -label=>'',
                    );
print <<DONE;
PBL Height (shaded)&nbsp;&nbsp;&nbsp;contour interval: increases with height 100 - 500 m<br>
&nbsp;&nbsp;&nbsp;Maximum PBL Height shading level: 4000 m
<br>
DONE
  %labels = (
    "PBLCBCust", "&nbsp;<img src=/rip/PBLCBCust.jpg>&nbsp;&nbsp;&nbsp;4-Hue Colorblind Friendly (\"PBLCBCust\")",
    );
   $radios = $query->radio_group(-name=>'pblhgt_color',
			     -values=>['PBLCBCust'],
			     -default=>$DEFAULT{'pblhgt_color'},
			     -labels=>\%labels,
			     -linebreak=>'true',
		    );
   $radios =~ s/\&lt;/\</g;
   $radios =~ s/\&gt;/\>/g;
   $radios =~ s/\&amp;/\&/g;
   print $radios;

print <<DONE;
<p>
DONE

# frzlvlhgt plot definition
   print $query->checkbox(-name=>'frzlvlhgt_name',
                          -value=>'FzLevHgt',
                          -checked=>$DEFAULT{'frzlvlhgt_name'},
                          -label=>'',
                     );
print <<DONE;
Freezing Level Height above ground level (shaded)&nbsp;&nbsp;&nbsp;contour interval: 100 - 500 m<br>
&nbsp;&nbsp;&nbsp;Maximum Freezing Level Height shading level: 5000 m
<br>
DONE
  %labels = (
    "5StepCBCust", "&nbsp;<img src=/rip/5StepCBCust.jpg>&nbsp;&nbsp;&nbsp;5-Hue Colorblind Friendly (\"5StepCBCust\")",
    );
   $radios = $query->radio_group(-name=>'frzlvlhgt_color',
                                 -values=>['5StepCBCust'],
                                 -default=>$DEFAULT{'frzlvlhgt_color'},
                                 -labels=>\%labels,
                                 -linebreak=>'true',
                    );
   $radios =~ s/\&lt;/\</g;
   $radios =~ s/\&gt;/\>/g;
   $radios =~ s/\&amp;/\&/g;
   print $radios;

print <<DONE;
<p>
DONE

# frzlvlhgtAMSL plot definition
   print $query->checkbox(-name=>'frzlvlhgtAMSL_name',
                          -value=>'FzLevHgtAMSL',
                          -checked=>$DEFAULT{'frzlvlhgtAMSL_name'},
                          -label=>'',
                     );
print <<DONE;
Freezing Level Height above mean sea level (shaded)&nbsp;&nbsp;&nbsp;contour interval: 100 - 500 m<br>
&nbsp;&nbsp;&nbsp;Maximum Freezing Level Height shading level: 5000 m
<br>
DONE
  %labels = (
    "5StepCBCust", "&nbsp;<img src=/rip/5StepCBCust.jpg>&nbsp;&nbsp;&nbsp;5-Hue Colorblind Friendly (\"5StepCBCust\")",
    );
   $radios = $query->radio_group(-name=>'frzlvlhgtAMSL_color',
                                 -values=>['5StepCBCust'],
                                 -default=>$DEFAULT{'frzlvlAMSLhgt_color'},
                                 -labels=>\%labels,
                                 -linebreak=>'true',
                    );
   $radios =~ s/\&lt;/\</g;
   $radios =~ s/\&gt;/\>/g;
   $radios =~ s/\&amp;/\&/g;
   print $radios;

print <<DONE;
<p>
DONE

# sfcvis plot definition
   print $query->checkbox(-name=>'sfcvis_name',
                           -value=>'sfcvis',
                           -checked=>$DEFAULT{'sfcvis_name'},
                           -label=>'',
                    );
print <<DONE;
Visibility (shaded)<br>
&nbsp;&nbsp;&nbsp;Maximum Visibility level
DONE
  %labels = (
    "16", "16 km",
    "30", "30 km",
    );
   print $query->radio_group(-name=>'sfcvis_sha_max',
			     -values=>['16','30'],
			     -default=>$DEFAULT{'sfcvis_sha_max'},
			     -labels=>\%labels,
		    );
print <<DONE;
<br>
DONE
  %labels = (
    "VisCBCust", "&nbsp;<img src=/rip/VisCBCust.jpg>&nbsp;&nbsp;&nbsp;3-Hue Colorblind Friendly (\"VisCBCust\")",
    );
   $radios = $query->radio_group(-name=>'sfcvis_color',
			     -values=>['VisCBCust'],
			     -default=>$DEFAULT{'sfcvis_color'},
			     -labels=>\%labels,
			     -linebreak=>'true',
		    );
   $radios =~ s/\&lt;/\</g;
   $radios =~ s/\&gt;/\>/g;
   $radios =~ s/\&amp;/\&/g;
   print $radios;

print <<DONE;
<p>
DONE

# ms500h plot definition
   print $query->checkbox(-name=>'ms500h_name',
                           -value=>'ms500h',
                           -checked=>$DEFAULT{'ms500h_name'},
                           -label=>'',
                    );
print <<DONE;
MSLP (contoured) and 1000-500hPa Thickness (dashed)<br>
&nbsp;&nbsp;&nbsp;MSLP contour intervals for each domain: 4, 2, 1, 1 hPa<br>
&nbsp;&nbsp;&nbsp;1000-500 hPa Thickness highlight line:
DONE
  %labels = (
    "534", "534 dam",
    "540", "540 dam",
    );
   print $query->radio_group(-name=>'ms500h_ctr_hghlt',
                             -values=>['534','540'],
                             -default=>$DEFAULT{'ms500h_ctr_hghlt'},
                             -labels=>\%labels,
                    );

print <<DONE;
<br>&nbsp;&nbsp;&nbsp;1000-500hPa Thickness intervals:
DONE
  %labels = (
    "3,2,1.5,1", "3,2,1.5,1 dam",
    "6,3,1,0.5", "6, 3, 1, 0.5 dam",
    );
   print $query->radio_group(-name=>'ms500h_ctr_int',
			     -values=>['3,2,1.5,1','6,3,1,0.5'],
			     -default=>$DEFAULT{'ms500h_ctr_int'},
			     -labels=>\%labels,
		    );

print <<DONE;
<p>
DONE

# Z925MSLP plot definition
   print $query->checkbox(-name=>'Z925MSLP_name',
                           -value=>'Z925MSLP',
                           -checked=>$DEFAULT{'Z925MSLP_name'},
                           -label=>'',
                    );
print <<DONE;
MSLP (contoured) 925 hPa Height (dashed)<br>
&nbsp;&nbsp;&nbsp;MSLP contour intervals for each domain: 4, 2, 1, 1 hPa<br>
&nbsp;&nbsp;&nbsp;1000-500hPa Thickness intervals: 30, 30, 7.5, 7.5 m
DONE


print <<DONE;
<p>
<a name="upper"><h3>Upper Air Plots</h3>

Upper Level Temperature (shaded), Heights (contoured) and Wind Barbs<br>
&nbsp;&nbsp;&nbsp;Temperature contour intervals for each domain: 4, 3, 2, 2 C<br>
DONE
# Color scales for following upper air Temp/Height/Wind plots
  %labels = (
    "TCBUniform", "&nbsp;<img src=/rip/TCBUniform.jpg>&nbsp;&nbsp;&nbsp;5-Hue Colorblind Friendly (\"TCBUniform\")",
    "Temperature", "&nbsp;<img src=/rip/Temperature.jpg>&nbsp;&nbsp;&nbsp;Classic Color Scale (\"Temperature\")",
    );
   $radios = $query->radio_group(-name=>'TXXXWnd_color',
			     -values=>['TCBUniform','Temperature'],
			     -default=>$DEFAULT{'TXXXWnd_color'},
			     -labels=>\%labels,
			     -linebreak=>'true',
		    );
   $radios =~ s/\&lt;/\</g;
   $radios =~ s/\&gt;/\>/g;
   $radios =~ s/\&amp;/\&/g;
   print $radios;

print <<DONE;
<p>
DONE

# T975Wnd plot definition
   print $query->checkbox(-name=>'T975Wnd_name',
                           -value=>'T975Wnd',
                           -checked=>$DEFAULT{'T975Wnd_name'},
                           -label=>'',
                    );
print <<DONE;
975 hPa with 30 m Geopotential Height contour interval
<br>
DONE

print <<DONE;
<p>
DONE

# T950Wnd plot definition
   print $query->checkbox(-name=>'T950Wnd_name',
                           -value=>'T950Wnd',
                           -checked=>$DEFAULT{'T950Wnd_name'},
                           -label=>'',
                    );
print <<DONE;
950 hPa with 30 m Geopotential Height contour interval
<br>
DONE

print <<DONE;
<p>
DONE

# T925Wnd plot definition
   print $query->checkbox(-name=>'T925Wnd_name',
                           -value=>'T925Wnd',
                           -checked=>$DEFAULT{'T925Wnd_name'},
                           -label=>'',
                    );
print <<DONE;
925 hPa with 30 m Geopotential Height contour interval
<br>
DONE

print <<DONE;
<p>
DONE

# T900Wnd plot definition
   print $query->checkbox(-name=>'T900Wnd_name',
                           -value=>'T900Wnd',
                           -checked=>$DEFAULT{'T900Wnd_name'},
                           -label=>'',
                    );
print <<DONE;
900 hPa with 30 m Geopotential Height contour interval
<br>
DONE

print <<DONE;
<p>
DONE

# T850Wnd plot definition
   print $query->checkbox(-name=>'T850Wnd_name',
                           -value=>'T850Wnd',
                           -checked=>$DEFAULT{'T850Wnd_name'},
                           -label=>'',
                    );
print <<DONE;
850 hPa with 30 m Geopotential Height contour interval<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Summer Temperature shading minimum
DONE
  %labels = (
    "-24", "-24 C",
    "-16", "-16 C",
    );
   print $query->radio_group(-name=>'T850Wnd_min_smr',
			     -values=>['-24','-16'],
			     -default=>$DEFAULT{'T850Wnd_min_smr'},
			     -labels=>\%labels,
		    );
print <<DONE;
<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Summer Temperature shading maximum
DONE
  %labels = (
    "40", "40 C",
    "48", "48 C",
    );
   print $query->radio_group(-name=>'T850Wnd_max_smr',
                             -values=>['40','48'],
                             -default=>$DEFAULT{'T850Wnd_max_smr'},
                             -labels=>\%labels,
                    );
print <<DONE;
<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Winter Temperature shading minimum
DONE
  %labels = (
    "-40", "-40 C",
    "-32", "-32 C",
    "-24", "-24 C",
    );
   print $query->radio_group(-name=>'T850Wnd_min_wtr',
                             -values=>['-40','-32','-24'],
                             -default=>$DEFAULT{'T850Wnd_min_wtr'},
                             -labels=>\%labels,
                    );
print <<DONE;
<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Winter Temperature shading maximum
DONE
  %labels = (
    "24", "24 C",
    "32", "32 C",
    "40", "40 C",
    );
   print $query->radio_group(-name=>'T850Wnd_max_wtr',
                             -values=>['24','32','40'],
                             -default=>$DEFAULT{'T850Wnd_max_wtr'},
                             -labels=>\%labels,
                    );

print <<DONE;
<p>
DONE

# T800Wnd plot definition
   print $query->checkbox(-name=>'T800Wnd_name',
                           -value=>'T800Wnd',
                           -checked=>$DEFAULT{'T800Wnd_name'},
                           -label=>'',
                    );
print <<DONE;
800 hPa with 30 m Geopotential Height contour interval<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Summer Temperature shading minimum
DONE
  %labels = (
    "-24", "-24 C",
    "-16", "-16 C",
    );
   print $query->radio_group(-name=>'T800Wnd_min_smr',
                             -values=>['-24','-16'],
                             -default=>$DEFAULT{'T800Wnd_min_smr'},
                             -labels=>\%labels,
                    );
print <<DONE;
<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Summer Temperature shading maximum
DONE
  %labels = (
    "40", "40 C",
    "48", "48 C",
    );
   print $query->radio_group(-name=>'T800Wnd_max_smr',
                             -values=>['40','48'],
                             -default=>$DEFAULT{'T800Wnd_max_smr'},
                             -labels=>\%labels,
                    );
print <<DONE;
<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Winter Temperature shading minimum
DONE
  %labels = (
    "-40", "-40 C",
    "-32", "-32 C",
    "-24", "-24 C",
    );
   print $query->radio_group(-name=>'T800Wnd_min_wtr',
                             -values=>['-40','-32','-24'],
                             -default=>$DEFAULT{'T800Wnd_min_wtr'},
                             -labels=>\%labels,
                    );
print <<DONE;
<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Winter Temperature shading maximum
DONE
  %labels = (
    "24", "24 C",
    "32", "32 C",
    "40", "40 C",
    );
   print $query->radio_group(-name=>'T800Wnd_max_wtr',
                             -values=>['24','32','40'],
                             -default=>$DEFAULT{'T800Wnd_max_wtr'},
                             -labels=>\%labels,
                    );

print <<DONE;
<p>
DONE

# T750Wnd plot definition
   print $query->checkbox(-name=>'T750Wnd_name',
                          -value=>'T750Wnd',
                          -checked=>$DEFAULT{'T750Wnd_name'},
                          -label=>'',
                    );
print <<DONE;
750 hPa with 30 m Geopotential Height contour interval<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Summer Temperature shading minimum
DONE
  %labels = (
    "-24", "-24 C",
    "-16", "-16 C",
    );
   print $query->radio_group(-name=>'T750Wnd_min_smr',
                             -values=>['-24','-16'],
                             -default=>$DEFAULT{'T750Wnd_min_smr'},
                             -labels=>\%labels,
                   );
print <<DONE;
<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Summer Temperature shading maximum
DONE
  %labels = (
    "40", "40 C",
    "48", "48 C",
    );
   print $query->radio_group(-name=>'T750Wnd_max_smr',
                             -values=>['40','48'],
                             -default=>$DEFAULT{'T750Wnd_max_smr'},
                             -labels=>\%labels,
                    );
print <<DONE;
<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Winter Temperature shading minimum
DONE
  %labels = (
    "-40", "-40 C",
    "-32", "-32 C",
    "-24", "-24 C",
    );
   print $query->radio_group(-name=>'T750Wnd_min_wtr',
                             -values=>['-40','-32','-24'],
                             -default=>$DEFAULT{'T750Wnd_min_wtr'},
                             -labels=>\%labels,
                    );
print <<DONE;
<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Winter Temperature shading maximum
DONE
  %labels = (
    "24", "24 C",
    "32", "32 C",
    "40", "40 C",
    );
   print $query->radio_group(-name=>'T750Wnd_max_wtr',
                             -values=>['24','32','40'],
                             -default=>$DEFAULT{'T750Wnd_max_wtr'},
                             -labels=>\%labels,
                    );

print <<DONE;
<p>
DONE

# T700Wnd plot definition
   print $query->checkbox(-name=>'T700Wnd_name',
                           -value=>'T700Wnd',
                           -checked=>$DEFAULT{'T700Wnd_name'},
                           -label=>'',
                    );
print <<DONE;
700 hPa with 30 m Geopotential Height contour interval<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Summer Temperature shading minimum
DONE
  %labels = (
    "-24", "-24 C",
    "-16", "-16 C",
    );
   print $query->radio_group(-name=>'T700Wnd_min_smr',
                             -values=>['-24','-16'],
                             -default=>$DEFAULT{'T700Wnd_min_smr'},
                             -labels=>\%labels,
                    );
print <<DONE;
<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Summer Temperature shading maximum
DONE
  %labels = (
    "40", "40 C",
    "48", "48 C",
    );
   print $query->radio_group(-name=>'T700Wnd_max_smr',
                             -values=>['40','48'],
                             -default=>$DEFAULT{'T700Wnd_max_smr'},
                             -labels=>\%labels,
                    );
print <<DONE;
<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Winter Temperature shading minimum
DONE
  %labels = (
    "-40", "-40 C",
    "-32", "-32 C",
    "-24", "-24 C",
    );
   print $query->radio_group(-name=>'T700Wnd_min_wtr',
                             -values=>['-40','-32','-24'],
                             -default=>$DEFAULT{'T700Wnd_min_wtr'},
                             -labels=>\%labels,
                    );
print <<DONE;
<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Winter Temperature shading maximum
DONE
  %labels = (
    "24", "24 C",
    "32", "32 C",
    "40", "40 C",
    );
   print $query->radio_group(-name=>'T700Wnd_max_wtr',
                             -values=>['24','32','40'],
                             -default=>$DEFAULT{'T700Wnd_max_wtr'},
                             -labels=>\%labels,
                    );

print <<DONE;
<p>
DONE

# T600Wnd plot definition
   print $query->checkbox(-name=>'T600Wnd_name',
                           -value=>'T600Wnd',
                           -checked=>$DEFAULT{'T600Wnd_name'},
                           -label=>'',
                    );
print <<DONE;
600 hPa with 30 m Geopotential Height contour interval
<br>
DONE

print <<DONE;
<p>
DONE

# T500Wnd plot definition
   print $query->checkbox(-name=>'T500Wnd_name',
                           -value=>'T500Wnd',
                           -checked=>$DEFAULT{'T500Wnd_name'},
                           -label=>'',
                    );
print <<DONE;
500 hPa with 30 m Geopotential Height contour interval
<br>
DONE

print <<DONE;
<p>
DONE

# T400Wnd plot definition
   print $query->checkbox(-name=>'T400Wnd_name',
                           -value=>'T400Wnd',
                           -checked=>$DEFAULT{'T400Wnd_name'},
                           -label=>'',
                    );
print <<DONE;
400 hPa with 120 m Geopotential Height contour interval
<p>
DONE

# T300Wnd plot definition
   print $query->checkbox(-name=>'T300Wnd_name',
                           -value=>'T300Wnd',
                           -checked=>$DEFAULT{'T300Wnd_name'},
                           -label=>'',
                    );
print <<DONE;
300 hPa with 120 m Geopotential Height contour interval
<p>
DONE

# T250Wnd plot definition
   print $query->checkbox(-name=>'T250Wnd_name',
                           -value=>'T250Wnd',
                           -checked=>$DEFAULT{'T250Wnd_name'},
                           -label=>'',
                    );
print <<DONE;
250 hPa with 120 m Geopotential Height contour interval
<p>
DONE

# T200Wnd plot definition
   print $query->checkbox(-name=>'T200Wnd_name',
                           -value=>'T200Wnd',
                           -checked=>$DEFAULT{'T200Wnd_name'},
                           -label=>'',
                    );
print <<DONE;
200 hPa with 120 m Geopotential Height contour interval

<p>
Upper Level Relative Humidity (shaded) and Wind Barbs<br>
&nbsp;&nbsp;&nbsp;RH contour intervals for each domain: 10%<br>
DONE
# Color scales for following upper air RH/Wind plots
  %labels = (
    "HumidityCB", "&nbsp;<img src=/rip/HumidityCB.jpg>&nbsp;&nbsp;&nbsp;2-Hue Colorblind Friendly (\"HumidityCB\")",
    "Humidity", "&nbsp;<img src=/rip/Humidity.jpg>&nbsp;&nbsp;&nbsp;Classic Color Scale (\"Humidity\")",
    );
   $radios = $query->radio_group(-name=>'RHXXXWnd_color',
			     -values=>['HumidityCB','Humidity'],
			     -default=>$DEFAULT{'RHXXXWnd_color'},
			     -labels=>\%labels,
			     -linebreak=>'true',
		    );
   $radios =~ s/\&lt;/\</g;
   $radios =~ s/\&gt;/\>/g;
   $radios =~ s/\&amp;/\&/g;
   print $radios;

print <<DONE;
<p>
DONE

# RH975Wnd plot definition
   print $query->checkbox(-name=>'RH975Wnd_name',
                           -value=>'RH975Wnd',
                           -checked=>$DEFAULT{'RH975Wnd_name'},
                           -label=>'',
                    );
print <<DONE;
975 hPa RHwater
<br>
DONE

# RH950Wnd plot definition
   print $query->checkbox(-name=>'RH950Wnd_name',
                           -value=>'RH950Wnd',
                           -checked=>$DEFAULT{'RH950Wnd_name'},
                           -label=>'',
                    );
print <<DONE;
950 hPa RHwater
<br>
DONE

# RH925Wnd plot definition
   print $query->checkbox(-name=>'RH925Wnd_name',
                           -value=>'RH925Wnd',
                           -checked=>$DEFAULT{'RH925Wnd_name'},
                           -label=>'',
                    );
print <<DONE;
925 hPa RHwater
<br>
DONE

# RH900Wnd plot definition
   print $query->checkbox(-name=>'RH900Wnd_name',
                           -value=>'RH900Wnd',
                           -checked=>$DEFAULT{'RH900Wnd_name'},
                           -label=>'',
                    );
print <<DONE;
900 hPa RHwater
<br>
DONE

# RH850Wnd plot definition
   print $query->checkbox(-name=>'RH850Wnd_name',
                           -value=>'RH850Wnd',
                           -checked=>$DEFAULT{'RH850Wnd_name'},
                           -label=>'',
                    );
print <<DONE;
850 hPa RHwater
<br>
DONE

# RH800Wnd plot definition
   print $query->checkbox(-name=>'RH800Wnd_name',
                           -value=>'RH800Wnd',
                           -checked=>$DEFAULT{'RH800Wnd_name'},
                           -label=>'',
                    );
print <<DONE;
800 hPa RHwater
<br>
DONE

# RH750Wnd plot definition
   print $query->checkbox(-name=>'RH750Wnd_name',
                          -value=>'RH750Wnd',
                          -checked=>$DEFAULT{'RH750Wnd_name'},
                          -label=>'',
                    );
print <<DONE;
750 hPa RHwater
<br>
DONE

# RH700Wnd plot definition
   print $query->checkbox(-name=>'RH700Wnd_name',
                           -value=>'RH700Wnd',
                           -checked=>$DEFAULT{'RH700Wnd_name'},
                           -label=>'',
                    );
print <<DONE;
700 hPa RHwater
<br>
DONE

# RH500Wnd plot definition
   print $query->checkbox(-name=>'RH500Wnd_name',
                           -value=>'RH500Wnd',
                           -checked=>$DEFAULT{'RH500Wnd_name'},
                           -label=>'',
                    );
print <<DONE;
500 hPa RHice
<br>
DONE

# RH400Wnd plot definition
   print $query->checkbox(-name=>'RH400Wnd_name',
                           -value=>'RH400Wnd',
                           -checked=>$DEFAULT{'RH400Wnd_name'},
                           -label=>'',
                    );
print <<DONE;
400 hPa RHice
<br>
DONE

# RH300Wnd plot definition
   print $query->checkbox(-name=>'RH300Wnd_name',
                           -value=>'RH300Wnd',
                           -checked=>$DEFAULT{'RH300Wnd_name'},
                           -label=>'',
                    );
print <<DONE;
300 hPa RHice
<br>
DONE

# RH250Wnd plot definition
   print $query->checkbox(-name=>'RH250Wnd_name',
                           -value=>'RH250Wnd',
                           -checked=>$DEFAULT{'RH250Wnd_name'},
                           -label=>'',
                    );
print <<DONE;
250 hPa RHice
<br>
DONE

# RH200Wnd plot definition
   print $query->checkbox(-name=>'RH200Wnd_name',
                           -value=>'RH200Wnd',
                           -checked=>$DEFAULT{'RH200Wnd_name'},
                           -label=>'',
                    );
print <<DONE;
200 hPa RHice
<br>
DONE


print <<DONE;
<p>
DONE

# VV600m plot definition
   print $query->checkbox(-name=>'VV600m_name',
                           -value=>'VV600m',
                           -checked=>$DEFAULT{'VV600m_name'},
                           -label=>'',
                    );
print <<DONE;
600 m AGL vertical motion (contoured) and Terrain (shaded)<br>
&nbsp;&nbsp;&nbsp;See Terrain Height section at bottom to define terrain contour mins and maxes
DONE

print <<DONE;
<br>
&nbsp;&nbsp;&nbsp;Vertical motion contour interval: 
DONE
  %labels = (
    "10,20,40,50", "10, 20, 40, 50 cm/s",
    "2,10,25,25", "2, 10, 25, 25 cm/s",
    );
   print $query->radio_group(-name=>'VV600m_ctr_int',
                             -values=>['10,20,40,50','2,10,25,25'],
                             -default=>$DEFAULT{'VV600m_ctr_int'},
                             -labels=>\%labels,
                    );
print <<DONE;
<br>
DONE
  %labels = (
    "Grayscale", "&nbsp;<img src=/rip/Grayscale.jpg>&nbsp;&nbsp;&nbsp;Colorblind Friendly (\"Grayscale\")",
    "Terrain", "&nbsp;<img src=/rip/Terrain.jpg>&nbsp;&nbsp;&nbsp;Classic Color Scale (\"Terrain\")",
    );
   $radios = $query->radio_group(-name=>'VV600m_color',
			     -values=>['Grayscale','Terrain'],
			     -default=>$DEFAULT{'VV600m_color'},
			     -labels=>\%labels,
			     -linebreak=>'true',
		    );
   $radios =~ s/\&lt;/\</g;
   $radios =~ s/\&gt;/\>/g;
   $radios =~ s/\&amp;/\&/g;
   print $radios;

print <<DONE;
<p>
DONE

# Omeg700RH plot definition
   print $query->checkbox(-name=>'Omeg700RH_name',
                           -value=>'Omeg700RH',
                           -checked=>$DEFAULT{'Omeg700RH_name'},
                           -label=>'',
                    );
print <<DONE;
700 hPa Omega (dashed), Relative Humidity (shaded) and Height (contoured)<br>
&nbsp;&nbsp;&nbsp;Omega contour interval for each domain: 
DONE
  %labels = (
    "5,10,25,40", "5, 10, 25, 40 Pa/s",
    "2.5,5,10,20", "2.5, 5, 10, 20 Pa/s",
    );
   print $query->radio_group(-name=>'Omeg700RH_ctr_int',
                             -values=>['5,10,25,40','2.5,5,10,20'],
                             -default=>$DEFAULT{'Omeg700RH_ctr_int'},
                             -labels=>\%labels,
                    );
print <<DONE;
<br>
&nbsp;&nbsp;&nbsp;RH contour interval: 10%<br>
&nbsp;&nbsp;&nbsp;Geopotential height contour interval: 30 m<br>
DONE
  %labels = (
    "HumidityDimCB", "&nbsp;<img src=/rip/HumidityDimCB.jpg>&nbsp;&nbsp;&nbsp;Colorblind Friendly (\"HumidityDimCB\")",
    );
   $radios = $query->radio_group(-name=>'Omeg700RH_color',
			     -values=>['HumidityDimCB'],
			     -default=>$DEFAULT{'Omeg700RH_color'},
			     -labels=>\%labels,
			     -linebreak=>'true',
		    );
   $radios =~ s/\&lt;/\</g;
   $radios =~ s/\&gt;/\>/g;
   $radios =~ s/\&amp;/\&/g;
   print $radios;

print <<DONE;
<p>
DONE

# Vor500Z plot definition
   print $query->checkbox(-name=>'Vor500Z_name',
                           -value=>'Vor500Z',
                           -checked=>$DEFAULT{'Vor500Z_name'},
                           -label=>'',
                    );
print <<DONE;
500 hPa Vorticity (shaded) and Geopotential Height (contoured)<br>
&nbsp;&nbsp;&nbsp;Vorticity contour interval for each domain 5 s^-5<br>
&nbsp;&nbsp;&nbsp;Geopotential height contour interval: 60 m<br>
DONE
  %labels = (
    "PosNegCB", "&nbsp;<img src=/rip/PosNegCB.jpg>&nbsp;&nbsp;&nbsp;2-Hue Colorblind Friendly (\"PosNegCB\")",
    );
   $radios = $query->radio_group(-name=>'Vor500Z_color',
			     -values=>['PosNegCB'],
			     -default=>$DEFAULT{'Vor500Z_color'},
			     -labels=>\%labels,
			     -linebreak=>'true',
		    );
   $radios =~ s/\&lt;/\</g;
   $radios =~ s/\&gt;/\>/g;
   $radios =~ s/\&amp;/\&/g;
   print $radios;


# Potential Vorticity
print <<DONE;
<p>
Upper Level Potential Vorticity (shaded), Geopotential Height (contoured) and Wind Barbs<br>
&nbsp;&nbsp;&nbsp;Potential Vorticity contour interval: 0.5 PVU<br>
&nbsp;&nbsp;&nbsp;Geopotential height contour interval: 60 m<br>
DONE
# Color scales for following upper air Potential Vorticity plots
  %labels = (
    "PosNegCB", "&nbsp;<img src=/rip/PosNegCB.jpg>&nbsp;&nbsp;&nbsp;2-Hue Colorblind Friendly (\"PosNegCB\")",
    );
   $radios = $query->radio_group(-name=>'pvorXXXZ_color',
                             -values=>['PosNegCB'],
                             -default=>$DEFAULT{'pvorXXXZ_color'},
                             -labels=>\%labels,
                             -linebreak=>'true',
                    );
   $radios =~ s/\&lt;/\</g;
   $radios =~ s/\&gt;/\>/g;
   $radios =~ s/\&amp;/\&/g;
   print $radios;
print <<DONE;
<p>
DONE

# pvor700Z plot definition
   print $query->checkbox(-name=>'pvor700Z_name',
                           -value=>'pvor700Z',
                           -checked=>$DEFAULT{'pvor700Z_name'},
                           -label=>'',
                    );
print <<DONE;
700 hPa
<br>
DONE

# Wnd500Z plot definition
   print $query->checkbox(-name=>'pvor500Z_name',
                           -value=>'pvor500Z',
                           -checked=>$DEFAULT{'pvor500Z_name'},
                           -label=>'',
                    );
print <<DONE;
500 hPa
<br>
DONE


print <<DONE;
<p>
Upper Level Wind Speed (shaded and Barbs) and Geopotential Height (contoured)<br>
&nbsp;&nbsp;&nbsp;Wind speed contour interval for each domain: 10 m/s (or 20 knots)<br>
&nbsp;&nbsp;&nbsp;Geopotential height contour interval: 120 m<br>
DONE
# Color scales for following upper air Height/Wind plots
  %labels = (
    "PurpleMonoCB", "&nbsp;<img src=/rip/PurpleMonoCB.jpg>&nbsp;&nbsp;&nbsp;1-Hue Colorblind Friendly (\"PurpleMonoCB\")",
    );
   $radios = $query->radio_group(-name=>'WndXXXZ_color',
			     -values=>['PurpleMonoCB'],
			     -default=>$DEFAULT{'WndXXXZ_color'},
			     -labels=>\%labels,
			     -linebreak=>'true',
		    );
   $radios =~ s/\&lt;/\</g;
   $radios =~ s/\&gt;/\>/g;
   $radios =~ s/\&amp;/\&/g;
   print $radios;

print <<DONE;
<p>
DONE

# Wnd300Z plot definition
   print $query->checkbox(-name=>'Wnd300Z_name',
                           -value=>'Wnd300Z',
                           -checked=>$DEFAULT{'Wnd300Z_name'},
                           -label=>'',
                    );
print <<DONE;
300 hPa
<br>
DONE

# Wnd250Z plot definition
   print $query->checkbox(-name=>'Wnd250Z_name',
                           -value=>'Wnd250Z',
                           -checked=>$DEFAULT{'Wnd250Z_name'},
                           -label=>'',
                    );
print <<DONE;
250 hPa
<br>
DONE

# Wnd200Z plot definition
   print $query->checkbox(-name=>'Wnd200Z_name',
                           -value=>'Wnd200Z',
                           -checked=>$DEFAULT{'Wnd200Z_name'},
                           -label=>'',
                    );
print <<DONE;
200 hPa
<br>
DONE

# Wnd100Z plot definition
   print $query->checkbox(-name=>'Wnd100Z_name',
                           -value=>'Wnd100Z',
                           -checked=>$DEFAULT{'Wnd100Z_name'},
                           -label=>'',
                    );
print <<DONE;
100 hPa
<p>
DONE

# Div250Z plot definition
   print $query->checkbox(-name=>'Div250Z_name',
                           -value=>'Div250Z',
                           -checked=>$DEFAULT{'Div250Z_name'},
                           -label=>'',
                    );
print <<DONE;
250 hPa Divergence (shaded) and Geopotential Height (contoured)<br>
&nbsp;&nbsp;&nbsp;Divergence contour interval for each domain: 2.5, 2.5, 2.5, 2.5 *10^-5 1/s<br>
&nbsp;&nbsp;&nbsp;Geopotential height contour interval: 120 m<br>
DONE
  %labels = (
    "PosNegCB", "&nbsp;<img src=/rip/PosNegCB.jpg>&nbsp;&nbsp;&nbsp;2-Hue Colorblind Friendly (\"PosNegCB\")",
    );
   $radios = $query->radio_group(-name=>'Div250Z_color',
			     -values=>['PosNegCB'],
			     -default=>$DEFAULT{'Div250Z_color'},
			     -labels=>\%labels,
			     -linebreak=>'true',
		    );
   $radios =~ s/\&lt;/\</g;
   $radios =~ s/\&gt;/\>/g;
   $radios =~ s/\&amp;/\&/g;
   print $radios;

print <<DONE;
<p>
Isentropic Relative Humidity (shaded), Pressure (contoured) and Wind Barbs<br>
&nbsp;&nbsp;&nbsp;Relative humidity contour interval for each domain: 10%<br>
&nbsp;&nbsp;&nbsp;Pressure contour intervals for each domain: 40, 20, 10, 10 hPa<br>
DONE
# Color scales for following isentropic RH/pressure plots
  %labels = (
    "HumidityCB", "&nbsp;<img src=/rip/HumidityCB.jpg>&nbsp;&nbsp;&nbsp;2-Hue Colorblind Friendly (\"HumidityCB\")",
    "Humidity", "&nbsp;<img src=/rip/Humidity.jpg>&nbsp;&nbsp;&nbsp;Classic Color Scale (\"Humidity\")",
    );
   $radios = $query->radio_group(-name=>'isenXXXPRH_color',
                             -values=>['HumidityCB','Humidity'],
                             -default=>$DEFAULT{'isenXXXPRH_color'},
                             -labels=>\%labels,
                             -linebreak=>'true',
                    );
   $radios =~ s/\&lt;/\</g;
   $radios =~ s/\&gt;/\>/g;
   $radios =~ s/\&amp;/\&/g;
   print $radios;

print <<DONE;
<p>
DONE

# isen280PRH plot definition
   print $query->checkbox(-name=>'isen280PRH_name',
                          -value=>'isen280PRH',
                          -checked=>$DEFAULT{'isen280PRH_name'},
                          -label=>'',
                    );
print <<DONE;
280 K
<br>
DONE

# isen285PRH plot definition
   print $query->checkbox(-name=>'isen285PRH_name',
                          -value=>'isen285PRH',
                          -checked=>$DEFAULT{'isen285PRH_name'},
                          -label=>'',
                    );
print <<DONE;
285 K
<br>
DONE

# isen290PRH plot definition
   print $query->checkbox(-name=>'isen290PRH_name',
                          -value=>'isen290PRH',
                          -checked=>$DEFAULT{'isen290PRH_name'},
                          -label=>'',
                    );
print <<DONE;
290 K
<br>
DONE

# isen295PRH plot definition
   print $query->checkbox(-name=>'isen295PRH_name',
                          -value=>'isen295PRH',
                          -checked=>$DEFAULT{'isen295PRH_name'},
                          -label=>'',
                    );
print <<DONE;
295 K
<br>
DONE

# isen300PRH plot definition
   print $query->checkbox(-name=>'isen300PRH_name',
                          -value=>'isen300PRH',
                          -checked=>$DEFAULT{'isen300PRH_name'},
                          -label=>'',
                    );
print <<DONE;
300 K
<br>
DONE

# isen305PRH plot definition
   print $query->checkbox(-name=>'isen305PRH_name',
                          -value=>'isen305PRH',
                          -checked=>$DEFAULT{'isen305PRH_name'},
                          -label=>'',
                    );
print <<DONE;
305 K
<br>
DONE

# isen310PRH plot definition
   print $query->checkbox(-name=>'isen310PRH_name',
                          -value=>'isen310PRH',
                          -checked=>$DEFAULT{'isen310PRH_name'},
                          -label=>'',
                    );
print <<DONE;
310 K
<br>
DONE

print <<DONE;
<p>
<a name="custom"><h3>Custom Plots</h3>
DONE

# custom plots
   print $query->checkbox(-name=>'cust_plts_name',
                           -value=>'cust_plts',
                           -checked=>$DEFAULT{'cust_plts_name'},
                           -label=>'',
                    );
print <<DONE;
Include Custom plots for this range

<p>
<a name="others"><h3>Other Plan View Plots</h3>
DONE

# IRsat plot definition
   print $query->checkbox(-name=>'IRsat_name',
                           -value=>'IRsat',
                           -checked=>$DEFAULT{'IRsat_name'},
                           -label=>'',
                    );
print <<DONE;
Cloud Top Temperatures (shaded)<br>
&nbsp;&nbsp;&nbsp;Cloud top temperature contour interval for each domain: 5 C<br>
DONE
  %labels = (
    "Cloud3StepCBCust", "&nbsp;<img src=/rip/Cloud3StepCBCust.jpg>&nbsp;&nbsp;&nbsp;3-Hue Colorblind Friendly (\"Cloud3StepCBCust\")",
    "CloudTopTempCust", "&nbsp;<img src=/rip/CloudTopTempCust.jpg>&nbsp;&nbsp;&nbsp;Classic Color Scale (\"CloudTopTempCust\")",
    );
   $radios = $query->radio_group(-name=>'IRsat_color',
			     -values=>['Cloud3StepCBCust','CloudTopTempCust'],
			     -default=>$DEFAULT{'IRsat_color'},
			     -labels=>\%labels,
			     -linebreak=>'true',
		    );
   $radios =~ s/\&lt;/\</g;
   $radios =~ s/\&gt;/\>/g;
   $radios =~ s/\&amp;/\&/g;
   print $radios;

print <<DONE;
<p>
DONE

# slhflux plot definition
   print $query->checkbox(-name=>'slhflux_name',
                           -value=>'slhflux',
                           -checked=>$DEFAULT{'slhflux_name'},
                           -label=>'',
                    );
print <<DONE;
Surface Latent Heat Flux (shaded)<br>
&nbsp;&nbsp;&nbsp;Latent heat flux contour interval for each domain: 50,50,50,50<br>
DONE
  %labels = (
    "FluxCBCust", "&nbsp;<img src=/rip/FluxCBCust.jpg>&nbsp;&nbsp;&nbsp;2-Hue Colorblind Friendly (\"FluxCBCust\")",
    );
   $radios = $query->radio_group(-name=>'slhflux_color',
			     -values=>['FluxCBCust'],
			     -default=>$DEFAULT{'slhflux_color'},
			     -labels=>\%labels,
			     -linebreak=>'true',
		    );
   $radios =~ s/\&lt;/\</g;
   $radios =~ s/\&gt;/\>/g;
   $radios =~ s/\&amp;/\&/g;
   print $radios;

print <<DONE;
<p>
DONE

# sshflux plot definition
   print $query->checkbox(-name=>'sshflux_name',
                           -value=>'sshflux',
                           -checked=>$DEFAULT{'sshflux_name'},
                           -label=>'',
                    );
print <<DONE;
Surface Sensible Heat Flux (shaded)<br>
&nbsp;&nbsp;&nbsp;Latent heat flux contour interval for each domain: 50,50,50,50<br>
DONE
  %labels = (
    "FluxCBCust", "&nbsp;<img src=/rip/FluxCBCust.jpg>&nbsp;&nbsp;&nbsp;2-Hue Colorblind Friendly (\"FluxCBCust\")",
    );
   $radios = $query->radio_group(-name=>'sshflux_color',
			     -values=>['FluxCBCust'],
			     -default=>$DEFAULT{'sshflux_color'},
			     -labels=>\%labels,
			     -linebreak=>'true',
		    );
   $radios =~ s/\&lt;/\</g;
   $radios =~ s/\&gt;/\>/g;
   $radios =~ s/\&amp;/\&/g;
   print $radios;

print <<DONE;
<p>
DONE

# Skin Temperature plot definition
   print $query->checkbox(-name=>'SkinT_name',
                           -value=>'SkinT',
                           -checked=>$DEFAULT{'SkinT_name'},
                           -label=>'',
                    );
print <<DONE;
Skin Temperature (shaded)<br>
&nbsp;&nbsp;&nbsp;Skin temperature contour interval for each domain: 3, 3, 2, 2 C<br
DONE
  %labels = (
    "T5StepCBCust", "&nbsp;<img src=/rip/T4StepCBCust.jpg>&nbsp;&nbsp;&nbsp;4-Hue Colorblind Friendly (\"T4StepCBCust\")",
    "Temperature", "&nbsp;<img src=/rip/Temperature.jpg>&nbsp;&nbsp;&nbsp;Classic Color Scale (\"Temperature\")",
    );
   $radios = $query->radio_group(-name=>'SkinT_color',
                             -values=>['T5StepCBCust','Temperature'],
                             -default=>$DEFAULT{'SkinT_color'},
                             -labels=>\%labels,
                             -linebreak=>'true',
                    );
   $radios =~ s/\&lt;/\</g;
   $radios =~ s/\&gt;/\>/g;
   $radios =~ s/\&amp;/\&/g;
   print $radios;

print <<DONE;
&nbsp;&nbsp;&nbsp;Skin Temperature summer shading value range
DONE
  %labels = (
    "48;-12", "-12 to 48 C ",
    "48;0","0 to 48 C",
    );
   print $query->radio_group(-name=>'SkinT_rng_smr',
                             -values=>['48;-12','48;0'],
                             -default=>$DEFAULT{'SkinT_rng_smr'},
                             -labels=>\%labels,
                    );
print <<DONE;
<br>
&nbsp;&nbsp;&nbsp;Skin Temperature winter maximum shading value
DONE
  %labels = (
    "12;-60", "-60 to 12 C",
    "24;-48", "-48 to 24 C",
    "36;-36", "-36 to 36 C",
    "48;-12", "-12 to 48 C",
    );
   print $query->radio_group(-name=>'SkinT_rng_wtr',
                             -values=>['12;-60','24;-48','36;-36','48;-12'],
                             -default=>$DEFAULT{'SkinT_rng_wtr'},
                             -labels=>\%labels,
                    );

print <<DONE;
<p>
DONE

# soilt1 plot definition
   print $query->checkbox(-name=>'soilt1_name',
                           -value=>'soilt1',
                           -checked=>$DEFAULT{'soilt1_name'},
                           -label=>'',
                    );
print <<DONE;
Soil Layer 1 Temperature (shaded)<br>
&nbsp;&nbsp;&nbsp;Soil temperature contour interval for each domain: 3, 3, 2, 2 K<br
DONE
  %labels = (
    "T5StepCBCust", "&nbsp;<img src=/rip/T4StepCBCust.jpg>&nbsp;&nbsp;&nbsp;4-Hue Colorblind Friendly (\"T4StepCBCust\")",
    "Temperature", "&nbsp;<img src=/rip/Temperature.jpg>&nbsp;&nbsp;&nbsp;Classic Color Scale (\"Temperature\")",
    );
   $radios = $query->radio_group(-name=>'soilt1_color',
			     -values=>['T5StepCBCust','Temperature'],
			     -default=>$DEFAULT{'soilt1_color'},
			     -labels=>\%labels,
			     -linebreak=>'true',
		    );
   $radios =~ s/\&lt;/\</g;
   $radios =~ s/\&gt;/\>/g;
   $radios =~ s/\&amp;/\&/g;
   print $radios;

print <<DONE;
<p>
DONE

# soilm1 plot definition
   print $query->checkbox(-name=>'soilm1_name',
                           -value=>'soilm1',
                           -checked=>$DEFAULT{'soilm1_name'},
                           -label=>'',
                    );
print <<DONE;
Soil Layer 1 Moisture (shaded)<br>
&nbsp;&nbsp;&nbsp;Soil moisture contour interval for each domain: 0.05<br>
DONE
  %labels = (
    "HumidityCB", "&nbsp;<img src=/rip/HumidityCB.jpg>&nbsp;&nbsp;&nbsp;2-Hue Colorblind Friendly (\"HumidityCB\")",
    "Humidity", "&nbsp;<img src=/rip/Humidity.jpg>&nbsp;&nbsp;&nbsp;Classic Color Scale (\"Humidity\")",
    );
   $radios = $query->radio_group(-name=>'soilm1_color',
			     -values=>['HumidityCB','Humidity'],
			     -default=>$DEFAULT{'soilm1_color'},
			     -labels=>\%labels,
			     -linebreak=>'true',
		    );
   $radios =~ s/\&lt;/\</g;
   $radios =~ s/\&gt;/\>/g;
   $radios =~ s/\&amp;/\&/g;
   print $radios;

print <<DONE;
<p>
DONE

# Downard solar radiation flux at the surface (W m-2) (SWDOWN) plot definition
   print $query->checkbox(-name=>'SWDOWN_name',
                           -value=>'SWDOWN',
                           -checked=>$DEFAULT{'SWDOWN_name'},
                           -label=>'',
                    );
print <<DONE;
Downward Solar Radiation Flux (shaded)<br>
&nbsp;&nbsp;&nbsp;Solar radiation contour interval: 50 W m^-2<br
DONE
  %labels = (
    "Radia5StepCBCust", "&nbsp;<img src=/rip/Radia5StepCBCust.jpg>&nbsp;&nbsp;&nbsp;5-Hue Colorblind Friendly (\"Radia5StepCBCust\")",
    "Temperature", "&nbsp;<img src=/rip/Temperature.jpg>&nbsp;&nbsp;&nbsp;Classic Color Scale (\"Temperature\")",
    );
   $radios = $query->radio_group(-name=>'SWDOWN_color',
                             -values=>['Radia5StepCBCust','Temperature'],
                             -default=>$DEFAULT{'SWDOWN_color'},
                             -labels=>\%labels,
                             -linebreak=>'true',
                    );
   $radios =~ s/\&lt;/\</g;
   $radios =~ s/\&gt;/\>/g;
   $radios =~ s/\&amp;/\&/g;
   print $radios;

print <<DONE;
<p>
<a name="xs"><h3>Cross Sections</h3>
DONE

# xsthw plot definition
   print $query->checkbox(-name=>'xsthw_name',
                           -value=>'xsthw',
                           -checked=>$DEFAULT{'xsthw_name'},
                           -label=>'',
                    );
print <<DONE;
XS(s) of Potential Temperature (shaded) and Wind Barbs<br>
&nbsp;&nbsp;&nbsp;Potential Temperature contour interval for each domain: 5 K<br>
DONE
  %labels = (
    "ThE3StepCBCust", "&nbsp;<img src=/rip/ThE3StepCBCust.jpg>&nbsp;&nbsp;&nbsp;3-Hue Colorblind Friendly (\"ThE3StepCBCust\")",
    "ThetaE", "&nbsp;<img src=/rip/ThetaE.jpg>&nbsp;&nbsp;&nbsp;Classic Color Scale (\"ThetaE\")",
    );
   $radios = $query->radio_group(-name=>'xsthw_color',
			     -values=>['ThE3StepCBCust','ThetaE'],
			     -default=>$DEFAULT{'xsthw_color'},
			     -labels=>\%labels,
			     -linebreak=>'true',
		    );
   $radios =~ s/\&lt;/\</g;
   $radios =~ s/\&gt;/\>/g;
   $radios =~ s/\&amp;/\&/g;
   print $radios;

print <<DONE;
<p>
DONE

# xsrhw plot definition
   print $query->checkbox(-name=>'xsrhw_name',
                           -value=>'xsrhw',
                           -checked=>$DEFAULT{'xsrhw_name'},
                           -label=>'',
                    );
print <<DONE;
XS(s) of Relative Humidity (shaded) and Wind Barbs<br>
&nbsp;&nbsp;&nbsp;Relative Humidity contour interval for each domain: 10%<br>
DONE
  %labels = (
    "HumidityCB", "&nbsp;<img src=/rip/HumidityCB.jpg>&nbsp;&nbsp;&nbsp;2-Hue Colorblind Friendly (\"HumidityCB\")",
    "Humidity", "&nbsp;<img src=/rip/Humidity.jpg>&nbsp;&nbsp;&nbsp;Classic Color Scale (\"Humidity\")",
    );
   $radios = $query->radio_group(-name=>'xsrhw_color',
			     -values=>['HumidityCB','Humidity'],
			     -default=>$DEFAULT{'xsrhw_color'},
			     -labels=>\%labels,
			     -linebreak=>'true',
		    );
   $radios =~ s/\&lt;/\</g;
   $radios =~ s/\&gt;/\>/g;
   $radios =~ s/\&amp;/\&/g;
   print $radios;

print <<DONE;
<p>
DONE

# xswWnd plot definition
   print $query->checkbox(-name=>'xswWnd_name',
                           -value=>'xswWnd',
                           -checked=>$DEFAULT{'xswWnd_name'},
                           -label=>'',
                    );
print <<DONE;
XS(s) of Vertical Motion and Wind into the XS<br>
&nbsp;&nbsp;&nbsp;Vertical motion contour interval for each domain: 
DONE
  %labels = (
    "10,20,40,50", "10, 20, 40, 50 cm/s",
    "5,5,20,20", "5, 5, 20, 20 cm/s",
    );
   print $query->radio_group(-name=>'xswWnd_ctr_int',
                             -values=>['10,20,40,50','5,5,20,20'],
                             -default=>$DEFAULT{'xswWnd_ctr_int'},
                             -labels=>\%labels,
                    );
print <<DONE;
<br>
&nbsp;&nbsp;&nbsp;Wind speed contour interval: 4 m/s<br>
DONE
  %labels = (
    "PosNegCB", "&nbsp;<img src=/rip/PosNegCB.jpg>&nbsp;&nbsp;&nbsp;2-Hue Colorblind Friendly (\"PosNegCB\")",
    );
   $radios = $query->radio_group(-name=>'xswWnd_color',
			     -values=>['PosNegCB'],
			     -default=>$DEFAULT{'xswWnd_color'},
			     -labels=>\%labels,
			     -linebreak=>'true',
		    );
   $radios =~ s/\&lt;/\</g;
   $radios =~ s/\&gt;/\>/g;
   $radios =~ s/\&amp;/\&/g;
   print $radios;

print <<DONE;
<p>
DONE

# xsdBZ plot definition
   print $query->checkbox(-name=>'xsdBZ_name',
                           -value=>'xsdBZ',
                           -checked=>$DEFAULT{'xsdBZ_name'},
                           -label=>'',
                    );
print <<DONE;
XS(s) of Reflectivity<br>
DONE
  %labels = (
    "dBZ3StepCust", "&nbsp;<img src=/rip/dBZ3StepCust.jpg>&nbsp;&nbsp;&nbsp;3-Hue Colorblind Friendly (\"dBZ3StepCust\")",
    "RadarCust", "&nbsp;<img src=/rip/RadarCust.jpg>&nbsp;&nbsp;&nbsp;Classic Color Scale (\"RadarCust\")"
    );
   $radios = $query->radio_group(-name=>'xsdBZ_color',
                             -values=>['dBZ3StepCust','RadarCust'],
                             -default=>$DEFAULT{'xsdBZ_color'},
                             -labels=>\%labels,
                             -linebreak=>'true',
                    );
   $radios =~ s/\&lt;/\</g;
   $radios =~ s/\&gt;/\>/g;
   $radios =~ s/\&amp;/\&/g;
   print $radios;

print <<DONE;
<p>
DONE

# xsPcp plot definition
   print $query->checkbox(-name=>'xsPcp_name',
                           -value=>'xsPcp',
                           -checked=>$DEFAULT{'xsPcp_name'},
                           -label=>'',
                    );
print <<DONE;
XS(s) of Precipitation Mixing Ratio and Vertical Air Motion<br>
DONE
  %labels = (
    "RainQv3StepCust", "&nbsp;<img src=/rip/RainQv3StepCust.jpg>&nbsp;&nbsp;&nbsp;3-Hue Colorblind Friendly (\"RainQv3StepCust\")",
    "RainQv", "&nbsp;<img src=/rip/RainQv.jpg>&nbsp;&nbsp;&nbsp;Classic Color Scale (\"RainQv\")"
    );
   $radios = $query->radio_group(-name=>'xsPcp_color',
                             -values=>['RainQv3StepCust','RainQv'],
                             -default=>$DEFAULT{'xsPcp_color'},
                             -labels=>\%labels,
                             -linebreak=>'true',
                    );
   $radios =~ s/\&lt;/\</g;
   $radios =~ s/\&gt;/\>/g;
   $radios =~ s/\&amp;/\&/g;
   print $radios;

print <<DONE;
<p>
DONE

# xsrhcv plot definition
   print $query->checkbox(-name=>'xsrhcv_name',
                           -value=>'xsrhcv',
                           -checked=>$DEFAULT{'xsrhcv_name'},
                           -label=>'',
                    );
print <<DONE;
XS(s) of RH (shaded), Pot. Temp. (contoured) and Circulation Vectors<br>
&nbsp;&nbsp;&nbsp;Domains 3 and 4 only (Mountain Wave Plot)<br>
&nbsp;&nbsp;&nbsp;Relative Humidity contour interval: 10%<br>
&nbsp;&nbsp;&nbsp;Potential Temperature contour interval: 2 K<br>
DONE

  %labels = (
    "HumidityCB", "&nbsp;<img src=/rip/HumidityCB.jpg>&nbsp;&nbsp;&nbsp;2-Hue Colorblind Friendly (\"HumidityCB\")",
    "Humidity", "&nbsp;<img src=/rip/Humidity.jpg>&nbsp;&nbsp;&nbsp;Classic Color Scale (\"Humidity\")",
    );
   $radios = $query->radio_group(-name=>'xsrhcv_color',
			     -values=>['HumidityCB','Humidity'],
			     -default=>$DEFAULT{'xsrhcv_color'},
			     -labels=>\%labels,
			     -linebreak=>'true',
		    );
   $radios =~ s/\&lt;/\</g;
   $radios =~ s/\&gt;/\>/g;
   $radios =~ s/\&amp;/\&/g;
   print $radios;

# xsrhthgm plot definition
   print $query->checkbox(-name=>'xsrhthgm_name',
                           -value=>'xsrhthgm',
                           -checked=>$DEFAULT{'xsrhthgm_name'},
                           -label=>'',
                    );
print <<DONE;
XS(s) of RH (shaded), Eqv. Pot. Temp. (contoured) and Geostrophic Momentum<br>
&nbsp;&nbsp;&nbsp;Relative Humidity contour interval: 10%<br>
&nbsp;&nbsp;&nbsp;Equivalent Potential Temperature contour interval: 2 K<br>
&nbsp;&nbsp;&nbsp;Geostrophic Momentum contour interval: 5 m/s<br>
DONE

  %labels = (
    "HumidityCB", "&nbsp;<img src=/rip/HumidityCB.jpg>&nbsp;&nbsp;&nbsp;2-Hue Colorblind Friendly (\"HumidityCB\")",
    "Humidity", "&nbsp;<img src=/rip/Humidity.jpg>&nbsp;&nbsp;&nbsp;Classic Color Scale (\"Humidity\")",
    );
   $radios = $query->radio_group(-name=>'xsrhthgm_color',
                             -values=>['HumidityCB','Humidity'],
                             -default=>$DEFAULT{'xsrhthgm_color'},
                             -labels=>\%labels,
                             -linebreak=>'true',
                    );
   $radios =~ s/\&lt;/\</g;
   $radios =~ s/\&gt;/\>/g;
   $radios =~ s/\&amp;/\&/g;
   print $radios;


print <<DONE;
<p>
<b>Cross Section Endpoints</b><br>
<i>Lats/lons should be given in degrees North, degrees East<br>
Example: 36.3N should be given as 36.3, while 106.4W should be given as -106.4</i><br><br>
Domain 1<br>
&nbsp;&nbsp;&nbsp;
DONE
   print $query->checkbox(-name=>'xs1_d1_name',
                           -value=>'xs1_d1',
                           -checked=>$DEFAULT{'xs1_d1_name'},
                           -label=>'',
                    );
print <<DONE;
Cross Section 1<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Left Lat:
DONE
   print $query->textfield(-name=>'xs1_d1_lat_lf',
                    -value=>$DEFAULT{'xs1_d1_lat_lf'},
                    -size=>8,
                    -maxlength=>15);
print <<DONE;
deg N,
&nbsp;&nbsp;&nbsp;Lon:
DONE
   print $query->textfield(-name=>'xs1_d1_lon_lf',
                    -value=>$DEFAULT{'xs1_d1_lon_lf'},
                    -size=>8,
                    -maxlength=>15);
print <<DONE;
deg E<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Right Lat:
DONE
   print $query->textfield(-name=>'xs1_d1_lat_rt',
                    -value=>$DEFAULT{'xs1_d1_lat_rt'},
                    -size=>8,
                    -maxlength=>15);
print <<DONE;
deg N,
&nbsp;&nbsp;&nbsp;Lon:
DONE
   print $query->textfield(-name=>'xs1_d1_lon_rt',
                    -value=>$DEFAULT{'xs1_d1_lon_rt'},
                    -size=>8,
                    -maxlength=>15);
print <<DONE;
deg E<br>
&nbsp;&nbsp;&nbsp;
DONE
   print $query->checkbox(-name=>'xs2_d1_name',
                           -value=>'xs2_d1',
                           -checked=>$DEFAULT{'xs2_d1_name'},
                           -label=>'',
                    );
print <<DONE;
Cross Section 2<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Left Lat:
DONE
   print $query->textfield(-name=>'xs2_d1_lat_lf',
                    -value=>$DEFAULT{'xs2_d1_lat_lf'},
                    -size=>8,
                    -maxlength=>15);
print <<DONE;
deg N,
&nbsp;&nbsp;&nbsp;Lon:
DONE
   print $query->textfield(-name=>'xs2_d1_lon_lf',
                    -value=>$DEFAULT{'xs2_d1_lon_lf'},
                    -size=>8,
                    -maxlength=>15);
print <<DONE;
deg E<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Right Lat:
DONE
   print $query->textfield(-name=>'xs2_d1_lat_rt',
                    -value=>$DEFAULT{'xs2_d1_lat_rt'},
                    -size=>8,
                    -maxlength=>15);
print <<DONE;
deg N,
&nbsp;&nbsp;&nbsp;Lon:
DONE
   print $query->textfield(-name=>'xs2_d1_lon_rt',
                    -value=>$DEFAULT{'xs2_d1_lon_rt'},
                    -size=>8,
                    -maxlength=>15);
print <<DONE;
deg E
<p>

Domain 2<br>
&nbsp;&nbsp;&nbsp;
DONE
   print $query->checkbox(-name=>'xs1_d2_name',
                           -value=>'xs1_d2',
                           -checked=>$DEFAULT{'xs1_d2_name'},
                           -label=>'',
                    );
print <<DONE;
Cross Section 1<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Left Lat:
DONE
   print $query->textfield(-name=>'xs1_d2_lat_lf',
                    -value=>$DEFAULT{'xs1_d2_lat_lf'},
                    -size=>8,
                    -maxlength=>15);
print <<DONE;
deg N,
&nbsp;&nbsp;&nbsp;Lon:
DONE
   print $query->textfield(-name=>'xs1_d2_lon_lf',
                    -value=>$DEFAULT{'xs1_d2_lon_lf'},
                    -size=>8,
                    -maxlength=>15);
print <<DONE;
deg E<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Right Lat:
DONE
   print $query->textfield(-name=>'xs1_d2_lat_rt',
                    -value=>$DEFAULT{'xs1_d2_lat_rt'},
                    -size=>8,
                    -maxlength=>15);
print <<DONE;
deg N,
&nbsp;&nbsp;&nbsp;Lon:
DONE
   print $query->textfield(-name=>'xs1_d2_lon_rt',
                    -value=>$DEFAULT{'xs1_d2_lon_rt'},
                    -size=>8,
                    -maxlength=>15);
print <<DONE;
deg E<br>
&nbsp;&nbsp;&nbsp;
DONE
   print $query->checkbox(-name=>'xs2_d2_name',
                           -value=>'xs2_d2',
                           -checked=>$DEFAULT{'xs2_d2_name'},
                           -label=>'',
                    );
print <<DONE;
Cross Section 2<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Left Lat:
DONE
   print $query->textfield(-name=>'xs2_d2_lat_lf',
                    -value=>$DEFAULT{'xs2_d2_lat_lf'},
                    -size=>8,
                    -maxlength=>15);
print <<DONE;
deg N,
&nbsp;&nbsp;&nbsp;Lon:
DONE
   print $query->textfield(-name=>'xs2_d2_lon_lf',
                    -value=>$DEFAULT{'xs2_d2_lon_lf'},
                    -size=>8,
                    -maxlength=>15);
print <<DONE;
deg E<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Right Lat:
DONE
   print $query->textfield(-name=>'xs2_d2_lat_rt',
                    -value=>$DEFAULT{'xs2_d2_lat_rt'},
                    -size=>8,
                    -maxlength=>15);
print <<DONE;
deg N,
&nbsp;&nbsp;&nbsp;Lon:
DONE
   print $query->textfield(-name=>'xs2_d2_lon_rt',
                    -value=>$DEFAULT{'xs2_d2_lon_rt'},
                    -size=>8,
                    -maxlength=>15);
print <<DONE;
deg E
<p>

Domain 3<br>
&nbsp;&nbsp;&nbsp;
DONE
   print $query->checkbox(-name=>'xs1_d3_name',
                           -value=>'xs1_d3',
                           -checked=>$DEFAULT{'xs1_d3_name'},
                           -label=>'',
                    );
print <<DONE;
Cross Section 1<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Left Lat:
DONE
   print $query->textfield(-name=>'xs1_d3_lat_lf',
                    -value=>$DEFAULT{'xs1_d3_lat_lf'},
                    -size=>8,
                    -maxlength=>15);
print <<DONE;
deg N,
&nbsp;&nbsp;&nbsp;Lon:
DONE
   print $query->textfield(-name=>'xs1_d3_lon_lf',
                    -value=>$DEFAULT{'xs1_d3_lon_lf'},
                    -size=>8,
                    -maxlength=>15);
print <<DONE;
deg E<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Right Lat:
DONE
   print $query->textfield(-name=>'xs1_d3_lat_rt',
                    -value=>$DEFAULT{'xs1_d3_lat_rt'},
                    -size=>8,
                    -maxlength=>15);
print <<DONE;
deg N,
&nbsp;&nbsp;&nbsp;Lon:
DONE
   print $query->textfield(-name=>'xs1_d3_lon_rt',
                    -value=>$DEFAULT{'xs1_d3_lon_rt'},
                    -size=>8,
                    -maxlength=>15);
print <<DONE;
deg E<br>
&nbsp;&nbsp;&nbsp;
DONE
   print $query->checkbox(-name=>'xs2_d3_name',
                           -value=>'xs2_d3',
                           -checked=>$DEFAULT{'xs2_d3_name'},
                           -label=>'',
                    );
print <<DONE;
Cross Section 2<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Left Lat:
DONE
   print $query->textfield(-name=>'xs2_d3_lat_lf',
                    -value=>$DEFAULT{'xs2_d3_lat_lf'},
                    -size=>8,
                    -maxlength=>15);
print <<DONE;
deg N,
&nbsp;&nbsp;&nbsp;Lon:
DONE
   print $query->textfield(-name=>'xs2_d3_lon_lf',
                    -value=>$DEFAULT{'xs2_d3_lon_lf'},
                    -size=>8,
                    -maxlength=>15);
print <<DONE;
deg E<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Right Lat:
DONE
   print $query->textfield(-name=>'xs2_d3_lat_rt',
                    -value=>$DEFAULT{'xs2_d3_lat_rt'},
                    -size=>8,
                    -maxlength=>15);
print <<DONE;
deg N,
&nbsp;&nbsp;&nbsp;Lon:
DONE
   print $query->textfield(-name=>'xs2_d3_lon_rt',
                    -value=>$DEFAULT{'xs2_d3_lon_rt'},
                    -size=>8,
                    -maxlength=>15);
print <<DONE;
deg E
<p>

Domain 4<br>
&nbsp;&nbsp;&nbsp;
DONE
   print $query->checkbox(-name=>'xs1_d4_name',
                           -value=>'xs1_d4',
                           -checked=>$DEFAULT{'xs1_d4_name'},
                           -label=>'',
                    );
print <<DONE;
Cross Section 1<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Left Lat:
DONE
   print $query->textfield(-name=>'xs1_d4_lat_lf',
                    -value=>$DEFAULT{'xs1_d4_lat_lf'},
                    -size=>8,
                    -maxlength=>15);
print <<DONE;
deg N,
&nbsp;&nbsp;&nbsp;Lon:
DONE
   print $query->textfield(-name=>'xs1_d4_lon_lf',
                    -value=>$DEFAULT{'xs1_d4_lon_lf'},
                    -size=>8,
                    -maxlength=>15);
print <<DONE;
deg E<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Right Lat:
DONE
   print $query->textfield(-name=>'xs1_d4_lat_rt',
                    -value=>$DEFAULT{'xs1_d4_lat_rt'},
                    -size=>8,
                    -maxlength=>15);
print <<DONE;
deg N,
&nbsp;&nbsp;&nbsp;Lon:
DONE
   print $query->textfield(-name=>'xs1_d4_lon_rt',
                    -value=>$DEFAULT{'xs1_d4_lon_rt'},
                    -size=>8,
                    -maxlength=>15);
print <<DONE;
deg E<br>
&nbsp;&nbsp;&nbsp;
DONE
   print $query->checkbox(-name=>'xs2_d4_name',
                           -value=>'xs2_d4',
                           -checked=>$DEFAULT{'xs2_d4_name'},
                           -label=>'',
                    );
print <<DONE;
Cross Section 2<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Left Lat:
DONE
   print $query->textfield(-name=>'xs2_d4_lat_lf',
                    -value=>$DEFAULT{'xs2_d4_lat_lf'},
                    -size=>8,
                    -maxlength=>15);
print <<DONE;
deg N,
&nbsp;&nbsp;&nbsp;Lon:
DONE
   print $query->textfield(-name=>'xs2_d4_lon_lf',
                    -value=>$DEFAULT{'xs2_d4_lon_lf'},
                    -size=>8,
                    -maxlength=>15);
print <<DONE;
deg E<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Right Lat:
DONE
   print $query->textfield(-name=>'xs2_d4_lat_rt',
                    -value=>$DEFAULT{'xs2_d4_lat_rt'},
                    -size=>8,
                    -maxlength=>15);
print <<DONE;
deg N,
&nbsp;&nbsp;&nbsp;Lon:
DONE
   print $query->textfield(-name=>'xs2_d4_lon_rt',
                    -value=>$DEFAULT{'xs2_d4_lon_rt'},
                    -size=>8,
                    -maxlength=>15);
print <<DONE;
deg E
<p>

<a name="sounding"><h3>Model Soundings</h3>

<i>Please provide either the 3-letter station location (e.g., DEN) or the lat/lon coordinates (e.g., 40.000,-105.000) for each sounding location<br>
Lat/lon coordinates should be given in degrees North, degrees East</i><br>
<p>
Domain1<br>
&nbsp;&nbsp;&nbsp;
DONE
   print $query->checkbox(-name=>'snd1_d1_name',
                           -value=>'snd1_d1',
                           -checked=>$DEFAULT{'snd1_d1_name'},
                           -label=>'',
                    );
print <<DONE;
Sounding 1 Location: 
DONE
   print $query->textfield(-name=>'snd1_d1_loc',
                    -value=>$DEFAULT{'snd1_d1_loc'},
                    -size=>12,
                    -maxlength=>20);
print <<DONE;
<br>&nbsp;&nbsp;&nbsp;
DONE
   print $query->checkbox(-name=>'snd2_d1_name',
                           -value=>'snd2_d1',
                           -checked=>$DEFAULT{'snd2_d1_name'},
                           -label=>'',
                    );
print <<DONE;
Sounding 2 Location: 
DONE
   print $query->textfield(-name=>'snd2_d1_loc',
                    -value=>$DEFAULT{'snd2_d1_loc'},
                    -size=>12,
                    -maxlength=>20);
print <<DONE;
<br>&nbsp;&nbsp;&nbsp;
DONE
   print $query->checkbox(-name=>'snd3_d1_name',
                           -value=>'snd3_d1',
                           -checked=>$DEFAULT{'snd3_d1_name'},
                           -label=>'',
                    );
print <<DONE;
Sounding 3 Location: 
DONE
   print $query->textfield(-name=>'snd3_d1_loc',
                    -value=>$DEFAULT{'snd3_d1_loc'},
                    -size=>12,
                    -maxlength=>20);
print <<DONE;
<br>&nbsp;&nbsp;&nbsp;
DONE
   print $query->checkbox(-name=>'snd4_d1_name',
                           -value=>'snd4_d1',
                           -checked=>$DEFAULT{'snd4_d1_name'},
                           -label=>'',
                    );
print <<DONE;
Sounding 4 Location: 
DONE
   print $query->textfield(-name=>'snd4_d1_loc',
                    -value=>$DEFAULT{'snd4_d1_loc'},
                    -size=>12,
                    -maxlength=>20);
print <<DONE;
<br>&nbsp;&nbsp;&nbsp;
DONE
   print $query->checkbox(-name=>'snd5_d1_name',
                           -value=>'snd5_d1',
                           -checked=>$DEFAULT{'snd5_d1_name'},
                           -label=>'',
                    );
print <<DONE;
Sounding 5 Location: 
DONE
   print $query->textfield(-name=>'snd5_d1_loc',
                    -value=>$DEFAULT{'snd5_d1_loc'},
                    -size=>12,
                    -maxlength=>20);

print <<DONE;
<p>
Domain2<br>
&nbsp;&nbsp;&nbsp;
DONE
   print $query->checkbox(-name=>'snd1_d2_name',
                           -value=>'snd1_d2',
                           -checked=>$DEFAULT{'snd1_d2_name'},
                           -label=>'',
                    );
print <<DONE;
Sounding 1 Location: 
DONE
   print $query->textfield(-name=>'snd1_d2_loc',
                    -value=>$DEFAULT{'snd1_d2_loc'},
                    -size=>12,
                    -maxlength=>20);
print <<DONE;
<br>&nbsp;&nbsp;&nbsp;
DONE
   print $query->checkbox(-name=>'snd2_d2_name',
                           -value=>'snd2_d2',
                           -checked=>$DEFAULT{'snd2_d2_name'},
                           -label=>'',
                    );
print <<DONE;
Sounding 2 Location: 
DONE
   print $query->textfield(-name=>'snd2_d2_loc',
                    -value=>$DEFAULT{'snd2_d2_loc'},
                    -size=>12,
                    -maxlength=>20);
print <<DONE;
<br>&nbsp;&nbsp;&nbsp;
DONE
   print $query->checkbox(-name=>'snd3_d2_name',
                           -value=>'snd3_d2',
                           -checked=>$DEFAULT{'snd3_d2_name'},
                           -label=>'',
                    );
print <<DONE;
Sounding 3 Location: 
DONE
   print $query->textfield(-name=>'snd3_d2_loc',
                    -value=>$DEFAULT{'snd3_d2_loc'},
                    -size=>12,
                    -maxlength=>20);
print <<DONE;
<br>&nbsp;&nbsp;&nbsp;
DONE
   print $query->checkbox(-name=>'snd4_d2_name',
                           -value=>'snd4_d2',
                           -checked=>$DEFAULT{'snd4_d2_name'},
                           -label=>'',
                    );
print <<DONE;
Sounding 4 Location: 
DONE
   print $query->textfield(-name=>'snd4_d2_loc',
                    -value=>$DEFAULT{'snd4_d2_loc'},
                    -size=>12,
                    -maxlength=>20);
print <<DONE;
<br>&nbsp;&nbsp;&nbsp;
DONE
   print $query->checkbox(-name=>'snd5_d2_name',
                           -value=>'snd5_d2',
                           -checked=>$DEFAULT{'snd5_d2_name'},
                           -label=>'',
                    );
print <<DONE;
Sounding 5 Location: 
DONE
   print $query->textfield(-name=>'snd5_d2_loc',
                    -value=>$DEFAULT{'snd5_d2_loc'},
                    -size=>12,
                    -maxlength=>20);

print <<DONE;
<p>
Domain3<br>
&nbsp;&nbsp;&nbsp;
DONE
   print $query->checkbox(-name=>'snd1_d3_name',
                           -value=>'snd1_d3',
                           -checked=>$DEFAULT{'snd1_d3_name'},
                           -label=>'',
                    );
print <<DONE;
Sounding 1 Location: 
DONE
   print $query->textfield(-name=>'snd1_d3_loc',
                    -value=>$DEFAULT{'snd1_d3_loc'},
                    -size=>12,
                    -maxlength=>20);
print <<DONE;
<br>&nbsp;&nbsp;&nbsp;
DONE
   print $query->checkbox(-name=>'snd2_d3_name',
                           -value=>'snd2_d3',
                           -checked=>$DEFAULT{'snd2_d3_name'},
                           -label=>'',
                    );
print <<DONE;
Sounding 2 Location: 
DONE
   print $query->textfield(-name=>'snd2_d3_loc',
                    -value=>$DEFAULT{'snd2_d3_loc'},
                    -size=>12,
                    -maxlength=>20);
print <<DONE;
<br>&nbsp;&nbsp;&nbsp;
DONE
   print $query->checkbox(-name=>'snd3_d3_name',
                           -value=>'snd3_d3',
                           -checked=>$DEFAULT{'snd3_d3_name'},
                           -label=>'',
                    );
print <<DONE;
Sounding 3 Location: 
DONE
   print $query->textfield(-name=>'snd3_d3_loc',
                    -value=>$DEFAULT{'snd3_d3_loc'},
                    -size=>12,
                    -maxlength=>20);
print <<DONE;
<br>&nbsp;&nbsp;&nbsp;
DONE
   print $query->checkbox(-name=>'snd4_d3_name',
                           -value=>'snd4_d3',
                           -checked=>$DEFAULT{'snd4_d3_name'},
                           -label=>'',
                    );
print <<DONE;
Sounding 4 Location: 
DONE
   print $query->textfield(-name=>'snd4_d3_loc',
                    -value=>$DEFAULT{'snd4_d3_loc'},
                    -size=>12,
                    -maxlength=>20);
print <<DONE;
<br>&nbsp;&nbsp;&nbsp;
DONE
   print $query->checkbox(-name=>'snd5_d3_name',
                           -value=>'snd5_d3',
                           -checked=>$DEFAULT{'snd5_d3_name'},
                           -label=>'',
                    );
print <<DONE;
Sounding 5 Location: 
DONE
   print $query->textfield(-name=>'snd5_d3_loc',
                    -value=>$DEFAULT{'snd5_d3_loc'},
                    -size=>12,
                    -maxlength=>20);
print <<DONE;
<br>&nbsp;&nbsp;&nbsp;
DONE
   print $query->checkbox(-name=>'snd6_d3_name',
                           -value=>'snd6_d3',
                           -checked=>$DEFAULT{'snd6_d3_name'},
                           -label=>'',
                    );
print <<DONE;
Sounding 6 Location:
DONE
   print $query->textfield(-name=>'snd6_d3_loc',
                    -value=>$DEFAULT{'snd6_d3_loc'},
                    -size=>12,
                    -maxlength=>20);
print <<DONE;
<br>&nbsp;&nbsp;&nbsp;
DONE
   print $query->checkbox(-name=>'snd7_d3_name',
                           -value=>'snd7_d3',
                           -checked=>$DEFAULT{'snd7_d3_name'},
                           -label=>'',
                    );
print <<DONE;
Sounding 7 Location:
DONE
   print $query->textfield(-name=>'snd7_d3_loc',
                    -value=>$DEFAULT{'snd7_d3_loc'},
                    -size=>12,
                    -maxlength=>20);
print <<DONE;
<br>&nbsp;&nbsp;&nbsp;
DONE
   print $query->checkbox(-name=>'snd8_d3_name',
                           -value=>'snd8_d3',
                           -checked=>$DEFAULT{'snd8_d3_name'},
                           -label=>'',
                    );
print <<DONE;
Sounding 8 Location:
DONE
   print $query->textfield(-name=>'snd8_d3_loc',
                    -value=>$DEFAULT{'snd8_d3_loc'},
                    -size=>12,
                    -maxlength=>20);
print <<DONE;
<br>&nbsp;&nbsp;&nbsp;
DONE
   print $query->checkbox(-name=>'snd9_d3_name',
                           -value=>'snd9_d3',
                           -checked=>$DEFAULT{'snd9_d3_name'},
                           -label=>'',
                    );
print <<DONE;
Sounding 9 Location:
DONE
   print $query->textfield(-name=>'snd9_d3_loc',
                    -value=>$DEFAULT{'snd9_d3_loc'},
                    -size=>12,
                    -maxlength=>20);
print <<DONE;
<br>&nbsp;&nbsp;&nbsp;
DONE
   print $query->checkbox(-name=>'snd10_d3_name',
                           -value=>'snd10_d3',
                           -checked=>$DEFAULT{'snd10_d3_name'},
                           -label=>'',
                    );
print <<DONE;
Sounding 10 Location:
DONE
   print $query->textfield(-name=>'snd10_d3_loc',
                    -value=>$DEFAULT{'snd10_d3_loc'},
                    -size=>12,
                    -maxlength=>20);

print <<DONE;
<p>
Domain4<br>
&nbsp;&nbsp;&nbsp;
DONE
   print $query->checkbox(-name=>'snd1_d4_name',
                           -value=>'snd1_d4',
                           -checked=>$DEFAULT{'snd1_d4_name'},
                           -label=>'',
                    );
print <<DONE;
Sounding 1 Location: 
DONE
   print $query->textfield(-name=>'snd1_d4_loc',
                    -value=>$DEFAULT{'snd1_d4_loc'},
                    -size=>12,
                    -maxlength=>20);
print <<DONE;
<br>&nbsp;&nbsp;&nbsp;
DONE
   print $query->checkbox(-name=>'snd2_d4_name',
                           -value=>'snd2_d4',
                           -checked=>$DEFAULT{'snd2_d4_name'},
                           -label=>'',
                    );
print <<DONE;
Sounding 2 Location: 
DONE
   print $query->textfield(-name=>'snd2_d4_loc',
                    -value=>$DEFAULT{'snd2_d4_loc'},
                    -size=>12,
                    -maxlength=>20);
print <<DONE;
<br>&nbsp;&nbsp;&nbsp;
DONE
   print $query->checkbox(-name=>'snd3_d4_name',
                           -value=>'snd3_d4',
                           -checked=>$DEFAULT{'snd3_d4_name'},
                           -label=>'',
                    );
print <<DONE;
Sounding 3 Location: 
DONE
   print $query->textfield(-name=>'snd3_d4_loc',
                    -value=>$DEFAULT{'snd3_d4_loc'},
                    -size=>12,
                    -maxlength=>20);
print <<DONE;
<br>&nbsp;&nbsp;&nbsp;
DONE
   print $query->checkbox(-name=>'snd4_d4_name',
                           -value=>'snd4_d4',
                           -checked=>$DEFAULT{'snd4_d4_name'},
                           -label=>'',
                    );
print <<DONE;
Sounding 4 Location: 
DONE
   print $query->textfield(-name=>'snd4_d4_loc',
                    -value=>$DEFAULT{'snd4_d4_loc'},
                    -size=>12,
                    -maxlength=>20);
print <<DONE;
<br>&nbsp;&nbsp;&nbsp;
DONE
   print $query->checkbox(-name=>'snd5_d4_name',
                           -value=>'snd5_d4',
                           -checked=>$DEFAULT{'snd5_d4_name'},
                           -label=>'',
                    );
print <<DONE;
Sounding 5 Location: 
DONE
   print $query->textfield(-name=>'snd5_d4_loc',
                    -value=>$DEFAULT{'snd5_d4_loc'},
                    -size=>12,
                    -maxlength=>20);
print <<DONE;
<br>&nbsp;&nbsp;&nbsp;
DONE
   print $query->checkbox(-name=>'snd6_d4_name',
                           -value=>'snd6_d4',
                           -checked=>$DEFAULT{'snd6_d4_name'},
                           -label=>'',
                    );
print <<DONE;
Sounding 6 Location: 
DONE
   print $query->textfield(-name=>'snd6_d4_loc',
                    -value=>$DEFAULT{'snd6_d4_loc'},
                    -size=>12,
                    -maxlength=>20);
print <<DONE;
<br>&nbsp;&nbsp;&nbsp;
DONE
   print $query->checkbox(-name=>'snd7_d4_name',
                           -value=>'snd7_d4',
                           -checked=>$DEFAULT{'snd7_d4_name'},
                           -label=>'',
                    );
print <<DONE;
Sounding 7 Location: 
DONE
   print $query->textfield(-name=>'snd7_d4_loc',
                    -value=>$DEFAULT{'snd7_d4_loc'},
                    -size=>12,
                    -maxlength=>20);
print <<DONE;
<br>&nbsp;&nbsp;&nbsp;
DONE
   print $query->checkbox(-name=>'snd8_d4_name',
                           -value=>'snd8_d4',
                           -checked=>$DEFAULT{'snd8_d4_name'},
                           -label=>'',
                    );
print <<DONE;
Sounding 8 Location: 
DONE
   print $query->textfield(-name=>'snd8_d4_loc',
                    -value=>$DEFAULT{'snd8_d4_loc'},
                    -size=>12,
                    -maxlength=>20);
print <<DONE;
<br>&nbsp;&nbsp;&nbsp;
DONE
   print $query->checkbox(-name=>'snd9_d4_name',
                           -value=>'snd9_d4',
                           -checked=>$DEFAULT{'snd9_d4_name'},
                           -label=>'',
                    );
print <<DONE;
Sounding 9 Location: 
DONE
   print $query->textfield(-name=>'snd9_d4_loc',
                    -value=>$DEFAULT{'snd9_d4_loc'},
                    -size=>12,
                    -maxlength=>20);
print <<DONE;
<br>&nbsp;&nbsp;&nbsp;
DONE
   print $query->checkbox(-name=>'snd10_d4_name',
                           -value=>'snd10_d4',
                           -checked=>$DEFAULT{'snd10_d4_name'},
                           -label=>'',
                    );
print <<DONE;
Sounding 10 Location: 
DONE
   print $query->textfield(-name=>'snd10_d4_loc',
                    -value=>$DEFAULT{'snd10_d4_loc'},
                    -size=>12,
                    -maxlength=>20);

print <<DONE;
<p>

<a name="overlays"><h3>Overlays</h3>

<b>Provide the range outlines on which domains?</b><br>
&nbsp;&nbsp;&nbsp;
DONE
   print $query->checkbox(-name=>'range_overlay_d1',
                           -value=>'1',
                           -checked=>$DEFAULT{'range_overlay_d1'},
                           -label=>'D01',
                    );
print <<DONE;
&nbsp;&nbsp;&nbsp;
DONE
   print $query->checkbox(-name=>'range_overlay_d2',
                           -value=>'2',
                           -checked=>$DEFAULT{'range_overlay_d2'},
                           -label=>'D02',
                    );
print <<DONE;
&nbsp;&nbsp;&nbsp;
DONE
   print $query->checkbox(-name=>'range_overlay_d3',
                           -value=>'3',
                           -checked=>$DEFAULT{'range_overlay_d3'},
                           -label=>'D03',
                    );
print <<DONE;
&nbsp;&nbsp;&nbsp;
DONE
   print $query->checkbox(-name=>'range_overlay_d4',
                           -value=>'4',
                           -checked=>$DEFAULT{'range_overlay_d4'},
                           -label=>'D04',
                    );
print <<DONE;
<p>

<b>Provide the county boundaries on which domains?</b><br>
&nbsp;&nbsp;&nbsp;
DONE
   print $query->checkbox(-name=>'county_overlay_d1',
                           -value=>'1',
                           -checked=>$DEFAULT{'county_overlay_d1'},
                           -label=>'D01',
                    );
print <<DONE;
&nbsp;&nbsp;&nbsp;
DONE
   print $query->checkbox(-name=>'county_overlay_d2',
                           -value=>'2',
                           -checked=>$DEFAULT{'county_overlay_d2'},
                           -label=>'D02',
                    );
print <<DONE;
&nbsp;&nbsp;&nbsp;
DONE
   print $query->checkbox(-name=>'county_overlay_d3',
                           -value=>'3',
                           -checked=>$DEFAULT{'county_overlay_d3'},
                           -label=>'D03',
                    );
print <<DONE;
&nbsp;&nbsp;&nbsp;
DONE
   print $query->checkbox(-name=>'county_overlay_d4',
                           -value=>'4',
                           -checked=>$DEFAULT{'county_overlay_d4'},
                           -label=>'D04',
                    );

print <<DONE;
<p>
<b>Use which political boundary dataset?</b><br>
&nbsp;&nbsp;&nbsp;Domain 3
DONE
  %labels = (
    "PS", "PS",
    "RG2", "RG2",
    );

   print $query->radio_group(-name=>'poli_bdry_d3',
                             -values=>['PS','RG2'],
                             -default=>$DEFAULT{'poli_bdry_d3'},
                             -labels=>\%labels,
                    );
print <<DONE;
<br>&nbsp;&nbsp;&nbsp;Domain 4
DONE
  %labels = (
    "PS", "PS",
    "RG2", "RG2",
    );

   print $query->radio_group(-name=>'poli_bdry_d4',
                             -values=>['PS','RG2'],
                             -default=>$DEFAULT{'poli_bdry_d4'},
                             -labels=>\%labels,
                    );


print <<DONE;
<p>

<b>Mark the following locations on each map</b><br>
<i>Lats/lons should be given in degrees North, degrees East<br>
Example: 36.3N, 106.4W should be given as 36.3,-106.4<br>
Note: Use an * to only plot the station without a label</i><br>
<table border=1>
<tr>
<td>Location</td>
<td>Label</td>
<td>Lat,Lon</td>
<td>D1</td>
<td>D2</td>
<td>D3</td>
<td>D4</td>
</tr><tr>
<td>1</td>
<td>
DONE
   print $query->textfield(-name=>'sta1_mark_name',
                    -value=>$DEFAULT{'sta1_mark_name'},
                    -size=>8,
                    -maxlength=>12);
print <<DONE;
</td><td>
DONE
   print $query->textfield(-name=>'sta1_mark_loc',
                    -value=>$DEFAULT{'sta1_mark_loc'},
                    -size=>12,
                    -maxlength=>20);
print <<DONE;
</td><td>
DONE
   print $query->checkbox(-name=>'sta1_mark_d1',
                           -value=>'1',
                           -checked=>$DEFAULT{'sta1_mark_d1'},
                           -label=>'',
                    );
print <<DONE;
</td><td>
DONE
   print $query->checkbox(-name=>'sta1_mark_d2',
                           -value=>'2',
                           -checked=>$DEFAULT{'sta1_mark_d2'},
                           -label=>'',
                    );
print <<DONE;
</td><td>
DONE
   print $query->checkbox(-name=>'sta1_mark_d3',
                           -value=>'3',
                           -checked=>$DEFAULT{'sta1_mark_d3'},
                           -label=>'',
                    );
print <<DONE;
</td><td>
DONE
   print $query->checkbox(-name=>'sta1_mark_d4',
                           -value=>'4',
                           -checked=>$DEFAULT{'sta1_mark_d4'},
                           -label=>'',
                    );

print <<DONE;
</tr><tr>
<td>2</td>
<td>
DONE
   print $query->textfield(-name=>'sta2_mark_name',
                    -value=>$DEFAULT{'sta2_mark_name'},
                    -size=>8,
                    -maxlength=>12);
print <<DONE;
</td><td>
DONE
   print $query->textfield(-name=>'sta2_mark_loc',
                    -value=>$DEFAULT{'sta2_mark_loc'},
                    -size=>12,
                    -maxlength=>20);
print <<DONE;
</td><td>
DONE
   print $query->checkbox(-name=>'sta2_mark_d1',
                           -value=>'1',
                           -checked=>$DEFAULT{'sta2_mark_d1'},
                           -label=>'',
                    );
print <<DONE;
</td><td>
DONE
   print $query->checkbox(-name=>'sta2_mark_d2',
                           -value=>'2',
                           -checked=>$DEFAULT{'sta2_mark_d2'},
                           -label=>'',
                    );
print <<DONE;
</td><td>
DONE
   print $query->checkbox(-name=>'sta2_mark_d3',
                           -value=>'3',
                           -checked=>$DEFAULT{'sta2_mark_d3'},
                           -label=>'',
                    );
print <<DONE;
</td><td>
DONE
   print $query->checkbox(-name=>'sta2_mark_d4',
                           -value=>'4',
                           -checked=>$DEFAULT{'sta2_mark_d4'},
                           -label=>'',
                    );

print <<DONE;
</tr><tr>
<td>3</td>
<td>
DONE
   print $query->textfield(-name=>'sta3_mark_name',
                    -value=>$DEFAULT{'sta3_mark_name'},
                    -size=>8,
                    -maxlength=>12);
print <<DONE;
</td><td>
DONE
   print $query->textfield(-name=>'sta3_mark_loc',
                    -value=>$DEFAULT{'sta3_mark_loc'},
                    -size=>12,
                    -maxlength=>20);
print <<DONE;
</td><td>
DONE
   print $query->checkbox(-name=>'sta3_mark_d1',
                           -value=>'1',
                           -checked=>$DEFAULT{'sta3_mark_d1'},
                           -label=>'',
                    );
print <<DONE;
</td><td>
DONE
   print $query->checkbox(-name=>'sta3_mark_d2',
                           -value=>'2',
                           -checked=>$DEFAULT{'sta3_mark_d2'},
                           -label=>'',
                    );
print <<DONE;
</td><td>
DONE
   print $query->checkbox(-name=>'sta3_mark_d3',
                           -value=>'3',
                           -checked=>$DEFAULT{'sta3_mark_d3'},
                           -label=>'',
                    );
print <<DONE;
</td><td>
DONE
   print $query->checkbox(-name=>'sta3_mark_d4',
                           -value=>'4',
                           -checked=>$DEFAULT{'sta3_mark_d4'},
                           -label=>'',
                    );

print <<DONE;
</tr><tr>
<td>4</td>
<td>
DONE
   print $query->textfield(-name=>'sta4_mark_name',
                    -value=>$DEFAULT{'sta4_mark_name'},
                    -size=>8,
                    -maxlength=>12);
print <<DONE;
</td><td>
DONE
   print $query->textfield(-name=>'sta4_mark_loc',
                    -value=>$DEFAULT{'sta4_mark_loc'},
                    -size=>12,
                    -maxlength=>20);
print <<DONE;
</td><td>
DONE
   print $query->checkbox(-name=>'sta4_mark_d1',
                           -value=>'1',
                           -checked=>$DEFAULT{'sta4_mark_d1'},
                           -label=>'',
                    );
print <<DONE;
</td><td>
DONE
   print $query->checkbox(-name=>'sta4_mark_d2',
                           -value=>'2',
                           -checked=>$DEFAULT{'sta4_mark_d2'},
                           -label=>'',
                    );
print <<DONE;
</td><td>
DONE
   print $query->checkbox(-name=>'sta4_mark_d3',
                           -value=>'3',
                           -checked=>$DEFAULT{'sta4_mark_d3'},
                           -label=>'',
                    );
print <<DONE;
</td><td>
DONE
   print $query->checkbox(-name=>'sta4_mark_d4',
                           -value=>'4',
                           -checked=>$DEFAULT{'sta4_mark_d4'},
                           -label=>'',
                    );

print <<DONE;
</tr><tr>
<td>5</td>
<td>
DONE
   print $query->textfield(-name=>'sta5_mark_name',
                    -value=>$DEFAULT{'sta5_mark_name'},
                    -size=>8,
                    -maxlength=>12);
print <<DONE;
</td><td>
DONE
   print $query->textfield(-name=>'sta5_mark_loc',
                    -value=>$DEFAULT{'sta5_mark_loc'},
                    -size=>12,
                    -maxlength=>20);
print <<DONE;
</td><td>
DONE
   print $query->checkbox(-name=>'sta5_mark_d1',
                           -value=>'1',
                           -checked=>$DEFAULT{'sta5_mark_d1'},
                           -label=>'',
                    );
print <<DONE;
</td><td>
DONE
   print $query->checkbox(-name=>'sta5_mark_d2',
                           -value=>'2',
                           -checked=>$DEFAULT{'sta5_mark_d2'},
                           -label=>'',
                    );
print <<DONE;
</td><td>
DONE
   print $query->checkbox(-name=>'sta5_mark_d3',
                           -value=>'3',
                           -checked=>$DEFAULT{'sta5_mark_d3'},
                           -label=>'',
                    );
print <<DONE;
</td><td>
DONE
   print $query->checkbox(-name=>'sta5_mark_d4',
                           -value=>'4',
                           -checked=>$DEFAULT{'sta5_mark_d4'},
                           -label=>'',
                    );

print <<DONE;
</tr><tr>
<td>6</td>
<td>
DONE
   print $query->textfield(-name=>'sta6_mark_name',
                    -value=>$DEFAULT{'sta6_mark_name'},
                    -size=>8,
                    -maxlength=>12);
print <<DONE;
</td><td>
DONE
   print $query->textfield(-name=>'sta6_mark_loc',
                    -value=>$DEFAULT{'sta6_mark_loc'},
                    -size=>12,
                    -maxlength=>20);
print <<DONE;
</td><td>
DONE
   print $query->checkbox(-name=>'sta6_mark_d1',
                           -value=>'1',
                           -checked=>$DEFAULT{'sta6_mark_d1'},
                           -label=>'',
                    );
print <<DONE;
</td><td>
DONE
   print $query->checkbox(-name=>'sta6_mark_d2',
                           -value=>'2',
                           -checked=>$DEFAULT{'sta6_mark_d2'},
                           -label=>'',
                    );
print <<DONE;
</td><td>
DONE
   print $query->checkbox(-name=>'sta6_mark_d3',
                           -value=>'3',
                           -checked=>$DEFAULT{'sta6_mark_d3'},
                           -label=>'',
                    );
print <<DONE;
</td><td>
DONE
   print $query->checkbox(-name=>'sta6_mark_d4',
                           -value=>'4',
                           -checked=>$DEFAULT{'sta6_mark_d4'},
                           -label=>'',
                    );

print <<DONE;
</tr><tr>
<td>7</td>
<td>
DONE
   print $query->textfield(-name=>'sta7_mark_name',
                    -value=>$DEFAULT{'sta7_mark_name'},
                    -size=>8,
                    -maxlength=>12);
print <<DONE;
</td><td>
DONE
   print $query->textfield(-name=>'sta7_mark_loc',
                    -value=>$DEFAULT{'sta7_mark_loc'},
                    -size=>12,
                    -maxlength=>20);
print <<DONE;
</td><td>
DONE
   print $query->checkbox(-name=>'sta7_mark_d1',
                           -value=>'1',
                           -checked=>$DEFAULT{'sta7_mark_d1'},
                           -label=>'',
                    );
print <<DONE;
</td><td>
DONE
   print $query->checkbox(-name=>'sta7_mark_d2',
                           -value=>'2',
                           -checked=>$DEFAULT{'sta7_mark_d2'},
                           -label=>'',
                    );
print <<DONE;
</td><td>
DONE
   print $query->checkbox(-name=>'sta7_mark_d3',
                           -value=>'3',
                           -checked=>$DEFAULT{'sta7_mark_d3'},
                           -label=>'',
                    );
print <<DONE;
</td><td>
DONE
   print $query->checkbox(-name=>'sta7_mark_d4',
                           -value=>'4',
                           -checked=>$DEFAULT{'sta7_mark_d4'},
                           -label=>'',
                    );

print <<DONE;
</tr><tr>
<td>8</td>
<td>
DONE
   print $query->textfield(-name=>'sta8_mark_name',
                    -value=>$DEFAULT{'sta8_mark_name'},
                    -size=>8,
                    -maxlength=>12);
print <<DONE;
</td><td>
DONE
   print $query->textfield(-name=>'sta8_mark_loc',
                    -value=>$DEFAULT{'sta8_mark_loc'},
                    -size=>12,
                    -maxlength=>20);
print <<DONE;
</td><td>
DONE
   print $query->checkbox(-name=>'sta8_mark_d1',
                           -value=>'1',
                           -checked=>$DEFAULT{'sta8_mark_d1'},
                           -label=>'',
                    );
print <<DONE;
</td><td>
DONE
   print $query->checkbox(-name=>'sta8_mark_d2',
                           -value=>'2',
                           -checked=>$DEFAULT{'sta8_mark_d2'},
                           -label=>'',
                    );
print <<DONE;
</td><td>
DONE
   print $query->checkbox(-name=>'sta8_mark_d3',
                           -value=>'3',
                           -checked=>$DEFAULT{'sta8_mark_d3'},
                           -label=>'',
                    );
print <<DONE;
</td><td>
DONE
   print $query->checkbox(-name=>'sta8_mark_d4',
                           -value=>'4',
                           -checked=>$DEFAULT{'sta8_mark_d4'},
                           -label=>'',
                    );

print <<DONE;
</tr><tr>
<td>9</td>
<td>
DONE
   print $query->textfield(-name=>'sta9_mark_name',
                    -value=>$DEFAULT{'sta9_mark_name'},
                    -size=>8,
                    -maxlength=>12);
print <<DONE;
</td><td>
DONE
   print $query->textfield(-name=>'sta9_mark_loc',
                    -value=>$DEFAULT{'sta9_mark_loc'},
                    -size=>12,
                    -maxlength=>20);
print <<DONE;
</td><td>
DONE
   print $query->checkbox(-name=>'sta9_mark_d1',
                           -value=>'1',
                           -checked=>$DEFAULT{'sta9_mark_d1'},
                           -label=>'',
                    );
print <<DONE;
</td><td>
DONE
   print $query->checkbox(-name=>'sta9_mark_d2',
                           -value=>'2',
                           -checked=>$DEFAULT{'sta9_mark_d2'},
                           -label=>'',
                    );
print <<DONE;
</td><td>
DONE
   print $query->checkbox(-name=>'sta9_mark_d3',
                           -value=>'3',
                           -checked=>$DEFAULT{'sta9_mark_d3'},
                           -label=>'',
                    );
print <<DONE;
</td><td>
DONE
   print $query->checkbox(-name=>'sta9_mark_d4',
                           -value=>'4',
                           -checked=>$DEFAULT{'sta9_mark_d4'},
                           -label=>'',
                    );

print <<DONE;
</tr><tr>
<td>10</td>
<td>
DONE
   print $query->textfield(-name=>'sta10_mark_name',
                    -value=>$DEFAULT{'sta10_mark_name'},
                    -size=>8,
                    -maxlength=>12);
print <<DONE;
</td><td>
DONE
   print $query->textfield(-name=>'sta10_mark_loc',
                    -value=>$DEFAULT{'sta10_mark_loc'},
                    -size=>12,
                    -maxlength=>20);
print <<DONE;
</td><td>
DONE
   print $query->checkbox(-name=>'sta10_mark_d1',
                           -value=>'1',
                           -checked=>$DEFAULT{'sta10_mark_d1'},
                           -label=>'',
                    );
print <<DONE;
</td><td>
DONE
   print $query->checkbox(-name=>'sta10_mark_d2',
                           -value=>'2',
                           -checked=>$DEFAULT{'sta10_mark_d2'},
                           -label=>'',
                    );
print <<DONE;
</td><td>
DONE
   print $query->checkbox(-name=>'sta10_mark_d3',
                           -value=>'3',
                           -checked=>$DEFAULT{'sta10_mark_d3'},
                           -label=>'',
                    );
print <<DONE;
</td><td>
DONE
   print $query->checkbox(-name=>'sta10_mark_d4',
                           -value=>'4',
                           -checked=>$DEFAULT{'sta10_mark_d4'},
                           -label=>'',
                    );

print <<DONE;
</tr><tr>
<td>11</td>
<td>
DONE
   print $query->textfield(-name=>'sta11_mark_name',
                    -value=>$DEFAULT{'sta11_mark_name'},
                    -size=>8,
                    -maxlength=>12);
print <<DONE;
</td><td>
DONE
   print $query->textfield(-name=>'sta11_mark_loc',
                    -value=>$DEFAULT{'sta11_mark_loc'},
                    -size=>12,
                    -maxlength=>20);
print <<DONE;
</td><td>
DONE
   print $query->checkbox(-name=>'sta11_mark_d1',
                           -value=>'1',
                           -checked=>$DEFAULT{'sta11_mark_d1'},
                           -label=>'',
                    );
print <<DONE;
</td><td>
DONE
   print $query->checkbox(-name=>'sta11_mark_d2',
                           -value=>'2',
                           -checked=>$DEFAULT{'sta11_mark_d2'},
                           -label=>'',
                    );
print <<DONE;
</td><td>
DONE
   print $query->checkbox(-name=>'sta11_mark_d3',
                           -value=>'3',
                           -checked=>$DEFAULT{'sta11_mark_d3'},
                           -label=>'',
                    );
print <<DONE;
</td><td>
DONE
   print $query->checkbox(-name=>'sta11_mark_d4',
                           -value=>'4',
                           -checked=>$DEFAULT{'sta11_mark_d4'},
                           -label=>'',
                    );

print <<DONE;
</tr><tr>
<td>12</td>
<td>
DONE
   print $query->textfield(-name=>'sta12_mark_name',
                    -value=>$DEFAULT{'sta12_mark_name'},
                    -size=>8,
                    -maxlength=>12);
print <<DONE;
</td><td>
DONE
   print $query->textfield(-name=>'sta12_mark_loc',
                    -value=>$DEFAULT{'sta12_mark_loc'},
                    -size=>12,
                    -maxlength=>20);
print <<DONE;
</td><td>
DONE
   print $query->checkbox(-name=>'sta12_mark_d1',
                           -value=>'1',
                           -checked=>$DEFAULT{'sta12_mark_d1'},
                           -label=>'',
                    );
print <<DONE;
</td><td>
DONE
   print $query->checkbox(-name=>'sta12_mark_d2',
                           -value=>'2',
                           -checked=>$DEFAULT{'sta12_mark_d2'},
                           -label=>'',
                    );
print <<DONE;
</td><td>
DONE
   print $query->checkbox(-name=>'sta12_mark_d3',
                           -value=>'3',
                           -checked=>$DEFAULT{'sta12_mark_d3'},
                           -label=>'',
                    );
print <<DONE;
</td><td>
DONE
   print $query->checkbox(-name=>'sta12_mark_d4',
                           -value=>'4',
                           -checked=>$DEFAULT{'sta12_mark_d4'},
                           -label=>'',
                    );

print <<DONE;
</tr><tr>
<td>13</td>
<td>
DONE
   print $query->textfield(-name=>'sta13_mark_name',
                    -value=>$DEFAULT{'sta13_mark_name'},
                    -size=>8,
                    -maxlength=>12);
print <<DONE;
</td><td>
DONE
   print $query->textfield(-name=>'sta13_mark_loc',
                    -value=>$DEFAULT{'sta13_mark_loc'},
                    -size=>12,
                    -maxlength=>20);
print <<DONE;
</td><td>
DONE
   print $query->checkbox(-name=>'sta13_mark_d1',
                           -value=>'1',
                           -checked=>$DEFAULT{'sta13_mark_d1'},
                           -label=>'',
                    );
print <<DONE;
</td><td>
DONE
   print $query->checkbox(-name=>'sta13_mark_d2',
                           -value=>'2',
                           -checked=>$DEFAULT{'sta13_mark_d2'},
                           -label=>'',
                    );
print <<DONE;
</td><td>
DONE
   print $query->checkbox(-name=>'sta13_mark_d3',
                           -value=>'3',
                           -checked=>$DEFAULT{'sta13_mark_d3'},
                           -label=>'',
                    );
print <<DONE;
</td><td>
DONE
   print $query->checkbox(-name=>'sta13_mark_d4',
                           -value=>'4',
                           -checked=>$DEFAULT{'sta13_mark_d4'},
                           -label=>'',
                    );

print <<DONE;
</tr><tr>
<td>14</td>
<td>
DONE
   print $query->textfield(-name=>'sta14_mark_name',
                    -value=>$DEFAULT{'sta14_mark_name'},
                    -size=>8,
                    -maxlength=>12);
print <<DONE;
</td><td>
DONE
   print $query->textfield(-name=>'sta14_mark_loc',
                    -value=>$DEFAULT{'sta14_mark_loc'},
                    -size=>12,
                    -maxlength=>20);
print <<DONE;
</td><td>
DONE
   print $query->checkbox(-name=>'sta14_mark_d1',
                           -value=>'1',
                           -checked=>$DEFAULT{'sta14_mark_d1'},
                           -label=>'',
                    );
print <<DONE;
</td><td>
DONE
   print $query->checkbox(-name=>'sta14_mark_d2',
                           -value=>'2',
                           -checked=>$DEFAULT{'sta14_mark_d2'},
                           -label=>'',
                    );
print <<DONE;
</td><td>
DONE
   print $query->checkbox(-name=>'sta14_mark_d3',
                           -value=>'3',
                           -checked=>$DEFAULT{'sta14_mark_d3'},
                           -label=>'',
                    );
print <<DONE;
</td><td>
DONE
   print $query->checkbox(-name=>'sta14_mark_d4',
                           -value=>'4',
                           -checked=>$DEFAULT{'sta14_mark_d4'},
                           -label=>'',
                    );

print <<DONE;
</tr><tr>
<td>15</td>
<td>
DONE
   print $query->textfield(-name=>'sta15_mark_name',
                    -value=>$DEFAULT{'sta15_mark_name'},
                    -size=>8,
                    -maxlength=>12);
print <<DONE;
</td><td>
DONE
   print $query->textfield(-name=>'sta15_mark_loc',
                    -value=>$DEFAULT{'sta15_mark_loc'},
                    -size=>12,
                    -maxlength=>20);
print <<DONE;
</td><td>
DONE
   print $query->checkbox(-name=>'sta15_mark_d1',
                           -value=>'1',
                           -checked=>$DEFAULT{'sta15_mark_d1'},
                           -label=>'',
                    );
print <<DONE;
</td><td>
DONE
   print $query->checkbox(-name=>'sta15_mark_d2',
                           -value=>'2',
                           -checked=>$DEFAULT{'sta15_mark_d2'},
                           -label=>'',
                    );
print <<DONE;
</td><td>
DONE
   print $query->checkbox(-name=>'sta15_mark_d3',
                           -value=>'3',
                           -checked=>$DEFAULT{'sta15_mark_d3'},
                           -label=>'',
                    );
print <<DONE;
</td><td>
DONE
   print $query->checkbox(-name=>'sta15_mark_d4',
                           -value=>'4',
                           -checked=>$DEFAULT{'sta15_mark_d4'},
                           -label=>'',
                    );
print <<DONE;
</tr><tr>
<td>16</td>
<td>
DONE
   print $query->textfield(-name=>'sta16_mark_name',
                    -value=>$DEFAULT{'sta16_mark_name'},
                    -size=>8,
                    -maxlength=>12);
print <<DONE;
</td><td>
DONE
   print $query->textfield(-name=>'sta16_mark_loc',
                    -value=>$DEFAULT{'sta16_mark_loc'},
                    -size=>12,
                    -maxlength=>20);
print <<DONE;
</td><td>
DONE
   print $query->checkbox(-name=>'sta16_mark_d1',
                           -value=>'1',
                           -checked=>$DEFAULT{'sta16_mark_d1'},
                           -label=>'',
                    );
print <<DONE;
</td><td>
DONE
   print $query->checkbox(-name=>'sta16_mark_d2',
                           -value=>'2',
                           -checked=>$DEFAULT{'sta16_mark_d2'},
                           -label=>'',
                    );
print <<DONE;
</td><td>
DONE
   print $query->checkbox(-name=>'sta16_mark_d3',
                           -value=>'3',
                           -checked=>$DEFAULT{'sta16_mark_d3'},
                           -label=>'',
                    );
print <<DONE;
</td><td>
DONE
   print $query->checkbox(-name=>'sta16_mark_d4',
                           -value=>'4',
                           -checked=>$DEFAULT{'sta16_mark_d4'},
                           -label=>'',
                    );
print <<DONE;
</tr><tr>
<td>17</td>
<td>
DONE
   print $query->textfield(-name=>'sta17_mark_name',
                    -value=>$DEFAULT{'sta17_mark_name'},
                    -size=>8,
                    -maxlength=>12);
print <<DONE;
</td><td>
DONE
   print $query->textfield(-name=>'sta17_mark_loc',
                    -value=>$DEFAULT{'sta17_mark_loc'},
                    -size=>12,
                    -maxlength=>20);
print <<DONE;
</td><td>
DONE
   print $query->checkbox(-name=>'sta17_mark_d1',
                           -value=>'1',
                           -checked=>$DEFAULT{'sta17_mark_d1'},
                           -label=>'',
                    );
print <<DONE;
</td><td>
DONE
   print $query->checkbox(-name=>'sta17_mark_d2',
                           -value=>'2',
                           -checked=>$DEFAULT{'sta17_mark_d2'},
                           -label=>'',
                    );
print <<DONE;
</td><td>
DONE
   print $query->checkbox(-name=>'sta17_mark_d3',
                           -value=>'3',
                           -checked=>$DEFAULT{'sta17_mark_d3'},
                           -label=>'',
                    );
print <<DONE;
</td><td>
DONE
   print $query->checkbox(-name=>'sta17_mark_d4',
                           -value=>'4',
                           -checked=>$DEFAULT{'sta17_mark_d4'},
                           -label=>'',
                    );
print <<DONE;
</tr><tr>
<td>18</td>
<td>
DONE
   print $query->textfield(-name=>'sta18_mark_name',
                    -value=>$DEFAULT{'sta18_mark_name'},
                    -size=>8,
                    -maxlength=>12);
print <<DONE;
</td><td>
DONE
   print $query->textfield(-name=>'sta18_mark_loc',
                    -value=>$DEFAULT{'sta18_mark_loc'},
                    -size=>12,
                    -maxlength=>20);
print <<DONE;
</td><td>
DONE
   print $query->checkbox(-name=>'sta18_mark_d1',
                           -value=>'1',
                           -checked=>$DEFAULT{'sta18_mark_d1'},
                           -label=>'',
                    );
print <<DONE;
</td><td>
DONE
   print $query->checkbox(-name=>'sta18_mark_d2',
                           -value=>'2',
                           -checked=>$DEFAULT{'sta18_mark_d2'},
                           -label=>'',
                    );
print <<DONE;
</td><td>
DONE
   print $query->checkbox(-name=>'sta18_mark_d3',
                           -value=>'3',
                           -checked=>$DEFAULT{'sta18_mark_d3'},
                           -label=>'',
                    );
print <<DONE;
</td><td>
DONE
   print $query->checkbox(-name=>'sta18_mark_d4',
                           -value=>'4',
                           -checked=>$DEFAULT{'sta18_mark_d4'},
                           -label=>'',
                    );
print <<DONE;
</tr><tr>
<td>19</td>
<td>
DONE
   print $query->textfield(-name=>'sta19_mark_name',
                    -value=>$DEFAULT{'sta19_mark_name'},
                    -size=>8,
                    -maxlength=>12);
print <<DONE;
</td><td>
DONE
   print $query->textfield(-name=>'sta19_mark_loc',
                    -value=>$DEFAULT{'sta19_mark_loc'},
                    -size=>12,
                    -maxlength=>20);
print <<DONE;
</td><td>
DONE
   print $query->checkbox(-name=>'sta19_mark_d1',
                           -value=>'1',
                           -checked=>$DEFAULT{'sta19_mark_d1'},
                           -label=>'',
                    );
print <<DONE;
</td><td>
DONE
   print $query->checkbox(-name=>'sta19_mark_d2',
                           -value=>'2',
                           -checked=>$DEFAULT{'sta19_mark_d2'},
                           -label=>'',
                    );
print <<DONE;
</td><td>
DONE
   print $query->checkbox(-name=>'sta19_mark_d3',
                           -value=>'3',
                           -checked=>$DEFAULT{'sta19_mark_d3'},
                           -label=>'',
                    );
print <<DONE;
</td><td>
DONE
   print $query->checkbox(-name=>'sta19_mark_d4',
                           -value=>'4',
                           -checked=>$DEFAULT{'sta19_mark_d4'},
                           -label=>'',
                    );
print <<DONE;
</tr><tr>
<td>20</td>
<td>
DONE
   print $query->textfield(-name=>'sta20_mark_name',
                    -value=>$DEFAULT{'sta20_mark_name'},
                    -size=>8,
                    -maxlength=>12);
print <<DONE;
</td><td>
DONE
   print $query->textfield(-name=>'sta20_mark_loc',
                    -value=>$DEFAULT{'sta20_mark_loc'},
                    -size=>12,
                    -maxlength=>20);
print <<DONE;
</td><td>
DONE
   print $query->checkbox(-name=>'sta20_mark_d1',
                           -value=>'1',
                           -checked=>$DEFAULT{'sta20_mark_d1'},
                           -label=>'',
                    );
print <<DONE;
</td><td>
DONE
   print $query->checkbox(-name=>'sta20_mark_d2',
                           -value=>'2',
                           -checked=>$DEFAULT{'sta20_mark_d2'},
                           -label=>'',
                    );
print <<DONE;
</td><td>
DONE
   print $query->checkbox(-name=>'sta20_mark_d3',
                           -value=>'3',
                           -checked=>$DEFAULT{'sta20_mark_d3'},
                           -label=>'',
                    );
print <<DONE;
</td><td>
DONE
   print $query->checkbox(-name=>'sta20_mark_d4',
                           -value=>'4',
                           -checked=>$DEFAULT{'sta20_mark_d4'},
                           -label=>'',
                    );

print <<DONE;
</td></tr></table>

<a name="terrain"><h3>Terrain Heights</h3>
DONE

print <<DONE;
Domain 1<br>
&nbsp;&nbsp;&nbsp;Minimum Terrain Height shading level
DONE
  %labels = (
    "0", "0 m",
    );
   print $query->radio_group(-name=>'terr_sha_min_d1',
                             -values=>['0'],
                             -default=>$DEFAULT{'terr_sha_min_d1'},
                             -labels=>\%labels,
                    );
print <<DONE;
<br>
&nbsp;&nbsp;&nbsp;Maximum Terrain Height shading level
DONE
  %labels = (
    "2000", "2000 m",
    "3000", "3000 m",
    "4000", "4000 m",
    );
   print $query->radio_group(-name=>'terr_sha_max_d1',
                             -values=>['2000','3000','4000'],
                             -default=>$DEFAULT{'terr_sha_max_d1'},
                             -labels=>\%labels,
                    );

print <<DONE;
<br><br>
Domain 2<br>
&nbsp;&nbsp;&nbsp;Minimum Terrain Height shading level
DONE
  %labels = (
    "0", "0 m",
    );
   print $query->radio_group(-name=>'terr_sha_min_d2',
                             -values=>['0'],
                             -default=>$DEFAULT{'terr_sha_min_d2'},
                             -labels=>\%labels,
                    );
print <<DONE;
<br>
&nbsp;&nbsp;&nbsp;Maximum Terrain Height shading level
DONE
  %labels = (
    "2000", "2000 m",
    "3000", "3000 m",
    "4000", "4000 m",
    );
   print $query->radio_group(-name=>'terr_sha_max_d2',
                             -values=>['2000','3000','4000'],
                             -default=>$DEFAULT{'terr_sha_max_d2'},
                             -labels=>\%labels,
                    );

print <<DONE;
<br><br>
Domain 3<br>
&nbsp;&nbsp;&nbsp;Minimum Terrain Height shading level
DONE
  %labels = (
    "0", "0 m",
    "1000", "1000 m",
    );
   print $query->radio_group(-name=>'terr_sha_min_d3',
                             -values=>['0','1000'],
                             -default=>$DEFAULT{'terr_sha_min_d3'},
                             -labels=>\%labels,
                    );
print <<DONE;
<br>
&nbsp;&nbsp;&nbsp;Maximum Terrain Height shading level
DONE
  %labels = (
    "1000", "1000 m",
    "2000", "2000 m",
    "3000", "3000 m",
    "4000", "4000 m",
    );
   print $query->radio_group(-name=>'terr_sha_max_d3',
                             -values=>['1000','2000','3000','4000'],
                             -default=>$DEFAULT{'terr_sha_max_d3'},
                             -labels=>\%labels,
                    );

print <<DONE;
<br><br>
Domain 4<br>
&nbsp;&nbsp;&nbsp;Minimum Terrain Height shading level
DONE
  %labels = (
    "0", "0 m",
    "1000", "1000 m",
    );
   print $query->radio_group(-name=>'terr_sha_min_d4',
                             -values=>['0','1000'],
                             -default=>$DEFAULT{'terr_sha_min_d4'},
                             -labels=>\%labels,
                    );
print <<DONE;
<br>
&nbsp;&nbsp;&nbsp;Maximum Terrain Height shading level
DONE
  %labels = (
    "400", "400 m",
    "1000", "1000 m",
    "2000", "2000 m",
    "3000", "3000 m",
    "4000", "4000 m",
    );
   print $query->radio_group(-name=>'terr_sha_max_d4',
                             -values=>['400','1000','2000','3000','4000'],
                             -default=>$DEFAULT{'terr_sha_max_d4'},
                             -labels=>\%labels,
                    );

print <<DONE;
<p>
<a name="submit">
DONE

print $query->hidden(-name=>'Range',-default=>$Range);
print $query->submit('return','Submit');
print "&nbsp;&nbsp;&nbsp;";
print $query->reset;
print $query->end_form;

print $query->end_html;
