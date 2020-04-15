#!/usr/bin/perl -wT

require "./ReadWriteDB.pm";
use GDBM_File;
use CGI;
use strict;

my $query = new CGI;

print $query->header;

print $query->start_html(-title=>'WRF Image Configuration Form - Enter Range',
			 -BGCOLOR=>"#FFFFFF");

my $file = "/model/atecuser/web_extra/form_data/records.dbx";
my (%DATA,$key);

tie(%DATA, "GDBM_File", $file, GDBM_READER, 0666) || print $!;
my @keys = sort keys %DATA;
untie(%DATA);

print "<center><h2>WRF Image Configuration Form</h2></center>";

print $query->start_form(-method =>'POST', -action =>'display_rip_form.pl');

print <<DONE;
Select range from list of existing ranges:<p>
<select name='Range'>
DONE

foreach $key (@keys)
{
  if ($key eq "Default") {
     print "<option selected>$key<br>";
  } else {
     print "<option>$key<br>";
  }
}

print <<DONE;
</select><p>
You will be able to modify selected range and/or save it to a new name.
<p>
DONE

print $query->submit('return','Submit Range');
print $query->end_form;

print $query->end_html;

