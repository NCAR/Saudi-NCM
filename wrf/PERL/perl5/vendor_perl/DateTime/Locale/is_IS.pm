###########################################################################
#
# This file is auto-generated by the Perl DateTime Suite locale
# generator (0.05).  This code generator comes with the
# DateTime::Locale distribution in the tools/ directory, and is called
# generate-from-cldr.
#
# This file as generated from the CLDR XML locale data.  See the
# LICENSE.cldr file included in this distribution for license details.
#
# This file was generated from the source file is_IS.xml
# The source file version number was 1.50, generated on
# 2009/05/05 23:06:37.
#
# Do not edit this file directly.
#
###########################################################################

package DateTime::Locale::is_IS;

use strict;
use warnings;
use utf8;

use base 'DateTime::Locale::is';

sub cldr_version { return "1\.7\.1" }

{
    my $first_day_of_week = "7";
    sub first_day_of_week { return $first_day_of_week }
}

1;

__END__


=pod

=encoding utf8

=head1 NAME

DateTime::Locale::is_IS

=head1 SYNOPSIS

  use DateTime;

  my $dt = DateTime->now( locale => 'is_IS' );
  print $dt->month_name();

=head1 DESCRIPTION

This is the DateTime locale package for Icelandic Iceland.

=head1 DATA

This locale inherits from the L<DateTime::Locale::is> locale.

It contains the following data.

=head2 Days

=head3 Wide (format)

  mánudagur
  þriðjudagur
  miðvikudagur
  fimmtudagur
  föstudagur
  laugardagur
  sunnudagur

=head3 Abbreviated (format)

  mán
  þri
  mið
  fim
  fös
  lau
  sun

=head3 Narrow (format)

  m
  þ
  m
  f
  f
  l
  s

=head3 Wide (stand-alone)

  mánudagur
  þriðjudagur
  miðvikudagur
  fimmtudagur
  föstudagur
  laugardagur
  sunnudagur

=head3 Abbreviated (stand-alone)

  mán
  þri
  mið
  fim
  fös
  lau
  sun

=head3 Narrow (stand-alone)

  m
  þ
  m
  f
  f
  l
  s

=head2 Months

=head3 Wide (format)

  janúar
  febrúar
  mars
  apríl
  maí
  júní
  júlí
  ágúst
  september
  október
  nóvember
  desember

=head3 Abbreviated (format)

  jan
  feb
  mar
  apr
  maí
  jún
  júl
  ágú
  sep
  okt
  nóv
  des

=head3 Narrow (format)

  j
  f
  m
  a
  m
  j
  j
  á
  s
  o
  n
  d

=head3 Wide (stand-alone)

  janúar
  febrúar
  mars
  apríl
  maí
  júní
  júlí
  ágúst
  september
  október
  nóvember
  desember

=head3 Abbreviated (stand-alone)

  jan
  feb
  mar
  apr
  maí
  jún
  júl
  ágú
  sep
  okt
  nóv
  des

=head3 Narrow (stand-alone)

  j
  f
  m
  a
  m
  j
  j
  á
  s
  o
  n
  d

=head2 Quarters

=head3 Wide (format)

  1st fjórðungur
  2nd fjórðungur
  3rd fjórðungur
  4th fjórðungur

=head3 Abbreviated (format)

  F1
  F2
  F3
  F4

=head3 Narrow (format)

  1
  2
  3
  4

=head3 Wide (stand-alone)

  1. fjórðungur
  2. fjórðungur
  3. fjórðungur
  4. fjórðungur

=head3 Abbreviated (stand-alone)

  1F
  2F
  3F
  4F

=head3 Narrow (stand-alone)

  1
  2
  3
  4

=head2 Eras

=head3 Wide

  BCE
  CE

=head3 Abbreviated

  fyrir Krist
  eftir Krist

=head3 Narrow

  f.k.
  e.k.

=head2 Date Formats

=head3 Full

   2008-02-05T18:30:30 = þriðjudagur, 5. febrúar 2008
   1995-12-22T09:05:02 = föstudagur, 22. desember 1995
  -0010-09-15T04:44:23 = laugardagur, 15. september -10

=head3 Long

   2008-02-05T18:30:30 = 5. febrúar 2008
   1995-12-22T09:05:02 = 22. desember 1995
  -0010-09-15T04:44:23 = 15. september -10

=head3 Medium

   2008-02-05T18:30:30 = 5.2.2008
   1995-12-22T09:05:02 = 22.12.1995
  -0010-09-15T04:44:23 = 15.9.-010

=head3 Short

   2008-02-05T18:30:30 = 5.2.2008
   1995-12-22T09:05:02 = 22.12.1995
  -0010-09-15T04:44:23 = 15.9.-010

=head3 Default

   2008-02-05T18:30:30 = 5.2.2008
   1995-12-22T09:05:02 = 22.12.1995
  -0010-09-15T04:44:23 = 15.9.-010

=head2 Time Formats

=head3 Full

   2008-02-05T18:30:30 = 18:30:30 UTC
   1995-12-22T09:05:02 = 09:05:02 UTC
  -0010-09-15T04:44:23 = 04:44:23 UTC

=head3 Long

   2008-02-05T18:30:30 = 18:30:30 UTC
   1995-12-22T09:05:02 = 09:05:02 UTC
  -0010-09-15T04:44:23 = 04:44:23 UTC

=head3 Medium

   2008-02-05T18:30:30 = 18:30:30
   1995-12-22T09:05:02 = 09:05:02
  -0010-09-15T04:44:23 = 04:44:23

=head3 Short

   2008-02-05T18:30:30 = 18:30
   1995-12-22T09:05:02 = 09:05
  -0010-09-15T04:44:23 = 04:44

=head3 Default

   2008-02-05T18:30:30 = 18:30:30
   1995-12-22T09:05:02 = 09:05:02
  -0010-09-15T04:44:23 = 04:44:23

=head2 Datetime Formats

=head3 Full

   2008-02-05T18:30:30 = þriðjudagur, 5. febrúar 2008 18:30:30 UTC
   1995-12-22T09:05:02 = föstudagur, 22. desember 1995 09:05:02 UTC
  -0010-09-15T04:44:23 = laugardagur, 15. september -10 04:44:23 UTC

=head3 Long

   2008-02-05T18:30:30 = 5. febrúar 2008 18:30:30 UTC
   1995-12-22T09:05:02 = 22. desember 1995 09:05:02 UTC
  -0010-09-15T04:44:23 = 15. september -10 04:44:23 UTC

=head3 Medium

   2008-02-05T18:30:30 = 5.2.2008 18:30:30
   1995-12-22T09:05:02 = 22.12.1995 09:05:02
  -0010-09-15T04:44:23 = 15.9.-010 04:44:23

=head3 Short

   2008-02-05T18:30:30 = 5.2.2008 18:30
   1995-12-22T09:05:02 = 22.12.1995 09:05
  -0010-09-15T04:44:23 = 15.9.-010 04:44

=head3 Default

   2008-02-05T18:30:30 = 5.2.2008 18:30:30
   1995-12-22T09:05:02 = 22.12.1995 09:05:02
  -0010-09-15T04:44:23 = 15.9.-010 04:44:23

=head2 Available Formats

=head3 EEEd (d EEE)

   2008-02-05T18:30:30 = 5 þri
   1995-12-22T09:05:02 = 22 fös
  -0010-09-15T04:44:23 = 15 lau

=head3 HHmm (HH:mm)

   2008-02-05T18:30:30 = 18:30
   1995-12-22T09:05:02 = 09:05
  -0010-09-15T04:44:23 = 04:44

=head3 HHmmss (HH:mm:ss)

   2008-02-05T18:30:30 = 18:30:30
   1995-12-22T09:05:02 = 09:05:02
  -0010-09-15T04:44:23 = 04:44:23

=head3 Hm (H:mm)

   2008-02-05T18:30:30 = 18:30
   1995-12-22T09:05:02 = 9:05
  -0010-09-15T04:44:23 = 4:44

=head3 Hms (H:mm:ss)

   2008-02-05T18:30:30 = 18:30:30
   1995-12-22T09:05:02 = 9:05:02
  -0010-09-15T04:44:23 = 4:44:23

=head3 M (L.)

   2008-02-05T18:30:30 = 2.
   1995-12-22T09:05:02 = 12.
  -0010-09-15T04:44:23 = 9.

=head3 MEd (E d.M.)

   2008-02-05T18:30:30 = þri 5.2.
   1995-12-22T09:05:02 = fös 22.12.
  -0010-09-15T04:44:23 = lau 15.9.

=head3 MMM (LLL)

   2008-02-05T18:30:30 = feb
   1995-12-22T09:05:02 = des
  -0010-09-15T04:44:23 = sep

=head3 MMMEd (E d. MMM)

   2008-02-05T18:30:30 = þri 5. feb
   1995-12-22T09:05:02 = fös 22. des
  -0010-09-15T04:44:23 = lau 15. sep

=head3 MMMMEd (E d. MMMM)

   2008-02-05T18:30:30 = þri 5. febrúar
   1995-12-22T09:05:02 = fös 22. desember
  -0010-09-15T04:44:23 = lau 15. september

=head3 MMMMd (d. MMMM)

   2008-02-05T18:30:30 = 5. febrúar
   1995-12-22T09:05:02 = 22. desember
  -0010-09-15T04:44:23 = 15. september

=head3 MMMd (d. MMM)

   2008-02-05T18:30:30 = 5. feb
   1995-12-22T09:05:02 = 22. des
  -0010-09-15T04:44:23 = 15. sep

=head3 Md (d.M)

   2008-02-05T18:30:30 = 5.2
   1995-12-22T09:05:02 = 22.12
  -0010-09-15T04:44:23 = 15.9

=head3 d (d)

   2008-02-05T18:30:30 = 5
   1995-12-22T09:05:02 = 22
  -0010-09-15T04:44:23 = 15

=head3 hm (h:mm a)

   2008-02-05T18:30:30 = 6:30 e.h.
   1995-12-22T09:05:02 = 9:05 f.h.
  -0010-09-15T04:44:23 = 4:44 f.h.

=head3 hms (h:mm:ss a)

   2008-02-05T18:30:30 = 6:30:30 e.h.
   1995-12-22T09:05:02 = 9:05:02 f.h.
  -0010-09-15T04:44:23 = 4:44:23 f.h.

=head3 ms (mm:ss)

   2008-02-05T18:30:30 = 30:30
   1995-12-22T09:05:02 = 05:02
  -0010-09-15T04:44:23 = 44:23

=head3 y (y)

   2008-02-05T18:30:30 = 2008
   1995-12-22T09:05:02 = 1995
  -0010-09-15T04:44:23 = -10

=head3 yM (M. yyyy)

   2008-02-05T18:30:30 = 2. 2008
   1995-12-22T09:05:02 = 12. 1995
  -0010-09-15T04:44:23 = 9. -010

=head3 yMEd (EEE d.M.yyyy)

   2008-02-05T18:30:30 = þri 5.2.2008
   1995-12-22T09:05:02 = fös 22.12.1995
  -0010-09-15T04:44:23 = lau 15.9.-010

=head3 yMMM (MMM y)

   2008-02-05T18:30:30 = feb 2008
   1995-12-22T09:05:02 = des 1995
  -0010-09-15T04:44:23 = sep -10

=head3 yMMMEd (EEE d. MMM y)

   2008-02-05T18:30:30 = þri 5. feb 2008
   1995-12-22T09:05:02 = fös 22. des 1995
  -0010-09-15T04:44:23 = lau 15. sep -10

=head3 yMMMM (MMMM y)

   2008-02-05T18:30:30 = febrúar 2008
   1995-12-22T09:05:02 = desember 1995
  -0010-09-15T04:44:23 = september -10

=head3 yQ (Q. yyyy)

   2008-02-05T18:30:30 = 1. 2008
   1995-12-22T09:05:02 = 4. 1995
  -0010-09-15T04:44:23 = 3. -010

=head3 yQQQ (QQQ y)

   2008-02-05T18:30:30 = F1 2008
   1995-12-22T09:05:02 = F4 1995
  -0010-09-15T04:44:23 = F3 -10

=head3 yyQ (Q yy)

   2008-02-05T18:30:30 = 1 08
   1995-12-22T09:05:02 = 4 95
  -0010-09-15T04:44:23 = 3 10

=head3 yyyyM (M.yyyy)

   2008-02-05T18:30:30 = 2.2008
   1995-12-22T09:05:02 = 12.1995
  -0010-09-15T04:44:23 = 9.-010

=head3 yyyyMMMM (MMMM y)

   2008-02-05T18:30:30 = febrúar 2008
   1995-12-22T09:05:02 = desember 1995
  -0010-09-15T04:44:23 = september -10

=head2 Miscellaneous

=head3 Prefers 24 hour time?

Yes

=head3 Local first day of the week

sunnudagur


=head1 SUPPORT

See L<DateTime::Locale>.

=head1 AUTHOR

Dave Rolsky <autarch@urth.org>

=head1 COPYRIGHT

Copyright (c) 2008 David Rolsky. All rights reserved. This program is
free software; you can redistribute it and/or modify it under the same
terms as Perl itself.

This module was generated from data provided by the CLDR project, see
the LICENSE.cldr in this distribution for details on the CLDR data's
license.

=cut