#!/bin/csh

set echo
cat Pub9-volA.format-info >! gts_sttnid_input.wmo.cc
ex gts_sttnid_input.wmo.cc << "EOF" >& /dev/null
/^ CODE TABLE No. 6.1 /
1,.d
g'/.*$'s'''
g'^$'d
g'^  Code'd
g'^  code'd
g'^ Figure'd
g'---'d
g'___'d
g'^           'd
g'^.$'d
g'WMO PUBLICATION'd
g'CODE TABLE'd
g'STATIONS OPERATED BY 's'$' STTN'g
g'STATIONS OPERATED BY 's'''g
g'  's'' 'g
g'  's'' 'g
g'  's'' 'g
g'  's'' 'g
g'  's'' 'g
g'  's'' 'g
g'  's'' 'g
g'  's'' 'g
g'  's'' 'g
g'^ 's'''g
g'^[12345678]'mo$
/^1/
1,.-1d
g' THE 's'' 'g
g'UNITED REPUBLIC OF TANZANIA's''TANZANIA'
g'FORMER UNION OF SOVIET SOCIALIST REPUBLICS (IN ASIA)'s''FORMER U.S.S.R. (ASIA)'
g'FORMER UNION OF SOVIET SOCIALIST REPUBLICS (IN EUROPE)'s''FORMER U.S.S.R. (EUROPE)'
g'UNITED KINGDOM OF GREAT BRITAIN AND NORTHERN IRELAND's''U.K.'
g'UNITED STATES OF AMERICA's''U.S.A.'
g'INDIA (STATIONS.*'s''INDIA'
g'PUERTO RICO AND US POSSESSIONS IN CARIBBEAN AREA's''US (CARIBBEAN)'
g'ST MARTIN, ST BARTHELEMY.*'s''FRENCH PACIFIC ISLANDS'
g'FRENCH POLYNESIA (.*'s''FRENCH POLYNESIA'
g'ISLANDS IN PACIFIC OCEAN NORTH OF EQUATOR's''PACIFIC ISLANDS'
g' (UNIVERSITY OF WISCONSIN)'s'''
g'DEMOCRATIC's''DEM.'
g'UNITED's''U.'
g'REPUBLIC's''REP.'
g' AND 's'' \& '
w! gts_sttnid_cc
q
"EOF"

sort gts_sttnid_cc >! gts_sttnid_input.wmo.cc
\rm  gts_sttnid_cc

grep '^02' Pub9-volA.flatfile | sort >! gts_sttnid_input.wmo
