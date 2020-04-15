#! /bin/csh -f

#------------------------------------------------------------------------------#
# geth_newdate.csh ccyy-mm-dd(_hh:mn:ss) +/-tincr
#
# Find the new date given the old date and the time increment.
# The time increment is expressed in terms of the date unit (from day to second)
# For instance:
#
#  geth_newdate.csh 2001-06-19          +2 = 2001-06-21           (days)
#  geth_newdate.csh 2001-06-19_00       +2 = 2001-06-19_02        (hours)
#  geth_newdate.csh 2001-06-19_00:00    +2 = 2001-06-19_00:02    (minutes)
#  geth_newdate.csh 2001-06-19_00:00:00 +2 = 2001-06-19_00:00:02 (seconds)
#
# Beware of the date format ccyy-mm-dd_hh:mn:ss
#
# This script generates a file of commands which are passed to AWK to
# execute and determine the output date.
#
#------------------------------------------------------------------------------#
#set echo
# Conversion input format from ccyymmddhhmnss into ccyy-mm-dd_hh:mn:ss

set ccyymmddhhmnss = $1
set ccyy       = `echo ${ccyymmddhhmnss} |cut -c1-4`
set mm         = `echo ${ccyymmddhhmnss} |cut -c5-6`
set dd         = `echo ${ccyymmddhhmnss} |cut -c7-8`
set hh         = `echo ${ccyymmddhhmnss} |cut -c9-10`
set mn         = `echo ${ccyymmddhhmnss} |cut -c11-12`
set ss         = `echo ${ccyymmddhhmnss} |cut -c13-14`

set Hdate = ${ccyy}-${mm}-${dd}

if (${hh} =~  [0-9]*) then
 set Hdate = ${ccyy}-${mm}-${dd}_${hh}

if (${mn} =~  [0-9]*) then
 set Hdate = ${ccyy}-${mm}-${dd}_${hh}:${mn}

if (${ss} =~  [0-9]*) then
 set Hdate = ${ccyy}-${mm}-${dd}_${hh}:${mn}:${ss}

endif
endif
endif

# End of conversion input format

#set Hdate=$1
 set Deltat = $2

cat >! newdate.awpr << EOF1
BEGIN { 

  year=substr(HDATE,1,4)
  month=substr(HDATE,6,2)
  day=substr(HDATE,9,2); dtu="d"
  if (length(HDATE)>=13) {hour=substr(HDATE,12,2);   dtu="h"} else {hour=0}
  if (length(HDATE)>=16) {minute=substr(HDATE,15,2); dtu="m"} else {minute=0}
  if (length(HDATE)>=19) {second=substr(HDATE,18,2); dtu="s"} else {second=0}

  if (dtu=="s") {nmn=int(IDT/60); 
                 nsc=IDT-(nmn*60); 
                 nhr=int(nmn/60); 
		 nmn=nmn-(nhr*60);
                 ndy=int(nhr/24);
		 nhr=nhr-(ndy*24)}
  if (dtu=="m") {nsc=0;
                 nhr=int(IDT/60);
                 nmn=IDT-(nhr*60);
                 ndy=int(nhr/24);
		 nhr=nhr-(ndy*24)}
		 
  if (dtu=="h") {nsc=0; nmn=0;
                 ndy=int(IDT/24);
		 nhr=IDT-(ndy*24)}
  if (dtu=="d") {nsc=0; nmn=0;nhr=0;ndy=IDT}

if (IDT>=0) {

  mday=mdays(year,month)

 while (nsc!=0)
   { {second+=1; nsc-=1}
     {if (second>59) 
       { nmn+=1; second=0}
     }
   }

 while (nmn!=0)
   { {minute+=1; nmn-=1}
     {if (minute>59) 
       { nhr+=1; minute=0}
     }
   }

 while (nhr!=0)
   { {hour+=1; nhr-=1}
     {if (hour>23) 
       { ndy+=1; hour=0}
     }
   }

 while (ndy!=0) 
   {  {day+=1; ndy-=1} 
      {if (day>mday) 
	{  {month+=1; day=1}
	   if (month>12) {{month=1}{year+=1}}
	   mday=mdays(year,month)
	}
      }
   }
}

 else{
 while (nsc!=0)
   { {second-=1; nsc+=1}
     {if (second<0) 
       { nmn-=1; second=59}
     }
   }

 while (nmn!=0)
   { {minute-=1; nmn+=1}
     {if (minute<0) 
       { nhr-=1; minute=59}
     }
   }

 while (nhr!=0)
   { {hour-=1; nhr+=1}
     {if (hour<00) 
       { ndy-=1; hour=23}
     }
   }

 while (ndy!=0) 
   {  {day-=1; ndy+=1} 
      {if (day<1) 
	{  {month-=1; day=mdays(year,month)}
	   if (month<1) {{month=12}{year-=1}}
	}
      }
   }
 }

if (length(HDATE)>=19) 
 {printf "%4i-%02i-%02i_%02i:%02i:%02i\n", year,month,day,hour,minute,second}

else if (length(HDATE)>=16) 
 {printf "%4i-%02i-%02i_%02i:%02i\n", year,month,day,hour,minute}

else if (length(HDATE)>=13) 
 {printf "%4i-%02i-%02i_%02i\n", year,month,day,hour}

else
 {printf "%4i-%02i-%02i\n", year,month,day}

}

function mdays(year,month) 
{ 
  mm[0] = 31 
  mm[1] = 31 
  mm[2] = 28
  mm[3] = 31
  mm[4] = 30
  mm[5] = 31
  mm[6] = 30
  mm[7] = 31
  mm[8] = 31
  mm[9] = 30
  mm[10] = 31
  mm[11] = 30
  mm[12] = 31

  lmonth=month+0

    if (lmonth==2) { 
      if (int(year/4)==(year/4)) {
	if (int(year/100)==(year/100)) {
	  if (int(year/400)==(year/400)) {mmm=29}
	  else {mmm=28}}
	else {mmm=29}}
      else {mmm=28}}
    else {mmm=mm[month+0]}

  return mmm }
EOF1

#nawk -v HDATE=$Hdate -v IDT=$Deltat -f newdate.awpr
set Hdate = `nawk -v HDATE=$Hdate -v IDT=$Deltat -f newdate.awpr`

unalias rm
rm newdate.awpr

# Conversion output format from ccyy-mm-dd_hh:mn:ss into ccyymmdd.hhmnss

set ccyy       = `echo ${Hdate} |cut -c1-4`
set mm         = `echo ${Hdate} |cut -c6-7`
set dd         = `echo ${Hdate} |cut -c9-10`
set hh         = `echo ${Hdate} |cut -c12-13`
set mn         = `echo ${Hdate} |cut -c15-16`
set ss         = `echo ${Hdate} |cut -c18-19`

set Hdate = ${ccyy}${mm}${dd}

if (${hh} =~  [0-9]*) then
 set Hdate = ${ccyy}${mm}${dd}${hh}

if (${mn} =~  [0-9]*) then
 set Hdate = ${ccyy}${mm}${dd}${hh}${mn}

if (${ss} =~  [0-9]*) then
 set Hdate = ${ccyy}${mm}${dd}${hh}${mn}${ss}

endif
endif
endif

echo $Hdate
