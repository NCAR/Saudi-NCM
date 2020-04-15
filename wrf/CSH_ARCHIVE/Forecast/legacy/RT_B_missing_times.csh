#!/bin/csh -f

#	This shell interpolates REGRIDv3 files temporally.
###############################################################################
echo  "$CSH_ARCHIVE/Forecast/RT_B_missing_times.csh:"
echo  " ----------------------------------------------------------------------"
echo  " ---------------- Temporal interpolation for missing ETA --------------"
echo  " ----------------------------------------------------------------------"
###############################################################################

# set echo
set timestamp
setenv SUBSYSTEM MISSING_TIMES
setenv RM "rm -rf"
#
# ENVIRONMENT
#
set CFILE="$MM5HOME/cycle_code/CONFIG_FILES/cshrc_"

$CheckConfigFiles
set cfstat = $status
if ( $cfstat != 0 ) then
 echo "${SUBSYSTEM} -- Missing ConfigFile -> exiting"
 exit (2)
endif
source ${CFILE}user.mm5sys.${MM5HOST};
source ${CFILE}sizes.mm5sys.${MM5HOST}

#	Check usage

if      ( ${#argv} != 2 )  then
	echo "usage: $0 eta_date start_date"
	echo "where eta_date is the 0 h of the ETA forecast, and"
	echo "start_date is the 0h of the MM5 forecast"
	echo "where both are given in CCYYMMDDHH"
	exit ( 4 )
endif

set eta_date = $1
set start_date = $2
set datey = `echo $eta_date | cut -c 1-4`
set datem = `echo $eta_date | cut -c 5-6`
set dated = `echo $eta_date | cut -c 7-8`
set dateh = `echo $eta_date | cut -c 9-10`
set dateymdh = ${datey}-${datem}-${dated}_${dateh}:00:00.0000

#	Does the directory exist

$MustHaveDir $RUNDIR/${start_date}
$MustHaveDir $RUNDIR/${start_date}/MISSING_TIMES
cd $RUNDIR/${start_date}/MISSING_TIMES
echo "Now working in  $cwd"


#	See if all of the required files are available

if     ( ( -e $RUNDIR/$start_date/${eta_date}_REGRIDv3_+00h.${MM5HOST} ) && \
         ( -e $RUNDIR/$start_date/${eta_date}_REGRIDv3_+03h.${MM5HOST} ) && \
         ( -e $RUNDIR/$start_date/${eta_date}_REGRIDv3_+06h.${MM5HOST} ) && \
         ( -e $RUNDIR/$start_date/${eta_date}_REGRIDv3_+09h.${MM5HOST} ) && \
         ( -e $RUNDIR/$start_date/${eta_date}_REGRIDv3_+12h.${MM5HOST} ) && \
         ( -e $RUNDIR/$start_date/${eta_date}_REGRIDv3_+15h.${MM5HOST} ) && \
         ( -e $RUNDIR/$start_date/${eta_date}_REGRIDv3_+18h.${MM5HOST} ) && \
         ( -e $RUNDIR/$start_date/${eta_date}_REGRIDv3_+21h.${MM5HOST} ) && \
         ( -e $RUNDIR/$start_date/${eta_date}_REGRIDv3_+24h.${MM5HOST} ) && \
         ( -e $RUNDIR/$start_date/${eta_date}_REGRIDv3_+27h.${MM5HOST} ) && \
         ( -e $RUNDIR/$start_date/${eta_date}_REGRIDv3_+30h.${MM5HOST} ) && \
         ( -e $RUNDIR/$start_date/${eta_date}_REGRIDv3_+33h.${MM5HOST} ) && \
         ( -e $RUNDIR/$start_date/${eta_date}_REGRIDv3_+36h.${MM5HOST} ) ) then
	echo "INFO: all files available, no temporal interpolation"
	cat $RUNDIR/$start_date/${eta_date}_REGRIDv3_+*h.${MM5HOST} >>! $RUNDIR/$start_date/${eta_date}_REGRIDv3.${MM5HOST}
        ${RM} $RUNDIR/$start_date/${eta_date}_REGRIDv3_+*h.${MM5HOST}
	echo "${SUBSYSTEM} -- all files available, no temporal interpolation"
	exit ( 0 )
else if ( ( -e $RUNDIR/$start_date/${eta_date}_REGRIDv3_+00h.${MM5HOST} ) || \
          ( -e $RUNDIR/$start_date/${eta_date}_REGRIDv3_+03h.${MM5HOST} ) || \
          ( -e $RUNDIR/$start_date/${eta_date}_REGRIDv3_+06h.${MM5HOST} ) || \
          ( -e $RUNDIR/$start_date/${eta_date}_REGRIDv3_+09h.${MM5HOST} ) || \
          ( -e $RUNDIR/$start_date/${eta_date}_REGRIDv3_+12h.${MM5HOST} ) || \
          ( -e $RUNDIR/$start_date/${eta_date}_REGRIDv3_+15h.${MM5HOST} ) || \
          ( -e $RUNDIR/$start_date/${eta_date}_REGRIDv3_+18h.${MM5HOST} ) || \
          ( -e $RUNDIR/$start_date/${eta_date}_REGRIDv3_+21h.${MM5HOST} ) || \
          ( -e $RUNDIR/$start_date/${eta_date}_REGRIDv3_+24h.${MM5HOST} ) || \
          ( -e $RUNDIR/$start_date/${eta_date}_REGRIDv3_+27h.${MM5HOST} ) || \
          ( -e $RUNDIR/$start_date/${eta_date}_REGRIDv3_+30h.${MM5HOST} ) || \
          ( -e $RUNDIR/$start_date/${eta_date}_REGRIDv3_+33h.${MM5HOST} ) || \
          ( -e $RUNDIR/$start_date/${eta_date}_REGRIDv3_+36h.${MM5HOST} ) ) then
	echo "at least one file exists for temporal interpolation"
else
	echo "no REGRID files exist for temporal interpolation"
	exit ( 2 )
endif

#	Check for 00 h extrapolation

if ( ! -e $RUNDIR/$start_date/${eta_date}_REGRIDv3_+00h.${MM5HOST} ) then
        if ( ( -l fort.10 ) || ( -e fort.10 ) ) ${RM} fort.10
        if ( ( -l fort.12 ) || ( -e fort.12 ) ) ${RM} fort.12
        echo "$eta_date,-12" >! date.in
        ${EXECUTABLE_ARCHIVE}/advance_cymdh < date.in >! date.out
        set cymdhm12 = `cat date.out `
        if ( -e $RUNDIR/$start_date/${cymdhm12}_REGRIDv3_+12h.${MM5HOST} ) then
		ln -s $RUNDIR/${start_date}/${cymdhm12}_REGRIDv3_+12h.${MM5HOST} fort.10
		set ok = $status
		if ( $ok != 0 ) then
			cp $RUNDIR/${start_date}/${cymdhm12}_REGRIDv3_+12h.${MM5HOST} fort.10
		endif
		echo "Using previous 12 h forecast for current 0 h initial condition"
	else
		ln -s `ls -1 $RUNDIR/$start_date/${eta_date}_REGRIDv3_*h.${MM5HOST} | line` fort.10
		set ok = $status
		if ( $ok != 0 ) then
			cp `ls -1 $RUNDIR/$start_date/${eta_date}_REGRIDv3_*h.${MM5HOST} | line` fort.10
		endif
		echo "Using extrapolation for 0 h initial condition"
	endif
        echo "1" >! input
        echo "00,$dateymdh" >> input
        $MISSING_TIMES_EXE < input ../${eta_date}_missing_times_00_print.out
	mv fort.12 $RUNDIR/$start_date/${eta_date}_REGRIDv3_+00h.${MM5HOST}
endif

#	Check for 36 h extrapolation

if ( ! -e $RUNDIR/$start_date/${eta_date}_REGRIDv3_+24h.${MM5HOST} ) then
        if ( ( -l fort.10 ) || ( -e fort.10 ) ) ${RM} fort.10
        if ( ( -l fort.12 ) || ( -e fort.12 ) ) ${RM} fort.12
        ln -s `ls -1r $RUNDIR/$start_date/${eta_date}_REGRIDv3_*h.${MM5HOST} | line` fort.10
	set ok = $status
	if ( $ok != 0 ) then
		cp `ls -1r $RUNDIR/$start_date/${eta_date}_REGRIDv3_*h.${MM5HOST} | line` fort.10
	endif
	echo "$eta_date,36" >! date.in
	${EXECUTABLE_ARCHIVE}/advance_cymdh < date.in >! date.out
        set datey = `cat date.out | cut -c 1-4`
        set datem = `cat date.out | cut -c 5-6`
        set dated = `cat date.out | cut -c 7-8`
        set dateh = `cat date.out | cut -c 9-10`
        set dateymdh = ${datey}-${datem}-${dated}_${dateh}:00:00.0000
        echo "1" >! input
        echo "36,$dateymdh" >> input
        $MISSING_TIMES_EXE < input ../${eta_date}_missing_times_36_print.out
	mv fort.12 $RUNDIR/$start_date/${eta_date}_REGRIDv3_+36h.${MM5HOST}
endif

#	We temporally interpolate the data.  We just forced 00 and 36
#	to exist, so now the only possible missing times (mt) are
#	3 - 33 h.

foreach mt ( 03 06 09 12 15 18 21 24 27 30 33)

	#	If one of these times does not exist, we have work to do.

	if ( ! -e $RUNDIR/$start_date/${eta_date}_REGRIDv3_+${mt}h.${MM5HOST} ) then

		#	Find the time just before the missing time.

		set hh = 00
		set hhless = 00
		while ( $hh < $mt )
			if ( -e $RUNDIR/$start_date/${eta_date}_REGRIDv3_+${hh}h.${MM5HOST} ) then
				set hhless = $hh
			endif
			@ hh = $hh + 3
			if ($hh == 3 ) set hh = 03
			if ($hh == 6 ) set hh = 06
			if ($hh == 9 ) set hh = 09
		end

		#	Find the time just after the missing time

		set hh = 36
		set hhmore = 36
		while ( $hh > $mt )
			if ( -e $RUNDIR/$start_date/${eta_date}_REGRIDv3_+${hh}h.${MM5HOST} ) then
				set hhmore = $hh
			endif
			@ hh = $hh - 3
			if ($hh == 3 ) set hh = 03
			if ($hh == 6 ) set hh = 06
			if ($hh == 9 ) set hh = 09
		end

		#	We have the missing time (mt), the previous time (hhless),
		#	the time afterwards (hhmore).  Build the mdate of the
		#	missing time and we are ready.

		echo "$eta_date,$mt" >! date.in
		${EXECUTABLE_ARCHIVE}/advance_cymdh < date.in >! date.out
                set datey = `cat date.out | cut -c 1-4`
                set datem = `cat date.out | cut -c 5-6`
                set dated = `cat date.out | cut -c 7-8`
                set dateh = `cat date.out | cut -c 9-10`
                set dateymdh = ${datey}-${datem}-${dated}_${dateh}:00:00.0000

		if ( ( -l fort.10 ) || ( -e fort.10 ) ) ${RM} fort.10
		if ( ( -l fort.11 ) || ( -e fort.11 ) ) ${RM} fort.11
		if ( ( -l fort.12 ) || ( -e fort.12 ) ) ${RM} fort.12

		ln -s $RUNDIR/$start_date/${eta_date}_REGRIDv3_+${hhless}h.${MM5HOST} fort.10
		set ok = $status
		if ( $ok != 0 ) then
			cp $RUNDIR/$start_date/${eta_date}_REGRIDv3_+${hhless}h.${MM5HOST} fort.10
		endif
		ln -s $RUNDIR/$start_date/${eta_date}_REGRIDv3_+${hhmore}h.${MM5HOST} fort.11
		set ok = $status
		if ( $ok != 0 ) then
			cp $RUNDIR/$start_date/${eta_date}_REGRIDv3_+${hhmore}h.${MM5HOST} fort.11
		endif
		echo "2" >! input
		echo "$mt,$hhless,$hhmore,$dateymdh" >> input
		$MISSING_TIMES_EXE < input ../${eta_date}_missing_times_${mt}_print.out
		mv fort.12 $RUNDIR/$start_date/${eta_date}_REGRIDv3_+${mt}h.${MM5HOST}

	endif

end

#	See if all of the required files are available

if     ( ( -e $RUNDIR/$start_date/${eta_date}_REGRIDv3_+00h.${MM5HOST} ) && \
         ( -e $RUNDIR/$start_date/${eta_date}_REGRIDv3_+03h.${MM5HOST} ) && \
         ( -e $RUNDIR/$start_date/${eta_date}_REGRIDv3_+06h.${MM5HOST} ) && \
         ( -e $RUNDIR/$start_date/${eta_date}_REGRIDv3_+09h.${MM5HOST} ) && \
         ( -e $RUNDIR/$start_date/${eta_date}_REGRIDv3_+12h.${MM5HOST} ) && \
         ( -e $RUNDIR/$start_date/${eta_date}_REGRIDv3_+15h.${MM5HOST} ) && \
         ( -e $RUNDIR/$start_date/${eta_date}_REGRIDv3_+18h.${MM5HOST} ) && \
         ( -e $RUNDIR/$start_date/${eta_date}_REGRIDv3_+21h.${MM5HOST} ) && \
         ( -e $RUNDIR/$start_date/${eta_date}_REGRIDv3_+24h.${MM5HOST} ) && \
         ( -e $RUNDIR/$start_date/${eta_date}_REGRIDv3_+27h.${MM5HOST} ) && \
         ( -e $RUNDIR/$start_date/${eta_date}_REGRIDv3_+30h.${MM5HOST} ) && \
         ( -e $RUNDIR/$start_date/${eta_date}_REGRIDv3_+33h.${MM5HOST} ) && \
         ( -e $RUNDIR/$start_date/${eta_date}_REGRIDv3_+36h.${MM5HOST} ) ) then
	echo "all files available from temporal interpolation"
	cat $RUNDIR/$start_date/${eta_date}_REGRIDv3_+*h.${MM5HOST} >>! $RUNDIR/$start_date/${eta_date}_REGRIDv3.${MM5HOST}
        ${RM} $RUNDIR/$start_date/${eta_date}_REGRIDv3_+*h.${MM5HOST}
	exit ( 0 )
else
	echo "some of the files not created from temporal interpolation"
	cat $RUNDIR/$start_date/${eta_date}_REGRIDv3_+*h.${MM5HOST} >>! $RUNDIR/$start_date/${eta_date}_REGRIDv3.${MM5HOST}
	exit ( 3 )
endif
