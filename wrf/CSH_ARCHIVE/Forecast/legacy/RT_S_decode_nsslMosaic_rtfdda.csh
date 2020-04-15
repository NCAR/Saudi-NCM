#!/bin/tcsh  -f
#
###############################################################################
echo  ""
echo  " ----------------------------------------------------------------------"
echo  " ---------------- NSSL Mosaic decoder starts ---------------------------"
echo "$0 $argv[*]"
echo  " ----------------------------------------------------------------------"
###############################################################################

set timestamp
setenv SUBSYSTEM NSSL_MOSAIC_FDDA

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

#	Check Arguments

if ( ${#argv} < 4 ) then
    echo "ERROR (usage): $0 start_date end_date this_cycle last_cycle (interval)"
    echo "where start_date is the initial time of the MM5 fdda,"
    echo "end_date is the final time of the MM5 fdda; both are in CCYYMMDDHH "
    exit ( 4 )
endif

set start_date = `echo $1 | cut -c1-10`
set end_date   = `echo $2 | cut -c1-10`
set this_cycle = `echo $3 | cut -c1-10`
set last_cycle = `echo $4 | cut -c1-10`
set s = $MM5HOST
if ( ${#argv} > 4 ) then
  set out_min3 = $5
else
  set out_min3 = 15
endif

set DOMS = ( 2 3 )
echo  " nsslMosaic processing domain is hardwired to $DOMS now"
set input_min = 5
echo  " nsslMosaic updating interval is hardwired to $input_min min now"

if(-d GEAPSTMP_tmp) then
 if( ! -d $GEAPSTMP/DECODE_NSSL_MOSAIC) then
  $MustHaveDir $GEAPSTMP/DECODE_NSSL_MOSAIC
 endif
 cd $GEAPSTMP/DECODE_NSSL_MOSAIC
 echo "Current directory is $GEAPSTMP/DECODE_NSSL_MOSAIC "
else
 $MustHaveDir $RUNDIR/${this_cycle}/DECODE_NSSL_MOSAIC
 cd ${RUNDIR}/${this_cycle}/DECODE_NSSL_MOSAIC
 echo "Current directory is ${RUNDIR}/${this_cycle}/DECODE_NSSL_MOSAIC "
endif

setenv DBZ_2_MM5GRID_EXE ${EXECUTABLE_ARCHIVE}/dbz_2_mm5grid.exe
setenv RADAR_FOR_NUDGE_EXE ${EXECUTABLE_ARCHIVE}/radar_for_nudge.exe

# copy merged files of raw dbz to cycle decoding directory
# from ($start_date-1h) to $end_date
# (back an hour for temporal filtering/interpolation purpose)
#
set ccyymmddhh  = $start_date
echo "$ccyymmddhh, -1" >! input
${EXECUTABLE_ARCHIVE}/advance_cymdh.exe < input >! output
set ccyymmddhh = `cat output`

while ($ccyymmddhh <= $end_date)
  echo "copying  $ccyymmddhh"
  foreach d ( $DOMS )

    set ccyy = `echo $ccyymmddhh | cut -c1-4`
    set mm = `echo $ccyymmddhh | cut -c5-6`
    set dd = `echo $ccyymmddhh | cut -c7-8`
    set hh = `echo $ccyymmddhh | cut -c9-10`
    echo link ${ccyy}-${mm}-${dd}_${hh}"##"_${s}_D${d}.nc
    ln -s -f ${NSSL_MOSAIC_DATA_DIR}/${s}/${ccyy}-${mm}-${dd}_${hh}*_${s}_D${d}.nc .
  end
# Increment date
  echo "$ccyymmddhh, 1" >! input
  ${EXECUTABLE_ARCHIVE}/advance_cymdh.exe < input >! output
  set ccyymmddhh = `cat output`
end

foreach d ( $DOMS)

# Look for the first guess (from previous cycle forecast)
#
  @ n_mm5file = 0

  if (-e ../${last_cycle}_MMOUTPUT_DOMAIN${d}) then
    ln -s -f ../${last_cycle}_MMOUTPUT_DOMAIN${d} MMOUTPUT_DOMAIN${d}_00
    @ n_mm5file = 1
  else if (-e $RUNDIR/${last_cycle}/${last_cycle}_MMOUTPUT_DOMAIN${d}.${s}_F) then
    ln -s -f $RUNDIR/$last_cycle/${last_cycle}_MMOUTPUT_DOMAIN${d}.${s}_F MMOUTPUT_DOMAIN${d}_00
    @ n_mm5file = 1
    if (-e $RUNDIR/${last_cycle}/${last_cycle}_MMOUTPUT_DOMAIN${d}.${s}_P+FCST) then
      ln -s -f $RUNDIR/$last_cycle/${last_cycle}_MMOUTPUT_DOMAIN${d}.${s}_P+FCST MMOUTPUT_DOMAIN${d}_01
      @ n_mm5file ++
    else if (-d $RUNDIR/${last_cycle}/MM5_P ) then
      foreach file (`ls $RUNDIR/${last_cycle}/MM5_P/MMOUT_DOMAIN${d}* | head -8`)
        ln -s -f $file MMOUTPUT_DOMAIN${d}_0$n_mm5file
        @ n_mm5file ++
      end
    endif
  else if (-e ../${this_cycle}_MMINPUT_DOMAIN${d}.${s}) then
    ln -s -f ../${this_cycle}_MMINPUT_DOMAIN${d}.${s} MMOUTPUT_DOMAIN${d}_00
    @ n_mm5file = 1
  else
# Stop in absence of first guess
    echo "Cannot find first guess file for domain$d "
    exit (1)
  endif

# map the dbz fields onto model grid - read model grid info from the i.c. file

  set mm5ic = ../${this_cycle}_MMINPUT_DOMAIN${d}.${s}
  if (! -e $mm5ic )  set mm5ic = "MMOUTPUT_DOMAIN${d}_00"

  if($d > 2) @ out_min = $out_min3
  if( $d <= 2 && $out_min3 < 30) @ out_min = 2 * $out_min3
  echo " interval of output fields for nudging is $out_min min now"
  @ HOURLY_OUT =  60 / $out_min

  set ccyymmddhh  = $start_date
  set cnt = 0
  while ($ccyymmddhh <= $end_date)
    @ min = $cnt * $out_min

    set ccyy = `echo $ccyymmddhh | cut -c1-4`
    set mm = `echo $ccyymmddhh | cut -c5-6`
    set dd = `echo $ccyymmddhh | cut -c7-8`
    set hh = `echo $ccyymmddhh | cut -c9-10`
    if($min < 10) set min = 0$min
    set dateStr = ${ccyy}"-"${mm}"-"${dd}"_"${hh}":"${min}":00"
    set radar = ${dateStr}_${s}_D${d}.nc
    set dbz_out = ${dateStr}"_DBZ_D"$d".nc"
    if ( -e $dbz_out) /bin/rm $dbz_out

    echo DBZ_2_MM5GRID_EXE for ${dateStr}
    $DBZ_2_MM5GRID_EXE $mm5ic $radar ${dateStr} ${s} $dbz_out

    @ cnt ++
    if( $cnt == $HOURLY_OUT ) then
      set cnt = 0
      echo "$ccyymmddhh, 1" >! input
      ${EXECUTABLE_ARCHIVE}/advance_cymdh.exe < input >! output
      set ccyymmddhh = `cat output`
     endif
  end

  /bin/rm *_${s}_D${d}.nc

## cat radar datasets for all time levels together
## if ( -e ${this_cycle}_RADAR_DOMAIN${d}) rm ${this_cycle}_RADAR_DOMAIN${d}
## cat DBZ_D${d}*_${s}.nc >> ${this_cycle}_RADAR_DOMAIN${d}

#
# read mm5 files, convert radar data to qr/qs based on t3D
#      and insert qr/qs as model variable fields

  set output = ${this_cycle}_RADAR_DOMAIN${d}

  if ( -e $output ) /bin/rm $output
  echo RADAR_FOR_NUDGE_EXE MMOUTPUT_DOMAIN${d} $n_mm5file $output $start_date $end_date $out_min
  $RADAR_FOR_NUDGE_EXE MMOUTPUT_DOMAIN${d} $n_mm5file $output $start_date $end_date $out_min
  mv $output ../RADAR_DOMAIN${d}
  cp ../RADAR_DOMAIN${d} ../RADAR2_DOMAIN${d}
# ln -s -f ${RUNDIR}/${this_cycle}/DECODE_NSSL_MOSAIC/$output ../RADAR_DOMAIN${d}
# ln -s -f ${RUNDIR}/${this_cycle}/DECODE_NSSL_MOSAIC/$output ../RADAR2_DOMAIN${d}

  echo "NSSL_MOSAIC DATA for $this_cycle domain$d is processed"

end

exit 0
