#!/bin/csh -f
# $Id: RT_L_wrf_rtfdda.csh,v 1.61 2017/09/07 18:37:04 becky Exp $
#

###############################################################################
echo
echo  " ----------------------------------------------------------------------"
echo  " ---------------- WRF FDDA and FCST start -----------------------------"
echo  "$0 $argv[*]"
echo  " ----------------------------------------------------------------------"
#     " ------------------Yubao Liu 2005.2 - 2007.2---------------------------"
###############################################################################

# set echo
set timestamp
setenv SUBSYSTEM WRF
setenv RM "rm -rf"

set DOMS = ( 1 2 3 4 )
if($?NUM_DOMS) then
 if ($NUM_DOMS == 5) then
  set DOMS = ( 1 2 3 4 5 )
 else if ($NUM_DOMS == 4) then
  set DOMS = ( 1 2 3 4)
 else if ($NUM_DOMS == 3) then 
  set DOMS = ( 1 2 3)
 else if ($NUM_DOMS == 2) then 
  set DOMS = ( 1 2 )
 else if ($NUM_DOMS == 1) then 
  set DOMS = ( 1 )
 endif
endif

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


if ( ${#argv} != 9) then
	echo "usage: wrf.csh this_cycle fcst_id fcst_h start_date cpun d4start out_int NODE ETA"
	echo "where start_date and this_cycle is ccyymmddhh"
	echo "fcst_id: = 1  Cold start; = 2 Restart for last cycle; = 3 restart in this cycle"
	echo "FCST_H are fcst length in hour"
	echo "cpun is the number of parallel processors"
	echo "d4start is the time to begin domain 4 (minutes)"
	echo "out_int is the time interval of MMOUT "
	echo "NODE: 0 -mpp; 1 - 64 starting node for mpp jobs"
	echo "NAMGFS: ETA/NAM or AVN/GFS IC/BC"
	exit ( 1 )
endif

set this_cycle = $1
set fcst_id    = $2
set FCST_H     = $3
set start_date = $4
set cpun       = $5
set d4start    = $6
set out_int    = $7
set NODE       = $8
set NAMGFS     = $9

#	Get to the right directory

set CYCDIR = $RUNDIR/$this_cycle

if ( ($fcst_id == 1) || ($fcst_id == 2)) then   # Final FDDA cycle
set tmp_work = WRF_F
else
set tmp_work = WRF_P
endif

set this_y = `echo $this_cycle | cut -c 1-4`
set this_m = `echo $this_cycle | cut -c 5-6`
set this_d = `echo $this_cycle | cut -c 7-8`
set this_h = `echo $this_cycle | cut -c 9-10`
set this_dt = "${this_m}/${this_d}/${this_y} ${this_h}:00:00"
set this_time = `date -d "$this_dt" +%s`
set last_time = `echo $this_time \- $CYC_INT \* 3600 | bc`
set last_cycle = `date -d @${last_time} +%Y%m%d%H`

# Will work on main disk
set up_dir = $CYCDIR
set dir_work = $up_dir/$tmp_work
rm -rf $dir_work

# Unless a workdir on compute nodes has been created
if (-d $GEAPSTMP/1) then # $GEAPSTMP/1: mpirun does not like local disk
     # Will work on local disk
     set up_dir = $GEAPSTMP
     set dir_work = $up_dir/$tmp_work
     ln -sf $dir_work $CYCDIR/$tmp_work
endif

if (! -e $dir_work) $MustHaveDir $dir_work

# Go to Work dir
echo
echo "WORKDIR is $dir_work"
cd  $dir_work


# ES: These are MM env-vars. 
# BATCH_SYSTEM can be set to PBS, LSF, INTER. If it not set, default to INTER:
# MM is set to MM. If not set, default to "notMM"
if (! $?BATCH_SYSTEM) setenv BATCH_SYSTEM "INTER"
if (! $?MM ) setenv MM "notMM"

#setenv D1_LENGTH $FCST_H
if ( ! $?D1_LENGTH) setenv D1_LENGTH $FCST_H
if ( ! $?D2_LENGTH) setenv D2_LENGTH $FCST_H
if ( ! $?D3_LENGTH) setenv D3_LENGTH $FCST_H
if ( ! $?D4_LENGTH) setenv D4_LENGTH 13
if ( ! $?D5_LENGTH) setenv D5_LENGTH 13

if ( $?NORMAL ) then
   if ($NORMAL == 99) then
      if ( $?D1_COLD_LENGTH ) then
         setenv D1_LENGTH $D1_COLD_LENGTH
      endif
      if ( $?D2_COLD_LENGTH ) then
         setenv D2_LENGTH $D2_COLD_LENGTH
      endif
      if ( $?D3_COLD_LENGTH ) then
         setenv D3_LENGTH $D3_COLD_LENGTH
      endif
      if ( $?D4_COLD_LENGTH ) then
         setenv D4_LENGTH $D4_COLD_LENGTH
      endif
      if ( $?D5_COLD_LENGTH ) then
         setenv D5_LENGTH $D5_COLD_LENGTH
      endif
   endif
endif

set DOM_LENGTH = ( $D1_LENGTH $D2_LENGTH $D3_LENGTH $D4_LENGTH $D5_LENGTH )

if ( ! $?D1_OUT_INT) setenv D1_OUT_INT $OUT_INT
if ( ! $?D2_OUT_INT) setenv D2_OUT_INT $OUT_INT
if ( ! $?D3_OUT_INT) setenv D3_OUT_INT $OUT_INT
if ( ! $?D4_OUT_INT) setenv D4_OUT_INT $OUT_INT
if ( ! $?D5_OUT_INT) setenv D5_OUT_INT $OUT_INT
set out_intv = ( $D1_OUT_INT $D2_OUT_INT $D3_OUT_INT $D4_OUT_INT $D5_OUT_INT)

if($fcst_id == 2) set FCST_H = $CYC_INT

set i=1
set end_date=$start_date
while ( $i <= 216 && $i <= $FCST_H) 
#echo "$end_date , 1" >! input
#${EXECUTABLE_ARCHIVE}/advance_cymdh < input >! output
#set end_date = `cat output`
 set end_date = `${EXECUTABLE_ARCHIVE}/geth_newdate.exe $end_date 1`
 @ i ++
end

echo "start=$start_date end=$end_date FH=$FCST_H"

set y_start = `echo $start_date | cut -b 1-4`
set m_start = `echo $start_date | cut -b 5-6`
set d_start = `echo $start_date | cut -b 7-8`
set h_start = `echo $start_date | cut -b 9-10`

set BC_INT = 10800

set y_end = `echo  $end_date | cut -b 1-4`
set m_end = `echo  $end_date | cut -b 5-6`
set d_end = `echo  $end_date | cut -b 7-8`
set h_end = `echo  $end_date | cut -b 9-10`

# Original capability: D4 end-time only...
if ( $fcst_id == 3) then
  set d4end_date = `${EXECUTABLE_ARCHIVE}/geth_newdate.exe $start_date $D4_LENGTH`
else
 set d4end_date = $end_date
endif
set d4y_end = `echo $d4end_date | cut -b 1-4`
set d4m_end = `echo $d4end_date | cut -b 5-6`
set d4d_end = `echo $d4end_date | cut -b 7-8`
set d4h_end = `echo $d4end_date | cut -b 9-10`

echo "Done with D4 settings  $D4_LENGTH"
# New capability: end-time specified for each domain

set dy_end = ( 0 0 0 0 0 0)
set dm_end = ( 0 0 0 0 0 0)
set dd_end = ( 0 0 0 0 0 0)
set dh_end = ( 0 0 0 0 0 0)

foreach i ($DOMS)

 if ( $fcst_id == 3) then
  set dom_end_date=$start_date
  set dom_end_date = `${EXECUTABLE_ARCHIVE}/geth_newdate.exe $dom_end_date $DOM_LENGTH[$i]`
 else
  set dom_end_date = $end_date
 endif

 if ($i == 1) then
    set domenddates = $dom_end_date
    set domfcstlens = $DOM_LENGTH[$i]
    set domoutintvs = $out_intv[$i]
 else
    set domenddates = $domenddates,$dom_end_date
    set domfcstlens = $domfcstlens,$DOM_LENGTH[$i]
    set domoutintvs = $domoutintvs,$out_intv[$i]
 endif

 set dy_end[$i] = `echo $dom_end_date | cut -b 1-4`
 set dm_end[$i] = `echo $dom_end_date | cut -b 5-6`
 set dd_end[$i] = `echo $dom_end_date | cut -b 7-8`
 set dh_end[$i] = `echo $dom_end_date | cut -b 9-10`

 echo "Done with domain $i settings: length $DOM_LENGTH[$i] hours"

end

set NODEMEM = ""
set ETKFMEM = "NO"
set ONEFILE = `pwd`
if(! $?ENSEMBLE ) set ENSEMBLE = "no"
echo "ENSEMBLE = ${ENSEMBLE}"
if(! $?ETKF ) set ETKF = "no"
echo "ETKF = ${ETKF}"
if(! $?MPPJOB ) set MPPJOB = "no"
echo "MPPJOB = ${MPPJOB}"
if(! $?CYC_INT) set CYC_INT = 3

set OUTTAG = $MM5HOST

if( $ENSEMBLE == "yes") then
  foreach i (15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50)
   @ ip = $i + 3
   @ iq = $i + 6
   @ is = $i + 4
   @ ie = $i + 8
   set ccy4 = `echo $ONEFILE |cut -c $i-$ip`
   set ccy3 = `echo $ONEFILE |cut -c $is-$iq`
   set ccy5 = `echo $ONEFILE |cut -c $is-$ie`
   if($ccy4 == "NAM_" || $ccy4 == "GFS_" || $ccy4 == "ECM_")   then
    set NAMGFS  = $ccy4
    set NODEMEM = $ccy5
    if($ccy3 == "MET") set ETKFMEM = "YES"
   endif
  end
  set OUTTAG = $NAMGFS$NODEMEM
endif

#	Bring wrf namelist and executables over that we need

 if(-e $GSJOBDIR/namelists/wrf.nl.template.$NODE) then
  cp $GSJOBDIR/namelists/wrf.nl.template.$NODE wrf.nl
  echo "${SUBSYSTEM} -- Using $GSJOBDIR/namelists/wrf.nl.template.$NODE"
 else if ( -e $GSJOBDIR/namelists/wrf.nl.template) then
  cp $GSJOBDIR/namelists/wrf.nl.template wrf.nl
  echo "${SUBSYSTEM} -- Using $GSJOBDIR/namelists/wrf.nl.template"
 else
  echo "${SUBSYSTEM} -- Missing wrf.nl -> exiting"
  exit (4)
 endif

 if ($MPPJOB == "no") then
  if(-e $GSJOBDIR/executables/wrf.exe.$NODE) then
   cp $GSJOBDIR/executables/wrf.exe.$NODE wrf.exe
   echo "${SUBSYSTEM} -- Using $GSJOBDIR/executables/wrf.exe.$NODE"
  else if ( -e $GSJOBDIR/executables/wrf.exe) then
   cp $GSJOBDIR/executables/wrf.exe wrf.exe
   echo "${SUBSYSTEM} -- Using $GSJOBDIR/executables/wrf.exe"
  else if ( -e ${WRF_EXE}) then
   cp ${WRF_EXE} wrf.exe 
   echo "${SUBSYSTEM} -- Using ${WRF_EXE}"
  else
   echo "${SUBSYSTEM} -- Missing wrf.executable --  exiting"
   exit (4)
  endif
 else 
  if(-e $GSJOBDIR/executables/wrf.mpich.$NODE) then
   cp $GSJOBDIR/executables/wrf.mpich.$NODE wrf.mpich
   echo "${SUBSYSTEM} -- Using $GSJOBDIR/executables/wrf.mpich.$NODE"
  else if ( -e $GSJOBDIR/executables/wrf.mpich) then
   cp $GSJOBDIR/executables/wrf.mpich wrf.mpich
   echo "${SUBSYSTEM} -- Using $GSJOBDIR/executables/wrf.mpich"
  else if ( -e ${WRF_MPICH}) then
   cp ${WRF_MPICH} wrf.mpich 
   echo "${SUBSYSTEM} -- Using ${WRF_MPICH}"
  else
   echo "${SUBSYSTEM} -- Missing wrf.executable --  exiting"
   exit (4)
  endif
 endif


if ( $fcst_id == 1 ) then          # cold start for Final FDDA
 set ifrest = "FALSE"
 @ savefrq  = $FCST_H * 60
else if ( $fcst_id == 2 ) then      # Normal Final FDDA
 set ifrest = "TRUE"
 @ savefrq  = $FCST_H * 60
else if ( $fcst_id == 3 ) then      # Primary FDDA + FCST
 set ifrest = "TRUE"
 set savefrq  = 44600                # No savefile for restart is needed
endif

set metl  = 27
if($NAMGFS == ETA || $NAMGFS == NAM) set metl = 40
if($NAMGFS == CFSV1 || $NAMGFS == CFSV2 || $NAMGFS == CFSR || $NAMGFS == CFSF)  set metl = 38

# build the wrf namelist

echo "Start: $y_start $m_start $d_start $h_start"
echo "End & NUM_DOMS: $y_end $m_end $d_end $h_end $NUM_DOMS"
echo "  $FCST_H $savefrq $ifrest $out_int $d4y_end $d4m_end $d4d_end $d4h_end"

set NDOM = $NUM_DOMS

if ($NAMGFS == CFSV1 || $NAMGFS == CFSV2 || $NAMGFS == CFSR || $NAMGFS == CFSF) then

elo " build wrf namelist with python  program ..."
echo "$NDOM  $start_date  $end_date  $FCST_H  $ifrest  $savefrq  $out_int  $metl"
set tmpnml = './wrf.nl'
echo "template nml file:  $tmpnml"

$PYTHON_ARCHIVE/tools/updatewrfnml.py -i $tmpnml -N $NDOM -b $start_date -f $domfcstlens
$PYTHON_ARCHIVE/tools/updatewrfnml.py -s 'time_control' -k 'run_hours' -v $FCST_H
$PYTHON_ARCHIVE/tools/updatewrfnml.py -s 'time_control' -k 'restart' -v $ifrest
$PYTHON_ARCHIVE/tools/updatewrfnml.py -s 'time_control' -k 'restart_interval' -v $savefrq
$PYTHON_ARCHIVE/tools/updatewrfnml.py -s 'time_control' -k 'history_interval' -v $domoutintvs
$PYTHON_ARCHIVE/tools/updatewrfnml.py -s 'domains' -k 'num_metgrid_levels' -v $metl
$PYTHON_ARCHIVE/tools/updatewrfnml.py -o './wrf.nl'
if (-e nml.pkl) then
   rm -f nml.pkl
endif

else # CFS

ed wrf.nl << EOF > /dev/null
g/SYY/s//$y_start/g
g/SMM/s//$m_start/g
g/SDD/s//$d_start/g
g/SHH/s//$h_start/g
g/EYY/s//$y_end/g
g/EMM/s//$m_end/g
g/EDD/s//$d_end/g
g/EHH/s//$h_end/g
g/D4YY/s//$d4y_end/g
g/D4MM/s//$d4m_end/g
g/D4DD/s//$d4d_end/g
g/D4HH/s//$d4h_end/g
g/E1YY/s//$dy_end[1]/g
g/E1MM/s//$dm_end[1]/g
g/E1DD/s//$dd_end[1]/g
g/E1HH/s//$dh_end[1]/g
g/E2YY/s//$dy_end[2]/g
g/E2MM/s//$dm_end[2]/g
g/E2DD/s//$dd_end[2]/g
g/E2HH/s//$dh_end[2]/g
g/E3YY/s//$dy_end[3]/g
g/E3MM/s//$dm_end[3]/g
g/E3DD/s//$dd_end[3]/g
g/E3HH/s//$dh_end[3]/g
g/E4YY/s//$dy_end[4]/g
g/E4MM/s//$dm_end[4]/g
g/E4DD/s//$dd_end[4]/g
g/E4HH/s//$dh_end[4]/g
g/E5YY/s//$dy_end[5]/g
g/E5MM/s//$dm_end[5]/g
g/E5DD/s//$dd_end[5]/g
g/E5HH/s//$dh_end[5]/g
g/DOM/s//$NDOM/g
g/FCSTH/s//$FCST_H/g
g/SaveFrq/s//$savefrq/g
g/OUTINT/s//$out_int/g
g/OUT1INT/s//$out_intv[1]/g
g/OUT2INT/s//$out_intv[2]/g
g/OUT3INT/s//$out_intv[3]/g
g/OUT4INT/s//$out_intv[4]/g
g/OUT5INT/s//$out_intv[5]/g
g/IfRest/s//$ifrest/g
g/METL/s//$metl/g
w
q
EOF

endif # CFS

if ($?DUST_ONLY) then
  sed -i -e  's/auxinput5_interval_m.*/auxinput5_interval_m = 0,/' \
  -e 's/auxinput8_interval_m.*/auxinput8_interval_m = 0,/' \
  -e 's/io_form_auxinput5.*/io_form_auxinput5 = 0,/' \
  -e 's/io_form_auxinput8.*/io_form_auxinput8 = 0,/' \
  -e 's/frames_per_auxinput5.*/frames_per_auxinput5 = 0,/' \
  -e 's/frames_per_auxinput8.*/frames_per_auxinput8 = 0,/' \
  -e 's/emiss_inpt_opt.*/emiss_inpt_opt = 0,/' \
  -e 's/emiss_opt.*/emiss_opt = 0,/' \
  -e 's/io_style_emissions.*/io_style_emissions = 0,/' wrf.nl
endif

cp -f wrf.nl $CYCDIR/.
mv -f wrf.nl namelist.input
if ( -e $GSJOBDIR/namelists/wrf.nl.template.rda) then
  cp -p $GSJOBDIR/namelists/wrf.nl.template.rda namelist.input.rda
endif

set WRF_HOME = $MM5HOME/cycle_code
##yw begin added by Wei Yu
#yw if($?HYBRID) then
if("$tmp_work" == "WRF_F") then
   if(! $?DATADIR) set DATADIR = "$DATA_DIR"
   if(! $?RANGE) set RANGE = "$MM5HOST"
   if(! $?BATCH_SYS) set BATCH_SYS = "$BATCH_SYSTEM"

   set _this_script_name = "`basename $0`"
   if ( ! $?HYBRID ) set HYBRID = ""
   if ( ! $?RDA_HYBRID ) set RDA_HYBRID = ""

   if ( "$HYBRID" == "TRUE" ) then
      set _Hybrid3DVar_script = $WRF_HOME/HY3DVAR/scripts/hy3dvarDriver.pl
      if(-e "$_Hybrid3DVar_script") then
         echo "3DVAR analysis start ....."
         echo "perl $_Hybrid3DVar_script $this_cycle $CYC_INT  $RUNDIR $GSJOBDIR $RANGE $NUM_PROCS $DATADIR $BATCH_SYS $WRF_HOME $#DOMS"
         perl $_Hybrid3DVar_script $this_cycle $CYC_INT  $RUNDIR $GSJOBDIR $RANGE $NUM_PROCS $DATADIR $BATCH_SYS $WRF_HOME $#DOMS $fcst_id
      endif
   else if ( "$RDA_HYBRID" == "TRUE" ) then
      # Mei Xu  20101221
      # Note: Processing namelist.input is moved to RT_L_rda.csh and
      #       hy3dvarDriver.pl is called by get_and_decode_Radar_data at rda_processor.pm
      set _rda_script = "$CSH_ARCHIVE/rda/RT_L_rda.csh"
      if ( ! -e $_rda_script ) set _rda_script = "`dirname $0`/../rda/RT_L_rda.csh"
      if ( -e $_rda_script ) then
         set _rda_script_args = "$this_cycle $CYC_INT $CYCDIR $RUNDIR $GSJOBDIR $RANGE $NUM_PROCS $DATADIR $BATCH_SYS $WRF_HOME $#DOMS $fcst_id"
         set _rda_command = "$_rda_script $_rda_script_args"
         echo " ${_this_script_name}: $_rda_command"
         csh $_rda_command
      else
         echo "${_this_script_name}: INFO - Skip RDA task because [$_rda_script] does not exist." 
      endif
      ##MX end
   endif
endif
##yw end

#Now prepare data to run wrf

# link proper input and bc files for wrf runs 

foreach i ( $DOMS )
 if($cpun < 2) then
  ln -s $up_dir/${this_cycle}_wrfinput_d0${i}_cold wrfinput_d0$i
  if (-e $up_dir/${this_cycle}_wrffdda_d0${i}) then
   ln -s $up_dir/${this_cycle}_wrffdda_d0${i} wrffdda_d0${i}
  endif
 else
  ln -s $CYCDIR/${this_cycle}_wrfinput_d0${i}_cold wrfinput_d0$i
  if (-e $CYCDIR/${this_cycle}_wrffdda_d0${i}) then
   ln -s $CYCDIR/${this_cycle}_wrffdda_d0${i} wrffdda_d0${i}
  endif
 endif 
end
  ln -s $CYCDIR/${this_cycle}_wrfbdy_d01 wrfbdy_d01

# OBS nudging files 

foreach i ( $DOMS )
  set ii = 0
  foreach f  (`/bin/ls  $CYCDIR/*_s`)
  @ ii ++
  if($ii < 10) then
   #ln -f -s $f  MM5OBS_DOMAIN${i}0${ii}
   ln -f -s $f  OBS_DOMAIN${i}0${ii}
  else
   #ln -f -s $f  MM5OBS_DOMAIN${i}${ii}
   ln -f -s $f  OBS_DOMAIN${i}${ii}
  endif
  end
end

# Restart file

foreach i ( $DOMS )
 if ($fcst_id == 2) then 
   if($RESTART_PER_CORE) then
     ln -s $RUNDIR/${last_cycle}/WRF_F/r-0${i}-${y_start}-${m_start}-${d_start}_${h_start}:00:00_* .
   else
     ln -s $RUNDIR/${last_cycle}/WRF_F/wrfrst_d0${i}_${y_start}-$m_start-${d_start}_${h_start}:00:00 .
   endif
 else if ($fcst_id == 3) then
   if($RESTART_PER_CORE) then
     if ( -e $RUNDIR/${this_cycle}/WRF_F/r-0${i}-${y_start}-${m_start}-${d_start}_${h_start}:00:00_0000 ) then
       ln -s $RUNDIR/${this_cycle}/WRF_F/r-0${i}-${y_start}-${m_start}-${d_start}_${h_start}:00:00_* .
     else 
       echo "  === ERROR: restart file r-0${i}-${y_start}-${m_start}-${d_start}_${h_start}:00:00_0000 does not exist"
     endif
   else
     if ( -e $RUNDIR/${this_cycle}/WRF_F/wrfrst_d0${i}_${y_start}-$m_start-${d_start}_${h_start}:00:00 ) then 
       ln -s $RUNDIR/${this_cycle}/WRF_F/wrfrst_d0${i}_${y_start}-$m_start-${d_start}_${h_start}:00:00 .
     else
       echo "  === ERROR: restart file [${this_cycle}.SAVE_DOMAIN${i}] does not exist"
     endif
   endif
 endif
end

#ETKF Grid-nudging files

if($ETKFMEM == "YES") then
 foreach i ( $DOMS )
  if ( ( -l ETKFINPUT_DOMAIN$i ) || ( -e ETKFINPUT_DOMAIN$i ) ) ${RM} ETKFINPUT_DOMAIN$i
  if ( -l $RUNDIR/../etkf_out/${this_cycle}_ETKFINPUT_DOMAIN${i}.$NODE) then
    ln -s -f $RUNDIR/../etkf_out/${this_cycle}_ETKFINPUT_DOMAIN${i}.$NODE ETKFINPUT_DOMAIN$i
  endif
 end
endif

#	Run the wrf program

if( -e $GSJOBDIR/wrfrun.$NODE ) then
 echo "Using tables in $GSJOBDIR/wrfrun.$NODE"
 cp -r $GSJOBDIR/wrfrun.$NODE/* .
else if( -e $GSJOBDIR/wrfrun ) then
 echo "Using tables in $GSJOBDIR/wrfrun"
 cp -r $GSJOBDIR/wrfrun/* .
else
 echo "Using tables in ${CONSTANT_FILES}/wrfrun"
 cp ${CONSTANT_FILES}/wrfrun/* .
endif

if ( ! -e wrf.exe && ! -e wrf.mpich) then
   echo "${SUBSYSTEM} -- No WRF executable - wrf.exe or wrf.mpich exit..."
   exit
endif

if( -e wrf.exe ) then   
  #(ssh node$NODE "setenv OMP_NUM_THREADS $cpun;setenv MPSTKZ 64M;limit stacksize unlimited;cd $dir_work; ./wrf.exe " ) >&! wrf_print.out
  if ($BATCH_SYSTEM == "INTER") then 
    (ssh node$NODE "setenv OMP_NUM_THREADS $cpun; limit stacksize unlimited;cd $dir_work; ./wrf.exe " ) >&! wrf_print.out
  else if ($BATCH_SYSTEM == "PBS" || $BATCH_SYSTEM == "LSF") then
    setenv OMP_NUM_THREADS $cpun
    limit stacksize unlimited
    cd $dir_work
    ./wrf.exe  >&! wrf_print.out
  endif
else 
  rm rsl* show*;
  if ($BATCH_SYSTEM == "INTER") then
    if ($NODE == 0) then
      echo \
     "aprun -n $cpun -machinefile $GMPICONF wrf.mpich >&! wrf_print.out"
      $MPICMD_BIN_DIR/aprun -n $cpun -machinefile $GMPICONF ./wrf.mpich >&! wrf_print.out
    else
      #mpirun -np $cpun wrf.mpich >&! wrf_print.out 
      if(-e $GSJOBDIR/machinefile) then
        cp $GSJOBDIR/machinefile $RUNDIR/hosts
      endif
      echo \
     "aprun -np $cpun -machinefile $RUNDIR/hosts ./wrf.mpich >&! wrf_print.out"
      $MPICMD_BIN_DIR/aprun -np $cpun -machinefile $RUNDIR/hosts ./wrf.mpich >&! wrf_print.out
    endif
  else if ($BATCH_SYSTEM == "PBS") then
    # run it using mpiexec-0.80:
    # per mpiexec-0.80's man page:
    #   Run the executable a.out as a parallel mpi code on each process allocated by pbs
    #   mpiexec a.out
    ### The PBS directive: we may not need it here.
    #PBS -l nodes=${NUM_NODES}:ppn=${PPN}
    echo "aprun -n $cpun ./wrf.mpich >&! wrf_print.out"
    limit stacksize unlimited
    $MPICMD_BIN_DIR/aprun -n $cpun ./wrf.mpich >&! wrf_print.out
  else if ($BATCH_SYSTEM == "SLURM") then
    echo "srun -n $cpun ./wrf.mpich >&! wrf_print.out"
    echo `ldd wrf.mpich`
    #$MPICMD_BIN_DIR/srun -n $cpun  --hint=nomultithread ./wrf.mpich >&! wrf_print.out
    module swap PrgEnv-cray PrgEnv-intel
    module list
    $MPICMD_BIN_DIR/srun --hint=nomultithread ./wrf.mpich >& wrf_print.out
  else if ($BATCH_SYSTEM == "LSF") then
     $GSJOBDIR/mpirun.lsf ./wrf.mpich  #>&! wrf_print.out
     exit (0)
  else
     echo
     echo "ERROR: Unknown value BATCH_SYSTEM = $BATCH_SYSTEM!"
     echo "       BATCH_SYSTEM must be PBS, LSF or INTER (interactive)"
     echo
  endif
endif

if (! $?MM || $MM != "MM") then
# In MM we are doing the moving and cleaning in post_process_clean.pl

#
# Check if failed
#
set error_status = 0
set CFL_ERROR_MSG = "exceeded cfl"
set SEG_FAULT_MSG = "Segmentation fault"
set BASETIME_MSG  = "normalize_basetime"

set seg_fault_count  =  `grep -i "$SEG_FAULT_MSG" rsl.error.* | wc -l`
set seg_fault_count2 =  `grep -i "$SEG_FAULT_MSG" wrf_print.out | wc -l`
set basetime_count   =  `grep -i "$BASETIME_MSG" rsl.error.* | wc -l`
set cfl_error_count  =  `grep "$CFL_ERROR_MSG" rsl.error.* | wc -l`
if ( $seg_fault_count > 0 || $seg_fault_count2 > 0 || $basetime_count > 0 ) then
  if ( $seg_fault_count > 0 || $seg_fault_count2 > 0 ) then
     set error_status = 1
     set MSG = $SEG_FAULT_MSG
  else
     set error_status = 2
     set MSG = $BASETIME_MSG
  endif

  echo ""
  echo "  ===== ERROR: Found $MSG ====="
  if ( $seg_fault_count > 0 ) then
     grep -i "$SEG_FAULT_MSG" rsl.error.*
  else if ( $seg_fault_count2 > 0 ) then
     grep -i "$SEG_FAULT_MSG" wrf_print.out
  else
     grep -i "$BASETIME_MSG" rsl.error.*
  endif
  echo ""

  set wrf_count = `ls -1 wrfout_d01* | wc -l`
  if ( $wrf_count == 1 ) then
#    Rerun wrf when only initial wrfout files are written and rsl* having segfaults
     echo "Rerun WRF..."
     rm rsl* wrfout* fort.???
     sleep 60
     if ($BATCH_SYSTEM == "INTER") then
       if ($NODE == 0) then
         echo \
        "$MPICMD_BIN_DIR/mpirun -np $cpun -machinefile $GMPICONF ./wrf.mpich >&! wrf_print.out"
         $MPICMD_BIN_DIR/mpirun -np $cpun -machinefile $GMPICONF ./wrf.mpich >&! wrf_print.out
       else
         if(-e $GSJOBDIR/machinefile) then
           cp $GSJOBDIR/machinefile $RUNDIR/hosts
         endif
         echo \
        "$MPICMD_BIN_DIR/mpirun -np $cpun -machinefile $RUNDIR/hosts ./wrf.mpich >&! wrf_print.out"
         $MPICMD_BIN_DIR/mpirun -np $cpun -machinefile $RUNDIR/hosts ./wrf.mpich >&! wrf_print.out
       endif
     else if ($BATCH_SYSTEM == "PBS") then
       echo \
      "$MPICMD_BIN_DIR/mpiexec /bin/csh -c 'limit stacksize unlimited; ./wrf.mpich' >&! wrf_print.out"
       $MPICMD_BIN_DIR/mpiexec /bin/csh -c "limit stacksize unlimited; ./wrf.mpich" >&! wrf_print.out
     else if ($BATCH_SYSTEM == "LSF") then
        $GSJOBDIR/mpirun.lsf ./wrf.mpich  #>&! wrf_print.out
        exit (0)
     endif
  endif
endif
if ( $cfl_error_count > 0 ) then
  set error_status = `echo $error_status + 10 | bc`
  echo ""
  echo "  ===== ERROR: cfl error ====="
  grep "$CFL_ERROR_MSG" rsl.error.*
  echo ""
endif

if ( $error_status == 0 ) then
  echo ""
  echo " ******************* No major problems *******************"
  set rsl_error_file_count = `ls rsl.error.* | wc -w`
  set completed_count = `grep "SUCCESS COMPLETE" rsl.error.* | wc -l`
  if ( $completed_count < $rsl_error_file_count ) then
    echo " ***** INFO $completed_count tasks (out of $rsl_error_file_count) were completed successfully ****"
  endif
  echo ""
endif

#set ncdump_cmd = ""
#if ( $?NETCDF ) then
#  set ncdump_cmd = "$NETCDF/bin/ncdump"
#else
#  set ncdump_cmd = "/opt/netcdf/bin/ncdump"
#endif

# Output 
@ HOURLY_OUT =  60 / $OUT_INT
foreach d ( $DOMS )

  if( ($fcst_id == 1) || ($fcst_id == 2) ) then
    set cnt = 0
    set cnt2 = 0
    foreach f (`/bin/ls wrfout_d0{$d}*`)
     @ cnt ++
     if( $cnt == $HOURLY_OUT ) then
     ## keep hourly D1 analysis only at GEAPSTMP directory (INTER only)
     #if ($BATCH_SYSTEM == "INTER") then 
     #if( -d $GEAPSTMP && $d == 1 ) cp $f $GEAPSTMP/ 
     #endif
      set cnt = 0
      @ cnt2 ++
      if( $cnt2 == 3 && $ETKF == "yes" && $NODEMEM == "CTRL") then 
       cp $f $RUNDIR/../etkf_in/${this_cycle}_final$OUTTAG.WRFOUT_DOMAIN${d}
      endif
     endif
     echo \
    "mv  $f $CYCDIR/${f}.${OUTTAG}_F"   #save all final analysis for all domains
     mv  $f $CYCDIR/${f}.${OUTTAG}_F    #save all final analysis for all domains 
     ln -s $CYCDIR/${f}.${OUTTAG}_F $f  #preserve a soft link of the file

    end
    
# shouldn't need to do this ...
#    if ( $error_status > 0 ) then
#      if (-e "$ncdump_cmd" ) then
#        # Remove the wrfout with NaN
#        foreach f (`/bin/ls wrfout_d03* wrfout_d04*`)
#          set nan_count = `$ncdump_cmd $f | grep NaNf | wc -w`
#          #if ( $nan_count > 0 ) then
#          if ( $nan_count > 10 ) then
#            mv $CYCDIR/${f}.${OUTTAG}_F $CYCDIR/${f}.${OUTTAG}_F.bad
#            rm $f       #remove a soft link of the file
#          endif
#        end
#      endif
#    endif
    
  else if ($fcst_id == 3) then          
    set cnt = 0
    set cnt1 = 0
    set cnt2 = 0
    # keep hourly fcsts only
    foreach f (`/bin/ls wrfout_d0{$d}*`)
     @ cnt ++
     if( $cnt == $HOURLY_OUT ) then
     ## keep hourly D1 FCST only at /d1/GEAPSTMP with short cut-off (INTER only)
     #if ($BATCH_SYSTEM == "INTER") then 
     #if( -d $GEAPSTMP && $d == 1 && $cnt1 < $CYC_INT + 2 ) cp $f $GEAPSTMP/ 
     #endif
      set cnt = 0
      @ cnt1 ++
      @ cnt2 ++
     endif
     #if( $ETKF == "yes" &&  ($ETKFMEM == "YES" || $NODEMEM == "CRTL")) then
     if( $ETKF == "yes" ) then
     if( $cnt2 == 6 ) cp $f $RUNDIR/../etkf_in/${this_cycle}_06hfcst$OUTTAG.WRFOUT_DOMAIN${d}
     #if( $cnt2 == 12) cp $f $RUNDIR/../etkf_in/${this_cycle}_12hfcst$OUTTAG.WRFOUT_DOMAIN${d}
     endif
     echo \
    "mv $f $CYCDIR/${f}.${OUTTAG}_P+FCST" #all domains all time 
     mv $f $CYCDIR/${f}.${OUTTAG}_P+FCST  #all domains all time 

     ln -s $CYCDIR/${f}.${OUTTAG}_P+FCST $f #preserve a soft link of the file

    end
  endif
end

  ls -l >> wrf_print.out
  echo "  --------------- wrf namelist------------ " >> wrf_print.out
  cat namelist.input >> wrf_print.out
  echo "  --------------- OBS   ------------------ " >> wrf_print.out
  cat obs_* >> wrf_print.out
  echo "  --------------- PRINT OUT   ------------------ " >> wrf_print.out
  cat rsl.out.0000 >> wrf_print.out

# Move the print out from the workdir to the cycle dir
  cp -f wrf_print.out $CYCDIR/wrf_print_${tmp_work}.out

else
 set here = `pwd`
 echo
 echo "Content of run directory ${here}:"
 ls -alh
 echo

endif

# Clean up

if ($BATCH_SYSTEM != "INTER") then 

 cd $CYCDIR

 if($cpun > 1 && $CLEAN_GEAPSTMP > 0 && -e $GEAPSTMP) then
     echo
     echo "Removing  $GEAPSTMP"
     rm -rf $GEAPSTMP
 endif

 if($cpun > 1 && $CLEAN_GEAPSKEP > 0 && -e $GEAPSKEP) then
     echo
     echo "Removing  $GEAPSKEP"
     rm -rf $GEAPSKEP
 endif

endif

exit ( 0 ) 

