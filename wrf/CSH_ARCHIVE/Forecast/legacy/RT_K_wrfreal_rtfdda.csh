#!/bin/csh -f

###############################################################################
echo  ""
echo  " ----------------------------------------------------------------------"
echo  " -------------- WRF REAL to get WRF IC and BC--------------------------"
echo  "$0 $argv[*]"
echo  " ----------------------------------------------------------------------"
#     " ------------------Yubao Liu 2005.2 -----------------------------------"
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


if ( ${#argv} != 6) then
	echo "usage: wrf.csh this_cycle start_date end_date NODE normal"
	echo "where start_date, end_date and this_cycle is ccyymmddhh"
	echo "NODE: 0 -mpp; 1 - 31 running omp on node"
	echo "normal=1: restart "
	exit ( 1 )
endif

set this_cycle = $1
set start_date = $2
set end_date   = $3
set NODE       = $4
set normal     = $5
set cpun       = $6

set y_start = `echo $start_date | cut -b 1-4`
set m_start = `echo $start_date | cut -b 5-6`
set d_start = `echo $start_date | cut -b 7-8`
set h_start = `echo $start_date | cut -b 9-10`

set y_end = `echo  $end_date | cut -b 1-4`
set m_end = `echo  $end_date | cut -b 5-6`
set d_end = `echo  $end_date | cut -b 7-8`
set h_end = `echo  $end_date | cut -b 9-10`

set d4end_date = $end_date
set d4y_end = `echo $d4end_date | cut -b 1-4`
set d4m_end = `echo $d4end_date | cut -b 5-6`
set d4d_end = `echo $d4end_date | cut -b 7-8`
set d4h_end = `echo $d4end_date | cut -b 9-10`

set CYCDIR = $RUNDIR/$this_cycle
cd $CYCDIR

set dir_work = $CYCDIR/WRF_REAL

if(-d $GEAPSTMP/1) then # $GEAPSTMP/1: mpirun does not like local disk
 rm -r $GEAPSTMP/WRF_REAL
 $MustHaveDir $GEAPSTMP/WRF_REAL
 ln -s -f $GEAPSTMP/WRF_REAL $dir_work
else
 $MustHaveDir $dir_work
endif

cd $dir_work

#	Bring wrf/real namelist over that we need

if( $NODE == 0 ) then
 if( -e $GSJOBDIR/namelists/wrf.nl.template ) then
  cp $GSJOBDIR/namelists/wrf.nl.template wrf.nl
  echo "${SUBSYSTEM} -- Using $GSJOBDIR/namelists/wrf.nl.template
 else
  echo "${SUBSYSTEM} -- Missing wrf.nl -> exiting"
  exit (3)
 endif
else
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
endif

 cp $GSJOBDIR/wrfrun/gribmap.txt .

 set ifrest = "FALSE"
 set savefrq  = 44600                # No savefile for restart is needed
 set FCST_LENGTH=1000                # No savefile for restart is needed
 set out_int = 60                    # No savefile for restart is needed

# build the wrf and real namelist

echo $y_start $m_start $d_start $h_start
echo $y_end $m_end $d_end $h_end $NUM_DOMS
echo $savefrq $ifrest $d4y_end $d4m_end $d4d_end $d4h_end

set NDOM = $NUM_DOMS
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
g/DOM/s//$NDOM/g
g/FCSTH/s//$FCST_LENGTH/g
g/SaveFrq/s//$savefrq/g
g/OUTINT/s//$out_int/g
g/IfRest/s//$ifrest/g
w
q
EOF

cp wrf.nl $CYCDIR/wrf.nl.real

# Run real to get ICs and BCs

setenv MOAD_DATAROOT $RUNDIR/$this_cycle/WRFSI
ls $MOAD_DATAROOT/siprd/wrf_real_input_*

rm wrf_real_input_*
cp `ls $MOAD_DATAROOT/siprd/wrf_real_input_em.d01.* | head -1` .
set atmp = `ls wrf_real_input_em.d01.*`

#create namelists for real for each domains
${EXECUTABLE_ARCHIVE}/wrf.nl.nest.pl

foreach d ( 05 04 03 02 )
 set btmp = `ls $MOAD_DATAROOT/siprd/wrf_real_input_em.d$d.* |head -1`
 cp namelist.input$d namelist.input
 echo $btmp
 if($btmp != "") then
  rm $atmp
  ln -s $btmp $atmp
  $GSJOBDIR/executables/real.exe
  #/opt/mpich/bin/mpirun -np $cpun -machinefile $RUNDIR/hosts $GSJOBDIR/executables/real.mpich >>& real.print.out
  if(-e wrfinput_d01) mv wrfinput_d01 wrfinput_d$d
 endif
end

#create real namelists for the coarse domain

rm wrf_real_input_*
foreach e ($MOAD_DATAROOT/siprd/wrf_real_input_em.d01*)
 ln -s $e .
end

cp ../wrf.nl.real namelist.input
 $GSJOBDIR/executables/real.exe
 # /opt/mpich/bin/mpirun -np $cpun -machinefile $RUNDIR/hosts $GSJOBDIR/executables/real.mpich >>& real.print.out

foreach i ( $DOMS )
 if($normal == 1) then
  mv wrfinput_d0$i $CYCDIR/${this_cycle}_wrfinput_d0$i
 else
  #cp  wrfinput_d0$i $up_dir/${this_cycle}_wrfinput_d0${i}_cold #save a copy on local disk
  mv wrfinput_d0$i $CYCDIR/${this_cycle}_wrfinput_d0${i}_cold  #cold-start
 endif
 if (-e wrffdda_d0$i) then
     mv wrffdda_d0$i $CYCDIR/${this_cycle}_wrffdda_d0$i
 endif
end
  mv wrfbdy_d01 $CYCDIR/${this_cycle}_wrfbdy_d01

endif     #--- real ends

# real has been done.

#  Clean up
#  rm wrf_real_input_em*

exit ( 0 )

