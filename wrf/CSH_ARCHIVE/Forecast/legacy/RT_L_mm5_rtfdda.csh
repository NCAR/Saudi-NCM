#!/bin/csh -fv

###############################################################################
echo
echo  " ----------------------------------------------------------------------"
echo  " ---------------- MM5 FDDA and FCST start -----------------------------"
echo "$0 $argv[*]"
echo  " ----------------------------------------------------------------------"
###############################################################################

# set echo
set timestamp
setenv SUBSYSTEM MM5
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

echo "MPPJOB = ${MPPJOB}"

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

echo "MPPJOB = ${MPPJOB}"


if ( ${#argv} != 9) then
	echo "usage: mm5.csh this_cycle fcst_id timemax resttime cpun d4start out_int NODE ETA"
	echo "where start_date and this_cycle is ccyymmddhh"
	echo "fcst_id: = 1  Cold start; = 2 Restart for last cycle; = 3 restart in this cycle"
	echo "timemax and resttime are fcst and restart time in minutes"
	echo "cpun is the number of parallel processors"
	echo "d4start is the time to begin domain 4 (minutes)"
	echo "out_int is the time interval of MMOUT "
	echo "NODE: 0 -mpp; 1 - 31 running omp on node"
	echo "ETAAVN: ETA or AVN IC/BC"
	exit ( 1 )
endif

set this_cycle = $1
set fcst_id    = $2
set timemax    = $3
set resttime   = $4
set cpun       = $5
set d4start    = $6
set out_int    = $7
set NODE       = $8
set ETAAVN     = $9

# ES: These are MM env-vars. 
# BATCH_SYSTEM can be set to PBS, LSF, INTER. If it not set, default to INTER:
# MM is set to MM. If not set, default to "notMM"
if (! $?BATCH_SYSTEM) setenv BATCH_SYSTEM "INTER"
if (! $?MM ) setenv MM "notMM"

if ( ! $?D4_LENGTH) then
setenv D4_LENGTH 13
endif

if ( $fcst_id == 3) then
@ d4end = $resttime + $D4_LENGTH * 60
else
set d4end = 44640
endif

set NODEMEM = ""
set ETKFMEM = "NO"
set ONEFILE = `pwd`
if(! $?ETKF ) set ETKF = "no"
echo "MPPJOB = ${MPPJOB}"
if(! $?MPPJOB ) set MPPJOB = "no"
echo "MPPJOB = ${MPPJOB}"
if(! $?CYC_INT) set CYC_INT = 3
if( $ETKF == "yes") then
  foreach i (15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 )
   @ ip = $i + 3
   @ iq = $i + 5
   @ is = $i + 4
   @ ie = $i + 7
   set ccy4 = `echo $ONEFILE |cut -c $i-$ip`
   set ccy3 = `echo $ONEFILE |cut -c $is-$iq`
   set ccy5 = `echo $ONEFILE |cut -c $is-$ie`
   if($ccy4 == "ETA_" || $ccy4 == "AVN_" || $ccy4 == "ECM_")   then
    set NODEMEM = $ccy5
    if($ccy3 == "ET") set ETKFMEM = "YES"
   endif
  end
endif

#	Get to the right directory

set sdir = $RUNDIR/$this_cycle

if ( ($fcst_id == 1) || ($fcst_id == 2)) then   # Final FDDA cycle
set tmp_work = MM5_F
else
set tmp_work = MM5_P
endif
set dir_work = $sdir/$tmp_work
$MustHaveDir $dir_work

# use a separate common disk for etkf members' MM5 run
 if($ETKFMEM == "YES_NO") then
  rm -rf $dir_work /input1/$NODE/$tmp_work
  $MustHaveDir /input1/$NODE
  $MustHaveDir /input1/$NODE/$tmp_work
  ln -s /input1/$NODE/$tmp_work $dir_work
 endif
#

echo $dir_work 
cd  $dir_work
echo "Now working in  $cwd"


if(-d $GEAPSTMP) then
 rm -r $GEAPSTMP/$tmp_work
 ln -s $dir_work $GEAPSTMP/$tmp_work
 set up_dir = $GEAPSTMP
else
 set up_dir = $sdir
endif

#	Bring stuff over that we need

if( $NODE == 0 ) then
 if( -e $GSJOBDIR/namelists/MM5.namelist.template.V ) then
  cp $GSJOBDIR/namelists/MM5.namelist.template.V mmlif
  echo "${SUBSYSTEM} -- Using $GSJOBDIR/namelists/MM5.namelist.template.V"
 else if ( -e ${MM5_TEMPLATE}.V.${MM5HOST} ) then
  cp ${MM5_TEMPLATE}.V.${MM5HOST} mmlif
  echo "${SUBSYSTEM} -- Using ${MM5_TEMPLATE}.V.${MM5HOST}"
 else
  echo "${SUBSYSTEM} -- Missing mmlif -> exiting"
  exit (3)
 endif

 if ($ETAAVN ==  "ETA") then
   set BCIC = "" 
 else 
   set BCIC = "AVN"
 endif


 if(-e $GSJOBDIR/executables/mm5.mpich${cpun}$BCIC.$NODE) then
  cp $GSJOBDIR/executables/mm5.mpich${cpun}$BCIC.$NODE mm5.mpich
  echo "${SUBSYSTEM} -- Using $GSJOBDIR/executables/mm5.mpich${cpun}$BCIC.$NODE"
 else if(-e $GSJOBDIR/executables/mm5.mpich$cpun.$NODE) then
  cp $GSJOBDIR/executables/mm5.mpich$cpun.$NODE mm5.mpich
  echo "${SUBSYSTEM} -- Using $GSJOBDIR/executables/mm5.mpich$cpun.$NODE"
 else if ( -e $GSJOBDIR/executables/mm5.mpich${cpun}$BCIC ) then
  cp $GSJOBDIR/executables/mm5.mpich${cpun}$BCIC mm5.mpich
  echo "${SUBSYSTEM} -- Using $GSJOBDIR/executables/mm5.mpich${cpun}$BCIC"
 else if ( -e $GSJOBDIR/executables/mm5.mpich$cpun) then
  cp $GSJOBDIR/executables/mm5.mpich$cpun mm5.mpich
  echo "${SUBSYSTEM} -- Using $GSJOBDIR/executables/mm5.mpich$cpun"
 else if ( -e ${MM5_MPICH}${cpun}$BCIC.$NODE ) then
  cp ${MM5_MPICH}${cpun}$BCIC.$NODE mm5.mpich
  echo "${SUBSYSTEM} -- Using  ${MM5_MPICH}${cpun}$BCIC.$NODE"
 else if ( -e ${MM5_MPICH}${cpun}$BCIC ) then
  cp ${MM5_MPICH}${cpun}$BCIC mm5.mpich
  echo "${SUBSYSTEM} -- Using ${MM5_MPICH}${cpun}$BCIC"
 else
  echo "${SUBSYSTEM} -- Missing mm5.mpp executable --  exiting"
  exit (4)
 endif

else
 if(-e $GSJOBDIR/namelists/MM5.namelist.template.V.$NODE) then
  cp $GSJOBDIR/namelists/MM5.namelist.template.V.$NODE mmlif
  echo "${SUBSYSTEM} -- Using $GSJOBDIR/namelists/MM5.namelist.template.V.$NODE"
 else if ( -e $GSJOBDIR/namelists/MM5.namelist.template.V) then
  cp $GSJOBDIR/namelists/MM5.namelist.template.V mmlif
  echo "${SUBSYSTEM} -- Using $GSJOBDIR/namelists/MM5.namelist.template.V"
 else if ( -e ${MM5_TEMPLATE}.V.${MM5HOST}.$NODE) then
  cp ${MM5_TEMPLATE}.V.${MM5HOST}.$NODE mmlif
  echo "${SUBSYSTEM} -- Using ${MM5_TEMPLATE}.V.${MM5HOST}.$NODE"
 else if ( -e ${MM5_TEMPLATE}.V.${MM5HOST}) then
  cp ${MM5_TEMPLATE}.V.${MM5HOST} mmlif 
  echo "${SUBSYSTEM} -- Using ${MM5_TEMPLATE}.V.${MM5HOST}"
 else
  echo "${SUBSYSTEM} -- Missing mmlif -> exiting"
  exit (4)
 endif

echo "MPPJOB = ${MPPJOB}"
 if ( $MPPJOB == "no") then
  if(-e $GSJOBDIR/executables/mm5.exe.$NODE) then
   cp $GSJOBDIR/executables/mm5.exe.$NODE mm5.exe
   echo "${SUBSYSTEM} -- Using $GSJOBDIR/executables/mm5.exe.$NODE"
  else if ( -e $GSJOBDIR/executables/mm5.exe) then
   cp $GSJOBDIR/executables/mm5.exe mm5.exe
   echo "${SUBSYSTEM} -- Using $GSJOBDIR/executables/mm5.exe"
  else if ( -e ${MM5_EXE}.$NODE) then
   cp ${MM5_EXE}.$NODE mm5.exe
   echo "${SUBSYSTEM} -- Using  ${MM5_EXE}.$NODE"
  else if ( -e ${MM5_EXE}) then
   cp ${MM5_EXE} mm5.exe 
   echo "${SUBSYSTEM} -- Using ${MM5_EXE}"
  else
   echo "${SUBSYSTEM} -- Missing mm5.executable --  exiting"
   exit (4)
  endif
 else 
  if(-e $GSJOBDIR/executables/mm5.mpich$cpun.$NODE) then
   cp $GSJOBDIR/executables/mm5.mpich$cpun.$NODE mm5.mpich
   echo "${SUBSYSTEM} -- Using $GSJOBDIR/executables/mm5.mpich$cpun.$NODE"
  else if ( -e $GSJOBDIR/executables/mm5.mpich$cpun) then
   cp $GSJOBDIR/executables/mm5.mpich$cpun mm5.mpich
   echo "${SUBSYSTEM} -- Using $GSJOBDIR/executables/mm5.mpich$cpun"
  else if ( -e ${MM5_MPICH}$cpun.$NODE) then
   cp ${MM5_MPICH}$cpun.$NODE mm5.mpich
   echo "${SUBSYSTEM} -- Using  ${MM5_MPICH}$cpun.$NODE"
  else if ( -e ${MM5_MPICH}$cpun) then
   cp ${MM5_MPICH}$cpun mm5.mpich
   echo "${SUBSYSTEM} -- Using ${MM5_MPICH}$cpun"
  else
   echo "${SUBSYSTEM} -- Missing mm5.executable --  exiting"
   exit (4)
  endif
 endif
endif

if ( ( -l fort.8 ) || ( -e fort.8 ) ) ${RM} fort.8
ln -s $MM5_ehtran fort.8
set ok = $status
if ( $ok != 0 ) then
	cp $MM5_ehtran fort.8
endif

######## testing ##########################################
#cp /home/fddasys/TERRAIN* .

foreach d ( $DOMS )
if ( -e $GSJOBDIR/TERRAIN/TERRAIN_DOMAIN$d ) then
 ln -s $GSJOBDIR/TERRAIN/TERRAIN_DOMAIN$d TERRAIN_DOMAIN${d}
 echo "${SUBSYSTEM}  Using Terrain in $GSJOBDIR "
else if ( -e  $TERRAIN_DIR/Domain${d}_New_LU.V.${MM5HOST} ) then
 ln -s $TERRAIN_DIR/Domain${d}_New_LU.V.${MM5HOST} TERRAIN_DOMAIN${d}
 echo "${SUBSYSTEM}  Using Terrain in $TERRAIN_DIR"
else
 echo "${SUBSYSTEM}  Missing file -> NO terrain in $GSJOBDIR or $TERRAIN_DIR"
 exit (2)
endif
end

# Input files

 if ( ( -l BDYOUT_DOMAIN1 ) || ( -e BDYOUT_DOMAIN1) ) ${RM} BDYOUT_DOMAIN1
 if( -e $up_dir/${this_cycle}_BDYOUT.${MM5HOST} ) then
  ln -s -f $up_dir/${this_cycle}_BDYOUT.${MM5HOST}          BDYOUT_DOMAIN1
 else
  ln -s -f $sdir/${this_cycle}_BDYOUT.${MM5HOST}          BDYOUT_DOMAIN1
 endif

foreach i ( $DOMS )
 if ( ( -l MMINPUT_DOMAIN$i ) || ( -e MMINPUT_DOMAIN$i ) ) ${RM} MMINPUT_DOMAIN$i
 if ( ( -l LOWBDY_DOMAIN$i  ) || ( -e LOWBDY_DOMAIN$i  ) ) ${RM} LOWBDY_DOMAIN$i
 if ( -e $up_dir/${this_cycle}_MMINPUT_DOMAIN${i}.${MM5HOST}) then
  ln -s -f $up_dir/${this_cycle}_MMINPUT_DOMAIN${i}.${MM5HOST} MMINPUT_DOMAIN$i
 else
  ln -s -f $sdir/${this_cycle}_MMINPUT_DOMAIN${i}.${MM5HOST} MMINPUT_DOMAIN${i}
 endif
 if( -e $up_dir/${this_cycle}_LOWBDY_DOMAIN${i}.${MM5HOST}) then
  ln -s -f $up_dir/${this_cycle}_LOWBDY_DOMAIN${i}.${MM5HOST} LOWBDY_DOMAIN${i}
 else
  ln -s -f $sdir/${this_cycle}_LOWBDY_DOMAIN${i}.${MM5HOST} LOWBDY_DOMAIN${i}
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

# OBS nudging files 
#if ( ($fcst_id == 1) || ($fcst_id == 2) ) then  # for testing, remove prelim
foreach i ( $DOMS )
  set ii = 0
#   foreach f  (`/bin/ls  $up_dir/*_s`)
  foreach f  (`/bin/ls  $sdir/*_s`)
  @ ii ++
  if($ii < 10) then
   ln -f -s $f  MM5OBS_DOMAIN${i}0${ii}
  else
   ln -f -s $f  MM5OBS_DOMAIN${i}${ii}
  endif
  end
end
#endif

# Restart file
if( -e mm5.exe ) then
foreach i ( $DOMS )
 if ($fcst_id <= 2) then 
  touch $up_dir/${this_cycle}.SAVE_DOMAIN${i}_F
  ln -s $up_dir/${this_cycle}.SAVE_DOMAIN${i}_F SAVE_DOMAIN${i}
 endif
 if ( ( -l RESTART_DOMAIN${i}) || ( -e RESTART_DOMAIN${i} ) ) ${RM} RESTART_DOMAIN${i}
 if ($fcst_id == 2) then
  ln -s $up_dir/${this_cycle}.RESTART_DOMAIN${i} RESTART_DOMAIN${i}
 else if ( $fcst_id == 3 ) then
  ln -s $up_dir/${this_cycle}.SAVE_DOMAIN${i}_F RESTART_DOMAIN${i}
 endif
end
endif

if ( $fcst_id == 1 ) then          # cold start for Final FDDA
 set ifrest = FALSE
 set savefrq  = $timemax
else if ( $fcst_id == 2 ) then      # Normal Final FDDA
 set ifrest = TRUE
 set savefrq  = $timemax
else if ( $fcst_id == 3 ) then      # Primary FDDA + FCST
 set ifrest = TRUE
 set savefrq  = 44600                # No savefile for restart is needed
endif

##
##if ( $d4start == 0 ) then
	set ioverw = 1
##else
##	set ioverw = 0
##endif

#	Modify namelist

ed mmlif << EOF > /dev/null
g/OUT_INT/s//$out_int/g
g/D4Start/s//$d4start/g
g/D4End/s//$d4end/g
g/IoverW/s//$ioverw/g
g/TimeMax/s//$timemax/g
g/Ifrest/s//$ifrest/g
g/Resttime/s//$resttime/g
g/Savefrq/s//$savefrq/g
w
q
EOF

if ( $ETAAVN != "ETA") then
 if ( $ETAAVN != "AVNFTP" || $this_cycle < 2005053112) then 
    #AVN started using Noah 4-layer LSM on 2005053112
ed mmlif << EOF > /dev/null
g/ISTLYR = 10, 40, 100, 200/s//ISTLYR = 10, 200/
g/ISMLYR = 10, 40, 100, 200/s//ISMLYR = 10, 200/
g/ISTLYR = 10,40,100,200/s//ISTLYR = 10, 200/
g/ISMLYR = 10,40,100,200/s//ISMLYR = 10, 200/
w
q
EOF
 endif
endif

#	Run the program

cp ${CONSTANT_FILES}/MM53.6/*.TBL .
cp ${CONSTANT_FILES}/MM53.6/RRTM_DATA .

#echo "${SUBSYSTEM} -- Starting MM5 on $cpun processors"
#setenv MP_SET_NUMTHREADS $cpun
#setenv OMP_SET_NUMTHREADS $cpun
#setenv _DSM_PLACEMENT ROUND_ROBIN
#setenv _DSM_WAIT SPIN
#setenv OMP_DYNAMIC FALSE
#setenv MPC_GANG OFF

if ( ! -e mm5.exe && ! -e mm5.mpich) then
   echo "${SUBSYSTEM} -- No MM5 executable - $MM5_EXE "
endif

if( -e mm5.exe ) then   
 #(ssh node$NODE "setenv OMP_NUM_THREADS $cpun;setenv MPSTKZ 64M;limit stacksize unlimited;cd $dir_work; ./mm5.exe " ) >! mm5_print.out
 if ($BATCH_SYSTEM == "INTER") then
  (ssh node$NODE "setenv OMP_NUM_THREADS $cpun; limit stacksize unlimited;cd $dir_work; ./mm5.exe " ) >! mm5_print.out
 else if ($BATCH_SYSTEM == "PBS" || $BATCH_SYSTEM == "LSF") then
    setenv OMP_NUM_THREADS $cpun
    limit stacksize unlimited
    cd $dir_work
    ./mm5.exe  >! mm5_print.out
 endif
else 
 ln -s -f $RUNDIR/restart_files restrts
 rm rsl* show*;
 if ($BATCH_SYSTEM == "INTER") then
  if ($NODE == 0) then
   #mpirun -np $cpun mm5.mpich >>& mm5_print.out 
   $MPICMD_BIN_DIR/mpirun -np $cpun -machinefile $GMPICONF mm5.mpich >>& mm5_print.out
  else
    #mpirun -np $cpun mm5.mpich >>& mm5_print.out 
    if(-e $GSJOBDIR/machinefile) then
     cp $GSJOBDIR/machinefile $RUNDIR/hosts
    endif
    #/usr/local/mpich/bin/mpirun -np $cpun -machinefile $RUNDIR/hosts mm5.mpich >>& mm5_print.out
    #/usr/local/mpich/1.2.5..10/pgi/i686/bin/mpirun -np $cpun -machinefile $RUNDIR/hosts mm5.mpich >>& mm5_print.out
    #mpirun -np $cpun mm5.mpich >>& mm5_print.out 
    echo " RUNDIR = $RUNDIR"
    $MPICMD_BIN_DIR/mpirun -np $cpun -machinefile $RUNDIR/hosts mm5.mpich >>& mm5_print.out
    #/opt/mpich-gm.orig/bin/mpirun -np $cpun -machinefile $RUNDIR/hosts mm5.mpich >>& mm5_print.out
    #/opt/mpich/bin/mpirun -np $cpun -machinefile $GMPICONF mm5.mpich >>& mm5_print.out
  endif
 else if ($BATCH_SYSTEM == "PBS") then
    # run it using mpiexec-0.80:
    # per mpiexec-0.80's man page:
    #   Run the executable a.out as a parallel mpi code on each process allocated by pbs
    #   mpiexec a.out
    ### The PBS directive: we may not need it here.
    #PBS -l nodes=${NUM_NODES}:ppn=${PPN}
    $MPICMD_BIN_DIR/mpiexec mm5.mpich >>& mm5_print.out
 else if ($BATCH_SYSTEM == "LSF") then
     $GSJOBDIR/mpirun.lsf ./mm5.mpich  #>>&! mm5_print.out
     exit (0)
 else
     echo
     echo "ERROR: Unknown value BATCH_SYSTEM = $BATCH_SYSTEM!"
     echo "       BATCH_SYSTEM must be PBS, LSF or INTER (interactive)"
     echo
 endif 
endif

# /opt/scali/bin/mpimon -stdin 0 -verbose $MM5_EXE -- n2 2 n3 2 n4 2 n5 2 n6 2 n7 2 n8 2 >>& mm5_print.out
#rsh n2 "cd $dir_work; rm rsl* show*; /home/bourgeoi/mpich-1.2.0/bin/mpirun -np 14 -machinefile ~/hosts $MM5_MPICH >>& mm5_print.out"
#rsh node2 "cd $dir_work; rm rsl* show*; /usr/local/mpich-1.2.5-eth/bin/mpirun -np $cpun -machinefile $GMPICONF $MM5_MPICH >>& mm5_print.out"
##LPC##/opt/mpi/mpich-1.2..8/bin/mpirun -np $cpun $MM5_MPICH >>& mm5_print.out
#/opt/mpi/mpich-1.2..8_with_newer_gm/bin/mpirun -np $cpun mm5.mpich >>& mm5_print.out
#/usr/local/mpich/bin/mpirun -np $cpun mm5.mpich >>& mm5_print.out

if (! $?MM || $MM != "MM") then
# In MM we are doing the moving and cleaning in post_process_clean.pl

#	Move the important files around

## /home/mm5sys/bin/snuff ingest_mm5.pl
##sleep 5 


@ HOURLY_OUT =  60 / $out_int
@ HOURLY3_OUT =  180 / $out_int
foreach d ( $DOMS )
  if ( ($fcst_id == 1) || ($fcst_id == 2) ) then
   set fn=${this_cycle}_MMOUTPUT_DOMAIN${d}.${MM5HOST}_F
   set fnsave=${this_cycle}.SAVE_DOMAIN${d}_F
   set fnpr=${this_cycle}_mm5_f_print.out
  else if ( $fcst_id == 3 ) then
   set fn=${this_cycle}_MMOUTPUT_DOMAIN${d}.${MM5HOST}_P+FCST
   set fnsave=${this_cycle}.SAVE_DOMAIN${d}_P+FCST
   set fnpr=${this_cycle}_mm5_p+fcst_print.out
  endif

  if( ($fcst_id == 1) || ($fcst_id == 2) ) then
   # save Final analysis for all domains all times? ----
   # cat MMOUT_DOMAIN${d}* > $sdir/$fn    
    set cnt = 0
    set cnt2 = 0
    foreach f (`/bin/ls MMOUT_DOMAIN${d}*`)
     @ cnt ++
     if( $cnt == $HOURLY_OUT ) then
      cat $f >> $sdir/$fn    #save HOURly Final analysis for all domains  ----
      # keep hourly D1 analysis only at GEAPSTMP directory
      if( -d $GEAPSTMP && $d == 1 ) then
        cat $f >> $up_dir/$fn
      endif
      set cnt = 0
      @ cnt2 ++
      if( $ETKF == "yes" && $NODEMEM == "CTRL") then 
       set LAST_ANA_HR = $CYC_INT    
       if($fcst_id == 1) @ LAST_ANA_HR = $CYC_INT + 1    
       if( $cnt2 == $LAST_ANA_HR ) then 
        cp $f $RUNDIR/../etkf_in/${this_cycle}_final$ETAAVN$NODEMEM.MMOUT_DOMAIN${d}
       endif
      endif
     endif
    end
   # keep partial SAVE file on /data:
   if( -d $GEAPSTMP) then
     cat SAVE_DOMAIN${d} | head -1000 > $sdir/$fnsave 
     echo "GEAPSTMP $GEAPSTMP"
   else
     rm SAVE_DOMAIN${d}
   endif
  else if ($fcst_id == 3) then          
    set cnt = 0
    set cnt1 = 0
    set cnt2 = 0
    # keep hourly fcsts only
    foreach f (`/bin/ls MMOUT_DOMAIN${d}*`)
     @ cnt ++
     if( ($cnt == $HOURLY_OUT && $cnt1 <= 50) || ($cnt == $HOURLY3_OUT && $cnt1 > 50)) then
      #cat $f | head -210m >> $sdir/$fn  #all domains on /data
      cat $f >> $sdir/$fn  #all domains on /data
      # keep hourly D1 FCST only at /d1/GEAPSTMP with short cut-off 
      if( -d $GEAPSTMP && $d == 1 && $cnt1 < $CYC_INT + 2 ) then
       cat $f >> $up_dir/$fn
      endif
      set cnt = 0
      @ cnt1 ++
      @ cnt2 ++
     endif
     #if( $ETKF == "yes" &&  ($ETKFMEM == "YES" || $NODEMEM == "CRTL")) then
     if( $ETKF == "yes" ) then
      if( $cnt2 == 6 ) cp $f $RUNDIR/../etkf_in/${this_cycle}_06hfcst$ETAAVN$NODEMEM.MMOUT_DOMAIN${d}
#     if( $cnt2 == 12) cp $f $RUNDIR/../etkf_in/${this_cycle}_12hfcst$ETAAVN$NODEMEM.MMOUT_DOMAIN${d}
     endif
    end
   #cp SAVE_DOMAIN${d} $up_dir/$fnsave 
  endif
end

  ls -l >> mm5_print.out
  echo "  --------------- MMLIF ------------------ "
  cat mmlif >> mm5_print.out
  echo " -------------- OBS   ------------------ "
  cat obs_* >> mm5_print.out
  cp mm5_print.out $sdir/${this_cycle}_mm5_p+fcst_print.out

  # use a separate common disk for etkf members' MM5 run
  if($ETKFMEM == "YES_NO") then
    rm -rf $dir_work 
    $MustHaveDir $dir_work
    cp mm5_print.out mmlif mm5* rsl* mm5_print.out *TBL RRTM_DATA $dir_work/
    cd ../
    rm -rf /input1/$tmp_work
  endif
  #

# if( -d $GEAPSTMP) then
#  rm $GEAPSTMP/$tmp_work/MMOUT_DOMAIN*
#  rm $dir_work
#  mv $GEAPSTMP/$tmp_work $dir_work
#  ln -s $dir_work $GEAPSTMP/$tmp_work
# endif
endif

exit ( 0 ) 

#	Clean up
