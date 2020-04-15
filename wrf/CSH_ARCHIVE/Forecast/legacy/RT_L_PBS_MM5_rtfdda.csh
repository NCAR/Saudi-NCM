#!/bin/csh -f

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

# $CSHRC_RT is $GSJOBDIR/tmp/$this_cycle/cshrc
echo sourcing $CSHRC_RT
source $CSHRC_RT

# $CSHRC_WRF is RUNDIR/$this_cycle/cshrc.MM5_F or cshrc.MM5_P+FCST
echo sourcing $CSHRC_MM5
source $CSHRC_MM5

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

echo "  PBS_JOBID: $PBS_JOBID"
echo "  Allocated nodes are: "
cat $PBS_NODEFILE
echo ""
#PBS -l nodes=${NUM_NODES}:ppn=${PPN}

#MPICH path is set in fddasys-PATH
#set MPICH = /opt/pgi/linux86/5.1
#set PATH=$MPICH/bin:$PATH

#
# ENVIRONMENT
#
set CFILE="$MM5HOME/cycle_code/CONFIG_FILES/cshrc_"

setenv

$CheckConfigFiles
set cfstat = $status
if ( $cfstat != 0 ) then
 echo "${SUBSYSTEM} -- Missing ConfigFile -> exiting"
 exit (2)
endif

source ${CFILE}user.mm5sys.${MM5HOST};
source ${CFILE}sizes.mm5sys.${MM5HOST}

set this_cycle = $this_cycle
set fcst_id    = $fcst_id
set timemax    = $timemax
set resttime   = $resttime
set cpun       = $NUM_PROCS
set d4start    = $D4_start
set out_int    = $OUT_INT
set NODE       = $NODE
set ETAAVN     = $BCS

if ( ! $?D4_LENGTH) then
setenv D4_LENGTH 13
endif

if ( $fcst_id == 3) then
@ d4end = $resttime + $D4_LENGTH * 60
else
set d4end = 44640
endif

#set aa = `echo $NODE |cut -c -1`
#set EXP = ""
#if( $aa == "0") then
#echo $aa
#set EXP = `echo $NODE |cut -c 2-`
#set NODE = $aa
#echo "NODE=$NODE  EXP=$EXP "
#endif

#	Get to the right directory

set sdir = $RUNDIR/$this_cycle

if ( ($fcst_id == 1) || ($fcst_id == 2)) then   # Final FDDA cycle
  set tmp_work = MM5_F
else
  set tmp_work = MM5_P
endif
set dir_work = $sdir/$tmp_work
$MustHaveDir $dir_work

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

 if ($MPPJOB == "no" || $cpun <= 2) then
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

 if( $cpun > 2) then
  set in_dir = $sdir
 else
  set in_dir = $up_dir
 endif
 if ( ( -l BDYOUT_DOMAIN1 ) || ( -e BDYOUT_DOMAIN1) ) ${RM} BDYOUT_DOMAIN1
 ln -s $in_dir/${this_cycle}_BDYOUT.${MM5HOST}          BDYOUT_DOMAIN1
 set ok = $status
 if ( $ok != 0 ) then
	cp $in_dir/${this_cycle}_BDYOUT          BDYOUT_DOMAIN1
 endif

foreach i ( $DOMS )
 if ( ( -l MMINPUT_DOMAIN$i ) || ( -e MMINPUT_DOMAIN$i ) ) ${RM} MMINPUT_DOMAIN$i
 if ( ( -l LOWBDY_DOMAIN$i  ) || ( -e LOWBDY_DOMAIN$i  ) ) ${RM} LOWBDY_DOMAIN$i
 ln -s $in_dir/${this_cycle}_MMINPUT_DOMAIN${i}.${MM5HOST} MMINPUT_DOMAIN$i
 set ok = $status
 if ( $ok != 0 ) then
        cp $up_dir/${this_cycle}_MMINPUT_DOMAIN${i}.${MM5HOST} MMINPUT_DOMAIN${i}
 endif
 ln -s $in_dir/${this_cycle}_LOWBDY_DOMAIN${i}.${MM5HOST} LOWBDY_DOMAIN${i}
 set ok = $status
 if ( $ok != 0 ) then
        cp $in_dir/${this_cycle}_LOWBDY_DOMAIN${i}.${MM5HOST} LOWBDY_DOMAIN${i}
 endif
end

# OBS nudging files
#if ( ($fcst_id == 1) || ($fcst_id == 2) ) then  # for testing, remove prelim
foreach i ( $DOMS )
  set ii = 0
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

if ( $ETAAVN != "ETA" ) then
 if ( $ETAAVN != "AVNFTP" || $this_cycle < 2005053112) then
  #AVN started using Noah 4-layer LSM on 2005053112
ed mmlif << EOF > /dev/null
g/ISTLYR = 10, 40, 100, 200/s//ISTLYR = 10, 200,0,0/
g/ISMLYR = 10, 40, 100, 200/s//ISMLYR = 10, 200,0,0/
g/ISTLYR = 10,40,100,200/s//ISTLYR = 10, 200,0,0/
g/ISMLYR = 10,40,100,200/s//ISMLYR = 10, 200,0,0/
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
  (ssh node$NODE "setenv OMP_NUM_THREADS $cpun; limit stacksize unlimited;cd $dir_work; ./mm5.exe " ) >! mm5_print.out
else
  ln -s -f $RUNDIR/restart_files restrts
  rm rsl* show*;
  cd $dir_work

 # run it using mpiexec-0.80:
 # per mpiexec-0.80's man page:
 # Run the executable a.out as a parallel mpi code on each process allocated by pbs
 # mpiexec a.out
  $MPICMD_BIN_DIR/mpiexec mm5.mpich >>& mm5_print.out
endif

#	Move the important files around


@ HOURLY_OUT =  60 / $out_int
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

  if ( ($fcst_id == 1) || ($fcst_id == 2) ) then
    # save Final analysis for all domains all times? ----
    # cat MMOUT_DOMAIN${d}* > $sdir/$fn
    set cnt = 0
    foreach f (`/bin/ls MMOUT_DOMAIN${d}*`)
      @ cnt ++
      if ( $cnt == $HOURLY_OUT ) then
        set valid_time = `$EXECUTABLE_ARCHIVE/read_mm5_validtime.exe $f`
	mv $f $sdir/${valid_time}_MMOUTPUT_DOMAIN${d}.${MM5HOST}_F
        #cat $f >> $sdir/$fn    #save HOURly Final analysis for all domains  ----
        #if ( -d $GEAPSTMP && $d == 1 ) "cat $f >> $up_dir/$fn"
        set cnt = 0
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
    set cnt3 = 0

    # keep hourly fcsts only
    foreach f (`/bin/ls MMOUT_DOMAIN${d}*`)
     @ cnt ++
     @ cnt1 ++
     @ cnt3 ++
     if( ($cnt1 == $HOURLY_OUT && $cnt <= 24) || ($cnt3 == 3 && $cnt > 24)) then
      cat $f | head -210m >> $sdir/$fn  #all domains on /data
      # keep hourly D1 FCST only at /d1/GEAPSTMP with short cut-off
      if( -d $GEAPSTMP && $d == 1 && $cnt < $CYC_INT + 2 ) "cat $f >> $up_dir/$fn"
      set cnt1 = 0
      set cnt3 = 0
     endif
    end
  endif
end

  ls -l >> mm5_print.out
  echo " \n\n --------------- MMLIF ------------------ \n\n"
  cat mmlif >> mm5_print.out
  echo " \n --------------- OBS   ------------------ \n\n"
  cat obs_* >> mm5_print.out
  cp mm5_print.out $sdir/${this_cycle}_mm5_p+fcst_print.out

  if( -d $GEAPSTMP) then
	ln -s $dir_work $GEAPSTMP/$tmp_work
  endif

exit ( 0 )

#	Clean up
