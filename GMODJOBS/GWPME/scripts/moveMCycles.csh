#! /bin/csh -f
# set echo
###############################################################################
#
# Usage: moveMcycles.csh -b begin_cycle -e end_cycle -t output_type -d domId
#                        -f env_var_file <-n no_move> <-h>
#
################################################################################

set interval = 0
set account = 0
set start = 0


#------------------------------------------------------------------------------#
if ($#argv > 0) then

# Arguments can be passed through the command line (interactive)
set argv=`getopt b:e:i:s:f:d:n:t:h $*`

#set output_type = wrfout
#set begin_cycle = 0
#set end_cycle = 0
#set env_var_file = "./env_vars.csh"

set argsstr="$argv"

echo
echo "$0 $argsstr"
echo

foreach name ($argv)

  switch ($name)
            case -b:
               setenv begin_cycle $2
               breaksw
            case -d:
               setenv domId  $2
               breaksw
            case -e:
               setenv end_cycle $2
               breaksw
            case -f:
               setenv env_var_file $2
               breaksw
            case -i:
               set interval = $2
               breaksw
            case -n:
               if ($2 == "no_move") then
                   setenv no_move 1
               endif
               breaksw
            case -s:
               set start = $2
               breaksw
            case -t:
               set output_type = $2
               breaksw
            case -h:
               echo "$0 -b begin_cycle -e end_cycle -f env_var_file <-t output_type -d domId -n no_move>"
               echo " where  "
               echo "   -b begin_cycle: the first cycle, format is YYYYMMDDhh (required)"
               echo "   -e end_cycle: the last cycle, format is YYYYMMDDhh (required)"
#              echo "   -i interval: the interval between cycles (in hours),"
#              echo "                has to be the same as CYC_INT in flexinput.pl (required)"
#              echo "   -s start_time: when should Moab run the jobs, format is YYYYMMDDhhmm.ss (optional)"
               echo "   -f env_var_file:  e.g. /raidN/Logname/GMODJOBS/JOBID/scripts/env_vars.csh, defaults to ./env_vars.csh (required)"
               echo "   -t output_type: wrfout or qc_out (default wrfout)"
               echo "   -d domId: the domain ID (default all domains)"
               echo "   -n no_move:  Count but do not move the file"
               echo " "
               exit
               breaksw
            case --:
               breaksw
  endsw

  shift
end


endif

#------------------------------------------------------------------------------#
# Check required arguments (can be passed through environment under Torque)

if ($?begin_cycle < 1) then
    echo "ERROR in ${0}: begin_cycle cycle, eg: 1999090100, hasn't been defined"
    $0 -h
    exit -1
endif

if ($?end_cycle < 1) then
    echo "ERROR in ${0}: end_cycle, eg: 1999090900, hasn't been defined"
    $0 -h
    exit -1
endif

if ($?env_var_file < 1) then
    echo "ERROR in ${0}: env_var_file, eg: ./env_vars.csh, hasn't been defined"
    $0 -h
    exit -1
endif

# Fill with default values for no_move and output_type
if ($?no_move < 1) then
    setenv no_move 0
endif

if ($?output_type < 1) then
    set output_type = "wrfout"
endif

if ($output_type != "wrfout" && $output_type != "qc_out") then
    echo "ERROR in ${0}: output_type must be wrfout or qc_out"
    $0 -h
    exit -1
endif

# Source MM job main config file 

if (! -e $env_var_file) then
    echo "ERROR in ${0}: Cannot find env_var file ${env_var_file}"
    exit -1
endif

echo "Sourcing ${env_var_file}"
source ${env_var_file}

# Make sure OUTPUT_DIR has been defined in the MM job main config file

if (! $?OUTPUT_DIR) then
    echo "ERROR in ${0}: OUTPUT_DIR has not been defined in env_var file ${env_var_file}"
    exit -1
endif

# Source the environment file $GSJOBDIR/tmp/cshrc that has been created during WRF runs

if (! -e ${GSJOBDIR}/tmp/$begin_cycle/cshrc) then
    echo "WARNING in ${0}: Cannot find file ${GSJOBDIR}/tmp/$begin_cycle/cshrc"
    if (! -e ${GSJOBDIR}/tmp/$end_cycle/cshrc) then
        echo "ERROR in ${0}: Cannot find file ${GSJOBDIR}/tmp/$end_cycle/cshrc"
        exit -1
    else
        echo sourcing ${GSJOBDIR}/tmp/$end_cycle/cshrc
               source ${GSJOBDIR}/tmp/$end_cycle/cshrc
    endif
else
    echo sourcing ${GSJOBDIR}/tmp/$begin_cycle/cshrc
           source ${GSJOBDIR}/tmp/$begin_cycle/cshrc
endif

# Make sure MM5HOSTS has been et
if ($?MM5HOST < 1) then
    echo
    echo "ERROR: MM5HOST is undefined"
    echo "Set the variable, eg setenv MM5HOST GRM and rerun"
    exit -1
endif


# Grab The maximum number of domains from the $GSJOBDIR/tmp/cshrc file

if ($output_type == "wrfout") then 

    if ($?domId > 0) then
        set dom_min = $domId 
        set dom_max = $domId 
    else
        set dom_min = 1
        set dom_max = $NUM_DOMS
    endif


else
# For qc_out files, there's only one domain
    set dom_min = 1
    set dom_max = 1
endif

@ num_doms = $dom_max - $dom_min
@ num_doms = $num_doms + 1

#echo
#echo "dom_min = $dom_min"
#echo "dom_max = $dom_max"
#echo "num_doms = $num_doms"

#------------------------------------------------------------------------------#
# Overwrite debug option here (0 = normal, 1 = more prints-out)
# Warning this hardcoded value will overwrite the variable defined in flexinput

set DEBUG = 1

set argsstr = "moveCycles.csh -b $begin_cycle -e $end_cycle -f $env_var_file -t $output_type"
echo "$?domId"
if ($?domId > 0)  then
    set argsstr = "$argsstr -d $domId"
endif
if ($no_move > 0) then
    set argsstr = "$argsstr -n no_move"
endif

echo
echo "$argsstr"
echo

#------------------------------------------------------------------------------#
# Go to cycles directory and look for files

echo
echo "Scan directory $RUNDIR"

cd  $RUNDIR

# Loop over domains

@ nfound   = 0
@ nmissing = 0
@ nhour    = 0

@ dom = $dom_min

while ($dom <= $dom_max)

       @ nfound_dom   = 0
       @ nmissing_dom = 0
       @ nhour_dom    = 0

# Create output dir
       if ($output_type == "wrfout") then
           set OUTPUT_DIR_DOM = $OUTPUT_DIR/D$dom
       else
           set OUTPUT_DIR_DOM = $OUTPUT_DIR/QC
       endif

       echo
       echo "Move output in $OUTPUT_DIR_DOM"

       if ($no_move >= 1) then
           echo "Argument -n no_move was passed, do not acually move the files"
       endif

       mkdir -p $OUTPUT_DIR_DOM

# Skip cold start
       set this_cycle = `$EXECUTABLE_ARCHIVE/geth_newdate.exe $begin_cycle $CYC_INT`

echo "---------------------------------------------------------"

       while ($this_cycle <= $end_cycle)

# Find the corresponding cycle

              if (! -d $this_cycle) then
                  echo "Cannot find directory $RUNDIR/$this_cycle"
                  set this_cycle = `$EXECUTABLE_ARCHIVE/geth_newdate.exe $this_cycle +$CYC_INT`
                  continue
              endif

# Count the file in the time period [cycle-CYC_INT,cycle]

              set ccyymmddhh = $this_cycle
              set ccyymmddhh = `$EXECUTABLE_ARCHIVE/geth_newdate.exe $ccyymmddhh -$CYC_INT`

# Do not count last file valid at cycle_time
              @ nfound_cycle   = 0
              @ nmissing_cycle = 0
              @ nhour_cycle    = 0

# Look for every hour between cycle_time-CYC_TIME and cycle_time

              while ($ccyymmddhh < $this_cycle)
   
                     @ nhour_cycle ++
                     @ nhour_dom ++
                     @ nhour ++

                     set ccyy = `echo $ccyymmddhh |cut -c1-4`
                     set mm   = `echo $ccyymmddhh |cut -c5-6`
                     set dd   = `echo $ccyymmddhh |cut -c7-8`
                     set hh   = `echo $ccyymmddhh |cut -c9-10`

                     if ($output_type == "wrfout") then
                         set file  = "wrfout_d0${dom}_${ccyy}-${mm}-${dd}_${hh}:00:00.${MM5HOST}_F"
                     else
                         set file  = "RAP_RTFDDA/qc_out_${ccyy}-${mm}-${dd}_${hh}:00:00.${MM5HOST}_F"
                     endif

# Skip if file is missing

                     if (! -e $this_cycle/$file) then
                         echo "Missing $RUNDIR/$this_cycle/$file"
                         @ nmissing ++
                         @ nmissing_dom ++
                         @ nmissing_cycle ++
                         set ccyymmddhh = `$EXECUTABLE_ARCHIVE/geth_newdate.exe $ccyymmddhh +1`
                         continue
                     endif

# Found forecast time in wrfout file
                     if ($output_type == "wrfout") then

                         set tt = `ncdump -v XTIME $this_cycle/$file |grep " XTIME =" | awk -F, '{ print $1 }' | awk -F" " '{ print $3 }'`
                         set tforecast = `echo $tt | awk -F. '{ print $1 }'`
                         @ tforecast = $tforecast / 60

# Beware of cold start

                         if ($tforecast < $CYC_INT) then
                             if ($DEBUG > 0) echo "Cold start $RUNDIR/$this_cycle/$file +${tforecast}hrs"
                             echo "CYCLE $this_cycle COLD STARTED, SKIP...." 
                              exit -1
                         endif

                         if ($DEBUG > 0) echo "Found $this_cycle/$file +${tforecast}hrs"

                         if ($DEBUG > 0)  echo "mv -f $this_cycle/$file $OUTPUT_DIR_DOM/."
                     else

                         if ($DEBUG > 0) echo "Found $this_cycle/$file"

                     endif

                     if ($no_move <= 0) then
                          mv $this_cycle/$file $OUTPUT_DIR_DOM
                     endif

                     @ nfound ++
                     @ nfound_dom ++
                     @ nfound_cycle ++

                     set ccyymmddhh = `$EXECUTABLE_ARCHIVE/geth_newdate.exe $ccyymmddhh +1`
                     if ($ccyymmddhh > $end_cycle) continue 

              end  # Loop over hours in cycle

              if ($DEBUG > 0) \
              echo "---------------------------------------------------------"
              echo  "Moved $nfound_cycle out of $nhour_cycle $output_type files in cycle $this_cycle for domain $dom"
              echo "---------------------------------------------------------"

              set this_cycle = `$EXECUTABLE_ARCHIVE/geth_newdate.exe $this_cycle +$CYC_INT`

    end # Loop over cycles

    echo
    echo  "Moved $nfound_dom out of $nhour_dom $output_type files valid in [$begin_cycle,$end_cycle] for domain $dom" 
    echo

    @ dom ++

end # Loop over domain

if ($num_doms > 1) then
echo "========================================================================="
echo  "Moved $nfound out of $nhour $output_type files valid in [$begin_cycle,$end_cycle] for $num_doms domain(s)" 
echo "========================================================================="
endif

echo 

exit 0
