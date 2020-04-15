#! /bin/csh -f
#set echo
#------------------------------------------------------------------------------#
# Run simultaneously the rtfdda_ens_postproc.pl for all the members for a given
# cycle ccyymmddhh
#
# Command line args required: 
# --------------------------
#  do_ensplots.csh $THIS_CYCLE $RANGE $JOB_ID $USER_NAME
#
# Other parameters to be set:
# --------------------------
#  TMAX = MM5 Forecast Lenght in m
#  TOUT = MM5 output frequency in mn
#  domains = MM5 domains to process (1 2 3)
#  debug   = debugging level
#------------------------------------------------------------------------------#
 set TMAX  = 180       # Maximal forecast length in mn 
 set TOUT  =  20       # Ouput files frequency in mn
 set domains = "1 2 3" # Domain id to process
 set debug = 2         # 0=real-time, 1=Do not wait and scrub
                       # 2=like 1 and do not fire rtfdd_ens_postproc.pl
                       # 3=run on dataproc
#------------------------------------------------------------------------------#
if ($debug >= 3) then 
## Run on dataproc
   setenv DATA       /ptmp/$user
   setenv ENSPLOTS   $HOME/ENSPLOTS
   setenv NCARG_LIB  /usr/local/lib64/r4i4
   setenv NCARG_ROOT /contrib/ncl-4.2.0.axx
   setenv CSM        $NCARG_ROOT/lib/ncarg/nclscripts/csm
   setenv EXECUTABLE_ARCHIVE $ENSPLOTS
else 
## Run on dev-c1
   setenv DATA       /data
   setenv ENSPLOTS   /data/cycles/POSTPROCS/ENSPLOTS  
   setenv NCARG_LIB  /usr/local/ncarg/lib
   setenv NCARG_ROOT /usr/local/ncl
   setenv CSM        $NCARG_ROOT/lib/ncarg/nclscripts/csm
   setenv EXECUTABLE_ARCHIVE /data/fddahome/cycle_code/EXECUTABLE_ARCHIVE
endif
#------------------------------------------------------------------------------#

## Get arguments from command line

 if ($#argv < 3) then
## Error in command line arguments
     echo ""
     echo "Missing arguments: do_ensplots.csh [THIS_CYCLE] RANGE JOB_ID USER_NAME"
     echo ""
     echo "eg: do_ensplots.csh 2003091713 GRM GM0397 ncaruser"
     echo ""
     exit(1)
 else if ($#argv <= 3) then
## Figure out cycle from current UTC time
     set RANGE = $1
     set JOB_ID = $2
     set USER_NAME = $3
     set cc = `date -u '+%C'`
     set yy = `date -u '+%y'`
     set mm = `date -u '+%m'`
     set dd = `date -u '+%d'`
     set hh = `date -u '+%H'`
     set mn = `date -u '+%M'`
     set ss = `date -u '+%S'`
     set THIS_CYCLE = ${cc}${yy}${mm}${dd}${hh}
 else
## Normal call mode
     set THIS_CYCLE = $1
     set RANGE = $2
     set JOB_ID = $3
     set USER_NAME = $4
     set cc = `echo $THIS_CYCLE |cut -c1-2`
     set yy = `echo $THIS_CYCLE |cut -c3-4`
     set mm = `echo $THIS_CYCLE |cut -c5-6`
     set dd = `echo $THIS_CYCLE |cut -c7-8`
     set hh = `echo $THIS_CYCLE |cut -c9-10`
     set mn = "00"
     set ss = "00"
 endif

## Set-up general path 

 set WORK_DIRNC = "${DATA}/cycles/${JOB_ID}/NCL"

## Make the working directory

 if (! -d $WORK_DIRNC) then
     mkdir $WORK_DIRNC
     set cold = 0
 else
     set cold = 1
 endif

## Set-up path for this cycle

 set WORK_DIRNC    = "${WORK_DIRNC}/${THIS_CYCLE}"

## Make the working directory

 if (! -d $WORK_DIRNC) mkdir $WORK_DIRNC

## Allow some time for the cold start

 if (($cold == 0) && ($debug <= 0)) then
     set current_date = `date -u +%D`
     set current_time = `date -u +%T`
     echo "\n${current_time}: Allowing 2mn for cold start."
     sleep 120
 endif

## Get the number of ensemble members (nodes)

 if (! -f ${DATA}/GMODJOBS/${JOB_ID}/member-nodes) then
     echo "Cannot find file: ${DATA}/GMODJOBS/${JOB_ID}/member-nodes"
     exit (1)
 endif

 set member_nodes = `cat  ${DATA}/GMODJOBS/${JOB_ID}/member-nodes`

##  Count members

 set nodes = 0
 set NODES_MEM = ""

 foreach nnn ($member_nodes)

   if (${nnn} =~ [0-9]*) then
       set NODES_MEM = "$NODES_MEM $nnn" 
       @ nodes = $nodes + 1
   endif

 end

## Print-out

 set current_date = `date -u +%D`
 set current_time = `date -u +%T`

 echo "\n${current_date} at ${current_time}:"
 echo "\nProcess GEAPS job id $JOB_ID of range $RANGE at $THIS_CYCLE"
 echo "\nUse $nodes nodes: $NODES_MEM"
 echo "\nWorking directory is: $WORK_DIRNC\n"

## Go to source directory

 set HERE = `pwd`
 cd ${ENSPLOTS}


#==============================================================================#

## Fire NETCDF convertor for all nodes

  set n = 1

  foreach NODE_MEM ($NODES_MEM)

    if ($debug <= 1) then
        sleep 60  # Wait 60s for the next ensemble directory to show up.
     endif

    if ($NODE_MEM < 10) then
        set nn = 0${NODE_MEM}
    else 
        set nn = ${NODE_MEM}
    endif

    set current_date = `date -u +%D`
    set current_time = `date -u +%T`

    echo "${current_time}: ($n) rtfdda_ens_postproc.pl ${RANGE} ${nn} ${JOB_ID} ${THIS_CYCLE}"

    if ($debug <= 1) then
        rtfdda_ens_postproc.pl ${RANGE} ${NODE_MEM} ${JOB_ID} ${THIS_CYCLE} >&!\
       ${WORK_DIRNC}/rtfdda_ens_postproc.${RANGE}_${nn}_${JOB_ID}_${THIS_CYCLE}.log &
    endif

    @ n = $n + 1

  end

## Go to output files directory, get the date stamp of the most recent file

  cd ${WORK_DIRNC}


## Scrub any existing meta, ps and gif files

      find . -name \*.ncgm -exec rm {} \;
      find . -name \*.gif  -exec rm {} \;
      find . -name \*.ps   -exec rm {} \;
      find . -name out.cdf -exec rm {} \;

## Scrub any files older than forecast time or 3hr.

  if ($debug <= 0 ) then

      if ($TMAX > 0) then
          find . -type f -amin +${TMAX} -exec rm {} \;
      else
          find . -type f -amin +180 -exec rm {} \;
      endif

  endif     

## Initial and final time for output files (cycle-1hr, cycle-1hr+TMAX)

 set ccyymmddhh = `${EXECUTABLE_ARCHIVE}/geth_newdate.csh $THIS_CYCLE -1`

 if ($cold == 0) then
     set ccyymmddhhmn = ${ccyymmddhh}00
 else
     set ccyymmddhhmn = `${EXECUTABLE_ARCHIVE}/geth_newdate.csh ${ccyymmddhh}00 +$TOUT`
 endif

 set ccyymmddhhmne = `${EXECUTABLE_ARCHIVE}/geth_newdate.csh ${THIS_CYCLE}00 +$TMAX`

## Cycle over the forecast times up to TMAX

 set nsx = 0

 while ($ccyymmddhhmn <= $ccyymmddhhmne) 

   set current_date = `date -u +%D`
   set current_time = `date -u +%T`

   echo " "
   echo \
"=============================================================================="
     echo "${current_time}: Process forecast time $ccyymmddhhmn of cycle $THIS_CYCLE\n"

## Copy the stations and the threshold files

   if (-f ${ENSPLOTS}/stationmap_new) then
       cp ${ENSPLOTS}/stationmap_new stationmap.list
   else 
       echo "Cannot find file ${ENSPLOTS}/stationmap_new"
       echo "No interpolation will be performed"
   endif

   if (-f ${DATA}/GMODJOBS/${JOB_ID}/thresholds/thresholds_${JOB_ID}) then
       cp ${DATA}/GMODJOBS/${JOB_ID}/thresholds/thresholds_${JOB_ID} thresholds.list
   else 
       echo ""
       echo "Cannot find file ${DATA}/GMODJOBS/${JOB_ID}/thresholds/thresholds_${JOB_ID}"
       echo "Use default file $ENSPLOTS/thresholds.txt"
             cp $ENSPLOTS/thresholds.txt thresholds.list
   endif

## Copy the US county boundaries file

   if (-f ${ENSPLOTS}/us_county.ascii) then
       cp ${ENSPLOTS}/us_county.ascii .
   else 
       echo ""
       echo "Cannot find file ${ENSPLOTS}/us_county.ascii"
       echo "County lines will not be plotted"
   endif

   set current_date = `date -u +%D`
   set current_time = `date -u +%T`

## Link with terrain files 

   set Ndom = 0

   foreach d ($domains)

     if (-f ${DATA}/GMODJOBS/${JOB_ID}/TERRAIN/TERRAIN_DOMAIN$d) then
         ln -s -f ${DATA}/GMODJOBS/${JOB_ID}/TERRAIN/TERRAIN_DOMAIN$d .
     else
         echo ""
         echo "Cannot find file TERRAIN_DOMAIN$d in ${DATA}/cycles/${JOB_ID}"
         echo  "Stations map cannot be generated"
     endif

     @ Ndom = $Ndom + 1

   end

## Create directories on remote server

   if ($debug <= 2) then

       set current_date = `date -u +%D`
       set current_time = `date -u +%T`

      echo "\n${current_time}: Copy constants files to 4dwx@atec-server:/$RANGE/$USER_NAME/$JOB_ID/gifs/${ccyymmddhhmn}"

      ssh 4dwx@atec-server " if (! -d /$RANGE/$USER_NAME/$JOB_ID/gifs) mkdir /$RANGE/$USER_NAME/$JOB_ID/gifs "

      ssh 4dwx@atec-server " if (! -d /$RANGE/$USER_NAME/$JOB_ID/gifs/${ccyymmddhhmn}) mkdir /$RANGE/$USER_NAME/$JOB_ID/gifs/${ccyymmddhhmn} "

      ssh 4dwx@atec-server " if (! -d /$RANGE/$USER_NAME/$JOB_ID/histograms) mkdir /$RANGE/$USER_NAME/$JOB_ID/histograms "

      ssh 4dwx@atec-server " if (! -d /$RANGE/$USER_NAME/$JOB_ID/histograms/${ccyymmddhhmn}) mkdir /$RANGE/$USER_NAME/$JOB_ID/histograms/${ccyymmddhhmn} "

   endif

## Generate the stations map

   if (-f gmeta ) rm gmeta

   ${ENSPLOTS}/pltsites.exe stationmap.list $Ndom >&! pltsites_${ccyymmddhhmn}.log

   if (-f gmeta && ! -z gmeta) then

## Split gmeta file

       switch ($Ndom)
         case 1:
           med -e 'split1' gmeta
           breaksw
         case 2:
           med -e 'split2' gmeta
           breaksw
         case 3:
           med -e 'split3' gmeta
           breaksw
         case 4:
           med -e 'split4' gmeta
           breaksw
         case 5:
           med -e 'split5' gmeta
           breaksw
         default
           echo "\nError in domains definition domains = $domains\n"
           exit (1)
         breaksw
       endsw

## Convert into gif gifes.

       set d = 1

       foreach d ($domains)

## Convert meta file into gif

          if (-f med00$d.ncgm) ${ENSPLOTS}/ncgm2gif_ugui.csh med00$d.ncgm

#   ctrans -d ps.color -f font1 med00$d.ncgm  >! med00$d.ps 
#   convert -crop 0x0 med00$d.ps med00$d.gif

## Rename meta and gif files

          if (-f med00$d.gif)  mv med00$d.gif  grid${d}map.gif
          if (-f med00$d.ncgm) mv med00$d.ncgm grid${d}map.ncgm

          if (-f med00$d.sun)  rm -f med00$d.sun

          if ($debug <= 3) then
              if (-f grid${d}map.ncgm) rm -f grid${d}map.ncgm 
              if (-f gmeta) rm -f gmeta
          endif

## Copy gridmap gif files to remote server in gifs directory

          if (-f  grid${d}map.gif) then
              if ($debug <= 2) then
                  scp grid${d}map.gif 4dwx@atec-server:/$RANGE/$USER_NAME/$JOB_ID/histograms/${ccyymmddhhmn}/.
              endif
              if ($debug <= 3) rm grid${d}map.gif
          else
              echo "No file grid${d}map.gif was found at ${ccyymmddhhmn}\n"
          endif

## Copy HTML file

          if (-f  grid${d}map.html) then
              if ($debug <= 2) then
                  scp grid${d}map.html 4dwx@atec-server:/$RANGE/$USER_NAME/$JOB_ID/histograms/${ccyymmddhhmn}/.
              endif
              if ($debug <= 3) rm grid${d}map.html
          else
              echo "No file grid${d}map.html was found at ${ccyymmddhhmn}\n"
          endif

       end

   else
     echo ""
     echo "${current_time}: Cannot generate stations map files"
   endif


##=============================================================================#
##  Reorder domains backward

   set d = $Ndom
   set domainsi = ""

   while ($d >= 1)
     set domainsi = "$domainsi $d" 
     @ d = $d - 1
   end

## Loop backward over domains

#  foreach dom (${domainsi})
   foreach dom (${domains})

     set current_date = `date -u +%D`
     set current_time = `date -u +%T`
     echo " "
     echo \
"------------------------------------------------------------------------------"
     echo "${current_time}: Process domain $dom for forecast time $ccyymmddhhmn\n"


## Check if there are any files valid at the time

     set t0 = `date +%s`
     set t1 = `date +%s`
     set new_files  = 0
     set first_file = 0

     if ($debug >= 2) then
         set new_files =  `find . -name ${ccyymmddhhmn}_MMOUTPUT_DOMAIN${dom}.${RANGE}_\*.cdf`
     else

     while ($#new_files < $nodes)

## Record the time we are waiting

       set tf = `date +%s`

## Check for new files

       set new_files =  `find . -name ${ccyymmddhhmn}_MMOUTPUT_DOMAIN${dom}.${RANGE}_\*.cdf`

## Note when the first file appeared

       if (($#new_files >= 1) && ($first_file == 0)) then
            set t1 = `date +%s`
            set first_file = 1
       endif

## Print the current status

       set current_date = `date -u +%D`
       set current_time = `date -u +%T`

       echo "${current_time}: found $#new_files on $nodes forecasts valid at ${ccyymmddhhmn}."

##  Exit when: 

       @ dtf = $tf - $t0
       @ dt1 = $tf - $t1

##  a) All the files are here 

       if ($#new_files >= $nodes) then
           echo "          All the nodes have finished, process data..."
           break
       endif

## b) More than 10mn has passed since the first file appeared

       if (($first_file == 1) && ($dt1 >= 600)) then
           echo "          The first file appeared 10mn ago, wait no longer for other files."
           break
       endif

## b) Have been waiting for more than 15mn (rtfdda_ens_postproc.pl wait 24mn)

       if ($dtf >= 900) then
           echo "          Have been waiting more than 15mn, wait no longer for other files."
           break
       endif

## Wait a little bit for other nodes to complete

       echo "          Wait two more minutes for other nodes to complete..."
       sleep 120

     end

     endif

## Skip when there is no nodes at all

     set current_time = `date -u +%T`

     if ($#new_files == 0) then
         echo "\n${current_time}: No Netcdf files valid at time: ${ccyymmddhhmn}, skip to next time..."
     else

        echo "\n${current_time}: Process $#new_files file(s) valid at time: ${ccyymmddhhmn}."

## Kill all similar existing NCL jobs already running (hanging jobs)

#       if ($debug <= 0) then
#           killall driver_geapsd${dom}.ncl
#       endif

## Scrub any existing ps and gif files from previous ncl runs

#       find . -name d\*_histo_\*.ps -exec rm {} \;
#       find . -name d\*_histo_\*.gif -exec rm {} \;
#       find . -name d\*_prob\*.ps -exec rm {} \;
#       find . -name d\*_prob\*.gif -exec rm {} \;
#       find . -name grid\*map_histo\*.html -exec rm {} \;

        find . -name \*.ps -exec rm {} \;
        find . -name \*.gif -exec rm {} \;
        find . -name \*.html -exec rm {} \;
        find . -name out.cdf -exec rm {} \;

## Generate the NCL driver scripts at the newest date and for each domain

cat >! driver_geapsd${dom}.ncl << EOF${dom}

;*******************************************************************************
;  driver_d${dom}.ncl: Driver for GEAPS processing for domain ${dom}
;*******************************************************************************
load "$ENSPLOTS/process_geaps.ncl"
load "$ENSPLOTS/plot_geaps.ncl"

begin
;*******************************************************************************
; 1. Define foreast date, range and analysis/forecast
;*******************************************************************************

  fdate = "${ccyymmddhhmn}"
  dom   = ${dom}
  range = "${RANGE}"
  fcst  = ""

; graph_type = "ncgm" ; meta file output type
  graph_type = "ps" ; meta file output type

;*******************************************************************************
; 2. Read files and compute statistics
;*******************************************************************************

  filenameo = new (1,string)

  is = process_geaps (dom, fdate, range, fcst, filenameo)

;*******************************************************************************
; 3. Generate the plots
;*******************************************************************************

  is = plot_geaps (filenameo, dom, fdate, graph_type)

;*******************************************************************************
; 4. End
;*******************************************************************************
end

EOF${dom}

## Run the NCL script for each domain

        set current_date = `date -u +%D`
        set current_time = `date -u +%T`

        echo \
   "\n${current_time}: ncl < driver_geapsd${dom}.ncl >! driver_geapsd${dom}_${ccyymmddhhmn}.log"

##==============================================================================

        ncl < driver_geapsd${dom}.ncl >! driver_geapsd${dom}_${ccyymmddhhmn}.log

##==============================================================================

## Remove unwanted stations

#       set FILER = `find . -name d\*_histo_\*.ps | grep -v IAD`

#       if ($#FILER > 0) then
#           rm -f $FILER
#       endif


## Convert ps map files into gif files

        set nf = `find . -name d${dom}\*_prob\*.ps -print`

        if ($#nf > 0) then

            set current_date = `date -u +%D`
            set current_time = `date -u +%T`
            echo "\n${current_time}: Convert and Copy maps files to 4dwx@atec-server:/$RANGE/$USER_NAME/$JOB_ID/gifs/${ccyymmddhhmn}"

            foreach fil (d${dom}*_prob*.ps)

               set filr = $fil:r
               set dom  = `echo $fil  |cut -c2-2`
               set dat  = `echo $filr |cut -f4 -d_`
               set name = `echo $fil  | cut -f1 -d.`
               set names = `echo $name | cut -c1-16`

## Do not process old images

               if ($#dat == 0) then

## Rotate rainrate domain 1 exceedence probability only

                   if (-f $fil) then

#                      if ($debug >= 1) then
#                          echo "Convert $fil ${name}.gif"
#                      endif

                       convert -crop 0x0 $fil ${name}.gif

## Check conversion

                       if ((! -f ${name}.gif) || (-z ${name}.gif)) then
                           echo "File ${name}.gif is missing or empty"
                       else

## Copy over gif files

                           if ($debug <= 2) \
                               scp ${name}.gif 4dwx@atec-server:/$RANGE/$USER_NAME/$JOB_ID/gifs/${ccyymmddhhmn}/.
                           if ($debug <= 3) rm ${name}.gif

                           @ nsx = $nsx + 1

                       endif

## Rename file with the date

                      if ($debug <= 3) then
                          rm $fil
                      else
                          mv $fil ${name}_${ccyymmddhhmn}.ps
                      endif

                   else
                      echo "File $fil is missing or empty"
                      exit
                   endif

               endif

            end

        endif

## Convert ps histogram files into gif files

        set nf = `find . -name d${dom}\*_histo_\*.ps -print`

        if ($#nf > 0) then

            set current_date = `date -u +%D`
            set current_time = `date -u +%T`
            echo "\n${current_time}: Convert and Copy histograms files to 4dwx@atec-server:/$RANGE/$USER_NAME/$JOB_ID/histograms/${ccyymmddhhmn}"

            foreach fil (d${dom}*_histo_*.ps)

               set name = `echo $fil | cut -f1 -d.`
               set dat  = `echo $fil |cut -f5 -d_`

## Do not process old images

               if ($#dat == 0) then

## Rotate histograms

                   if (-f $fil) then

#                      if ($debug >= 1) then
#                          echo "Convert $fil ${name}.gif"
#                      endif

                       convert -rotate 270 -crop 0x0 $fil ${name}.gif

## Check conversion

                       if ((! -f ${name}.gif) || (-z ${name}.gif)) then
                           echo "File ${name}.gif is missing or empty"
                       else

## Copy over gif files

                           if ($debug <= 2) \
                               scp ${name}.gif 4dwx@atec-server:/$RANGE/$USER_NAME/$JOB_ID/histograms/${ccyymmddhhmn}/.
                           if ($debug <= 3) rm ${name}.gif

                           @ nsx = $nsx + 1

                       endif

## Rename ps file with the date

                       if ($debug <= 3) then
                           rm $fil
                       else
                           mv $fil ${name}_${ccyymmddhhmn}.ps
                       endif

                   else
                       echo "File $fil is missing or empty"
                       exit
                   endif

               endif

            end

        endif

## Copy over html files to remote server in histograms directory

        if ($debug <= 2) then

            echo "\n${current_time}: Copy html files to 4dwx@atec-server:/$RANGE/$USER_NAME/$JOB_ID/histograms/${ccyymmddhhmn}"

            set nhtlm = `find . -name grid${dom}map\*.html -print`

            if ($#nhtlm > 0) then
                foreach f (grid${dom}map*.html)
                  scp $f 4dwx@atec-server:/$RANGE/$USER_NAME/$JOB_ID/histograms/${ccyymmddhhmn}/.
                  if ($debug <= 3) rm  $f
                end

            else
                echo "No html file for domain $dom was found at ${ccyymmddhhmn}\n"
            endif

            @ nsx = $nsx + 1

        endif 

## End of no file skipping

      endif 

## End of loop over domains

   end

# Increment date with TOUT

   set ccyymmddhhmn = `${EXECUTABLE_ARCHIVE}/geth_newdate.csh  ${ccyymmddhhmn}  +${TOUT}`

## End of loop over forecast time

 end

## Go back to initial directory

 cd ${HERE}

## Print out status

 if ($nsx == 0) then
     set current_date = `date -u +%D`
     set current_time = `date -u +%T`
     echo "\n${current_time}: No output files were found for cycle: $THIS_CYCLE"
 endif

## Print end message

 echo " "
 echo \
"------------------------------------------------------------------------------"
 echo "\nEnd of ensemble plots for $JOB_ID at range $RANGE at $ccyymmddhhmne for cycle $THIS_CYCLE"

 set current_date = `date -u +%D`
 set current_time = `date -u +%T`

 echo "\n${current_date} at ${current_time}\n"

exit (0)
