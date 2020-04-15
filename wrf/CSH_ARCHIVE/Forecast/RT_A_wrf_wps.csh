#!/bin/csh 
# set echo
# wrfwps cycling script for RTFDDA
# Yubao Liu, 2007, Feb .
#set echo
set timestamp
setenv SUBSYSTEM  WRF_WPS
setenv RM "rm -rf"
#
#
# Environment from MM
#
if(-e $GSJOBDIR/tmp/$this_cycle/cshrc) then
  source $GSJOBDIR/tmp/$this_cycle/cshrc
  ${PERL_ARCHIVE}/MMPerlLog.perl "INFO" "Found and Sourced $GSJOBDIR/tmp/$this_cycle/cshrc"
endif

${PERL_ARCHIVE}/MMPerlLog.perl "INFO" " " 
${PERL_ARCHIVE}/MMPerlLog.perl "INFO" "$0 $argv[*]"
${PERL_ARCHIVE}/MMPerlLog.perl "INFO" " ++++ ++++++++++++++++++++++++++++++++++++++++++ "
${PERL_ARCHIVE}/MMPerlLog.perl "INFO" " ++++ start GMODJOBS $GSJOBDIR wrfwps +++++++++++ "
${PERL_ARCHIVE}/MMPerlLog.perl "INFO" " ++++ ++++++++++++++++++++++++++++++++++++++++++ "


if ( ${#argv} < 8) then

echo "usage $0 this_cycle bcsst_date bcsen_date NPROC NODE bcic bc_interval dom"
exit

endif

if ($1 != "") set this_cycle  = $1
if ($2 != "") set bcsst_date  = $2
if ($3 != "") set bcsen_date  = $3
if ($4 != "") set NPROC       = $4
if ($5 != "") set NODE        = $5
if ($6 != "") set BCIC        = $6
if ($7 != "") set BC_INT      = $7
if ($8 != "") set dom         = $8

${PERL_ARCHIVE}/MMPerlLog.perl "INFO" "$0 $1 $2 $3 $4 $5 $6 $7 $8"

if($BCIC == "AVNFTP") then
 set BCIC="GFS"
else if($BCIC == "ETA") then
 set BCIC="NAM"
endif
#
# ENVIRONMENT
#
set CFILE="$MM5HOME/cycle_code/CONFIG_FILES/cshrc_"

$CheckConfigFiles
set cfstat = $status
if ( $cfstat != 0 ) then
 ${PERL_ARCHIVE}/MMPerlLog.perl "INFO" "${SUBSYSTEM} -- Missing ConfigFile -> exiting"
 exit (2)
endif

${PERL_ARCHIVE}/MMPerlLog.perl "INFO" "Sourcing ${CFILE}user.mm5sys"
source ${CFILE}user.mm5sys
${PERL_ARCHIVE}/MMPerlLog.perl "INFO" "Check METGRID_MPICH $METGRID_MPICH"

# Those variables need to be initialized
set FG_NAME = ""
set FGNAME = ""

# extend this 'ltrs' list for climo run with CFSR data
# WWU Oct. 15 2013
 
set ltrs = ( AAA AAB AAC AAD AAE AAF AAG AAH AAI AAJ AAK AAL AAM \
             AAN AAO AAP AAQ AAR AAS AAT AAU AAV AAW AAX AAY AAZ \
             ABA ABB ABC ABD ABE ABF ABG ABH ABI ABJ ABK ABL ABM \
             ABN ABO ABP ABQ ABR ABS ABT ABU ABV ABW ABX ABY ABZ \
             ACA ACB ACC ACD ACE ACF ACG ACH ACI ACJ ACK ACL ACM \
             ACN ACO ACP ACQ ACR ACS ACT ACU ACV ACW ACX ACY ACZ \
             ADA ADB ADC ADD ADE ADF ADG ADH ADI ADJ ADK ADL ADM \
             ADN ADO ADP ADQ ADR ADS ADT ADU ADV ADW ADX ADY ADZ \
             BAA BAB BAC BAD BAE BAF BAG BAH BAI BAJ BAK BAL BAM \
             BAN BAO BAP BAQ BAR BAS BAT BAU BAV BAW BAX BAY BAZ \
             BBA BBB BBC BBD BBE BBF BBG BBH BBI BBJ BBK BBL BBM \
             BBN BBO BBP BBQ BBR BBS BBT BBU BBV BBW BBX BBY BBZ \
             BCA BCB BCC BCD BCE BCF BCG BCH BCI BCJ BCK BCL BCM \
             BCN BCO BCP BCQ BCR BCS BCT BCU BCV BCW BCX BCY BCZ \
             BDA BDB BDC BDD BDE BDF BDG BDH BDI BDJ BDK BDL BDM \
             BDN BDO BDP BDQ BDR BDS BDT BDU BDV BDW BDX BDY BDZ \
             CAA CAB CAC CAD CAE CAF CAG CAH CAI CAJ CAK CAL CAM \
             CAN CAO CAP CAQ CAR CAS CAT CAU CAV CAW CAX CAY CAZ \
             CBA CBB CBC CBD CBE CBF CBG CBH CBI CBJ CBK CBL CBM \
             CBN CBO CBP CBQ CBR CBS CBT CBU CBV CBW CBX CBY CBZ \
             CCA CCB CCC CCD CCE CCF CCG CCH CCI CCJ CCK CCL CCM \
             CCN CCO CCP CCQ CCR CCS CCT CCU CCV CCW CCX CCY CCZ \
             CDA CDB CDC CDD CDE CDF CDG CDH CDI CDJ CDK CDL CDM \
             CDN CDO CDP CDQ CDR CDS CDT CDU CDV CDW CDX CDY CDZ \
             DAA DAB DAC DAD DAE DAF DAG DAH DAI DAJ DAK DAL DAM \
             DAN DAO DAP DAQ DAR DAS DAT DAU DAV DAW DAX DAY DAZ \
             DBA DBB DBC DBD DBE DBF DBG DBH DBI DBJ DBK DBL DBM \
             DBN DBO DBP DBQ DBR DBS DBT DBU DBV DBW DBX DBY DBZ \
             DCA DCB DCC DCD DCE DCF DCG DCH DCI DCJ DCK DCL DCM \
             DCN DCO DCP DCQ DCR DCS DCT DCU DCV DCW DCX DCY DCZ \
             DDA DDB DDC DDD DDE DDF DDG DDH DDI DDJ DDK DDL DDM \
             DDN DDO DDP DDQ DDR DDS DDT DDU DDV DDW DDX DDY DDZ \
             EAA EAB EAC EAD EAE EAF EAG EAH EAI EAJ EAK EAL EAM \
             EAN EAO EAP EAQ EAR EAS EAT EAU EAV EAW EAX EAY EAZ \
             EBA EBB EBC EBD EBE EBF EBG EBH EBI EBJ EBK EBL EBM \
             EBN EBO EBP EBQ EBR EBS EBT EBU EBV EBW EBX EBY EBZ \
             ECA ECB ECC ECD ECE ECF ECG ECH ECI ECJ ECK ECL ECM \
             ECN ECO ECP ECQ ECR ECS ECT ECU ECV ECW ECX ECY ECZ \
             EDA EDB EDC EDD EDE EDF EDG EDH EDI EDJ EDK EDL EDM \
             EDN EDO EDP EDQ EDR EDS EDT EDU EDV EDW EDX EDY EDZ )

set icount = 0

set CYCDIR="$RUNDIR/$this_cycle"
$MustHaveDir $RUNDIR
$MustHaveDir $RUNDIR/data
$MustHaveDir $CYCDIR
cd $CYCDIR
${PERL_ARCHIVE}/MMPerlLog.perl "INFO" "Now working in  $cwd"

rm -r WRF_WPS

if( -d $GEAPSTMP/1 ) then # GEAPSTMP/1: mpirun does not like local disk
 setenv WPS_DIR "$GEAPSTMP/WRF_WPS"
 $MustHaveDir $WPS_DIR
 ln -s $WPS_DIR $CYCDIR/WRF_WPS
#ln -s $CYCDIR /d1/bridge
else
 setenv WPS_DIR "$CYCDIR/WRF_WPS"
 $MustHaveDir $WPS_DIR
endif

# 0: Prepare right namelists and large-scale model data for all

cd $WPS_DIR
${PERL_ARCHIVE}/MMPerlLog.perl "INFO" "Now working in  $cwd"

set FH = 0
set tmp_date = $bcsst_date
${PERL_ARCHIVE}/MMPerlLog.perl "INFO" "$tmp_date $bcsst_date $bcsen_date $FH"
while ($tmp_date < $bcsen_date)
 echo "$tmp_date , 1" >! input
 ${EXECUTABLE_ARCHIVE}/advance_cymdh < input >! output
 set tmp_date = `cat output`
 @ FH = $FH + 1
 ${PERL_ARCHIVE}/MMPerlLog.perl "INFO" "$tmp_date $FH"
end

${PERL_ARCHIVE}/MMPerlLog.perl "INFO" "$bcsst_date $bcsen_date $FH"
${PERL_ARCHIVE}/MMPerlLog.perl "INFO" "$CYCDIR $BCIC $BC_INT"

set yyyy_start = `echo $bcsst_date | cut -b 1-4`
set mm_start = `echo $bcsst_date | cut -b 5-6`
set dd_start = `echo $bcsst_date | cut -b 7-8`
set hh_start = `echo $bcsst_date | cut -b 9-10`

set yyyy_end = `echo $bcsen_date | cut -b 1-4`
set mm_end = `echo $bcsen_date | cut -b 5-6`
set dd_end = `echo $bcsen_date | cut -b 7-8`
set hh_end = `echo $bcsen_date | cut -b 9-10`

if( -e $GSJOBDIR/wps.$NODE ) then
 cp -ar $GSJOBDIR/wps.$NODE/* .
else
 cp -ar $GSJOBDIR/wps/* .
endif

cp $GSJOBDIR/namelists/wps.nl.template namelist.wps

echo "updating namelist.wps start/end time"
#module load python/2.7.14
$PYTHON_ARCHIVE/tools/updatewrfnml.py -i './namelist.wps' -N $dom -b $bcsst_date -e $bcsen_date
$PYTHON_ARCHIVE/tools/updatewrfnml.py  -s 'share' -k 'interval_seconds' -v $BC_INT
$PYTHON_ARCHIVE/tools/updatewrfnml.py -o './namelist.wps' -s 'ungrib' -k 'prefix' -v 'FILE'

# 1. Run UNGRIB

${PERL_ARCHIVE}/MMPerlLog.perl "INFO" "start UNGRIB ...."

if ($BCIC == "NNRP") then
    set F = `ls ../NNRP_REGRID/UPANNRP_* ../NNRP_REGRID/SFCNNRP_*`
else if ($BCIC == "NNRP2") then
    set F = `ls ../NNRP2_REGRID/UPANNRP_* ../NNRP2_REGRID/SFCNNRP_*`
else if ($BCIC == "CFSV1") then
    set F = `ls $RUNDIR/data/CFSV1/*gdas*`
else if ($BCIC == "CFSV2") then
    set F = `ls $RUNDIR/data/CFSV2/*cdas*`
else if ($BCIC == "CFSR") then
    set F = `ls $RUNDIR/data/CFSR/cfs.*`
else if ($BCIC == "CFSF") then
    set F = `ls $RUNDIR/data/CFSF/cfs.*`
else if ($BCIC == "FNL") then
    set F = `ls ../FNL_REGRID/fnl_*`
else if ($BCIC == "UKMO") then
   #set F = `ls $RUNDIR/data/model-UKMET*`
    set F = `ls $RUNDIR/data/model-bracknell*`
else if ($BCIC == "DWD") then
    set F = `ls $RUNDIR/data/model-offenbach*`
else if ($BCIC == "ECMWF") then
    set F = `ls $RUNDIR/data/model-ecmwf2*`
else if ($BCIC == "ECMWF9KM") then
    set F = `ls $RUNDIR/data/model-ecmwf-*`
else if ($BCIC == "GEM") then
    set F = `ls $RUNDIR/data/*GEM*`
else
    set F = `ls $RUNDIR/data/*tl.press*`
endif
 
if ($#F <= 0) then
    ${PERL_ARCHIVE}/MMPerlLog.perl "INFO" " " 
    ${PERL_ARCHIVE}/MMPerlLog.perl "INFO" "ERROR in RT_A_wrf_wps.csh:"
    ${PERL_ARCHIVE}/MMPerlLog.perl "INFO" "Cannot find $BCIC input files $RUNDIR/data/*tl.press*"
    ${PERL_ARCHIVE}/MMPerlLog.perl "INFO" " " 
    exit -1
endif

cp ${EXECUTABLE_ARCHIVE}/ungrib.exe .

#------------------------------------------------------------------------------#
# CFSV1 (1979-2010) and CFSV2 (2011-current) input data
#------------------------------------------------------------------------------#

# for CFSV1 or CFSV2 we need run ungrid twice: UPR and SFC seprately 
# WWU Oct. 15 2013 addition to accomodate CFS data (CFSV1 and CFSV2)
if ($BCIC == "CFSV1" || $BCIC == "CFSV2") then

# subdirectory $BCIC to hold intermediate files: output from ungrib
   if ( ! -d $BCIC ) then
      mkdir $BCIC
   endif

   if ( $BCIC == "CFSV1" ) then
      set sfcfld = "flxf06"
      set uprfld = "pgbhnl"
      set CFSIN = "$RUNDIR/data/CFSV1"
   else if ($BCIC == "CFSV2") then
      set sfcfld = "sfluxgrbf"
      set uprfld = "pgrbh"
      set CFSIN = "$RUNDIR/data/CFSV2"
   endif

   foreach product ($sfcfld $uprfld)
     set F = `ls $CFSIN/*${product}*.grb2`
     foreach f ($F)
        @ icount ++
        set ltr = $ltrs[$icount]
        set fn = `echo $f | tr -d =@=`
        if (! -e $fn) then
            ${PERL_ARCHIVE}/MMPerlLog.perl "INFO" "WARNING: missing file $fn"
            ${PERL_ARCHIVE}/MMPerlLog.perl "INFO" "Some fields may be missing in metgrid"
            @ icount --
        else
         ln -s -f $fn GRIBFILE.${ltr}
        endif
     end # f 
#------------------------------------------------------------------------------#
# Grab the table for this product
#------------------------------------------------------------------------------#
      echo
      echo \
"------------------------------------------------------------------------------"
      echo "Running ungrib on CFSV1 analysis $product data"
      echo

      rm -f Vtable

      if ($product == pgbhnl || $product == "pgrbh") then
         set uors = "UPR"
          echo \
         "ln -sf ./Variable_Tables/Vtable.CFSR_press_pgbh06 Vtable"
          ln -sf ./Variable_Tables/Vtable.CFSR_press_pgbh06 Vtable
      else if ($product == flxf06 || $product == "sfluxgrbf") then
         set uors = "SFC"
          echo \
         "ln -sf ./Variable_Tables/Vtable.CFSR_sfc_flxf06 Vtable"
          ln -sf ./Variable_Tables/Vtable.CFSR_sfc_flxf06 Vtable
      endif # pgbhnl


      ${PERL_ARCHIVE}/MMPerlLog.perl "INFO" "------------------------------------------------------"
      ${PERL_ARCHIVE}/MMPerlLog.perl "INFO" " Running ungrib for CFSV1"
      ${PERL_ARCHIVE}/MMPerlLog.perl "INFO" "------------------------------------------------------"

      ccmrun ./ungrib.exe  > print.out_wrfwps
      if ( -e 'ungrib.log') then
         mv ungrib.log ungrib.log.icbc
      else
         ${PERL_ARCHIVE}/MMPerlLog.perl "INFO" "ungrib.log for IC/BC dataset does not exist! ungrib.exe apparently has failed!"
      endif

#------------------------------------------------------------------------------#
# Rename output
#------------------------------------------------------------------------------#

      set name = ${BCIC}${uors}

      foreach f (FILE:????-??-??_??)
             set ff = `echo $f |cut -c1-4`
             set dd = `echo $f |cut -c6-`
             echo \
            "mv $f ${BCIC}/${name}:${dd}"
             mv $f ${BCIC}/${name}:${dd}
      end
#------------------------------------------------------------------------------#
# Add product to WPS namelist's file names
#------------------------------------------------------------------------------#

     set FG_NAME = ("$FG_NAME \'${BCIC}\/${name}\',")
     set FGNAME = "$FGNAME$BCIC/$name,"

   end # product 

#------------------------------------------------------------------------------#
# CFSR = CFSV1 (1979-2010) + CFSV2 (2011-current)
#------------------------------------------------------------------------------#

# for CFSR data we need to run ungrid twice: UPR and SFC seprately 
# WWU 20131115 addition to accomodate CFS data 
else if ($BCIC == "CFSR" || $BCIC == "CFSF") then
   # subdirectory $BCIC to hold intermediate files: output from ungrib
   if ( ! -d $BCIC ) then
      mkdir $BCIC
   endif

   set CFSIN = "$RUNDIR/data/$BCIC"

   echo " inside  RT_A_wrf_wps.csh CFSIN: $CFSIN"
   foreach product (sfc upr)
     set icount = 0
     set F = `ls $CFSIN/cfs.${product}*.grb2`
     foreach f ($F)
        @ icount ++
        set ltr = $ltrs[$icount]
        set fn = `echo $f | tr -d =@=`
        echo "$product $icount $fn $ltr"
        if (! -e $fn) then
            ${PERL_ARCHIVE}/MMPerlLog.perl "INFO" "WARNING: missing file $fn"
            ${PERL_ARCHIVE}/MMPerlLog.perl "INFO" "Some fields may be missing in metgrid"
            @ icount --
        else
         ln -s -f $fn GRIBFILE.${ltr}
        endif
     end # f 
#------------------------------------------------------------------------------#
# Grab the table for this product
#------------------------------------------------------------------------------#
      echo
      echo \
"------------------------------------------------------------------------------"
      echo "Running ungrib on CFS $product data" 
      echo

      rm -f Vtable

      if ($product == upr) then
         set uors = "UPR"
          echo \
         "ln -sf ./Variable_Tables/Vtable.CFSR_press_pgbh06 Vtable"
          ln -sf ./Variable_Tables/Vtable.CFSR_press_pgbh06 Vtable
      else if ($product == sfc) then
         set uors = "SFC"
          echo \
         "ln -sf ./Variable_Tables/Vtable.CFSR_sfc_flxf06 Vtable"
          ln -sf ./Variable_Tables/Vtable.CFSR_sfc_flxf06 Vtable
      endif # pgbhnl

      ${PERL_ARCHIVE}/MMPerlLog.perl "INFO" "------------------------------------------------------"
      ${PERL_ARCHIVE}/MMPerlLog.perl "INFO" " Running ungrib for CFS"
      ${PERL_ARCHIVE}/MMPerlLog.perl "INFO" "------------------------------------------------------"

      ccmrun ./ungrib.exe  > print.out_wrfwps
      if ( -e 'ungrib.log') then
         mv ungrib.log ungrib.log.icbc
      else
         ${PERL_ARCHIVE}/MMPerlLog.perl "INFO" "ungrib.log for IC/BC dataset does not exist! ungrib.exe apparently has failed!"
      endif

#------------------------------------------------------------------------------#
# Rename output
#------------------------------------------------------------------------------#

     #set name = ${BCIC}${uors}   
      set name = ${uors}   

      foreach f (FILE:????-??-??_??)
             set ff = `echo $f |cut -c1-4`
             set dd = `echo $f |cut -c6-`
             echo \
            "mv $f ${BCIC}/${name}:${dd}"
             mv $f ${BCIC}/${name}:${dd}
      end

#------------------------------------------------------------------------------#
# Add product to WPS namelist's file names
#------------------------------------------------------------------------------#

     set FG_NAME = ("$FG_NAME \'${BCIC}\/${name}\',")
     set FGNAME = "$FGNAME$BCIC/$name,"

   end # product 

else  # CFSV1,CFSV2,CFSR,CFSF

  foreach f ($F)
          @ icount ++
          set ltr = $ltrs[$icount]
          set fn = `echo $f | tr -d =@=`
#        if(-e $fn) ln -s -f $fn GRIBFILE.${ltr}
          if (! -e $fn) then
              ${PERL_ARCHIVE}/MMPerlLog.perl "INFO" "WARNING: missing file $fn"
              ${PERL_ARCHIVE}/MMPerlLog.perl "INFO" "Some fields may be missing in metgrid"
              @ icount --
          else
           ln -s -f $fn GRIBFILE.${ltr}
          endif
  end


  ln -s -f ./Variable_Tables/Vtable.$BCIC Vtable

  ${PERL_ARCHIVE}/MMPerlLog.perl "INFO" "------------------------------------------------------"
  ${PERL_ARCHIVE}/MMPerlLog.perl "INFO" " Running ungrib for ICBC"
  ${PERL_ARCHIVE}/MMPerlLog.perl "INFO" "------------------------------------------------------"

  #$MPICMD_BIN_DIR/aprun -n 1 -d 32 -cc none -a xt ./ungrib.exe  > print.out_wrfwps
  $MPICMD_BIN_DIR/srun -n 1  --hint=nomultithread ./ungrib.exe
  if ( -e 'ungrib.log') then
     mv ungrib.log ungrib.log.icbc
  else
     ${PERL_ARCHIVE}/MMPerlLog.perl "INFO" "ungrib.log for IC/BC dataset does not exist! ungrib.exe apparently has failed!"
  endif

# If DWD or UKMO or GEM data are used - need to add GFS soil fields
  if ($BCIC == "DWD" || $BCIC == "UKMO" || $BCIC == "GEM") then
      set F = `ls $RUNDIR/data/*tl.press*`
      set icount = 0
      rm -f GRIBFILE.*

      foreach f ($F)
          @ icount ++
          set ltr = $ltrs[$icount]
          set fn = `echo $f | tr -d =@=`
          if (! -e $fn) then
              ${PERL_ARCHIVE}/MMPerlLog.perl "INFO" "WARNING: missing file $fn"
              ${PERL_ARCHIVE}/MMPerlLog.perl "INFO" "Some fields may be missing in metgrid"
              @ icount --
          else
           ln -s -f $fn GRIBFILE.${ltr}
          endif
      end

      cp namelist.wps namelist.wps.save
    
      if ($BCIC == "GEM") then
         set GFS_BC_INT = 10800
      else
         set GFS_BC_INT = 21600
      endif

      sed -e 's/ITT/'$GFS_BC_INT'/g' \
          -e 's/'\''FILE'\''/'\''GFSs'\''/' namelist.wps > namelist.wps.tmp

      mv namelist.wps.tmp namelist.wps

      if ($BCIC == "GEM") then
         ln -s -f ./Variable_Tables/Vtable.GFSsoil_GEM Vtable
      else
         ln -s -f ./Variable_Tables/Vtable.GFSsoil Vtable
      endif

      ${PERL_ARCHIVE}/MMPerlLog.perl "INFO" "------------------------------------------------------"
      ${PERL_ARCHIVE}/MMPerlLog.perl "INFO" " Running ungrib for GFS soil fields"
      ${PERL_ARCHIVE}/MMPerlLog.perl "INFO" "------------------------------------------------------"

      ccmrun ./ungrib.exe  > print.out_wrfwps.gfsl

  endif # DWD, UKMO, GEM

endif # CFSV1,CFSV2,CFSR,CFSF

set BCINTinHR = `expr $BC_INT / 3600`
echo "Checking BC interval in hours: $BC_INT $BCINTinHR"

if ($USE_GLDAS > 0) then
    ${PERL_ARCHIVE}/MMPerlLog.perl "INFO" "Will use GLDAS LAND data from directory:"
    ${PERL_ARCHIVE}/MMPerlLog.perl "INFO" "$DATA_LAND_DIR/025deg/data/<CCYY>"
    cp ${EXECUTABLE_ARCHIVE}/gldasncl2met.exe gldas2met.exe
    chmod +x gldas2met.exe
#   take data from panassas so on-fly data inquiry is no longer needed
#   the data were 'scp-ed' to local directory through scpgldas.py
#   echo \
#  "$PYTHON_ARCHIVE/flex/ICBC/gldasinquiry.py -b $bcsst_date -e $bcsen_date -d $DATA_LAND_DIR"
#   $PYTHON_ARCHIVE/flex/ICBC/gldasinquiry.py -b $bcsst_date -e $bcsen_date -d $DATA_LAND_DIR 
    echo \
   "$PYTHON_ARCHIVE/flex/ICBC/gldas4wps.py $bcsst_date $bcsen_date $BCINTinHR $RUNDIR/data/GLDAS $WPS_DIR"
    $PYTHON_ARCHIVE/flex/ICBC/gldas4wps.py $bcsst_date $bcsen_date $BCINTinHR $RUNDIR/data/GLDAS $WPS_DIR
$   $PYTHON_ARCHIVE/flex/ICBC/gldas4wps.py $bcsst_date $bcsen_date $BCINTinHR $DATA_LAND_DIR $WPS_DIR
#   GLDAS needs a specific table: METGRID.TBL.GLDAS
    cp -f metgrid/METGRID.TBL.GLDAS metgrid/METGRID.TBL
    set FGNAME = "${FGNAME}GLDAS/DAS,"
endif
  
if ($USE_OI_DAILY_V2 > 0) then
    ${PERL_ARCHIVE}/MMPerlLog.perl "INFO" "Will use NCEP OI-SST from directory:"
    ${PERL_ARCHIVE}/MMPerlLog.perl "INFO" "$DATA_SST_DIR/data/<CCYY>/AVHRR"
    echo "${EXECUTABLE_ARCHIVE}/oi_daily_v2_nc.exe oi_daily_v2_nc.exe"
    cp ${EXECUTABLE_ARCHIVE}/oi_daily_v2_nc.exe oi_daily_v2_nc.exe
    chmod +x oi_daily_v2_nc.exe
    echo "processing OIV2 SST."
    
    echo "$bcsst_date $bcsen_date"
    echo "${yyyy_start}${mm_start}${dd_start}"
    echo "${yyyy_end}${mm_end}${dd_end}"

#   take data from panassas so on-fly data inquiry is no longer needed
#   the data were 'scp-ed' to local directory through scpoisst.py
#   $PYTHON_ARCHIVE/flex/ICBC/oisstinquiry.py -b $bcsst_date -e $bcsen_date -d $DATA_SST_DIR/data
    echo \
    "$PYTHON_ARCHIVE/flex/ICBC/oisst4wps.py -b $bcsst_date -e $bcsen_date -t $BCINTinHR -d $RUNDIR/data/OISST"
    $PYTHON_ARCHIVE/flex/ICBC/oisst4wps.py -b $bcsst_date -e $bcsen_date -t $BCINTinHR -d $RUNDIR/data/OISST
    set FGNAME = "${FGNAME}OISST/SST"
else #USE_OI_DAILY_V2 

## Rong's editing starts here
if ($?DATA_DIR) then
${PERL_ARCHIVE}/MMPerlLog.perl "INFO" "running addtional code for RTG SST..."

set runSST = 0

set cycYYYY = `echo $this_cycle | cut -c1-4`
set cycMM = `echo $this_cycle | cut -c5-6`
set cycDD = `echo $this_cycle | cut -c7-8`
set cycHH = `echo $this_cycle | cut -c9-10`
set cycSecs = `date -d "${cycYYYY}-${cycMM}-${cycDD} ${cycHH}:00:00" +%s`

set DAY_BACK = 0
set tmp_date = $this_cycle

while ($DAY_BACK <= 3)
 if ($DAY_BACK == 0) then
    set HR_BACK = 0
 else
    set HR_BACK = -24
 endif

 echo "$tmp_date , $HR_BACK" >! input
 ${EXECUTABLE_ARCHIVE}/advance_cymdh < input >! output
 set tmp_date = `cat output`

 ${PERL_ARCHIVE}/MMPerlLog.perl "INFO" "todays date: $tmp_date "
 ### We set RTG dates to equal todays date
 set sstYYYY = `echo $tmp_date | cut -c1-4`
 set sstMM = `echo $tmp_date | cut -c5-6`
 set sstDD = `echo $tmp_date | cut -c7-8`
 set sstHH = "00" 
 set sstSecs = `date -d "${sstYYYY}-${sstMM}-${sstDD} ${sstHH}:00:00" +%s`

 set SST = ${sstYYYY}${sstMM}${sstDD}.rtg_sst
 set SST083 = ${sstYYYY}${sstMM}${sstDD}.rtg_sst083
 ${PERL_ARCHIVE}/MMPerlLog.perl "INFO" "rtg data: ${SST083} "
 
 set secsDiff = `echo $cycSecs - $sstSecs | bc -l`

 if (-s $DATA_DIR/sst083/$SST083) then
    rm -f GRIBFILE.*
    ln -sf $DATA_DIR/sst083/$SST083 GRIBFILE.AAA
    ${PERL_ARCHIVE}/MMPerlLog.perl "INFO" "SST file $DATA_DIR/sst083/$SST083 is $secsDiff seconds old"
    set runSST = 1
    break
 else if (-s $DATA_DIR/sst/$SST) then 
    rm -f GRIBFILE.*
    ln -sf $DATA_DIR/sst/$SST GRIBFILE.AAA
    ${PERL_ARCHIVE}/MMPerlLog.perl "INFO" "RTG SST file $DATA_DIR/sst/$SST is $secsDiff seconds old"
    set runSST = 1
    break
 else
    ${PERL_ARCHIVE}/MMPerlLog.perl "INFO" "WARNING: No sst or sst083 file valid for day ${sstYYYY}-${sstMM}-${sstDD}"
 endif

 @ DAY_BACK = $DAY_BACK + 1
end

### MODIS SST is preferred even if it is 6 days old 
### we start with today and go back 6 days.  If none are found we default to RTG down below
set tmp_date = $this_cycle
set modis_sstYYYY = `echo $tmp_date | cut -c1-4`
set modis_sstMM = `echo $tmp_date | cut -c5-6`
set modis_sstDD = `echo $tmp_date | cut -c7-8`
set SST_MODIS="$DATA_DIR/modis_sst/composite.${modis_sstYYYY}${modis_sstMM}${modis_sstDD}.L3m_DAY_SST_sst_4km.nc"


if ($USE_MODIS == 1) then

# modisDate needs to be defined here first, otherwise, USE_MODIS will fail
# when rrunning in archive mode when the SST_MODIS has a date stamp that's
# current, because the following while loop will not entered! In real-time
# production, modisDate will be redefined to an older date anyway once the
# while loop is entered!
set modisDate = "${modis_sstYYYY}${modis_sstMM}${modis_sstDD}00"

set HR_BACK = -24
set DAY_COUNT = 0
while ( (! -e $SST_MODIS) && ($DAY_COUNT < 5) )
   @ DAY_COUNT = $DAY_COUNT + 1
   echo "$tmp_date , $HR_BACK" >! input
   ${EXECUTABLE_ARCHIVE}/advance_cymdh < input >! output
   set tmp_date = `cat output`
   set modis_sstYYYY = `echo $tmp_date | cut -c1-4`
   set modis_sstMM = `echo $tmp_date | cut -c5-6`
   set modis_sstDD = `echo $tmp_date | cut -c7-8`
   set SST_MODIS = "$DATA_DIR/modis_sst/composite.${modis_sstYYYY}${modis_sstMM}${modis_sstDD}.L3m_DAY_SST_sst_4km.nc"
   set modisDate = "${modis_sstYYYY}${modis_sstMM}${modis_sstDD}00"
   ${PERL_ARCHIVE}/MMPerlLog.perl "INFO" "modis data: ${SST_MODIS} ${tmp_date} ${DAY_COUNT}"
end
${PERL_ARCHIVE}/MMPerlLog.perl "INFO" "modis data: ${SST_MODIS} "
endif #USE_MODIS

### We want the following section to run when RTG sst is available ($runSST) OR if MODIS is available
### previously it had to be both
if ( $runSST || ( ( $USE_MODIS == 1 ) && (-e $SST_MODIS) )) then


   mv namelist.wps namelist.wps.save

   cp $GSJOBDIR/namelists/wps.nl.template namelist.wps

  #...edit namelist.wps to reflect the valid time for RTG SST and use SST prefix

   sed -e 's/SYY/'$sstYYYY'/g' \
       -e 's/SMM/'$sstMM'/g' \
       -e 's/SDD/'$sstDD'/g' \
       -e 's/SHH/'$sstHH'/g' \
       -e 's/EYY/'$sstYYYY'/g' \
       -e 's/EMM/'$sstMM'/g' \
       -e 's/EDD/'$sstDD'/g' \
       -e 's/EHH/'$sstHH'/g' \
       -e 's/ITT/'$BC_INT'/g' \
       -e 's/DOM/'$dom'/g' \
       -e 's/'\''FILE'\''/'\''SST'\''/' namelist.wps > namelist.wps.tmp

   mv namelist.wps.tmp namelist.wps

   set sstFile = `echo SST:${sstYYYY}-${sstMM}-${modis_sstDD}_${sstHH}`

   ln -sf ./Variable_Tables/Vtable.SST Vtable

   ${PERL_ARCHIVE}/MMPerlLog.perl "INFO" "------------------------------------------------------"
   ${PERL_ARCHIVE}/MMPerlLog.perl "INFO" " Re-running ungrib on SST files"
   ${PERL_ARCHIVE}/MMPerlLog.perl "INFO" "------------------------------------------------------"

   if( (-e $SST_MODIS) && (-e "${EXECUTABLE_ARCHIVE}/modis_sst.exe") && ($USE_MODIS == 1) ) then
       ${PERL_ARCHIVE}/MMPerlLog.perl "INFO" "modis SST are used. $SST_MODIS"
       ${PERL_ARCHIVE}/MMPerlLog.perl "INFO" "${EXECUTABLE_ARCHIVE}/modis_sst.exe $SST_MODIS $modisDate 0"
       ${EXECUTABLE_ARCHIVE}/modis_sst.exe $SST_MODIS $modisDate 0
       ln -sf SST:* SST
   else
       ${PERL_ARCHIVE}/MMPerlLog.perl "INFO" "RTG SST are used. $SST083"
       ccmrun ./ungrib.exe > print.out_wrfwps.sst
       ln -sf ${sstFile} SST
   endif

   if ( -e 'ungrib.log') then
      mv ungrib.log ungrib.log.sst
   else
      ${PERL_ARCHIVE}/MMPerlLog.perl "INFO" "ungrib.log for SST dataset does not exist! ungrib.exe apparently has failed!"
   endif

   #instert Salt Lake Temperatures into SST file
   if( $USE_GSL == 1) then
      ${PERL_ARCHIVE}/MMPerlLog.perl "INFO" "------------------------------------------------------"
      ${PERL_ARCHIVE}/MMPerlLog.perl "INFO" "Inserting Great Salt Lake Temperatures"
      ${PERL_ARCHIVE}/MMPerlLog.perl "INFO" "${EXECUTABLE_ARCHIVE}/add_GSL_WST_WPS.exe $WPS_DIR ${sstFile} $DATA_DIR/gsl_lst"
      ${PERL_ARCHIVE}/MMPerlLog.perl "INFO" "------------------------------------------------------"
      ${EXECUTABLE_ARCHIVE}/add_GSL_WST_WPS.exe $WPS_DIR ${sstFile} $DATA_DIR/gsl_lst 
   endif

   mv namelist.wps namelist.wps.sst
   mv namelist.wps.save namelist.wps

   sed -e 's/TAVGSFC/SST/' namelist.wps > namelist.wps.tmp

   mv namelist.wps.tmp namelist.wps 

   ln -s -f ./Variable_Tables/Vtable.$BCIC Vtable

else # if runSST
  ${PERL_ARCHIVE}/MMPerlLog.perl "INFO" " " 
  ${PERL_ARCHIVE}/MMPerlLog.perl "INFO" "Did not find SST files <CCYYMMDD>.rtg_sst]083] in directory:"
  ${PERL_ARCHIVE}/MMPerlLog.perl "INFO" " $DATA_DIR/sst[083]"
  ${PERL_ARCHIVE}/MMPerlLog.perl "INFO" "Skipping SST ungrib"
endif  # if runSST

endif # $?DATA_DIR
## Rong's editing ends

endif # USE_OI_DAILY_V2

#if $BATCH_SYSTEM is not set, set it to "INTER" (interactive, no batch)
if (! $?BATCH_SYSTEM) set BATCH_SYSTEM = "INTER"

#if $MPI_PRE_PROCESS is not set, set it to "1", i.e., will run real.mpich
if (! $?MPI_PRE_PROCESS ) set MPI_PRE_PROCESS  = 1

#if $REAL_MPICH is not set, set to real.mpich
if (! $?REAL_MPICH ) set REAL_MPICH  = real.mpich


if ( $BATCH_SYSTEM == "PBS" )then
  if ($?PBS_JOBID) then
  echo "  PBS_JOBID: $PBS_JOBID"
  echo "  Allocated nodes are: "
  cat $PBS_NODEFILE
  echo ""
  endif
endif

### The PBS directive
#PBS -l nodes=${NUM_NODES}:ppn=${PPN}

# WWU 2013115 Addition to accomodate CFS data 
# check the template namelist.wps to ensure the following replacement right
if ($BCIC == "CFSV1" || $BCIC == "CFSV2" || $BCIC == "CFSR" || $BCIC == "CFSF" || $USE_OI_DAILY_V2 > 0) then
   echo "update metgrid namelist before metgrid...."
   $PYTHON_ARCHIVE/tools/updatewrfnml.py -s metgrid -k fg_name -v $FGNAME
   $PYTHON_ARCHIVE/tools/updatewrfnml.py -o './namelist.wps'
   if (-e nml.pkl) then
      rm -f nml.pkl
   endif
endif

# 2. Run METGRID

if ( $MPI_PRE_PROCESS == 0 ) then

 if(-e $GSJOBDIR/executables/metgrid.exe) then
  cp $GSJOBDIR/executables/metgrid.exe metgrid.exe
  ${PERL_ARCHIVE}/MMPerlLog.perl "INFO" "${SUBSYSTEM} -- Using $GSJOBDIR/executables/metgrid.exe"
 else if ( -e ${METGRID_EXE} ) then
  cp ${METGRID_EXE} metgrid.exe
  ${PERL_ARCHIVE}/MMPerlLog.perl "INFO" "${SUBSYSTEM} -- Using ${METGRID_EXE}"
 else
  ${PERL_ARCHIVE}/MMPerlLog.perl "INFO" "${SUBSYSTEM} -- Missing metgrid.exe executable --  exiting"
  exit (4)
 endif

else

 if(-e $GSJOBDIR/executables/metgrid.mpich.$NODE) then
  cp $GSJOBDIR/executables/metgrid.mpich.$NODE metgrid.mpich
  ${PERL_ARCHIVE}/MMPerlLog.perl "INFO" "${SUBSYSTEM} -- Using $GSJOBDIR/executables/metgrid.mpich.$NODE"
 else if ( -e $GSJOBDIR/executables/metgrid.mpich ) then
  cp $GSJOBDIR/executables/metgrid.mpich metgrid.mpich
  ${PERL_ARCHIVE}/MMPerlLog.perl "INFO" "${SUBSYSTEM} -- Using $GSJOBDIR/executables/metgrid.mpich"
 else if ( -e ${METGRID_MPICH}.$NODE ) then
  cp ${METGRID_MPICH}.$NODE metgrid.mpich
  ${PERL_ARCHIVE}/MMPerlLog.perl "INFO" "${SUBSYSTEM} -- Using  ${METGRID_MPICH}.$NODE"
 else if ( -e ${METGRID_MPICH} ) then
  cp ${METGRID_MPICH} metgrid.mpich
  ${PERL_ARCHIVE}/MMPerlLog.perl "INFO" "${SUBSYSTEM} -- Using ${METGRID_MPICH}"
 else
  ${PERL_ARCHIVE}/MMPerlLog.perl "INFO" "${SUBSYSTEM} -- Missing metgrid.mpich executable --  exiting"
  exit (4)
 endif
endif

if ( $MPI_PRE_PROCESS == 0 ) then
    ccmrun ./metgrid.exe >>& metgrid.print.out
else

  ${PERL_ARCHIVE}/MMPerlLog.perl "INFO" "$BATCH_SYSTEM"
if ($BATCH_SYSTEM == "PBS") then
    $MPICMD_BIN_DIR/aprun -n $NPROC ./metgrid.mpich >>& metgrid.print.out
    echo "aprun -n $NPROC ./metgrid.mpich >>& metgrid.print.out"
else if ($BATCH_SYSTEM == "SLURM") then
    echo " $MPICMD_BIN_DIR/srun -n $NPROC  --hint=nomultithread  ./metgrid.mpich"
    $MPICMD_BIN_DIR/srun -n $NPROC  --hint=nomultithread  ./metgrid.mpich
    echo " $MPICMD_BIN_DIR/srun -n $NPROC  --hint=nomultithread  ./metgrid.mpich"
else if ($BATCH_SYSTEM == "LSF") then
#    $GSJOBDIR/mpirun.lsf ./metgrid.mpich  #>>&! metgrid.print.out
#    exit (0)
else if ($BATCH_SYSTEM == "INTER") then
#  Submit the job with mpirun
#  /opt/mpich/bin/mpirun -np $NUM_PROCS -machinefile $GSJOBDIR/machinefile metgrid.mpich >>& metgrid.print.out

     if(-e $GSJOBDIR/machinefile) then
       cp $GSJOBDIR/machinefile $RUNDIR/hosts
     endif
       $MPICMD_BIN_DIR/mpirun -np $NPROC -machinefile $RUNDIR/hosts ./metgrid.mpich >>& metgrid.print.out
     ##END of if ($BATCH_SYSTEM == "INTER")
else
    ${PERL_ARCHIVE}/MMPerlLog.perl "INFO" " " 
    ${PERL_ARCHIVE}/MMPerlLog.perl "INFO" "ERROR: Unknown value BATCH_SYSTEM = $BATCH_SYSTEM!"
    ${PERL_ARCHIVE}/MMPerlLog.perl "INFO" "       BATCH_SYSTEM must be PBS, LSF or INTER (interactive)"
    ${PERL_ARCHIVE}/MMPerlLog.perl "INFO" "Executing: $MPICMD_BIN_DIR/mpirun -np $NPROC -machinefile $RUNDIR/hosts metgrid.mpich"

    $MPICMD_BIN_DIR/mpirun -np $NPROC -machinefile $RUNDIR/hosts metgrid.mpich
endif

endif

# WRF_WPS done : the real input is located in $WPS_DIR/

ls $WPS_DIR/met_em_*

if (-e metgrid.log.0000) then
    cat metgrid.log.0000 >> print.out_wrfwps
    rm -f metgrid.log.*
endif

# cp print.out_wrfwps $CYCDIR/

exit
