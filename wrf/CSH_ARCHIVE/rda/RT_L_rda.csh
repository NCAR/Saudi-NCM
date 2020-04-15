#!/bin/csh
# $Id: RT_L_rda.csh,v 1.20 2014/10/16 23:19:29 hsoh Exp $
#
#  Adds the global attributes to NetCDF and modify the namelist values.
#
set _debug = 0
set _debug = 1
set _this_script_name = `basename $0`
set log_prefix = "  ${_this_script_name}"
echo "${log_prefix} is called" 

set this_cycle = "$1"
set CYC_INT    = "$2"
set CYCDIR     = "$3"
set RUNDIR     = "$4"
set GSJOBDIR   = "$5"
set RANGE      = "$6"
set NUM_PROCS  = "$7"
set DATADIR    = "$8"
set BATCH_SYS  = "$9"
set WRF_HOME   = "$10"
set NUM_DOMS   = "$11"
set fcst_id    = "$12"

if ( $_debug == 1) then
  echo "   WRF_HOME: $WRF_HOME"
  echo "   NUM_DOMS: $NUM_DOMS,   this_cycle: $this_cycle"
  echo "   CYCDIR: $CYCDIR"
  #echo "   fcst_id: $fcst_id"
endif
if ( ! $?DOMS ) set DOMS = ( `seq 1 $NUM_DOMS` )

set _Tools_dir =  $WRF_HOME/CSH_ARCHIVE/util
# hy3dvarDriver.pl is called by get_and_decode_Radar_data at rda_processor.pm
#set _Hybrid3DVar_script = $WRF_HOME/HY3DVAR/hy3dvarDriver.pl
#if ( -e "$_Hybrid3DVar_script" ) then
#  echo "${log_prefix}: Calling $_Hybrid3DVar_script $this_cycle $CYC_INT $RUNDIR $GSJOBDIR $RANGE $NUM_PROCS $DATADIR $BATCH_SYS $WRF_HOME $NUM_DOMS $fcst_id"
#  perl $_Hybrid3DVar_script $this_cycle $CYC_INT $RUNDIR $GSJOBDIR $RANGE $NUM_PROCS $DATADIR $BATCH_SYS $WRF_HOME $NUM_DOMS $fcst_id
#endif

# Checks wrffdda output from MRMS Mosaic data processing (radar_to_wrffdda.exe)
set activate_rda = 0
set rda_domain = ""
foreach domain_no ( $DOMS )
  set _wrffdda_file_name = wrffdda_d0${domain_no}
  set _wrffdda_domain = ../${_wrffdda_file_name}
  if ( ! -e "$_wrffdda_domain" ) set _wrffdda_domain = ../${this_cycle}_${_wrffdda_file_name}
  #echo "  $0 DEBUG: checking _wrffdda_domain: $_wrffdda_domain"
  #ls -l $_wrffdda_domain
  if ( -e "$_wrffdda_domain" ) then
    set activate_rda = 1
    set rda_domain = "$rda_domain $domain_no"
    ## added by lpan
    set _wrf_input = "$CYCDIR/${this_cycle}_wrfinput_d0${domain_no}_cold"
    if ( ! -e $_wrf_input ) then
      set _wrf_input = "$CYCDIR/${this_cycle}_wrfinput_d0${domain_no}"
    endif
    if ( -e $_wrf_input ) then
      # Note: This should be called at RT_S_decode_nsslMosaic_rtfdda_wrf.csh
      #       where $_wrffdda_domain is created, but $_wrf_input was not available there.
      echo "  ${_this_script_name}: Copy global attributes from `basename $_wrf_input` to $_wrffdda_domain"
      if ( $_debug != 0 ) echo "   $_Tools_dir/addAttr.csh $_wrf_input $_wrffdda_domain"
      $_Tools_dir/addAttr.csh $_wrf_input $_wrffdda_domain
    else
      echo "  ${_this_script_name}: Can't add global attributes because [$_wrf_input] does not exist!"
    endif
    if ( ! -e "${_wrffdda_file_name}" ) then
      ln -sf $_wrffdda_domain ${_wrffdda_file_name}
    endif
  #else
  #  echo "  ${_this_script_name}: Does not exist [$_wrffdda_domain]"
  endif
end

# Checks WRFVAR output from Level II data processing (WRFFA)
if ( $activate_rda == 1 ) then
  set activate_rda = 0
  set WRFVAR_dir = $CYCDIR/WRFVAR
  if (! -e $WRFVAR_dir ) set WRFVAR_dir = $RUNDIR/WRFVAR
  set wrfvar_missing_list = $WRFVAR_dir/varOut/missing_wrfvar_list
  set wrfvar_seccess_list = $WRFVAR_dir/varOut/success_wrfvar_list

  if ( -e $wrfvar_seccess_list ) then
    set missing_wrfvar_count = 0
    if ( -e $wrfvar_missing_list ) then
      #set missing_wrfvar_count = `cat $wrfvar_missing_list | grep -v $this_cycle | wc -l`
      set missing_wrfvar_count = `cat $wrfvar_missing_list | wc -l`
    endif
    if ( $missing_wrfvar_count == 0 ) then
      set activate_rda = 1
    else
      echo "  ${_this_script_name} INFO: Please check $wrfvar_missing_list for missing wrfvar outputs"
    endif
  else
    echo "  ${_this_script_name} No success with 3DVar ($wrfvar_seccess_list)"
  endif
  
endif

echo "  ${_this_script_name} activate_rda: $activate_rda, rda_domain: [$rda_domain] "
if ( $activate_rda == 1 ) then
  echo "  ${_this_script_name} modify namelist for grid nudging nsslMosaic QR analysis"
  
  # Backup namelist.input
  cp -p namelist.input namelist.input.org

  set rda_namelist_file = "namelist.input.rda"

  if ( -e $rda_namelist_file ) then
    set names_to_be_updated = "`grep -v '^#' $rda_namelist_file | grep -v '^.*&' | cut -f1 -d'=' | tr '\n' ' '`"
    echo "names_to_be_updated: [$names_to_be_updated]"

    #Note: modify wrf.nl.template.rda to update gt gg
    foreach updated_name ( $names_to_be_updated )
      set value_for_namelist = "`grep '^[ \t]*${updated_name}' $rda_namelist_file`"
      #echo " ${updated_name}: $value_for_namelist"
      if ( "" != "$value_for_namelist" ) then
        sed 's/^[ \t]*'"$updated_name "'.*/'"$value_for_namelist"'/ ;'\
            namelist.input >! namelist.input.tmp;
        mv namelist.input.tmp namelist.input
      else
        echo "  ${_this_script_name} $updated_name was not overridden at $rda_namelist_file"
      endif
    end
  else
    echo "  ${_this_script_name} Disabled RDA because $rda_namelist_file is missing"
  endif
  
  #Deleted because of soft links which is created above
  #sed 's$^.*gfdda_inname .*$ gfdda_inname              = \"../wrffdda_d<domain>\",$ ;'\
  #      namelist.input >! namelist.input.tmp;
  #mv namelist.input.tmp namelist.input

  echo "  ${_this_script_name} finish namelist for grid nudging nsslMosaic QR analysis"
endif
