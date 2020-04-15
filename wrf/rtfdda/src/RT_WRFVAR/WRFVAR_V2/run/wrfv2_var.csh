#! /bin/csh -f
#IBM:
# @ job_type   = parallel
## @ environment = COPY_ALL
# @ environment = MP_SHARED_MEMORY=true
# @ job_name   = wrfvar
# @ output     = wrfvar.out
# @ error      = wrfvar.err
# @ node       = 1
## @ network.MPI    = css0,shared,us
# @ network.MPI    = css0,shared,ip
# @ tasks_per_node = 16
# @ node_usage = not_shared
# @ checkpoint = no
# @ wall_clock_limit = 01:30:00
# NCEP IBM=dev
# NCAR IBM(bluesky)=com_rg8:
# NCAR IBM(blackforest)=com_reg:
# NCAR IBM(blackforest_nighthawk)=com_nh:
# @ class      =  com_nh
## @ class      =  com_reg
# @ queue
#
#FSL JET (Alpha/Linux):
#PBS -V -A sfmlidar
#PBS -lnodes=4:comp -lwalltime=1000
#Uncomment for JET: source /usr/local/bin/setup-mpi.csh
#-----------------------------------------------------------------------
# Script DA_Run_WRF-Var.csh
#
# Purpose: Top level script for running the WRF-Var system
#
# Method:  1) Set up links to run directory.
#          2) Run WRF-Var in run directory.
#
# History: 11/16/99  Original version. Dale Barker
#          12/01/99  Modifications to defaults and names. Dale Barker
#          10/14/03  Modifications to defaults for WRF 3DVAR. Wei Huang
#          06/06/05  Modifications for Polar & Geo AMV's      Syed RH Rizvi
#          07/15/05  Tidy up prior to WRF V2.1 release, rename DA_Run_WRF-Var.csh. Dale Barker
#          08/12/05  Set-up for RT-FDDA runs (F. Vandenberghe)
#
#-----------------------------------------------------------------------

#set echo

#unlimit

#setenv MP_SHARED_MEMORY yes

 echo ""
 echo "Running script wrfv2_var"
 echo "------------------------"
 echo ""

#-----------------------------------------------------------------------
# USER: Define non-default job via environment variables: 
#-----------------------------------------------------------------------

# Define ATEC RANGE (eg: DPG_F, DPG_P+FCST, etc.)

setenv RANGE DPG_F

# Define analysis date (CCYYMMDDHH): 

setenv START_DATE 2005090912

# Define control variables flag, obs format flag, first guess 
# format flag, number of processors and global/regional flag:

setenv DA_CV_OPTIONS 3             # Background error statistics: 2=NCAR, 3=NCEP
setenv DA_FG_FORMAT 1              # First guess format: 1=WRF, 2=MM5, 3=KMA
setenv DA_OB_FORMAT 2              # Observation format: 1=BUFR, 2=ASCII
setenv NUM_PROCS 1                 # Number of processors to run on.
setenv NPROC_X 1                   # 0 = Regional, 1 = Global

# Grid dimensions and distance
setenv WEST_EAST_GRID_NUMBER 98   # WRF actual grid size
setenv SOUTH_NORTH_GRID_NUMBER 84 # WRF actual grid size
setenv VERTICAL_GRID_NUMBER 37    # Like full sigmas (half sigma levels + 1)
setenv GRID_DISTANCE 30000        # in m

# Land Surface model
setenv DA_SF_SURFACE_PHYSICS 2  # 1 = Thermal diffusion, 2 = Noah LSM
setenv DA_NUM_SOIL_LAYERS 4     # 5 = Thermal diffusion LSM, 4 = Noah LSM

# Define run directories:
setenv RUN_DIR /raid/QC/RUN

# Define executable:
setenv WRFVAR_EXE /raid/QC/WRFV2_VAR/main/wrfvar.exe

# Static input data files (full path):
setenv LANDUSE  /raid/QC/DATA/LANDUSE.TBL

# Observation input file (must be consistent with DA_FG_FORMAT):
setenv DA_OBSERVATIONS /raid/QC/DATA/${START_DATE}.all.obs.glob_3dvar

# Statistics for for forecast error (must be consistent with DA_CV_OPTIONS):
setenv DA_BACK_ERRORS  /raid/QC/DATA/be.cv_${DA_CV_OPTIONS}

# Define first guess root name (wrfout_d01, wrfinput_d01) with path
# (must be consistent with DA_BG_FORMAT):
setenv DA_FIRST_GUESS_ROOT /raid/QC/DATA/wrfout_d01

# Actual file name will be DA_FIRST_GUESS_ROOT_CCYY-MM-DD_HH:MN:SS.RANGE).
##########################################################################
#USER: DO NOT MAKE CHANGES BELOW (if you do, you're on your own!) 
##########################################################################

#-----------------------------------------------------------------------
# [1.0] Specify default environment variables:
#-----------------------------------------------------------------------

 set DA_CY = `echo $START_DATE | cut -c1-4`
 set DA_MM = `echo $START_DATE | cut -c5-6`
 set DA_DD = `echo $START_DATE | cut -c7-8`
 set DA_HH = `echo $START_DATE | cut -c9-10`

 setenv DA_ANALYSIS_DATE ${DA_CY}-${DA_MM}-${DA_DD}_${DA_HH}:00:00

if ($?RANGE) then
    setenv DA_FIRST_GUESS  ${DA_FIRST_GUESS_ROOT}_${DA_ANALYSIS_DATE}.${RANGE}
else
    setenv DA_FIRST_GUESS  ${DA_FIRST_GUESS_ROOT}_${DA_ANALYSIS_DATE}
endif


 if (! -d $RUN_DIR) then
     echo "\n"
     echo "Cannot find directory $RUN_DIR"
     echo "\n"
     exit -1
 endif

 cd $RUN_DIR

#-----------------------------------------------------------------------
# [2.0] Create WRF V2.1 namelist.input file:
#-----------------------------------------------------------------------

cat >! namelist.input << EOF
 &time_control
 run_days                            = 0,
 run_hours                           = 12,
 run_minutes                         = 0,
 run_seconds                         = 0,
 start_year                          = $DA_CY, $DA_CY, $DA_CY,
 start_month                         = $DA_MM, $DA_MM, $DA_MM,
 start_day                           = $DA_DD, $DA_DD, $DA_DD,
 start_hour                          = $DA_HH, $DA_HH, $DA_HH,
 start_minute                        = 00,
 start_second                        = 00,
 end_year                            = $DA_CY, $DA_CY, $DA_CY,
 end_month                           = $DA_MM, $DA_MM, $DA_MM,
 end_day                             = $DA_DD, $DA_DD, $DA_DD,
 end_hour                            = $DA_HH, $DA_HH, $DA_HH,
 end_minute                          = 00,
 end_second                          = 00,
 interval_seconds                    = 21600,
 input_from_file                     = .true.,.false.,.false.,
 history_interval                    = 180,  60,   60,
 frames_per_outfile                  = 1000, 1000, 1000,
 restart                             = .false.,
 restart_interval                    = 5000,
 io_form_history                     = 2
 io_form_restart                     = 2
 io_form_input                       = 2
 io_form_boundary                    = 2
 debug_level                         = 10
 /

 &domains
 time_step                           = 180,
 time_step_fract_num                 = 0,
 time_step_fract_den                 = 1,
 max_dom                             = 1,
 s_we                                = 1, 1, 1,
 e_we                                = $WEST_EAST_GRID_NUMBER, $WEST_EAST_GRID_NUMBER, $WEST_EAST_GRID_NUMBER,
 s_sn                                = 1, 1, 1,
 e_sn                                = $SOUTH_NORTH_GRID_NUMBER, $SOUTH_NORTH_GRID_NUMBER, $SOUTH_NORTH_GRID_NUMBER,
 s_vert                              = 1, 1, 1,
 e_vert                              = $VERTICAL_GRID_NUMBER, $VERTICAL_GRID_NUMBER, $VERTICAL_GRID_NUMBER,
 dx                                  = $GRID_DISTANCE, $GRID_DISTANCE, $GRID_DISTANCE,
 dy                                  = $GRID_DISTANCE, $GRID_DISTANCE, $GRID_DISTANCE,
 grid_id                             = 1,     2,     3,
 parent_id                           = 0,     1,     2,
 i_parent_start                      = 0,     30,    30,
 j_parent_start                      = 0,     20,    30,
 parent_grid_ratio                   = 1,     3,     3,
 parent_time_step_ratio              = 1,     3,     3,
 feedback                            = 1,
 smooth_option                       = 0,
 nproc_x                             = $NPROC_X,
 /

 &physics
 mp_physics                          = 3,     3,     3,
 ra_lw_physics                       = 1,     1,     1,
 ra_sw_physics                       = 1,     1,     1,
 radt                                = 10,    10,    10,
 sf_sfclay_physics                   = 1,     1,     1,
 sf_surface_physics                  = ${DA_SF_SURFACE_PHYSICS},     1,     1,
 bl_pbl_physics                      = 1,     1,     1,
 bldt                                = 0,     0,     0,
 cu_physics                          = 1,     1,     0,
 cudt                                = 5,     5,     5,
 isfflx                              = 1,
 ifsnow                              = 0,
 icloud                              = 1,
 num_soil_layers                     = ${DA_NUM_SOIL_LAYERS},
 surface_input_source                = 1,
 mp_zero_out                         = 0,
 maxiens                             = 1,
 maxens                              = 3,
 maxens2                             = 3,
 maxens3                             = 16,
 ensdim                              = 144,
 /

 &dynamics
 dyn_opt                             = 2,
 rk_ord                              = 3,
 w_damping                           = 0,
 diff_opt                            = 0,
 km_opt                              = 1,
 damp_opt                            = 0,
 base_temp                           = 290.
 zdamp                               = 5000.,  5000.,  5000.,
 dampcoef                            = 0.01,   0.01,   0.01
 khdif                               = 0,      0,      0,
 kvdif                               = 0,      0,      0,
 smdiv                               = 0.1,    0.1,    0.1,
 emdiv                               = 0.01,   0.01,   0.01,
 epssm                               = 0.1,    0.1,    0.1
 non_hydrostatic                     = .true., .true., .true.,
 time_step_sound                     = 4,      4,      4,
 h_mom_adv_order                     = 5,      5,      5,
 v_mom_adv_order                     = 3,      3,      3,
 h_sca_adv_order                     = 5,      5,      5,
 v_sca_adv_order                     = 3,      3,      3,
 /

 &bdy_control
 spec_bdy_width                      = 5,
 spec_zone                           = 1,
 relax_zone                          = 4,
 specified                           = .true., .false.,.false.,
 periodic_x                          = .false.,.false.,.false.,
 symmetric_xs                        = .false.,.false.,.false.,
 symmetric_xe                        = .false.,.false.,.false.,
 open_xs                             = .false.,.false.,.false.,
 open_xe                             = .false.,.false.,.false.,
 periodic_y                          = .false.,.false.,.false.,
 symmetric_ys                        = .false.,.false.,.false.,
 symmetric_ye                        = .false.,.false.,.false.,
 open_ys                             = .false.,.false.,.false.,
 open_ye                             = .false.,.false.,.false.,
 nested                              = .false., .true., .true.,
 real_data_init_type                 = $DA_FG_FORMAT,
/
 &namelist_quilt
 nio_tasks_per_group = 0,
 nio_groups = 1,
 /

EOF

#-----------------------------------------------------------------------
# [3.0] Create Wnamelist.3dvar file:
#-----------------------------------------------------------------------

cat >! namelist.3dvar << EOF

&record1
 model_type = 'WRF',
 write_increments = .FALSE., 
 global           = .FALSE.,
 print_detail = 100,
 lvar4d = .FALSE.,
/

&record2
 analysis_type = '3D-VAR',
 analysis_date = '$DA_ANALYSIS_DATE',
 analysis_accu = 900,
 w_increments  = .FALSE.,
 dt_cloud_model =.FALSE..
 write_qcw = .FALSE.,  
 write_qrn = .FALSE.,  
 write_qci = .FALSE.,  
 write_qsn = .FALSE.,  
 write_qgr = .FALSE.,
/

&record3
 fg_format = $DA_FG_FORMAT, 
 ob_format = $DA_OB_FORMAT,
 num_fgat_time = 1,
/

&record4
 process_obs    = 'YES',
 obs_qc_pointer = 0,
 use_SynopObs   = .TRUE.,
 use_ShipsObs   = .TRUE.,
 use_MetarObs   = .TRUE.,
 use_RadarObs   = .FALSE.,
 use_Radar_rv   = .FALSE.,
 use_Radar_rf   = .FALSE.,
 use_PilotObs   = .TRUE.,
 use_SoundObs   = .TRUE.,
 use_SatemObs   = .TRUE.,
 use_GeoAMVObs  = .FALSE.,
 use_PolarAMVObs= .FALSE.
 use_AirepObs   = .TRUE.,
 use_GpspwObs   = .FALSE.,
 use_GpsrefObs  = .FALSE.,
 use_SsmiRetrievalObs = .FALSE.,
 use_SsmiTbObs  = .FALSE.,
 use_ssmt1obs   = .FALSE.,
 use_ssmt2obs   = .FALSE.,
 use_qscatobs   = .TRUE.,
 use_ProfilerObs= .TRUE.,
 use_BuoyObs    = .TRUE.,
 use_BogusObs   = .FALSE.,
 check_max_iv   = .TRUE.,
 use_obs_errfac = .FALSE.,
 put_rand_seed  = .FALSE.,
 omb_set_rand   = .FALSE.,
 omb_add_noise  = .FALSE.,
/

&record5
 time_window    = 3.,
 print_obs_info = .TRUE.,
/

&record6
 max_ext_its    = 2,
 eps            = 1.E-02, 1.E-02, 1.E-02, 1.E-02, 1.E-02, 1.E-02, 1.E-02,,
 ntmax          = 3,
 nsave          = 4,
 write_switch   = .FALSE.,
 write_interval = 5,
/

&record7
 rf_passes      = 6,
 var_scaling1   = 1.0,
 var_scaling2   = 1.0,
 var_scaling3   = 1.0,
 var_scaling4   = 1.0,
 var_scaling5   = 1.0,
 len_scaling1   = 1.0,
 len_scaling2   = 1.0,
 len_scaling3   = 1.0,
 len_scaling4   = 1.0,
 len_scaling5   = 1.0,
/

&record8
 def_sub_domain = .FALSE.,
 x_start_sub_domain = 55.0,
 y_start_sub_domain = 35.0,
 x_end_sub_domain   = 80.0,
 y_end_sub_domain   = 60.0,
/

&record10
 Testing_3DVAR  = .FALSE.,
 Test_Transforms = .FALSE.,
 Test_Statistics = .FALSE.,
 Interpolate_Stats = .TRUE.,
/
 
&record11
 minimisation_option = 2,
 write_outer_loop    = .FASLE., 
 lat_stats_option    = .FALSE..
calculate_cg_cost_function = .FALSE.,
 cv_options     = $DA_CV_OPTIONS,
 cv_options_hum = 1,
 check_rh       = 1,
 as1            = 0.25, 0.75, 1.5,
 as2            = 0.25, 0.75, 1.5,
 as3            = 0.25, 0.75, 1.5,
 as4            = 0.25, 0.75, 1.5,
 as5            = 0.25, 0.75, 1.5,
 sfc_assi_options = 1,
 set_omb_rand_fac = 1.0,
 seed_array1    = 0,
 seed_array2    = 0,
/
 
&record12
 balance_type   = 1,
/
 
&record13
 vert_corr      = 2,
 vertical_ip    = 0,
 vert_evalue    = 1,
 max_vert_var1  = 99.0,
 max_vert_var2  = 99.0,
 max_vert_var3  = 99.0,
 max_vert_var4  = 99.0,
 max_vert_var5  = 0.0,
/
 
&pseudo_ob_nl
 num_pseudo     = 0, 
 pseudo_x       = 1.0,
 pseudo_y       = 1.0,
 pseudo_z       = 1.0,
 pseudo_val     = 1.0,
 pseudo_err     = 1.0,
 pseudo_var     = 't',
/

EOF

#-------------------------------------------------------------------
# [3.0] Sanity check
#-----------------------------------------------------------------------


 foreach f ("namelist.input" "namelist.3dvar")

    if (! -e $f) then
        echo "\n"
        echo "Cannot find file $RUN_DIR/$f"
        echo "\n"
        exit -1
    endif

 end

 foreach f ($WRFVAR_EXE $LANDUSE)

    if (! -e $f) then
        echo "\n"
        echo "Cannot find file $f"
        echo "\n"
        exit -1
    endif

 end

 foreach f ($DA_FIRST_GUESS $DA_BACK_ERRORS $DA_OBSERVATIONS)

    if (! -e $f) then
        echo "\n"
        echo "Cannot find file $f"
        echo "\n"
        exit -1
    endif

 end

#-----------------------------------------------------------------------
# [3.0] Prepare for assimilation:
#-----------------------------------------------------------------------

 if ($DA_FG_FORMAT == 2) then # MM5
 ln -sf $DA_FIRST_GUESS         mm5_3dvar_input
 else                         # WRF
 ln -sf $DA_FIRST_GUESS		wrf_3dvar_input
 endif
 ln -sf $DA_BACK_ERRORS		fort.3${DA_CV_OPTIONS}
 ln -sf $DA_OBSERVATIONS	fort.9${DA_OB_FORMAT}
 ln -sf $LANDUSE                LANDUSE.TBL
 ln -sf $WRFVAR_EXE             wrfvar.exe

 echo "First Guess Input File:      $DA_FIRST_GUESS"
 echo "Background Error Input File: $DA_BACK_ERRORS"
 echo "Observation Input File:      $DA_OBSERVATIONS"

#-------------------------------------------------------------------
# [4.0] Run WRF-Var:
#-------------------------------------------------------------------

#PC-Linux:
#if ( $NUM_PROCS > 1  )then
# cp $WRFVAR_DIR/run/hosts .
#   mpirun -v -np ${NUM_PROCS} -nolocal -machinefile hosts ./wrfvar.exe >&! /dev/null
#else
#   mpirun -v -np 1 ./wrfvar.exe >&! /dev/null #Assumes compile in DM mode.
#endif

#DEC:
./wrfvar.exe #>&! wrfvar.out

#IBM (llsubmit):
#poe ./wrfvar.exe
#mpirun -np ${NUM_PROCS} ./wrfvar.exe

cp fort.12 DAProg_WRF-Var.statistics >&! /dev/null
cp fort.81 DAProg_WRF-Var.cost_fn >&! /dev/null
cp fort.82 DAProg_WRF_Var.grad_fn >&! /dev/null

echo "WRF-Var completed"

exit (0)
