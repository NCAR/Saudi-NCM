!
! List of namelist parameters needed by QC program
!
 REAL, PARAMETER    :: ussalr = 0.0065    ! lapse rate in Degree C per m by default

 INTEGER :: max_number_of_obs_nml
 LOGICAL :: fatal_if_exceed_max_obs
 LOGICAL :: model_qc
 LOGICAL :: turbulence_weighting

 REAL    :: tlapse     ! lapse rate in Degree C per m specified in namelist
 REAL    :: hsfc_below_diff_max !Maximal allowed difference in m of height 
                                ! for a station below the model surface
 REAL    :: tolerance_t, tolerance_p, tolerance_q, tolerance_u
 REAL    :: tolerance_t_sound, tolerance_p_sound, & 
            tolerance_q_sound, tolerance_u_sound

 CHARACTER (LEN=80) :: error_type

 LOGICAL :: log_time = .FALSE.

 INTEGER :: log_unit = 6
 INTEGER :: time_before, time_after

 INTEGER :: v_interp_coord

   !  The following parameters are used as flags to indicate either a
   !  MISSING data value (real number) in the "measurement" TYPE or an
   !  "end of data" flag (two fields with end_data  flag mean end of
   !  measurements for one observation).  The undefined values are when
   !  a GTS observation is read but determined to be in error (not according
   !  to the code).  Geopotential height is referenced as a positive value
   !  undefined, which explains the duplicate. Flags missing_r and missing
   !  are already defined in DA_Constants.F .

   INTEGER , PARAMETER   ::  undefined1    =  999999
   REAL    , PARAMETER   ::  undefined1_r  =  999999.
   INTEGER , PARAMETER   ::  undefined2    = -999999
   REAL    , PARAMETER   ::  undefined2_r  = -999999.
!  INTEGER , PARAMETER   ::  missing       = -888888
!  REAL    , PARAMETER   ::  missing_r     = -888888.
   INTEGER , PARAMETER   ::  end_data      = -777777
   REAL    , PARAMETER   ::  end_data_r    = -777777.

   !  QC values greater than 100000 (6 digits) are used for special purposes.
   !  Apply to Height and pressure (first_guess, model_surface, 
   !  model_surface_below). Other variables have QC between 0 (poor) and 
   !  10 very good.
   !


   INTEGER , PARAMETER   ::  first_guess   = 100000
   INTEGER , PARAMETER   ::  surface_below = 200000
   INTEGER , PARAMETER   ::  surface_above = 400000

   ! Maximal difference in altitude between the model surface and any stations
   ! below the model surface.

   REAL, PARAMETER                                :: height_below_difference_max = 500  ! m
