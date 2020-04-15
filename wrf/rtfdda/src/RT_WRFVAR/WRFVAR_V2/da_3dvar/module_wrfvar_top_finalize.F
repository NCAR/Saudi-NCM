!WRFVAR:DRIVER_LAYER:TOP
!

MODULE module_wrfvar_top_finalize
!<DESCRIPTION>
! This module defines top-level wrfvar_init(), wrfvar_run(), and wrfvar_finalize() 
! routines.  
!</DESCRIPTION>

   USE module_machine
   USE module_domain
   USE module_integrate
   USE module_driver_constants
   USE module_configure

   USE module_timing
   USE module_wrf_error

#ifdef DM_PARALLEL
   USE module_dm
#endif

   USE module_wrf_3dvar_io
   USE module_wrf_3dvar_interface

   IMPLICIT NONE

   REAL    :: time

   INTEGER :: loop , &
              levels_to_process

   TYPE (domain) , POINTER :: keep_grid, grid_ptr, null_domain
   TYPE (grid_config_rec_type), SAVE :: config_flags
   INTEGER                 :: number_at_same_level
   INTEGER                 :: time_step_begin_restart

   INTEGER :: max_dom , domain_id , fid , oid , idum1 , idum2 , ierr
   INTEGER :: debug_level
   LOGICAL :: input_from_file

#ifdef DM_PARALLEL
   INTEGER                 :: nbytes
   INTEGER, PARAMETER      :: configbuflen = 4* CONFIG_BUF_LEN
   INTEGER                 :: configbuf( configbuflen )
   LOGICAL , EXTERNAL      :: wrf_dm_on_monitor
#endif

   CHARACTER (LEN=80)      :: rstname
   CHARACTER (LEN=80)      :: message

!  INTERFACE  ! Commented May 2006, already defined in module_wrfvar_top_init
!    SUBROUTINE Setup_Timekeeping( grid )
!     USE module_domain
!     TYPE(domain), POINTER :: grid
!    END SUBROUTINE Setup_Timekeeping
!  END INTERFACE


CONTAINS


   SUBROUTINE wrfvar_finalize( no_shutdown )
!<DESCRIPTION>
!     WRF finalize routine.
!</DESCRIPTION>
     LOGICAL, OPTIONAL, INTENT(IN) :: no_shutdown

!--output wrfvar analysis

   if((config_flags%real_data_init_type == 1) .or. (config_flags%real_data_init_type == 3)) then
     CALL med_initialdata_output_3dvar( head_grid , config_flags )

     CALL med_shutdown_io ( head_grid , config_flags )
     CALL       wrf_debug ( 100 , 'wrf: back from med_shutdown_io' )
   endif

   CALL       wrf_debug (   0 , 'wrfvar: SUCCESS COMPLETE WRFVAR' )

   IF ( .NOT. PRESENT( no_shutdown ) ) THEN
     ! Finalize time manager
     CALL WRF_UTIL_Finalize
     CALL wrf_shutdown
   ENDIF

   END SUBROUTINE wrfvar_finalize

END MODULE module_wrfvar_top_finalize


