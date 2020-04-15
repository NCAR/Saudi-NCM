!WRFVAR:DRIVER_LAYER:TOP
!

MODULE module_wrfvar_top_run
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

   SUBROUTINE wrfvar_run( )
!<DESCRIPTION>
!     WRF run routine.
!</DESCRIPTION>

!<DESCRIPTION>
! Once the top-level domain has been allocated, configured, and
! initialized, the model time integration is ready to proceed.  The start
! and stop times for the domain are set to the start and stop time of the
! model run, and then <a href=integrate.html>integrate</a> is called to
! advance the domain forward through that specified time interval.  On
! return, the simulation is completed.  A Mediation Layer-provided
! subroutine, <a href=med_shutdown_io.html>med_shutdown_io</a> is called
! to allow the the model to do any I/O specific cleanup and shutdown, and
! then the WRF Driver Layer routine <a
! href=wrf_shutdown.html>wrf_shutdown</a> (quilt servers would be
! directed to shut down here) is called to properly end the run,
! including shutting down the communications (for example, most comm
! layers would call MPI_FINALIZE at this point if they're using MPI).
! 
!</DESCRIPTION>

   !  The forecast integration for the most coarse grid is now started.  The
   !  integration is from the first step (1) to the last step of the simulation.

!--do wrfvar

   CALL wrf_debug ( 100 , 'wrfvar: calling wrfvar ' )
   CALL da_3dvar_interface ( head_grid, config_flags )
   CALL wrf_debug ( 100 , 'wrfvar: back from wrfvar ' )

   END SUBROUTINE wrfvar_run

END MODULE module_wrfvar_top_run


