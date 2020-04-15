!MEDIATION_LAYER:SOLVE_V3D
!

SUBROUTINE solve_qc ( grid , config_flags , &
#include <em_dummy_args.inc>
                 )

! Driver layer modules
   USE module_domain
   USE module_configure
   USE module_driver_constants
   USE module_machine
   USE module_tiles
   USE module_dm
! Mediation layer modules
! Model layer modules
   USE module_model_constants

   USE DA_Constants
   USE DA_Define_Structures
   USE DA_Setup_Structures
   USE DA_Test
   USE DA_Tools
   USE DA_Minimisation
   USE par_util
!???   USE DA_Decoded
   USE DA_Decoded_Module

! Registry generated module
   USE module_state_description

   IMPLICIT NONE

   TYPE(domain),                intent(inout) :: grid
   TYPE (grid_config_rec_type), intent(inout) :: config_flags

   !  Definitions of dummy arguments to solve
#include <em_dummy_decl.inc>

   TYPE (xbx_type)              :: xbx         ! For header & non-grid arrays.
   TYPE (be_type)               :: be          ! Background error structure.
   TYPE (y_type)                :: ob          ! Observation structure.
   TYPE (ob_type)               :: iv          ! Obs. increment structure.

   TYPE (report), DIMENSION   (:), ALLOCATABLE :: obs, mdl, err
   INTEGER,       DIMENSION   (:), ALLOCATABLE :: index

!  INTEGER                      :: dco_unit  = 99
   INTEGER                      :: iunit_out
   INTEGER                      :: it
   INTEGER                      :: ids , ide , jds , jde , kds , kde , &
                                   ims , ime , jms , jme , kms , kme , &
                                   its , ite , jts , jte , kts , kte

   LOGICAL :: ldebug = .TRUE.
   INTEGER :: max_number_obs, number_obs

    CHARACTER (LEN=80)                :: file_out

!------------------------------------------------------------------------------

   WRITE (6, '(/,A,/)') ' ***  QC OPERATIONS ***'

!------------------------------------------------------------------------------
!  [1.0] Read 3DVAR namelist:
!------------------------------------------------------------------------------

   CALL da_read_namelist

!------------------------------------------------------------------------------
!  [2.0] Initialise 3D-Var parameters:
!------------------------------------------------------------------------------

   CALL da_init_3dvar( grid, xp, xb, &
                       ids, ide, jds, jde, kds, kde, &
                       ims, ime, jms, jme, kms, kme, &
                       its, ite, jts, jte, kts, kte )

!------------------------------------------------------------------------------
!  [3.0] Set up first guess field (xb):
!------------------------------------------------------------------------------

   CALL da_setup_firstguess( xbx, grid, &
#include <em_dummy_args.inc>
                           )

!------------------------------------------------------------------------------
!  [4.0] Set up observations (ob):
!------------------------------------------------------------------------------

!  CALL da_setup_obs_structures( xp, ob, iv )

!------------------------------------------------------------------------------
!  [5.0] Set up background errors (be):
!------------------------------------------------------------------------------

!  CALL da_setup_background_errors( xb, xbx, be, xp )

!------------------------------------------------------------------------------
!  [6.0] Load observations in decoded format
!------------------------------------------------------------------------------

   ALLOCATE (obs   (max_number_of_obs_nml))
   ALLOCATE (index (max_number_of_obs_nml))

   CALL decoded (max_number_of_obs_nml, fatal_if_exceed_max_obs, &
                 obs, index, number_obs, ldebug, grid, &
#include <em_dummy_args.inc>
)
      
!------------------------------------------------------------------------------
!  [7.0] ERROR MAX QC BASED ON OBSERVATIONS - MODEL DIFFERENCES
!------------------------------------------------------------------------------

! 7.1 Allocate space for model interpolation
!     --------------------------------------

      ALLOCATE (mdl (max_number_of_obs_nml))
      ALLOCATE (err (max_number_of_obs_nml))

! 7.2 Model interpolation
!     -------------------
      CALL intp_wrf (max_number_of_obs_nml, obs, mdl, index, number_obs, &
                     ldebug, grid, &
#include <em_dummy_args.inc>
)

! 7.3 Observational error interpolation
!     --------------------------------
      CALL intp_err (max_number_of_obs_nml, obs, mdl, err, index, number_obs, &
                     ldebug, grid, &
#include <em_dummy_args.inc>
)

! 7.2 Error Max QC based on innovation  (O-B)
!     ---------------------------------------

      CALL qc_fdda (max_number_of_obs_nml, obs, mdl, err, index, number_obs, &
                    ldebug, grid, &
#include <em_dummy_args.inc>
)

!-------------------------------------------------------------------------------
! [8.0] Output processed and model interpolated data in original format
!-------------------------------------------------------------------------------

! 8.1 Open file for observations output
!     ---------------------------------

      file_out  = "obs_wrfqc_output"
      iunit_out = dco_unit

! 8.2 Write out observations
!     ----------------------

      CALL write_decoded (file_out, iunit_out, number_obs, obs, grid, index)

! 8.3 Open file for model interpolated output
!     ---------------------------------------

      file_out  = "obs_wrfqc_virtual"
      iunit_out = dco_unit

! 8.4 Write out model interpolated data
!     ---------------------------------

      CALL write_decoded (file_out, iunit_out, number_obs, mdl, grid, index)

! 8.5 Open file for error profiles output
!     -----------------------------------

      file_out  = "obs_wrfqc_error"
      iunit_out = dco_unit

! 8.6 Write out error interpolated data
!     ---------------------------------

      CALL write_decoded (file_out, iunit_out, number_obs, err, grid, index)

! 8.7 Open file for observation plotting
!     ----------------------------------

      file_out  = "obs_wrfqc_pairs"
      iunit_out = dco_unit

! 8.8 Write out observations for plotting
!     ------------------------------------

      CALL plotobs_out (file_out, iunit_out, number_obs, obs, mdl, err, index)

! 8.9 Open file 3dvar ingest
!     ----------------------

      file_out  = "obs_wrfqc_3dvar"
      iunit_out = dco_unit

! 8.10 Write out observations for 3dvar
!      --------------------------------

      CALL write_3dvar (file_out, iunit_out, number_obs, obs, err, index, &
                        analysis_date, xb, grid)

!-------------------------------------------------------------------------------
! [9.0] End 
!-------------------------------------------------------------------------------

! 9.1 Free memory
!     -----------

      DEALLOCATE (obs)
      DEALLOCATE (mdl)
      DEALLOCATE (err)
      DEALLOCATE (index)

! 9.2 Print out end message
!     ---------------------

!     CALL prelog
!     WRITE  (log_unit,'(A)')  &
!     "-----------------------------------------------------------------------"
!     CALL prelog
!     WRITE  (log_unit,'(A,/)') "Succesful Completion of DAProg_Decoded"

      WRITE (6,'(A)')  &
     "-----------------------------------------------------------------------"
      WRITE (6,'(A,/)') ' *** QC completed successfully ***'

      CALL wrf_debug ( 200 , ' call end of da_solve_qc' )

CONTAINS

#include "da_init_3dvar.inc"

END SUBROUTINE solve_qc

