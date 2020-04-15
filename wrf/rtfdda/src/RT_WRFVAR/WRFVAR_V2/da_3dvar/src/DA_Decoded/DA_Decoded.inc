
 SUBROUTINE Decoded (max_number_obs, fatal_exceed, obs, index, number_obs, &
                     ldebug, grid, &
#include <em_dummy_args.inc>
)
!------------------------------------------------------------------------------!
!  Read "decoded" formatted observations, merge same stations together, 
!  compute pressure or height if missing when requested.
!
! Francois VANDENBERGHE, December 2002
!------------------------------------------------------------------------------!
  IMPLICIT NONE
  INTRINSIC MAX

!------------------------------------------------------------------------------!
! ARGUMENTS    

  INTEGER,            INTENT (in)  :: max_number_obs
  LOGICAL,            INTENT (in)  :: fatal_exceed

  TYPE (report), DIMENSION (max_number_obs), INTENT (out) :: obs
  INTEGER,       DIMENSION (max_number_obs), INTENT (out) :: index
  INTEGER,                                   INTENT (out) :: number_obs

  LOGICAL,            INTENT (in)  :: ldebug
  TYPE(domain),      intent(inout) :: grid
!------------------------------------------------------------------------------!
! GENERAL VARIABLES

  INTEGER :: loop_index
  INTEGER :: total_discard
  INTEGER :: total_dups_loc, total_dups_time
  INTEGER :: total_uncomplete
  INTEGER :: obs_num
  INTEGER :: nlevels
  INTEGER :: io_error

  LOGICAL :: ok, file_formatted

  INTEGER             :: diag_unit
  CHARACTER (LEN=80)  :: diag_file
  CHARACTER (LEN=80)  :: dco_filename

  CHARACTER (LEN=132) :: error_message

  CHARACTER (LEN=80)  :: message
  CHARACTER (LEN=32)  :: proc_name = "Decoded "

! INTEGER, PARAMETER  :: dia_unit = 98
! INTEGER, PARAMETER  :: dco_unit = 99
!------------------------------------------------------------------------------!
! DECODED DATA

  TYPE (measurement), POINTER :: current
  REAL    :: missing_flag

#include <em_dummy_decl.inc>

!------------------------------------------------------------------------------!

! 1.  OBSERVATIONS LOADING
! ========================

! 1.1 Open observation file
!     --------------------

      dco_filename   = "obs_wrfqc_input"
      file_formatted = .TRUE.

      CALL openin_unit (dco_filename, dco_unit, proc_name, file_formatted)

! 1.1 Read file
!     ---------

      CALL read_decoded (dco_unit, max_number_obs, fatal_exceed, obs, &
                         number_obs, missing_flag, grid, &
#include <em_dummy_args.inc>
)

! 1.2 Make sure input file is closed
!     ------------------------------

      INQUIRE (UNIT=dco_unit, OPENED=ok)

      IF (ok) CLOSE (UNIT=dco_unit)

! 1.2 Check for any data
!     -----------------

      IF (number_obs <= 0) THEN
          error_message = proc_name // &
         "Could not find any valid reports in obs. file."
          CALL error_handler (log_unit, dco_unit, error_message, .TRUE., .TRUE.)
      ENDIF

! 1.3 Clean data (not used anymore)
!     ----------

!     CALL clean_decoded (number_obs, obs)

! 1.4 Count valid stations read in
!     ----------------------------

      message = "Stations break-down after read in:"

      CALL count_stations (number_obs, obs, message)
 
!-------------------------------- SORT DATA -----------------------------------!


! 2.  OBSERVATION SORTING
! =======================

      !  Sort the observations so that they can be merged easily.  Really
      !  what happens is that the index array is uniquely sorted by location
      !  (except for observations that are from the same "place").  This
      !  puts duplicate location observations next to each other.
      !  Then, merge the observations to (try to) remove all duplicates and
      !  build composite data.
      !  Then, sort obervations upon time, type and location


! 2.1 Sort station per location
!     -------------------------

      CALL sort_obs (number_obs, obs, compare_loc, index)

! 2.2 Write loaded data in original format
!     ------------------------------------

      IF (ldebug) THEN
          diag_file = "obs_wrfqc_loaded"
          diag_unit = dco_unit
          CALL write_decoded (diag_file, diag_unit, number_obs, obs, grid, index)
      ENDIF

! 2.3 Merge duplicate stations (same location and same time)
!     ------------------------------------------------------
!     Because data merging is based on the pressure, the missing
!     pressure have been recovered in read_obs

      CALL check_duplicate_loc (number_obs, obs ,index, total_dups_loc, &
                                decoded_date_start)

! 2.4 Total Number of stations removed
!     --------------------------------

      total_discard = total_dups_loc + total_dups_time + total_uncomplete

! 2.5 Sort obs chronologically
!     ------------------------

      CALL sort_obs (number_obs, obs, compare_tim, index)

      ! Temporarily set back index to increasing order
      ! index = (/ ( loop_index , loop_index = 1 , number_obs ) /)

! 2.6  Count valid stations after processing
!      -------------------------------------

      message = "Stations break-down after processing:"

      CALL count_stations (number_obs, obs, message)

! 2.7  Count upper levels per valid stations after processing
!      ------------------------------------------------------

      CALL count_levels (number_obs, obs, index)

! 2.8 Write out observations after processing
!     ---------------------------------------

      IF (ldebug) THEN
          diag_file = "obs_wrfqc_merged"
          diag_unit = dco_unit
          CALL write_decoded (diag_file, diag_unit, number_obs, obs, grid, index)
      ENDIF
!------------------------------- END ------------------------------------------!

      RETURN

 END SUBROUTINE Decoded
!------------------------------------------------------------------------------!
