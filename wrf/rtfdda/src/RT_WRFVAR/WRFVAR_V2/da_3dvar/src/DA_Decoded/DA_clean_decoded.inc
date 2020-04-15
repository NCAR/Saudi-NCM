       SUBROUTINE clean_decoded (number_obs, obs)
!------------------------------------------------------------------------------!
! Make sure that stations with WMO code corresponding to surface type
! (FM-12, 13, 14, 15, etc) have only one vertical level.
! Turn the WMO code to the most appropriate WMO code, if not.
! 
! Francois VANDENBERGHE, December 2002
!------------------------------------------------------------------------------!
      IMPLICIT NONE
!------------------------------------------------------------------------------!
! ARGUMENTS

      INTEGER, INTENT (in) :: number_obs
      TYPE (report), DIMENSION (number_obs), INTENT (inout) :: obs

!------------------------------------------------------------------------------!
! LOCAL VARIABLES

      INTEGER :: obs_num, i, j

!------------------------------------------------------------------------------!

      DO obs_num = 1, number_obs

! 1. WIND PROFILER APPEARS WITH WMO CODE 12, SET TO 32 (PILOT)
! ============================================================

! 1.1 Correct WMO code
!     ----------------

        i = index ('PROFILER', obs (obs_num) % location % name)
        j = index ('FM-12',    obs (obs_num) % info % platform)

        IF ((i > 0) .AND. (j > 0)) THEN
           obs (obs_num) % info % platform = &
          "FM-32 PILOT                             "
        ENDIF

! 1.2 Set the sounding flag to true
!     -----------------------------

        IF (.NOT. obs (obs_num) % info % is_sound) THEN
            obs (obs_num) % info % is_sound =  .TRUE.
        ENDIF

! 1.3 Set the bogus flag to false
!     ---------------------------

        IF (obs (obs_num) % info % bogus) THEN
            obs (obs_num) % info % bogus =  .FALSE.
        ENDIF

! 2.  RYPG_SDG (??) APPEARS WITH WMO CODE 14, SET TO 38 (TEMP MOBIL)
! ================================================================

! 2.1 Correct WMO code
!     ----------------

        i = index ('Rypg_sdg', obs (obs_num) % location % name)
        j = index ('FM-14',    obs (obs_num) % info % platform)

        IF ((i > 0) .AND. (j > 0)) THEN
           obs (obs_num) % info % platform = &
          "FM-38 TEMP MOBIL                        "
        ENDIF

! 2.2 Set the sounding flag to true
!     -----------------------------

        IF (.NOT. obs (obs_num) % info % is_sound) THEN
            obs (obs_num) % info % is_sound =  .TRUE.
        ENDIF

! 2.3 Set the bogus flag to false
!     ---------------------------

        IF (obs (obs_num) % info % bogus) THEN
            obs (obs_num) % info % bogus =  .FALSE.
        ENDIF

! 3.  SAT WIND APPEARS WITH WMO CODE 14, SET TO 38 (TEMP MOBIL)
! ================================================================

! 3.1 Correct WMO code
!     ----------------
        i = index ('SATWNDS', obs (obs_num) % location % name)
        j = index ('FM-12',   obs (obs_num) % info % platform)

        IF ((i > 0) .AND. (j > 0)) THEN
           obs (obs_num) % info % platform = &
          "FM-88 SATOB                             "
        ENDIF

! 3.2 Set the sounding flag to false
!     ------------------------------

        IF (obs (obs_num) % info % is_sound) THEN
            obs (obs_num) % info % is_sound =  .FALSE.
        ENDIF

! 3.3 Set the bogus flag to false
!     ---------------------------

        IF (obs (obs_num) % info % bogus) THEN
            obs (obs_num) % info % bogus =  .FALSE.
        ENDIF

     ENDDO

 END SUBROUTINE clean_decoded
!------------------------------------------------------------------------------!
