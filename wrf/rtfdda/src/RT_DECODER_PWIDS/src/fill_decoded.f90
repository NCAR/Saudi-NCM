    SUBROUTINE fill_decoded (station, pwid, obs)
!------------------------------------------------------------------------------!
! Fill the decoded data structure for pwids data.
! (decoded is the observation input format for MM5 RAWINS and LITTLE programs.
!
! Copyright UCAR (c) 1992 - 2004.
! University Corporation for Atmospheric Research (UCAR),
! National Center for Atmospheric Research (NCAR),
! Research Applications Program (RAP),
! P.O.Box 3000, Boulder, Colorado, 80307-3000, USA,
! Francois VANDENBERGHE, vandenb@ucar.edu, April 2004. 
!------------------------------------------------------------------------------!
      USE module_decoded
      USE module_pwids
      USE module_date
!------------------------------------------------------------------------------!

      IMPLICIT NONE

      TYPE (pwid_station_type), INTENT (in)  :: station
      TYPE (pwid_type),         INTENT (in)  :: pwid
      TYPE (report),            INTENT (out) :: obs

      INTEGER              :: k, n, levels
      INTEGER              :: ccyy, yy, mm, dd, hh, mn, ss
      CHARACTER (LEN = 19) :: date_tmp1, date_tmp2

!------------------------------------------------------------------------------!

! 1.  FILL THE LOCATION DATA STRUCTURE
! =====================================

      obs % location % latitude  = station % latitude
      obs % location % longitude = station % longitude

      IF (obs % location % longitude .GT. 180.) THEN
          obs % location % longitude = obs % location % longitude - 360.
      ENDIF

      IF (station % id /= pwid % id) THEN
          WRITE (*,'(/,A,I4)') "ERROR: station  id = ",station % id
          WRITE (*,'(A,I4,/)') "       but pwid id = ",pwid % id
          CALL ABORT
      ENDIF

!     WRITE (obs % location % id,'(I5.5)') station % id
      WRITE (obs % location % id,'(I5.5)') pwid % id

      obs % location % name = " PWIDS "// TRIM (station % description (1:33))


! 2. FILL THE SOURCE INFO DATA STRUCTURE
! ======================================

      obs % info % platform    = "FM-12 SYNOP"
      WRITE (obs % info % source,'(A,I6)')  "LOGGER ID ", pwid % logger_id
      obs % info % elevation   = station % elevation
      obs % info % num_vld_fld = missing
      obs % info % num_error   = missing 
      obs % info % num_warning = missing 
      obs % info % num_dups    = missing 
      obs % info % is_sound    = .FALSE.
      obs % info % bogus       = .FALSE.
      obs % info % discard     = .FALSE.


! 3.  FILL THE TIME INFO DATA STRUCTURE
! =====================================

!...Date ccyy-mm-dd_hh:mn:ss Into ccyymmddhhmnss

      WRITE (obs % valid_time % date_char,'(A4,A2,A2,A2,A2,A2)') &
      pwid % date19 ( 1: 4), pwid % date19 ( 6: 7), pwid % date19 ( 9:10), &
      pwid % date19 (12:13), pwid % date19 (15:16), pwid % date19 (18:19)

!...Second accumulated from date_start_out

      date_tmp1 = pwid % date19
      date_tmp2 = decoded_date_start

      CALL GETH_IDTS (date_tmp1, date_tmp2, obs % valid_time % sut)

!...Julian day

      READ (pwid % date19 ( 1: 4),'(I4)') ccyy 
      READ (pwid % date19 ( 6: 7),'(I2)') mm
      READ (pwid % date19 ( 9:10),'(I2)') dd
      READ (pwid % date19 (12:14),'(I2)') hh

      CALL julian_day (ccyy, mm, dd, obs % valid_time % julian, 1)


! 4.  FILL THE TERRESTRIAL INFORMATION STRUCTURE
! ==============================================

! 4.1 General Reset
!     --------------

      obs % ground % slp      % data = missing_r
      obs % ground % slp      % qc   = missing
      obs % ground % ref_pres % data = missing_r
      obs % ground % ref_pres % qc   = missing
      obs % ground % ground_t % data = missing_r
      obs % ground % ground_t % qc   = missing
      obs % ground % sst      % data = missing_r
      obs % ground % sst      % qc   = missing
      obs % ground % psfc     % data = missing_r
      obs % ground % psfc     % qc   = missing
      obs % ground % precip   % data = missing_r
      obs % ground % precip   % qc   = missing
      obs % ground % t_max    % data = missing_r
      obs % ground % t_max    % qc   = missing
      obs % ground % t_min    % data = missing_r
      obs % ground % t_min    % qc   = missing

      obs % ground % t_min_night % data = missing_r
      obs % ground % t_min_night % qc   = missing
      obs % ground % p_tend03    % data = missing_r
      obs % ground % p_tend03    % qc   = missing
      obs % ground % p_tend24    % data = missing_r
      obs % ground % p_tend24    % qc   = missing
      obs % ground % cloud_cvr   % data = missing_r
      obs % ground % cloud_cvr   % qc   = missing
      obs % ground % ceiling     % data = missing_r
      obs % ground % ceiling     % qc   = missing

! 5.  RESET MEASUREMENT (UPPER LEVELS) DATA STRUCTURE
! ===================================================
       
upper_levels:&
      DO k = 1, pwid_vertical_levels

         obs % each % pressure    % data = missing_r
         obs % each % pressure    % qc   = missing
         obs % each % height      % data = missing_r
         obs % each % height      % qc   = missing
         obs % each % temperature % data = missing_r
         obs % each % temperature % qc   = missing
         obs % each % dew_point   % data = missing_r
         obs % each % dew_point   % qc   = missing  
         obs % each % speed       % data = missing_r
         obs % each % speed       % qc   = missing
         obs % each % direction   % data = missing_r
         obs % each % direction   % qc   = missing
         obs % each % u           % data = missing_r
         obs % each % u           % qc   = missing
         obs % each % v           % data = missing_r
         obs % each % v           % qc   = missing
         obs % each % rh          % data = missing_r
         obs % each % rh          % qc   = missing
         obs % each % thickness   % data = missing_r
         obs % each % thickness   % qc   = missing


! 8.  FILL UPPER LEVELS INFORMATION STRUCTURE WITH OBSERVATIONS 
! =============================================================

! 8.1 Height is read in file obs relatively to radiometer elevation
!     -------------------------------------------------------------

        IF (ABS (station % elevation - hei_missing) > 0.) THEN
            obs % each % height % data = station % elevation
            obs % each % height % qc   = 0
         ENDIF

! 8.2 Wind speed and direction
!     ------------------------

        IF (ABS (pwid  % spd - spd_missing) > 0.) THEN
            obs % each % speed % data = pwid % spd ! Wind Speed in m/s
            obs % each % speed % qc   = 0
        ENDIF

        IF (ABS (pwid  % dir - dir_missing) > 0.) THEN
            obs % each % direction % data = pwid % dir ! Wind direction in degree North
            obs % each % direction % qc   = 0
        ENDIF

! 8.4 Temperature and humidity
!     ------------------------

        IF (ABS (pwid  % tem - tem_missing) > 0.) THEN
            obs % each % temperature % data = pwid % tem + 273.15 !  Temperature in K 
            obs % each % temperature % qc   = 0
        ENDIF

        IF (ABS (pwid  % rhu - rhu_missing) > 0.) THEN
            obs % each % rh  % data = pwid % rhu ! Humidity in %
            obs % each % rh  % qc   = 0
        ENDIF

      ENDDO upper_levels

      obs % info % num_vld_fld = pwid_vertical_levels

      END SUBROUTINE fill_decoded
