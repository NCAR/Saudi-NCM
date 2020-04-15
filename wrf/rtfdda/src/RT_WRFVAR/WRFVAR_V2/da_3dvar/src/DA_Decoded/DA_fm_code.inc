
 SUBROUTINE  fm_code (fm, platform_string, reset, print_out)
!------------------------------------------------------------------------------!
! Given the WMO integer code fm, return the observation platform string
! and increment the corresponding counter. Any unknown code returns the
! "UNKNOWN" Platform string.
! 
! When reset      is present, all the counters are reset to 0
! When  print_out is present, all the non zero counters are print out
! 
!   F. VANDENBERGHE, March 2001
!------------------------------------------------------------------------------!
! ARGUMENTS

    IMPLICIT NONE
    INTEGER,              INTENT (in),  OPTIONAL :: fm
    CHARACTER (LEN = 40), INTENT (out), OPTIONAL :: platform_string
    LOGICAL,              INTENT (in),  OPTIONAL :: reset
    LOGICAL,              INTENT (in),  OPTIONAL :: print_out
!------------------------------------------------------------------------------!
! LOCAL VARIABLES: OBSERVATIONS COUNTER

    INTEGER :: n

    INTEGER,              PARAMETER        :: ns = 200
    INTEGER,              DIMENSION (0:ns) :: stations
    CHARACTER (LEN = 40), DIMENSION (0:ns) :: platform
    CHARACTER (LEN = 40), PARAMETER        :: unknown = 'UNKNOWN'

    SAVE stations
!------------------------------------------------------------------------------!
! PLATFORM NAME AS FUNCTION OF WMO CODE

    platform (0)  = unknown
    platform (12) = 'SYNOP';
    platform (13) = 'SHIP';
    platform (14) = 'SYNOP MOBIL' ;
    platform (15) = 'METAR';
    platform (16) = 'SPECI';
    platform (18) = 'BUOY';
    platform (20) = 'RADOB';
    platform (22) = 'RADREP';
    platform (32) = 'PILOT';
    platform (33) = 'PILOT SHIP';
    platform (34) = 'PILOT MOBIL';
    platform (35) = 'TEMP';
    platform (36) = 'TEMP SHIP';
    platform (37) = 'TEMP DROP';
    platform (38) = 'TEMP MOBIL';
    platform (39) = 'ROCOB';
    platform (40) = 'ROCOB SHIP';
    platform (41) = 'CODAR';
    platform (42) = 'AMDAR';
    platform (43) = 'ICEAN';
    platform (45) = 'IAC';
    platform (46) = 'IAC FLEET';
    platform (47) = 'GRID';
    platform (49) = 'GRAF';
    platform (50) = 'WINTEM';
    platform (51) = 'TAF';
    platform (53) = 'ARFOR';
    platform (54) = 'ROFOR';
    platform (57) = 'RADOF';
    platform (61) = 'MAFOR';
    platform (62) = 'TRACKOB';
    platform (63) = 'BATHY';
    platform (64) = 'TRESAC';
    platform (65) = 'WAVEOB';
    platform (66) = 'HYDRA';
    platform (67) = 'HYFOR';
    platform (71) = 'CLIMAT';
    platform (72) = 'CLIMAT SHIP';
    platform (73) = 'NACLI CLINP SPLCI CLISA INCLI';
    platform (75) = 'CLIMAT TEMP';
    platform (76) = 'CLIMAT TEMP SHIP';
    platform (81) = 'SFAZI';
    platform (82) = 'SFLOC';
    platform (83) = 'SFAZU';
    platform (85) = 'SAREP';
    platform (86) = 'SATEM';
    platform (87) = 'SARAD';
    platform (88) = 'SATOB';
    platform (96) = 'AIREP';
    platform (97) = 'TAMDAR';
    platform (105) = 'PROFW';
    platform (106) = 'WVR';
    platform (111) = 'GPSPW';
    platform (116) = 'GPSRF';
    platform (121) = 'SSMT1';
    platform (122) = 'SSMT2';
    platform (125) = 'SSMI';
    platform (126) = 'SSMI';
    platform (131) = 'TOVS';

!---------------------------------- RESET COUNTER -----------------------------!

    IF (PRESENT (reset)) THEN
      IF (reset) stations = 0
    ENDIF

!---------------------------------- PRINT OUT ---------------------------------!

    IF (PRESENT (print_out)) THEN

      IF (print_out) THEN

          IF (MAXVAL (stations) <= 0) RETURN
        
          CALL prelog
          WRITE  (log_unit,'(A)') "Platform     number"

          DO n = 0, ns
             IF (stations (n) > 0) THEN
                 CALL prelog 
                 WRITE (log_unit,'(A,2X,I5)') platform (n)(1:10), &
                                               stations (n)
             ENDIF
          ENDDO

          CALL prelog
          WRITE  (log_unit,'(A,2X,I5)') "TOTAL     ", SUM (stations)

      ENDIF

    ENDIF

!--------------------------------- COUNT OBS ----------------------------------!

    IF (PRESENT (fm)) THEN

       SELECT CASE ( fm )

          !  Report of surface observations from a fixed land station

          CASE (12) ; 
                stations (fm) = stations (fm) + 1;

          !  Report of surface observations from a sea station

          CASE (13) ; 
                stations (fm) = stations (fm) + 1;

          !  Report of surface observations from a mobile land station

          CASE (14) ; 
                stations (fm) = stations (fm) + 1;

          !  Aviation routine weather report (with/without trend forecast)

          CASE (15) ; 
                stations (fm) = stations (fm) + 1;

          !  Aviation selected special weather report 
          !  (with/without trend forecast)

          CASE (16) ; 
                stations (fm) = stations (fm) + 1;

          !  Report of buoy observation

          CASE (18) ; 
                stations (fm) = stations (fm) + 1;

          !  Report of ground radar weather observation

          CASE (20) ; 
                stations (fm) = stations (fm) + 1;

          !  Radiological data report (monitored on a routine basis and/or 
          !  in case of accident

          CASE (22) ; 
                stations (fm) = stations (fm) + 1;

          !  Upper-wind report from a fixed land station

          CASE (32) ; 
                stations (fm) = stations (fm) + 1;

          !  Upper-wind report from a sea station

          CASE (33) ; 
                stations (fm) = stations (fm) + 1;

          !  Upper-wind report from a mobile land station

          CASE (34) ; 
                stations (fm) = stations (fm) + 1;

          !  Upper-level pressure, temperature, humidity and wind report 
          !  from a fixed land station

          CASE (35) ; 
                stations (fm) = stations (fm) + 1;

          !  Upper-level pressure, temperature, humidity and wind report 
          !  from a sea station

          CASE (36) ; 
                stations (fm) = stations (fm) + 1;

          !  Upper-level pressure, temperature, humidity and wind report 
          !  from a sonde released by carrier balloons and aircraft

          CASE (37) ; 
                stations (fm) = stations (fm) + 1;

          !  Upper-level pressure, temperature, humidity and wind report 
          !  from a mobile land station

          CASE (38) ; 
                stations (fm) = stations (fm) + 1;

          !  Upper-level temperature, wind and air density from a land 
          !  rocketsonde station

          CASE (39) ; 
                stations (fm) = stations (fm) + 1;

          !  Upper-level temperature, wind and air density from a 
          !  rocketsonde station on a ship

          CASE (40) ; 
                stations (fm) = stations (fm) + 1;

          !  Upper-air report from an aircraft 
          !  (other than weather reconnaissance aircraft)

          CASE (41) ; 
                stations (fm) = stations (fm) + 1;

          !  Aircraft report (aircraft meteorological data relay)

          CASE (42) ; 
                stations (fm) = stations (fm) + 1;

          !  Ice analysis

          CASE (43) ; 
                stations (fm) = stations (fm) + 1;

          !  Analysis in full form

          CASE (45) ; 
                stations (fm) = stations (fm) + 1;

          !  Analysis in abbreviated form

          CASE (46) ; 
                stations (fm) = stations (fm) + 1;

          !  Processed data in form of grid-point values

          CASE (47) ; 
                stations (fm) = stations (fm) + 1;

          !  Processed data in form of grid-point values (abbreviated code form)

          CASE (49) ; 
                stations (fm) = stations (fm) + 1;

          !  Forecast upper wind and temperature for aviation

          CASE (50) ; 
                stations (fm) = stations (fm) + 1;

          !  Aerodrome forecast

          CASE (51) ; 
                stations (fm) = stations (fm) + 1;

          !  Area forecast for aviation

          CASE (53) ; 
                stations (fm) = stations (fm) + 1;

          !  Route forecast for aviation

          CASE (54) ; 
                stations (fm) = stations (fm) + 1;

          !  Radiological trajectory dose forecast 
          !  (defined time of arrival and location)

          CASE (57) ; 
                stations (fm) = stations (fm) + 1;

          !  Forecast for shipping

          CASE (61) ; 
                stations (fm) = stations (fm) + 1;

          !  Report of marine surface observations along a ship's track

          CASE (62) ; 
                stations (fm) = stations (fm) + 1;

          !  Report of bathymetrical observation

          CASE (63) ; 
                stations (fm) = stations (fm) + 1;

          !  Temperature salinity and current report from a sea station

          CASE (64) ; 
                stations (fm) = stations (fm) + 1;

          !  Report of spectral wave information from sea station or from
          !  a remote platform (airecraft or satellite)

          CASE (65) ; 
                stations (fm) = stations (fm) + 1;

          !  Report of hydrological observations from a hydrological station

          CASE (66) ; 
                stations (fm) = stations (fm) + 1;

          !  Hydrological forecast

          CASE (67) ; 
                stations (fm) = stations (fm) + 1;

          !  Report of monthly values from a land station

          CASE (71) ; 
                stations (fm) = stations (fm) + 1;

          !  Report of monthly means and total from an ocean weather station

          CASE (72) ; 
                stations (fm) = stations (fm) + 1;

          !  Report of monthly means for an oceanic area

          CASE (73) ; 
                stations (fm) = stations (fm) + 1;

          !  Report of monthly aerological means from a land station

          CASE (75) ; 
                stations (fm) = stations (fm) + 1;

          !  Report of monthly aerological means from an ocean weather station

          CASE (76) ; 
                stations (fm) = stations (fm) + 1;

          !  Synoptic report of bearings of sources of atmospherics

          CASE (81) ; 
                stations (fm) = stations (fm) + 1;

          !  Synoptic report of the geographical location of sources of 
          !  atmopsherics

          CASE (82) ; 
                stations (fm) = stations (fm) + 1;

          !  Detailed report of the distribution of sources of atmospherics
          !  by bearings for any period up to including 24 hours

          CASE (83) ; 
                stations (fm) = stations (fm) + 1;

          !  Report of synoptic interpretation of cloud data obtained by a
          !  meteorlogical satellite

          CASE (85) ; 
                stations (fm) = stations (fm) + 1;

          !  Report of satellite remote upper-air soundings of 
          !  pressure, temperature and humidity

          CASE (86) ; 
                stations (fm) = stations (fm) + 1;

          !  Report of satellite clear radiance observations

          CASE (87) ; 
                stations (fm) = stations (fm) + 1;

          !  Report of satellite remote upper-air soundings of 
          !  pressure, temperature and humidity

          CASE (88) ; 
                stations (fm) = stations (fm) + 1;

          !  Airep reports (not a WMO report)

          CASE (96:97) ;
                stations (fm) = stations (fm) + 1;

          !  Wind Profiler (not a WMO report)

          CASE (105) ; 
                stations (fm) = stations (fm) + 1;

          !  Water vapor radiometer (not a WMO report)

          CASE (106) ; 
                stations (fm) = stations (fm) + 1;

          !  GPS Precipitable Water Vapor (not a WMO report)

          CASE (111) ; 
                stations (fm) = stations (fm) + 1;

          !  GPS Refractivity (not a WMO report)
                stations (fm) = stations (fm) + 1;

          CASE (116) ; 
                stations (fm) = stations (fm) + 1;

          !  DMSP SSM/T-1 (not a WMO report)

          CASE (121) ; 
                stations (fm) = stations (fm) + 1;

          !  DMSP SSM/T-2 (not a WMO report)

          CASE (122) ; 
                stations (fm) = stations (fm) + 1;

          !  DMSP SSMI (not a WMO report)

          CASE (125,126) ; 
                stations (fm) = stations (fm) + 1;

          !  NOAA TOVS (not a WMO report)

          CASE (131) ; 
                stations (fm) = stations (fm) + 1;

          !  Others

          CASE DEFAULT ; 
                stations (0) = stations (0) + 1;

       END SELECT

    ENDIF

!--------------------------------- PLAFORM TYPE -------------------------------!

    IF (PRESENT (platform_string))  THEN

        platform_string = unknown

        IF (PRESENT (fm)) THEN
            IF ((fm > 0) .OR. (fm <= ns)) platform_string = platform (fm)
        ENDIF

    ENDIF

    RETURN
!------------------------------------------------------------------------------!
 END SUBROUTINE fm_code
