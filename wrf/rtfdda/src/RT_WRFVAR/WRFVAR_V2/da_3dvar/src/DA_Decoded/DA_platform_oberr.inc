  SELECT CASE (platform (7:11))

    CASE ('TEMP ')  
          iunit  = 20; ! RAWINS
          fm = 35
          platformo = 'FM-35 TEMP'
!         WRITE (platformo,'(A,I2,1X,A)') "FM-", fm, platform (7:)

    CASE ('MWVR ')     ! WATER VAPOR RADIOMETER
          iunit  = 20; ! RAWINS
          fm = 35      !  Assing TEMP
          platformo = 'FM-35 TEMP'
!         WRITE (platformo,'(A,I2,1X,A)') "FM-", fm, name (1:34)

    CASE ('NPN-P')     ! None WMO
          iunit  = 20; ! RAWINS
          fm = 32      !  Assing PILOT
          platformo = 'FM-32 PILOT'
!         WRITE (platformo,'(A,I2,1X,A)') "FM-", fm, name (1:34)

    CASE ('PILOT')  
          iunit  = 21; ! PIBAL
          fm = 32
          platformo = 'FM-32 PILOT'
!         WRITE (platformo,'(A,I2,1X,A)') "FM-", fm, platform (7:)

    CASE ('PROFI')     ! MAP WIND PROFILER  
          iunit  = 20; ! RAWINS
          fm = 32      !  Assing PILOT
          platformo = 'FM-32 PILOT'
!         WRITE (platformo,'(A,I2,1X,A)') "FM-", fm, name (1:34)

    CASE ('SATWI')     ! None WMO
          iunit  = 34  ! GOES LOW
          fm = 88      ! ASSIGN SATOB
          platformo = 'FM-88 SATOB'
!         WRITE (platformo,'(A,I2,1X,A)') "FM-", fm, name (1:34)

    CASE ('AMDAR')  
          iunit  = 20; ! RAWINS by default
          fm = 42
          platformo = 'FM-42 AMDAR'
!         WRITE (platformo,'(A,I2,1X,A)') "FM-", fm, name (1:34)

          IF (index (name,'AIREP')  > 0)  THEN
              iunit  = 30  ! AIREP/PIREP AIRCRAFT FLIGHT-LEVEL
          ELSE IF (index (name,'ASDAR')  > 0)  THEN
              iunit  = 31  ! ASDAR AIRCRAFD FLIGHT-LEVEL
          ELSE IF (index (name,'ACARS')  > 0)  THEN
              iunit  = 33  ! ACARS AIRCRAFT FLIGHT-LEVEL
          ENDIF

    CASE ('TAMDA')  
          iunit  = 20; ! RAWINS by default
          fm = 97
          platformo = 'FM-97 TAMDAR'
!         WRITE (platformo,'(A,I2,1X,A)') "FM-", fm, name (1:34)

    CASE ('ACARS')  
          iunit  = 20; ! RAWINS by default
          fm = 97
          platformo = 'FM-97 ACARS'
!         WRITE (platformo,'(A,I2,1X,A)') "FM-", fm, name (1:34)

    CASE ('ACAMD')  
          iunit  = 20; ! RAWINS by default
          fm = 97
          platformo = 'FM-97 ACARS'
!         WRITE (platformo,'(A,I2,1X,A)') "FM-", fm, name (1:34)

    CASE ('MDCRS')  
          iunit  = 20; ! RAWINS by default
          fm = 97
          platformo = 'FM-97 ACARS'
!         WRITE (platformo,'(A,I2,1X,A)') "FM-", fm, name (1:34)

    CASE ('MADIS')  
          iunit  = 20; ! RAWINS by default
          fm = 97
          platformo = 'FM-97 ACARS'
!         WRITE (platformo,'(A,I2,1X,A)') "FM-", fm, name (1:34)

    CASE ('METAR')  
          iunit  = 81  ! LAND SURFACE STATION WITH PRESSURE
          fm = 15 
          platformo = 'FM-15 METAR'
!         WRITE (platformo,'(A,I2,1X,A)') "FM-", fm, platform (7:)

    CASE ('SPECI')  
          iunit  = 81  ! LAND SURFACE STATION WITH PRESSURE
          fm = 16
          platformo = 'FM-16 SPECI'
!         WRITE (platformo,'(A,I2,1X,A)') "FM-", fm, platform (7:)

    CASE ('SHIP ')  
          iunit  = 81  ! LAND SURFACE STATION WITH PRESSURE
          fm = 13
          platformo = 'FM-13 SHIP'
!         WRITE (platformo,'(A,I2,1X,A)') "FM-", fm, platform (7:)

    CASE ('QSCAT')  
          iunit  = 86  ! LAND SURFACE STATION WITH PRESSURE
          fm = 13
          platformo = 'FM-13 QSCAT'
!         WRITE (platformo,'(A,I2,1X,A)') "FM-", fm, platform (7:)

    CASE ('SYNOP')     ! INCLUDE GTS, SAMS, METNET, OKMESO AND PROFILER
          iunit  = 81  ! LAND SURFACE STATION WITH PRESSURE
          fm = 12      ! ASSIGN SYNOP BY DEFAULT  
          platformo = 'FM-12 SYNOP'
!         WRITE (platformo,'(A,I2,1X,A)') "FM-", fm, name (1:34)

          IF (index (name,'AIREP')  > 0)  THEN
              iunit  = 30  ! AIREP/PIREP AIRCRAFT FLIGHT-LEVEL
          ELSE IF (index (name,'ASDAR')  > 0)  THEN
              iunit  = 31  ! ASDAR AIRCRAFD FLIGHT-LEVEL
          ELSE IF (index (name,'ACARS')  > 0)  THEN
              iunit  = 33  ! ACARS AIRCRAFT FLIGHT-LEVEL
          ELSE IF (index (name,'PROFILER')  > 0)  THEN
              iunit  = 20; ! RAWINS
              fm = 32      ! ASSIGN PILOT
              platformo = 'FM-32 PILOT'
!             WRITE (platformo,'(A,I2,1X,A)') "FM-", fm, name (1:34)
          ENDIF
 
    CASE DEFAULT

          iunit  = 20; ! RAWINS
          fm = 35      ! ASSIGN TEMP BY DEFAULT
          platformo = 'FM-35 TEMP'
!         WRITE (platformo,'(A,I2,1X,A)') "FM-", fm, name (1:34)

          CALL prelog
          WRITE  (log_unit,'(6A)') "Unknown platform: ",  &
                                    TRIM (platform),", ", &
                                    name (1:34),", ",     &
                                   "assign RAWINS observational error."

    END SELECT

