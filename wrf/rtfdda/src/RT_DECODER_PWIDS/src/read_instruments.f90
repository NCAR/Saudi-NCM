
  SUBROUTINE read_instruments (flnm, npwids, stations)
!------------------------------------------------------------------------------!
!                        read_instruments:
!                        -----------------
!
! Read the list and position of the PWIDS stations in the instruments file
!(normally file Instruments.txt with the following format:
!
!Dugway Instrumentation Location
!Pentagon Shield Test
!Date Created:   4/17/2004       Date Updated:   5/05/2004
!Instrument      Location Elv(m) Latitude(°N)    Longitude(°W) Alternative Name
!PWID             1      14              38.87453        -77.05696
!PWID             2      40              38.87219        -77.05763
!PWID             3      40              38.87284        -77.05472
!PWID             4      10              38.87284        -77.05122
!PWID             5      20              38.87098        -77.06209
!PWID             6      35              38.87156        -77.05665
!PWID             7      35              38.87166        -77.05548
!PWID             8      35              38.87069        -77.05681
!PWID             9      35              38.87088        -77.05496
!PWID            10      40              38.87006        -77.05833
!PWID            11      35              38.87021        -77.05579
!PWID            12      40              38.87064        -77.05336
!PWID            13      40              38.86892        -77.05560
!PWID            14      18              38.86797        -77.05813
!PWID            15      15              38.86798        -77.05230
!
! Remarks:
! -------
! a) The maximum number of PWID stations that can be loaded is limited to
!    "npwids_max". This values is set in file module_pwids.f90.
!
! b) Beware of the convention in longitude, if West longitudes are positive,
!    the parameter "west_longitudes" in file read_instruments.f90 must be set
!    to TRUE.
!
! Copyright UCAR (c) 1992 - 2004.
! University Corporation for Atmospheric Research (UCAR),
! National Center for Atmospheric Research (NCAR),
! Research Applications Program (RAP),
! P.O.Box 3000, Boulder, Colorado, 80307-3000, USA.
!
! Francois Vandenberghe, vandenb@ucar.edu, May 2005.
!------------------------------------------------------------------------------!
   USE module_pwids
!------------------------------------------------------------------------------!
   IMPLICIT NONE

   CHARACTER (LEN = 200), INTENT (in)  :: flnm
   INTEGER,               INTENT (out) :: npwids

   TYPE (pwid_station_type), DIMENSION (npwids_max), INTENT (out) :: stations

!------------------------------------------------------------------------------!

   INTEGER, PARAMETER    :: iunit = 9
   INTEGER               :: error_number
   INTEGER               :: id
   INTEGER               :: l
   CHARACTER (LEN =  90) :: long_line
   CHARACTER (LEN =  50) :: name

   TYPE (pwid_station_type) :: station

   LOGICAL, PARAMETER :: west_longitudes = .FALSE. 
!-----------------------------------------------------------------------------!

! 1.  INITIALIZATION
! ===================

! 1.1 Open the input file
!     -------------------

      OPEN (FILE   = flnm, UNIT = iunit, STATUS = 'OLD',   &
            ACCESS = 'SEQUENTIAL', FORM   = 'FORMATTED',   &
            ACTION = 'READ', IOSTAT = error_number )

      IF (error_number /= 0) THEN
          WRITE (UNIT = *, FMT = '(2A,/)') &
         "Error in opening instruments file: ",&
          TRIM (flnm)
          CALL ABORT
      ELSE
         WRITE (UNIT = *, FMT = '(2A,/)') "Read instruments file: ",&
         TRIM (flnm)
      ENDIF


! 2.  READ INPUT FILE LOOKING FOR KEYWORD PWID 
! =============================================

      l = 0
      npwids = 0

      DO WHILE (npwids <= npwids_max)

! 2.1 Read 1 line
!     -----------

         READ (UNIT=iunit, FMT='(A)', IOSTAT=error_number) long_line

         IF (error_number /= 0) EXIT

         l = l + 1

!        WRITE (UNIT=*, FMT='(A)') TRIM (long_line)

! 2.2 Check for keyword PWID
!     ------------------------

      IF (long_line (1:4) == "PWID") THEN

          WRITE (*,'(A)') TRIM (long_line)

! 2.3 Load data 
!     ---------

          READ (UNIT=long_line,FMT=*,IOSTAT=error_number) name, station

          id = station % id

! 2.4 Check if PWIDS id is admissible
!     -------------------------------

          IF (id > npwids_max) THEN
              WRITE (*,'(3(A,I4))') "Line ",l,", PWIDS id = ",id, &
             ", but the maximal ID is set to NPWIDS_MAX =", npwids_max
              WRITE (*,'(A,I4,A)')  "No more read, process the first ", &
                                     npwids_max," ID stations."
              EXIT
          ENDIF

! 2.5 Count data 
!     ----------

          npwids = npwids + 1

          IF (npwids > npwids_max) THEN
              WRITE (*,'(/,A,I4,A)') "Maximal number of PWID stations ", &
                                      npwids_max," reached!"
              WRITE (*,'(A,I4,A)')   "No more read, process the first ", &
                                      npwids_max," PWID stations."
              EXIT
          ENDIF

! 2.6 Load data
!     ---------

          stations (id) % id          = station % id
          stations (id) % latitude    = station % latitude
          stations (id) % longitude   = station % longitude
          stations (id) % elevation   = station % elevation
          stations (id) % description = station % description

! 2.7 For some reasons, West longitude is positive
!     --------------------------------------------

          IF (west_longitudes) THEN

              stations (id) % longitude = - station % longitude

              WRITE (*,'(2(A,F9.5))')"Assuming US station, changing longitude",&
                     station % longitude, " into  ", stations (id) % longitude
          ENDIF


! 2.8 Clean strings with bad characters
!     ---------------------------------

          CALL clean_string (stations (id) % description) 

         ENDIF

      ENDDO

! 2.8 Close input file
!     ----------------

      CLOSE (UNIT=iunit)

! 2.9 Process errors
!     --------------
     
      IF (error_number < 0) THEN
          WRITE (*,'(2A)') "Hit end of file: ", TRIM (flnm)
      ELSE
          WRITE (*,'(A,I4,2A,/)') "Error reading line: ",l," of file: ", &
          TRIM (flnm)
          CALL ABORT
      ENDIF

! 2.10 Print-out
!      ---------

      WRITE (*,'(/,A,I3,2A)') "Found ",npwids," PWID stations in file: ", &
                               TRIM (flnm)

!------------------------------------------------------------------------------!
   END SUBROUTINE read_instruments
