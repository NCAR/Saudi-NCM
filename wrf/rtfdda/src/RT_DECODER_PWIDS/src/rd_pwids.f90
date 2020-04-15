
  PROGRAM rd_pwids
!-------------------------------------------------------------------------------
! 
!                       PROGRAM rd_pwids
! 
! This is the main driver of the PWIDS decoder. The program rd_pwids.f90
! reads in an ASCII file (*.dat) write out an ASCII file (*.decoded) at the 
! input format of MM5 RAWINS, LITTLE_R and 3D-VAR programs.
! 
! Everything a user needs to run the program is contained in this directory.
! Complilation on LINUX (Portland pgf90) has been tested. Compilation is done 
! with the command "make". See below for more details.
!  
! For questions, please send emails to vandenb@ucar.edu
! 
! For detailed documentation about programs, please see below
! 
! For more information on PWIDS, please see:
! 
! http://www.rap.ucar.edu/~hahnd/darpa/doc/PWIDS/index.html
! 
! For details about MM5, please see:
! 
! http://www.mmm.ucar.edu/mm5/mm5-home.html
! 
!------------------------------------------------------------------------------
! Functions:
! ---------
! 
! 1) Read the list and position of the PWIDS stations in the instruments file
!   (normally file Instruments.txt with the following format:
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
!    "npwids_max" defined in file module_pwids.f90.
!
! b) Beware of the convention in longitude, if West longitudes are positive,
!    the parameter west_longitudes in file read_instruments.f90 must be set
!    to TRUE.
!
!
! 2) Read the PWIDS data file, eg: 2004_05_07_1200_pwids.dat, with format:
!
!logger_id,ccyy,Julian_Day,hhmn,ss,pwid_id,voltage,windspeed,Winddir,Temp,RH
!103,2004,128,1206,0,15,12.68,.57,186.2,18.55,75.9
!103,2004,128,1206,0,4,12.58,1.56,182.9,18.91,69.24
!103,2004,128,1206,10,15,12.68,1.13,200.3,18.59,75.7
!103,2004,128,1206,0,10,12.44,.64,155,18.59,80.1
!103,2004,128,1206,0,14,12.62,0,0,19.05,69.79
!103,2004,128,1206,0,11,12.85,.44,209,18.95,69.99
!103,2004,128,1206,0,5,12.89,.77,190.4,19.42,86.7
!
!
! 3) Assign the proper PWID station latitude, longitude, elevation to 
!    the corresponding measurement, average in time measurements at 
!    a frequency provided by the users in command line (-t averaging_mn)
!
! 4) Write out the averaged measurements at the MM5 RAWINS, LITTLE_R and 3D-VAR
!    "decoded" format. The code "FM-12 PWID" is arbitrarily assigned s WMO
!    platform code.
!
!------------------------------------------------------------------------------
! Compilation:
! ------------
! 
! Programs are in Fortran 90 and require Fortran 90 compiler.
! 
! The Makefile works on DEC Alpha, SGI Irix, SUN SunOS with their native 
! compilers, and PC running Linux using Portland Group Fortran 90 compiler.
! If your machine isn't one of the ones supported or the compiler isn't
! the native one from the vendor, you need to edit the Makefile.
! 
!------------------------------------------------------------------------------
! Input files:
! -----------
! 
!  1) An list of the PWID stations with their locations and ids
!     (eg Instruments.txt in this directory).
!  2) A PWIDS ASCII file 
!      (eg: 2004_05_07_1200_pwids.dat in this directory),
!  IDs in the data file shall match IDs in the instrument file.
! 
!------------------------------------------------------------------------------
! Command line arguments:
! ----------------------
! 
!  rd_pwids.exe -i input_file.dat [-n Instruments.txt -o output_file.decoded \
!               -t averaging_mn
! 
!  Where:
!  -----
!    input_file:    is the name (with full path) of PWIDS input file.
!    Instruments:   is the list of PWID stations with their locations and ids.
!    ouput_file:    is the name (with full path) of the output file.
!    averaging_mn:  is the averaging time in minutes.
! 
!  If -o argument is ommitted then output file will be "input_file.decoded",
!  If -n argument is ommitted then instrument file will be "Instruments.txt".
!  If -t argument is ommitted then averaging time will be 15 minutes.
! ------------------------------------------------------------------------------
! Output file:
! -----------
!
!  input_file.decoded: an ASCII file of PWIDS data at the MM5 RAWINS, 
!                      LITTLE_R  and 3D-VAR input format.
!
!-------------------------------------------------------------------------------
!  Copyright UCAR (c) 1992 - 2004.
!  University Corporation for Atmospheric Research (UCAR),
!  National Center for Atmospheric Research (NCAR),
!  Research Applications Program (RAP),
!  P.O.Box 3000, Boulder, Colorado, 80307-3000, USA. 
!
!  Francois Vandenberghe, vandenb@ucar.edu, May 2005.
!-------------------------------------------------------------------------------
   USE module_date
   USE module_pwids
   USE module_decoded
!-------------------------------------------------------------------------------

   IMPLICIT NONE

   TYPE (pwid_type) :: pwid_tmp
   TYPE (pwid_type), DIMENSION (npwids_max) :: pwids
   TYPE (pwid_station_type), DIMENSION (npwids_max) :: stations

   TYPE (report) :: obs

   CHARACTER (LEN =  19) :: time_window_min, time_window_max
   CHARACTER (LEN =  40) :: pwid_location
   CHARACTER (LEN =  80) :: long_line
   CHARACTER (LEN = 200) :: pwid_file_name, instruments_file, &
                            decoded_file_name

   LOGICAL               :: inside, print_obs_found, ldebug
   LOGICAL               :: okb, oke
   INTEGER               :: iunit, iunitin, iunitou
   INTEGER               :: sampling_mn, ts, ts1
   INTEGER               :: io_error
   INTEGER               :: mm, hh, dd, mn, ss
   INTEGER               :: k, l, n, idts, idtsl
   INTEGER               :: npwids
   INTEGER               :: id, nread, nwrite

   INTEGER, DIMENSION (npwids_max) :: mdts, idts_last, didts_last
   CHARACTER (LEN =  19), DIMENSION (npwids_max) :: date_last
   CHARACTER (LEN =  19), DIMENSION (npwids_max) :: date_end

   INTEGER :: averaging_mn
   INTEGER :: averaging_ss
!------------------------------------------------------------------------------!

! 1.  INPUT
! =========

! 1.1 Parse arguments
!     ---------------

      CALL arguments (pwid_file_name, instruments_file, decoded_file_name, &
                      averaging_mn, ldebug)

! 1.2 Reset station structure
!     ----------------------

      CALL reset_pwids_stations (npwids_max, stations)

! 1.3 Reset pwids structure
!     --------------------

      CALL reset_pwids (npwids_max, pwids)

! 1.4 Read PWIDS stations locations
!     -----------------------------

      CALL read_instruments (instruments_file, npwids, stations)


! 1.5 Allocate memory for decoded observation data structure
!     ------------------------------------------------------

      ALLOCATE (obs % each (pwid_vertical_levels))


! 1. OPEN FILES
! =============

      WRITE (*, '(A)') &
  "----------------------------------------------------------------------------"

! 1.1 Logical unit
!     ------------

      iunitin  = 103; iunitou  = 104;

! 1.2 Open one input file (PWIDS observations data)
!      --------------------------------------------

      CALL openin (pwid_file_name, iunitin, .TRUE.)

! 1.3 Open one output file (PWIDS decoded data)
!     -----------------------------------------

      !  File name for decoded format is as input file with extension "decoded"
      !  instead of "DAT" and are put in the local directory (path is removed)

      CALL openout (decoded_file_name, iunitou, .TRUE.)


! 2.  READ INPUT FILE
! ===================

      WRITE (*, '(A)') &
"------------------------------------------------------------------------------"
      WRITE (*, '(A,I2,A,/)') "Averaging time is ",averaging_mn,"mn."

! 2.1 Initialization
!     -------------

      n = 1
      mdts = 0
      idtsl = 0
      io_error = 0
      idts_last = 0
      didts_last = 0
      date_last = pwid_date_start
      date_end  = pwid_date_start
      averaging_ss = averaging_mn*60

read_file:&
      DO WHILE (io_error == 0) 

! 2.2 Read 1 line
!     -----------

         READ (UNIT=iunitin, FMT='(A)',IOSTAT=io_error) long_line 

         IF (io_error /= 0) EXIT read_file

! 2.3 Extract data
!     ------------

         CALL reset_pwid (pwid_tmp)

         READ (long_line,*)  pwid_tmp % logger_id,  &
               pwid_tmp % year, pwid_tmp % juliand, &
               pwid_tmp % hhmm, pwid_tmp % ss,      &
               pwid_tmp % id,   pwid_tmp % volt,    & 
               pwid_tmp % spd,  pwid_tmp % dir,     &
               pwid_tmp % tem,  pwid_tmp % rhu

! 2.4 Look for missing data
!     ---------------------

          IF (ABS (pwid_tmp % spd - spd_missing) <= 0. .OR. & 
              ABS (pwid_tmp % dir - dir_missing) <= 0. .OR. & 
              ABS (pwid_tmp % tem - tem_missing) <= 0. .OR. & 
              ABS (pwid_tmp % rhu - rhu_missing) <= 0.) THEN
              WRITE (*,'(A)') "----------"
              WRITE (*,'(A,I3,A)') "Incomplete report for PWID id ", id, &
                                     pwid_tmp % id,", skip it..."
              CYCLE read_file
          ENDIF


! 2.5 Check id
!     --------

          IF (pwid_tmp % id <= 0 .OR. pwid_tmp % id > npwids_max) THEN
              WRITE (*,'(A)') "----------"
              WRITE (*,'(2(A,I3),A)') &
              "Maximal PWID id was set to ", npwids_max, &
            ", found data id ",pwid_tmp % id,", skip it..."
              CYCLE read_file
          ENDIF

          id = pwid_tmp % id

! 2.6 Check if station is known
!     -------------------------

          IF (ABS (stations (id) % latitude  - hei_missing) <= 0. .OR. & 
              ABS (stations (id) % longitude - hei_missing) <= 0.) THEN
              WRITE (*,'(A)') "----------"
              WRITE (*,'(A,I3,A)') &
             "PWID# ",id,": has no known station, skip it ..."
              CYCLE read_file
          ENDIF

! 2.5 Build date
!     ----------

          CALL julian_day (pwid_tmp % year, mm, dd, pwid_tmp % juliand, 2)

          hh = pwid_tmp % hhmm / 100
          mn = pwid_tmp % hhmm - hh*100

          WRITE (UNIT=pwid_tmp % date19, &
                 FMT ='(I4.4,"-",I2.2,"-",I2.2,"_",I2.2,":",I2.2,":",I2.2)') &
                 pwid_tmp % year, mm, dd, hh, mn, pwid_tmp % ss

! 2.7 Check if data come in chronological order
!     -----------------------------------------

          CALL geth_idts (pwid_tmp % date19, date_last (id), idtsl) 

          IF (idtsl <= 0) THEN
              IF (ldebug) THEN
                  WRITE (*,'(A)') "----------"
                  WRITE (*,'(A,I3,3A)') "PWID# ",id,": has date in past:  ", &
                                         pwid_tmp % date19,", skip it..." 
              ENDIF
              CYCLE read_file
          ENDIF

          date_last (id) = pwid_tmp % date19


! 3.  TIME AVERAGINGE
! ===================
 
! 3.1 Process the case of no averaging
!     --------------------------------

          IF (averaging_ss <= 0) THEN
              pwids (id) = pwid_tmp
              pwids (id) % ntimes = 1
              mdts  (id) = 0
              date_end (id) = pwid_tmp % date19
          ENDIF

! 3.2 Simple copy for the first data to average
!     -----------------------------------------

          IF (pwids (id) % ntimes == 0) THEN 
              pwids (id) = pwid_tmp
              CALL reset_pwid_met (pwids (id))
              mdts  (id) = 0
!             pwids (id) % ntimes = 1
          ENDIF

! 3.3 Print-out
!     ---------

          IF (ldebug) THEN
              WRITE (*,'(A)') "----------"
              WRITE (*,'(A,I3,2A,I4,A)') &
             "PWID# ",id,": average started at ",pwids (id) % date19
          ENDIF

! 3.4 Check the time since the averaging started
!     -------------------------------------------

          CALL geth_idts (pwid_tmp % date19, pwids (id) % date19, idts)

                   
! 3.5 If time difference is less than averaging_mn, accumulate
!     --------------------------------------------------------

          IF (idts < averaging_ss) THEN

              pwids (id) % tem = pwids (id) % tem + pwid_tmp % tem
              pwids (id) % rhu = pwids (id) % rhu + pwid_tmp % rhu
              pwids (id) % spd = pwids (id) % spd + pwid_tmp % spd
              pwids (id) % dir = pwids (id) % dir + pwid_tmp % dir

              pwids (id) % ntimes = pwids (id) % ntimes + 1

              pwids (id) % id  = pwid_tmp % id
              pwids (id) % logger_id = pwid_tmp % logger_id

              mdts  (id) = mdts (id) + idts

! 3.6 Print-out averaging info
!     ------------------------

              IF (ldebug) THEN
                  WRITE (*,'(A,I3,2A,I4,A)') &
                 "PWID# ",id,": current time is    ", pwid_tmp % date19,& 
                  pwids  (id) % ntimes," accumulated report(s)"
              ENDIF

          ELSE

! 4.  FILL THE DECODED STRUCTURE AND WRITE OUT DECODED OBS
! ========================================================

! 4.1 Mean values
!     -----------

              IF (pwids (id) % ntimes > 0) THEN

                  pwids (id) % tem = pwids (id) % tem / pwids (id) % ntimes
                  pwids (id) % rhu = pwids (id) % rhu / pwids (id) % ntimes
                  pwids (id) % spd = pwids (id) % spd / pwids (id) % ntimes
                  pwids (id) % dir = pwids (id) % dir / pwids (id) % ntimes

! 4.2 Mean time lag
!     -------------
!     Include time of next obs, to make the mean of averaging_mn)
                  IF (idts - idts_last (id) == didts_last (id)) THEN
                      mdts  (id) = (mdts (id) + idts) / (pwids (id) % ntimes+1)
                  ELSE
                      mdts  (id) = (mdts (id) + idts_last (id)+ didts_last (id)) / (pwids (id) % ntimes+1)
                  ENDIF

              ELSE
                  WRITE (*,'(A,I3,A)') "Error PWDIS ID ",id,": ntimes = 0"
                  CALL ABORT
              ENDIF

! 4.3 Mean time date
!     --------------

              CALL geth_newdate (pwids (id) % date19, pwids (id) % date19, &
                                 mdts  (id))

! 4.4 Fill the decoded data structure
!     -------------------------------

              CALL fill_decoded (stations (id), pwids (id), obs)

! 4.5 Write the decoded data structure in output file
!     -----------------------------------------------

              CALL write_decoded (iunitou, obs)

! 4.6 Print-out
!     ---------

              WRITE (*, '(A,I3,A,A,I4,A,I2,A)') &
             "PWID# ",id,": average written at ",pwids (id) % date19,&
              pwids (id) % ntimes," reports over ", averaging_mn, "mn"

              IF (ldebug) THEN
                  WRITE (*,'(A,I3,2A,I4,A)') &
                 "PWID# ",id,": average finish. at ",date_end (id)
                  WRITE (*,'(A,I3,2A,2X,A)') &
                 "PWID# ",id,": current time is    ", pwid_tmp % date19, & 
                 "start new accumulation..."
              ENDIF


! 4.7 Increment/Reset counters
!     ------------------------

              mdts  (id) = 0
              pwids (id) = pwid_tmp
              pwids (id) % ntimes = 1

              nwrite = nwrite + 1

        ENDIF

! 4.8  end of reading
!      --------------

      date_end (id) = pwid_tmp % date19 
      nread = nread + 1

      didts_last (id) = idts - idts_last (id) 
      idts_last  (id) = idts


      ENDDO read_file


! 5.  PROCESS THE LAST READ DATA LINE
! ===================================

! 5.1 Expect to Hit the end of file
!     -----------------------------

      IF (io_error <  0) THEN

           WRITE (*, '(/,2A,/)') "Have reached end of input file: ", &
           TRIM  (pwid_file_name)

           nread = nread - 1

! 5.2 Loop over the stations
!     ----------------------

end_file:&
      DO id = 1, npwids_max

! 5.3 Skip when averaging is ommitted
!     -------------------------------

      IF (averaging_ss <= 0) EXIT end_file

! 5.4 Check if station is known
!     -------------------------

          IF (ABS (stations (id) % latitude  - hei_missing) <= 0. .OR. & 
              ABS (stations (id) % longitude - hei_missing) <= 0.) THEN
              WRITE (*,'(A,I3,A)') &
             "PWID# ",id,": has no known station, skip it ..."
              CYCLE end_file
          ENDIF

! 5.5 Print current status
!     --------------------

          IF (ldebug) THEN
              WRITE (*,'(A)') "----------"
              WRITE (*,'(A,I3,2A,I4,A)') &
             "PWID# ",id,": average started at ", pwids (id) % date19
          ENDIF

! 5.6 Check the time since the averaging started
!     -------------------------------------------

          CALL geth_idts (pwid_tmp % date19, pwids (id) % date19, idts)

! 5.7 Average current accumulated values
!     ----------------------------------

          IF (pwids (id) % ntimes > 0) THEN

              pwids (id) % tem = pwids (id) % tem / pwids (id) % ntimes
              pwids (id) % rhu = pwids (id) % rhu / pwids (id) % ntimes
              pwids (id) % spd = pwids (id) % spd / pwids (id) % ntimes
              pwids (id) % dir = pwids (id) % dir / pwids (id) % ntimes

! 5.8 Mean time lag
!     -------------
!     Include time of next obs, to make the mean of averaging_mn)
                  IF (idts - idts_last (id) == didts_last (id)) THEN
                      mdts  (id) = (mdts (id) + idts) / (pwids (id) % ntimes+1)
                  ELSE
                      mdts  (id) = (mdts (id) + idts_last (id)+ didts_last (id)) / (pwids (id) % ntimes+1)
                  ENDIF
!             IF (idts_last (id) == idts) THEN
!                 mdts  (id) = (mdts (id) + idts) / (pwids (id) % ntimes)
!             ELSE
!                 mdts  (id) = mdts (id) / pwids (id) % ntimes
!             ENDIF

          ELSE
              WRITE (*,'(A,I3,A)') "Error PWDIS ID ",id,": ntimes = ",0
              CALL ABORT
          ENDIF

! 5.9 Mean time
!     ---------

          CALL geth_newdate (pwids (id) % date19, pwids (id) % date19, &
                             mdts  (id))

! 5.9 Fill the decoded data structure
!     -------------------------------

          CALL fill_decoded (stations (id), pwids (id), obs)

! 5.10 Write the decoded data structure in output file
!      -----------------------------------------------

          CALL write_decoded (iunitou, obs)

! 5.11 Print-out
!      ---------

              WRITE (*, '(A,I3,A,A,I4,A,I2,A)') &
             "PWID# ",id,": average written at ",pwids (id) % date19,&
              pwids (id) % ntimes," reports over ", averaging_mn, "mn"

              IF (ldebug) THEN
                  WRITE (*,'(A,I3,2A,I4,A)') &
                 "PWID# ",id,": average finish. at ",date_end (id)
                  WRITE (*,'(A,I3,2A,2X,A)') &
                 "PWID# ",id,": current time is    ", pwid_tmp % date19, & 
                 "end of file."
              ENDIF


! 5.12 Increment/Reset counters
!       ------------------------

          mdts  (id) = 0
          pwids (id) % ntimes = 1

          nwrite = nwrite + 1

      ENDDO end_file


! 5.13 Process read errors
!     -------------------

      ELSE IF (io_error > 0) THEN

           WRITE (*, '(/,2A,/)') "Problem reading input file: ", &
           TRIM  (pwid_file_name)

      ENDIF

! 5.14 Close files
!      -----------

      CLOSE (UNIT=iunitin); 
      CLOSE (UNIT=iunitou);


! 6.  END
! =======

      WRITE (*, '(A)') &
"------------------------------------------------------------------------------"

! 8.1 Free memory
!     -----------

      DEALLOCATE (obs % each)

! 8.2 Print error message if any
!     --------------------------------

      IF (nread == 0) THEN
           WRITE (*, '(2A,/)') "Could not find any valid data in file: ", &
           TRIM  (pwid_file_name)
           CALL ABORT 
      ENDIF

! 8.3 Print out sum-up
!     ----------------

      WRITE (*, '(/,A,I4,A,A)') & 
     "Read:  ",nread, " PWID reports in input  file: ", TRIM  (pwid_file_name)
      WRITE (*, '(A,I4,A,A)') & 
     "Wrote: ",nwrite," PWID reports in output file: ", TRIM  (decoded_file_name)
      WRITE (*, '(/,A,I2,A,/)') & 
     "Averaging time was ",averaging_mn,"mn."

 END PROGRAM rd_pwids
!------------------------------------------------------------------------------!

      SUBROUTINE arguments (flnm1, flnm2, flnm3, averaging_mn, ldebug)
!------------------------------------------------------------------------------!
      IMPLICIT NONE

      CHARACTER (len=200) :: flnm1, flnm2, flnm3
      INTEGER             :: averaging_mn 
      LOGICAL             :: ldebug

      LOGICAL :: present_v    = .FALSE.
      LOGICAL :: present_help = .FALSE.
      LOGICAL :: present_file1 = .FALSE.
      LOGICAL :: present_file2 = .FALSE.
      LOGICAL :: present_file3 = .FALSE.
      LOGICAL :: present_debug = .FALSE.
      LOGICAL :: present_average = .FALSE.
      LOGICAL :: default_values  = .FALSE.
      INTEGER :: ierr, i, numarg

      CHARACTER (len=200) :: harg
      INTEGER, EXTERNAL   :: iargc
!------------------------------------------------------------------------------!

! 1.  PARSE ARGUMENTS
! ===================

! 1.1 Reset
!     -----

      flnm1 = ""
      flnm2 = ""
      flnm3 = ""
      ldebug = .FALSE.
      averaging_mn = -1

! 1.2 Parse arguments
!     ---------------

      numarg = iargc()

      IF (numarg .EQ. 0) CALL help 

      i = 1

      DO WHILE ( i .LE. numarg)

         CALL getarg(i, harg)

         IF (harg .EQ. "-h") THEN
              present_help = .TRUE.
         ELSE IF (harg .EQ. "-help") THEN
              present_help = .TRUE.
         ELSE IF (harg .EQ. "-v") THEN
              present_v = .TRUE.
         ELSE IF (harg .EQ. "-version") THEN
              present_v = .TRUE.
         ELSE IF (harg .EQ. "-debug") THEN
              present_debug = .TRUE.
              ldebug = .TRUE.
         ELSE IF (harg (1:2) .EQ. "-t") THEN
              i = i + 1
              CALL getarg(i, harg)
              READ (harg, *) averaging_mn
              present_average = .TRUE.
         ELSE IF (harg (1:2) .EQ. "-i") THEN
              i = i + 1
              CALL getarg(i, harg)
              flnm1 = TRIM (harg)
              present_file1 = .TRUE.
         ELSE IF (harg (1:2) .EQ. "-n") THEN
              i = i + 1
              CALL getarg(i, harg)
              flnm2 = TRIM (harg)
              present_file2 = .TRUE.
         ELSE IF (harg (1:2) .EQ. "-o") THEN
              i = i + 1
              CALL getarg(i, harg)
              flnm3 = TRIM (harg)
              present_file3 = .TRUE.
         ELSE
              CALL help
         ENDIF

         i = i + 1

      ENDDO

! 2. PRINT COMMAND LINE ARGUMENTS
! ===============================

     numarg = iargc()
     i = 0

     WRITE (*, '(A)') " "

     DO while ( i .LE. numarg)
        CALL getarg(i, harg)
        WRITE (*, '(2A)', ADVANCE='no') TRIM (harg)," "
        i = i + 1
     ENDDO

     WRITE (*, '(/)') 

! 3.  PRINT VERSION
! =================

      IF (present_v) THEN
          WRITE (*, '(/,A,/)') "Program rd_pwids version 1.0"
          CALL ABORT
      ENDIF

! 4.  CALL HELP
! =============

      IF (present_help) CALL help

! 5.  REQUESTED ARGUMENTS
! =======================

      IF (.NOT. present_file1) THEN
          CALL help
      ENDIF

! 5.  DEFAULT FOR OPTIONAL ARGUMENTS
! ==================================

      IF (.NOT. present_file2) THEN
          flnm2 = "Instruments.txt"
      ENDIF

      IF (.NOT. present_file3) THEN
          i = scan   (flnm1,".",.TRUE.)
          flnm3 = flnm1 (1:i-1)//".decoded"
      ENDIF

      IF (.NOT. present_average) THEN
          averaging_mn = 15
      ENDIF

      WRITE (*, '(A)') &
  "----------------------------------------------------------------------------"

      END SUBROUTINE arguments
!------------------------------------------------------------------------------!

      SUBROUTINE help
!------------------------------------------------------------------------------!
      IMPLICIT NONE
      CHARACTER (len=200)  :: cmd
!------------------------------------------------------------------------------!

      CALL getarg(0, cmd)

      WRITE (*, '(/,3A,/,A,30X,A)') "USAGE: ", TRIM (cmd), &
       " -i input_file.dat [-n Instruments.txt -o out_file.decoded", &
      "=====","-t averaging_mn -h -v -debug]"

       WRITE (*, '(1X,A)')  "input_file:   Data file to decode (required)"
       WRITE (*, '(1X,A)')  "Instruments:  Instruments list file of PWID locations  default: Instruments.txt"
      WRITE (*, '(1X,A)')  "out_file:     Output file for decoded data  default: input_file.decoded"
      WRITE (*, '(1X,A)')  "averaging_mn: Measurements averging time in minutes  default: 15mn"

      WRITE (*, '(A)') ""

      CALL ABORT

      END SUBROUTINE help
!------------------------------------------------------------------------------!
