
 MODULE module_observ_error_ncep
!------------------------------------------------------------------------------!
!
! Estimate observational error from NCEP tables.
!
! Errors are loaded on 33 pressure levels according to the instrumental
! platform code defined and described at the end of this file.
!
! http://www.emc.ncep.noaa.gov/gmb/bkistler/oberr/reanl-obs.html
!
! All obs files have catenated into one single file and read at once. Since
! height and relative humidity are not provided in this table, default values
! are assigned at the end of the routine.
!
!------------------------------------------------------------------------------!
! Copyright UCAR (c) 2010.
! University Corporation for Atmospheric Research (UCAR),
! National Center for Atmospheric Research (NCAR),
! Research Applications Laboratory (RAL),
! P.O.Box 3000, Boulder, Colorado, 80307-3000, USA.
!
! Francois Vandenberghe, vandenb@ucar.edu, October 2010.
!------------------------------------------------------------------------------!

! OBSERVATIONAL ERROR PROFILES

 INTEGER, PARAMETER :: err_levels_ncep    = 33
 INTEGER, PARAMETER :: err_platforms_ncep = 99
 REAL,    DIMENSION   (err_levels_ncep) :: Coord_P_mb
 REAL,    DIMENSION   (0:err_platforms_ncep,err_levels_ncep) :: &
                       Temp_K, Sp_Hum_gkg, uv_ms, P_mb, H_m, RH_water

!------------------------------------------------------------------------------!
 CONTAINS

!------------------------------------------------------------------------------!
 FUNCTION observ_error_ncep_intp (ivar,pob,platform,cv_options_hum,missing_flag) RESULT (oberr)

!------------------------------------------------------------------------------!
! INTERPOLATE NCEP ERROR PROFILES AT OBS VERTICAL LOCATION DEFINED BY ITS
! PRESSURE pob in hPA ACCORDING TO PLATFORM TYPE platform.
!
! PLATFORM IS DEFINED BY ITS WMO CODE, eg. FM-35 TEMP for sounding. 
! See routine unit_observ_error_ncep below for more example.
!
! RETURNED OBSERVATIONAL ERROR IS DEFINED BY VARIABLE TYPE ivar AS FOLLOWS:
!
!         1                    U  error in m/s
!         2                    V  error in m/s
!         3                    T  error in K
!         4                    Q  error in kg/kg  if cv_options_hum  = 1
!                              RH error in %      if cv_options_hum /= 1
!         5                    P  error in hPa
!
!------------------------------------------------------------------------------!
! Copyright UCAR (c) 2010.
! University Corporation for Atmospheric Research (UCAR),
! National Center for Atmospheric Research (NCAR),
! Research Applications Laboratory (RAL),
! P.O.Box 3000, Boulder, Colorado, 80307-3000, USA.
!
! Francois Vandenberghe, vandenb@ucar.edu, October 2010.
!------------------------------------------------------------------------------!
  IMPLICIT NONE

! ARGUMENTS
  INTEGER :: ivar
  REAL    :: pob
  CHARACTER*40 :: platform
  INTEGER :: cv_options_hum
  REAL :: missing_flag

  REAL    :: oberr
  INTEGER :: ii
  CHARACTER*200 :: error_message
!------------------------------------------------------------------------------!
 
! 1.  ERROR PROFILE INDEX BASED ON PLATFORM TYPE
! ==============================================

      ii = unit_observ_error_ncep (platform)

! 2.  INTERPOLATE ERROR PROFILE AR OBS PRESSURE LEVEL
! ===================================================

      SELECT CASE (ivar)

           CASE (1,2) ! U or V-wind error in m/s

                 oberr = SQRT (intplin (pob, Coord_P_mb, uv_ms (ii,:)**2))


           CASE (3) ! T error in K

                 oberr = SQRT (intplin (pob, Coord_P_mb, Temp_K (ii,:)**2))

!                 write(508,*)pob
!                 write(508,*)Coord_P_mb
!                 write(508,*)Temp_k(ii,:)
           CASE (4) ! RH error in %

                IF (cv_options_hum .eq. 1) THEN

                    !  Specific humidity error in g/kg

                    oberr = SQRT (intplin (pob, Coord_P_mb, Sp_Hum_gkg (ii,:)**2))
                    oberr = oberr*1E-3  ! From g/kg into kg/kg

                ELSE

                   ! RH error in %

                    oberr = SQRT (intplin (pob, Coord_P_mb, RH_water (ii,:)**2))

                ENDIF

           CASE (5) ! Pressure error in hPa

                 oberr = SQRT (intplin (pob, Coord_P_mb, P_mb (ii,:)**2))

           CASE default

                WRITE (error_message, '(A,I6,A)') &
               " Observation of unknown variable ivar = ",ivar, &
               " returning missing QC"
                 CALL wrf_message (error_message)
                
                 oberr = missing_flag
                 CALL ABORT

           RETURN

      END SELECT

 END FUNCTION observ_error_ncep_intp
!------------------------------------------------------------------------------!

 SUBROUTINE observ_error_ncep_in (file_in, unit_in, ldebug, ierr)
!------------------------------------------------------------------------------!
!
! Errors are loaded on 33 pressure levels according to the instrumental
! platform code defined and described below:
!
! http://www.emc.ncep.noaa.gov/gmb/bkistler/oberr/reanl-obs.html
!
! All obs files have catenated into one single file and read at once. Since
! height and relative humidity are not provided in this table, default values
! are assigned at the end of the routine.
!
!------------------------------------------------------------------------------!
! Copyright UCAR (c) 2010.
! University Corporation for Atmospheric Research (UCAR),
! National Center for Atmospheric Research (NCAR),
! Research Applications Laboratory (RAL),
! P.O.Box 3000, Boulder, Colorado, 80307-3000, USA.
!
! Francois Vandenberghe, vandenb@ucar.edu, October 2010.
!------------------------------------------------------------------------------!
  IMPLICIT NONE

! ARGUMENTS

  CHARACTER (LEN=80), INTENT (in)  :: file_in
  INTEGER,            INTENT (in)  :: unit_in
  LOGICAL,            INTENT (in)  :: ldebug
  INTEGER,            INTENT (out) :: ierr

!------------------------------------------------------------------------------!
! OTHER VARIABLES

  REAL    :: www, xxx, yyy, zzz

  LOGICAL :: ok
  INTEGER :: iret
  INTEGER :: i, k, l
  INTEGER :: io_error
  INTEGER :: iunit, unit_err, unit_ou

  INTEGER, DIMENSION (1) :: iii

  CHARACTER (LEN =  80) :: file_ou
  CHARACTER (LEN =  80) :: line80
  CHARACTER (LEN = 200) :: error_message

  REAL,DIMENSION (:), ALLOCATABLE :: ptmp

! PARAMETERS

  REAL,    PARAMETER :: missing_read = 1.E+09

  CHARACTER (LEN =  40), PARAMETER :: proc_name = 'observ_error_ncep_in '

! ajb added 08312016 to fix bottleneck for large processor counts
  LOGICAL, EXTERNAL :: wrf_dm_on_monitor  !externals

!------------------------------------------------------------------------------!

      WRITE  (error_message,'(A)') " ---------------------------------------"
      CALL wrf_message (error_message)
      WRITE  (error_message,'(A)') " Load NCEP observational error profiles"
      CALL wrf_message (error_message)


! 1.  INITIALIZATION
! ==================

! 1.1 Turn on status error
!     --------------------

      ierr = -1

! 1.2 Allocate space
!     -------------
 
      ALLOCATE (ptmp (err_levels_ncep))

! 1.3 Reset arrays
!     -----------

      Temp_K = 0.; Sp_Hum_gkg = 0.; uv_ms = 0.; P_mb = 0.; 
      RH_water = 0.; H_m = 0.;
      ptmp = 0.;


! 2.  LOAD OBSERVATIONAL ERROR ON PRESSURE LEVELS
! ===============================================
 
! 2.1 Open file
!     ---------

      INQUIRE (FILE = file_in, OPENED = ok)

      IF (.NOT. ok) &
      OPEN (UNIT = unit_in, FILE = file_in, FORM = 'formatted', &
           ACTION = 'read', STATUS = 'old', IOSTAT = iret)
      

! 2.2 Use default values if file is missing
!     -------------------------------------

      IF (iret /= 0) THEN
          WRITE (error_message,'(2A)') " Cannot find file ",TRIM (file_in)
          CALL wrf_message (error_message)
      ELSE
          WRITE  (error_message,'(2A)') &
        " Reading forecast error profiles in file ",TRIM (file_in)
          CALL wrf_message (error_message)
      ENDIF

! 2.3 Print details
!     -------------


      REWIND (unit=unit_in)

      l = 0

! 2.4 Start reading the file
!     ----------------------

      DO WHILE (iret == 0)

! 2.5 Read the header 
!     ---------------

         l = l + 1

         READ (UNIT=unit_in,FMT='(A)',IOSTAT=iret) line80

         IF (iret /= 0) EXIT

! 2.6 grab the platform code
!     ----------------------

         l = l + 1

         READ (line80 (2:4),'(I3)',IOSTAT=iret) iunit

         IF (iret /= 0) EXIT

! 2.7 Grab the data for the corresponding platform (from bottom to top)
!     --------------------------------------------

         IF (iunit >= 200) THEN

             unit_err = iunit - 200

             DO k = 1, err_levels_ncep
                l = l + 1

                READ (UNIT=unit_in,FMT='(1X,6E12.5)',IOSTAT=iret) &
                      Coord_P_mb (k), www,                     &
                      xxx, uv_ms (unit_err,k),                &
                      yyy, zzz

                IF (iret /= 0) EXIT
             ENDDO

         ELSE IF (iunit >= 100) THEN

             unit_err = iunit - 100

             DO k = 1, err_levels_ncep
                l = l + 1

                READ (UNIT=unit_in,FMT='(1X,6E12.5)',IOSTAT=iret) &
                      Coord_P_mb (k), Temp_K (unit_err,k), &
                      Sp_Hum_gkg (unit_err, k), xxx,          &
                      P_mb (unit_err,k), yyy
                      
                IF (iret /= 0) EXIT
             ENDDO

         ELSE

             WRITE  (error_message,'(5A,I6)') TRIM (proc_name),": ", &
                   "Error unexpected code in file ", &
                    TRIM (file_in)," at line ", l
             CALL wrf_message (error_message)
             iret = 1

             EXIT

         ENDIF
                       
! 2.8 If there is an read error, switch to default value
!     ---------------------------------------------------

         IF (iret > 0) THEN
             WRITE  (error_message,'(5A,I6)') TRIM (proc_name),": ", &
                                             "Error reading file ",  &
                                              TRIM (file_in)," at line ", l
             CALL wrf_message (error_message)
             EXIT 
         ENDIF

! 2.9 Extrapolate pressure to higher level
!     ------------------------------------

         ptmp = P_mb (unit_err,:) - missing_read

         iii = MAXLOC (ptmp) 

         IF (iii  (1) <= 1) THEN
             P_mb (unit_err,1) =  1. ! hPa error
             iii  (1) =  2
         ENDIF

         iii (1) = iii (1) - 1

         WHERE (P_mb (unit_err,:) >= missing_read) &
                P_mb (unit_err,:)  = P_mb (unit_err, iii (1))

      ENDDO

! 4.4 Close input file
!     ----------------

      CLOSE (UNIT=unit_in)


! 3.  PROCESS READ ERRORS (ONLY 20 and 81 are filled)
! =======================

      IF (iret > 0) THEN

          WRITE (error_message,'(A)') &
        " Using NCEP observational error default values"
          CALL wrf_message (error_message)

! 3.1 Get surface errors
!     ------------------

       CALL err_default_sfc (err_levels_ncep, Coord_P_mb,   &
                             Temp_K (81,:), Sp_Hum_gkg (81,:), &
                             uv_ms  (81,:), P_mb (81,:), ierr)

! 3.2 Get upper-air errors
!     --------------------

       CALL err_default_upr (err_levels_ncep, Coord_P_mb,   &
                             Temp_K (20,:), Sp_Hum_gkg (20,:), &
                             uv_ms  (20,:), P_mb (20,:), ierr)

      ENDIF


! 4.  CORRECT SOME VALUES
! =======================

! 4.1 Make sure moisture is not too small
!     -----------------------------------

      WHERE (Sp_Hum_gkg  < 0.1) Sp_Hum_gkg = 0.1

! 4.2 Make sure pressure is not too small
!     -----------------------------------

      WHERE (P_mb  < 1.) P_mb = 1.

! 4.3 Make sure pressure is not too big
!     ---------------------------------

      WHERE (P_mb  > 1.) P_mb = 1.



! 5.  OBS HEIGHT AND RELATIVE HUMIDITY ERROR SAME FOR ALL
! =======================================================

! 5.1 Get upper-air height error
!     --------------------------

       CALL err_height_upr (err_levels_ncep, Coord_P_mb, H_m (20,:), ierr)

! 5.2 Apply to other platform
!     -----------------------

       DO i = 0, err_platforms_ncep
          H_m (i,:)  =  H_m (20,:)
       ENDDO

! 5.3 Get surface height error
!     ------------------------

       CALL err_height_sfc (err_levels_ncep, Coord_P_mb, H_m (81,:), ierr)

! 5.4 Get upper-air RH error
!     ----------------------

       CALL err_rh_upr (err_levels_ncep, Coord_P_mb, RH_water (20,:), ierr)

! 5.5 Apply to other platform
!     -----------------------

       DO i = 0, err_platforms_ncep
          RH_water (i,:) = RH_water (20,:)
       ENDDO

! 5.6 Get surface RH error
!     --------------------

       CALL err_rh_sfc (err_levels_ncep, Coord_P_mb, RH_water (81,:), ierr)

! 5.7 Get pressure for upper-air
!     --------------------------

       CALL err_pres_upr (err_levels_ncep, Coord_P_mb, P_mb (20,:), ierr)

! 5.8 Apply to other platform
!     -----------------------

       DO i = 0, err_platforms_ncep
          P_mb (i,:) = P_mb (20,:)
       ENDDO

! 5.9 Get pressure for surface
!     ------------------------

       CALL err_pres_sfc (err_levels_ncep, Coord_P_mb, P_mb (81,:), ierr)



! 6.  FILL IN FOR OTHER PLATFORMS
! ===============================

! 6.1 Use radiosonde for all upper-data
!     ---------------------------------

      DO i = 0, err_platforms_ncep
         Temp_K (i,:) = Temp_K (20,:)
         uv_ms  (i,:) = uv_ms  (20,:)
         P_mb   (i,:) = P_mb   (20,:)
         H_m    (i,:) = H_m    (20,:)
         RH_water (i,:) = RH_water (20,:)
         Sp_Hum_gkg (i,:) = Sp_Hum_gkg (20,:)
      ENDDO

! 6.2 Define surface (80 and 81) as the lowest level of sounding
!     ----------------------------------------------------------

      Temp_K (80:81,:)     = Temp_K (20,1)*1.5    ! wrfvar times 1.5 so here we add it
!      Temp_K (80:81,:)     = Temp_K (20,1)
      uv_ms  (80:81,:)     = uv_ms  (20,1)
      P_mb   (80:81,:)     = P_mb   (20,1)
      H_m    (80:81,:)     = H_m    (20,1)
      RH_water (80:81,:)   = RH_water (20,1)
      Sp_Hum_gkg (80:81,:) = Sp_Hum_gkg (20,1)

! 6.3 Write out error in file
!     -----------------------

      file_ou = "errsnd_out.txt"
      unit_ou = 99

      WRITE (error_message,'(A,A)') &
    " Write radiosonde error profiles in file "//TRIM (file_ou)
      CALL wrf_message (error_message)

! 6.4 Open output ascii file
!     ----------------------

! ajb modified 08312016 to fix I/O bogdown that occurs for large processor counts.
! Now, only the monitor processor writes this file.
      if ( wrf_dm_on_monitor()) then
        OPEN (UNIT = unit_ou , FILE = file_ou, FORM = 'FORMATTED'  , &
             ACTION = 'WRITE' , STATUS = 'REPLACE', IOSTAT = io_error)

        IF (io_error /=  0) THEN
            WRITE (UNIT = error_message, FMT = '(A,A,/)') &
           " Unable to open output file ",TRIM (file_ou)
        ELSE

! 6.5 Write header
!     ------------

            WRITE (unit_ou,'(A)') &
           "Level   Temp K    UV m/s    RHum %  SHum gkg  Pres hPa   Heigh m"

! 6.6 Write profiles
!     ---------------

            DO k = err_levels_ncep, 1, -1
               WRITE (unit_ou,'(I4,6(2X,F8.1))')   NINT (Coord_P_mb (k)), &
                      Temp_K   (20,k), uv_ms (20,k),     &
                      RH_water (20,k), Sp_Hum_gkg(20,k), &
                      P_mb (20,k), H_m (20,k)
            ENDDO

! 6.7 Close file
!     -----------

            CLOSE (UNIT=unit_ou)

        ENDIF

      endif !wrf_dm_on_monitor()

! 7.  END
! =======

! 7.1 Free memory
!     -----------

      DEALLOCATE (ptmp)

! 7.2 Turn of success flag
!     -------------------

      ierr = 0

      WRITE  (error_message,'(A)') " ---------------------------------------"
      CALL wrf_message (error_message)

      RETURN

 END SUBROUTINE observ_error_ncep_in 
!------------------------------------------------------------------------------!

 SUBROUTINE err_default_upr (err_levels, Coord_P_mb_l, Temp_K_l, Sp_Hum_gkg_l, &
                             uv_ms_l, P_mb_l, ierr)
!------------------------------------------------------------------------------!
!
! Hardcoded obs error for platforms 120 and 220 (rawinsonde) from: 
!
! http://www.emc.ncep.noaa.gov/gmb/bkistler/oberr/oberr.120.html
! and
! http://www.emc.ncep.noaa.gov/gmb/bkistler/oberr/oberr.220.html
! 
!------------------------------------------------------------------------------!
! Copyright UCAR (c) 2010.
! University Corporation for Atmospheric Research (UCAR),
! National Center for Atmospheric Research (NCAR),
! Research Applications Laboratory (RAL),
! P.O.Box 3000, Boulder, Colorado, 80307-3000, USA.
!
! Francois Vandenberghe, vandenb@ucar.edu, October 2010.
!------------------------------------------------------------------------------!
  IMPLICIT NONE

  INTEGER, INTENT (in) :: err_levels
  REAL,    DIMENSION (err_levels), INTENT (out) :: Coord_P_mb_l, Temp_K_l,  &
                                                   Sp_Hum_gkg_l, uv_ms_l, &
                                                   P_mb_l
  INTEGER, INTENT (out) :: ierr

  INTEGER :: k
  CHARACTER (LEN =  40), PARAMETER :: proc_name = 'err_default_upr'
  CHARACTER (LEN = 200) :: error_message
!------------------------------------------------------------------------------!

! 1.  CHECK DIMENSIONS
! ====================

! 1.1 Turn error status on
!     --------------------

      ierr = -1

! 1.2 Check dimension
!     ---------------
 
      IF (err_levels /= 33) THEN
          WRITE (error_message,'(3A)') TRIM (proc_name),": ", &
                 "Error in dimensions, err_levels must be 33."
          CALL wrf_message (error_message)
          RETURN
      ENDIF


! 2.  MASS OBS ERRORS
! ===================

    k=33; Coord_P_mb_l (k) =   0.; Temp_K_l (k) = 4.0; P_mb_l (k) = 1.0; Sp_Hum_gkg_l (k) = 2.0;
    k=32; Coord_P_mb_l (k) =   1.; Temp_K_l (k) = 4.0; P_mb_l (k) = 1.0; Sp_Hum_gkg_l (k) = 2.0;
    k=31; Coord_P_mb_l (k) =   2.; Temp_K_l (k) = 4.0; P_mb_l (k) = 1.0; Sp_Hum_gkg_l (k) = 2.0;
    k=30; Coord_P_mb_l (k) =   3.; Temp_K_l (k) = 4.0; P_mb_l (k) = 1.0; Sp_Hum_gkg_l (k) = 2.0;
    k=29; Coord_P_mb_l (k) =   4.; Temp_K_l (k) = 4.0; P_mb_l (k) = 1.0; Sp_Hum_gkg_l (k) = 2.0;
    k=28; Coord_P_mb_l (k) =   5.; Temp_K_l (k) = 4.0; P_mb_l (k) = 1.0; Sp_Hum_gkg_l (k) = 2.0;
    k=27; Coord_P_mb_l (k) =  10.; Temp_K_l (k) = 4.0; P_mb_l (k) = 1.0; Sp_Hum_gkg_l (k) = 2.0;
    k=26; Coord_P_mb_l (k) =  20.; Temp_K_l (k) = 4.0; P_mb_l (k) = 1.0; Sp_Hum_gkg_l (k) = 2.0;
    k=25; Coord_P_mb_l (k) =  30.; Temp_K_l (k) = 4.0; P_mb_l (k) = 1.0; Sp_Hum_gkg_l (k) = 2.0;
    k=24; Coord_P_mb_l (k) =  40.; Temp_K_l (k) = 4.0; P_mb_l (k) = 1.0; Sp_Hum_gkg_l (k) = 2.0;
    k=23; Coord_P_mb_l (k) =  50.; Temp_K_l (k) = 4.0; P_mb_l (k) = 1.0; Sp_Hum_gkg_l (k) = 2.0;
    k=22; Coord_P_mb_l (k) =  75.; Temp_K_l (k) = 3.5; P_mb_l (k) = 1.0; Sp_Hum_gkg_l (k) = 2.0;
    k=21; Coord_P_mb_l (k) = 100.; Temp_K_l (k) = 3.1; P_mb_l (k) = 1.0; Sp_Hum_gkg_l (k) = 2.0;
    k=20; Coord_P_mb_l (k) = 150.; Temp_K_l (k) = 2.8; P_mb_l (k) = 1.0; Sp_Hum_gkg_l (k) = 2.0;
    k=19; Coord_P_mb_l (k) = 200.; Temp_K_l (k) = 2.5; P_mb_l (k) = 1.0; Sp_Hum_gkg_l (k) = 2.0;
    k=18; Coord_P_mb_l (k) = 250.; Temp_K_l (k) = 2.3; P_mb_l (k) = 1.0; Sp_Hum_gkg_l (k) = 2.0;
    k=17; Coord_P_mb_l (k) = 300.; Temp_K_l (k) = 2.0; P_mb_l (k) = 1.0; Sp_Hum_gkg_l (k) = 2.0;
    k=16; Coord_P_mb_l (k) = 350.; Temp_K_l (k) = 1.8; P_mb_l (k) = 1.0; Sp_Hum_gkg_l (k) = 2.0;
    k=15; Coord_P_mb_l (k) = 400.; Temp_K_l (k) = 1.6; P_mb_l (k) = 1.0; Sp_Hum_gkg_l (k) = 2.0;
    k=14; Coord_P_mb_l (k) = 450.; Temp_K_l (k) = 1.5; P_mb_l (k) = 1.0; Sp_Hum_gkg_l (k) = 2.0;
    k=13; Coord_P_mb_l (k) = 500.; Temp_K_l (k) = 1.3; P_mb_l (k) = 1.0; Sp_Hum_gkg_l (k) = 2.0;
    k=12; Coord_P_mb_l (k) = 550.; Temp_K_l (k) = 1.3; P_mb_l (k) = 1.0; Sp_Hum_gkg_l (k) = 2.0;
    k=11; Coord_P_mb_l (k) = 600.; Temp_K_l (k) = 1.3; P_mb_l (k) = 1.0; Sp_Hum_gkg_l (k) = 2.0;
    k=10; Coord_P_mb_l (k) = 650.; Temp_K_l (k) = 1.3; P_mb_l (k) = 1.0; Sp_Hum_gkg_l (k) = 2.0;
    k= 9; Coord_P_mb_l (k) = 700.; Temp_K_l (k) = 1.3; P_mb_l (k) = 1.0; Sp_Hum_gkg_l (k) = 2.0;
    k= 8; Coord_P_mb_l (k) = 750.; Temp_K_l (k) = 1.4; P_mb_l (k) = 1.0; Sp_Hum_gkg_l (k) = 2.0;
    k= 7; Coord_P_mb_l (k) = 800.; Temp_K_l (k) = 1.5; P_mb_l (k) = 1.0; Sp_Hum_gkg_l (k) = 2.0;
    k= 6; Coord_P_mb_l (k) = 850.; Temp_K_l (k) = 1.5; P_mb_l (k) = 1.0; Sp_Hum_gkg_l (k) = 2.0;
    k= 5; Coord_P_mb_l (k) = 900.; Temp_K_l (k) = 1.6; P_mb_l (k) = 1.0; Sp_Hum_gkg_l (k) = 2.0;
    k= 4; Coord_P_mb_l (k) = 950.; Temp_K_l (k) = 1.7; P_mb_l (k) = 1.0; Sp_Hum_gkg_l (k) = 2.0;
    k= 3; Coord_P_mb_l (k) =1000.; Temp_K_l (k) = 1.8; P_mb_l (k) = 1.0; Sp_Hum_gkg_l (k) = 2.0;
    k= 2; Coord_P_mb_l (k) =1050.; Temp_K_l (k) = 1.8; P_mb_l (k) = 1.0; Sp_Hum_gkg_l (k) = 2.0;
    k= 1; Coord_P_mb_l (k) =1100.; Temp_K_l (k) = 1.8; P_mb_l (k) = 1.0; Sp_Hum_gkg_l (k) = 2.0;


! 3.  WIND OBS ERRORS
! ===================

      k=33; Coord_P_mb_l (k) =   0.; uv_ms_l (k) = 2.7; 
      k=32; Coord_P_mb_l (k) =   1.; uv_ms_l (k) = 2.7; 
      k=31; Coord_P_mb_l (k) =   2.; uv_ms_l (k) = 2.7; 
      k=30; Coord_P_mb_l (k) =   3.; uv_ms_l (k) = 2.7;
      k=29; Coord_P_mb_l (k) =   4.; uv_ms_l (k) = 2.7; 
      k=28; Coord_P_mb_l (k) =   5.; uv_ms_l (k) = 2.7; 
      k=27; Coord_P_mb_l (k) =  10.; uv_ms_l (k) = 2.7; 
      k=26; Coord_P_mb_l (k) =  20.; uv_ms_l (k) = 2.7; 
      k=25; Coord_P_mb_l (k) =  30.; uv_ms_l (k) = 2.7; 
      k=24; Coord_P_mb_l (k) =  40.; uv_ms_l (k) = 2.7; 
      k=23; Coord_P_mb_l (k) =  50.; uv_ms_l (k) = 2.7; 
      k=22; Coord_P_mb_l (k) =  75.; uv_ms_l (k) = 2.6;
      k=21; Coord_P_mb_l (k) = 100.; uv_ms_l (k) = 2.5; 
      k=20; Coord_P_mb_l (k) = 150.; uv_ms_l (k) = 2.7; 
      k=19; Coord_P_mb_l (k) = 200.; uv_ms_l (k) = 3.0; 
      k=18; Coord_P_mb_l (k) = 250.; uv_ms_l (k) = 3.2; 
      k=17; Coord_P_mb_l (k) = 300.; uv_ms_l (k) = 3.4; 
      k=16; Coord_P_mb_l (k) = 350.; uv_ms_l (k) = 3.3; 
      k=15; Coord_P_mb_l (k) = 400.; uv_ms_l (k) = 3.1; 
      k=14; Coord_P_mb_l (k) = 450.; uv_ms_l (k) = 3.0;
      k=13; Coord_P_mb_l (k) = 500.; uv_ms_l (k) = 2.8;
      k=12; Coord_P_mb_l (k) = 550.; uv_ms_l (k) = 2.7;
      k=11; Coord_P_mb_l (k) = 600.; uv_ms_l (k) = 2.6;
      k=10; Coord_P_mb_l (k) = 650.; uv_ms_l (k) = 2.5;
      k= 9; Coord_P_mb_l (k) = 700.; uv_ms_l (k) = 2.4;
      k= 8; Coord_P_mb_l (k) = 750.; uv_ms_l (k) = 2.2;
      k= 7; Coord_P_mb_l (k) = 800.; uv_ms_l (k) = 2.1;
      k= 6; Coord_P_mb_l (k) = 850.; uv_ms_l (k) = 1.9;
      k= 5; Coord_P_mb_l (k) = 900.; uv_ms_l (k) = 1.7;
      k= 4; Coord_P_mb_l (k) = 950.; uv_ms_l (k) = 1.6;
      k= 3; Coord_P_mb_l (k) =1000.; uv_ms_l (k) = 1.4;
      k= 2; Coord_P_mb_l (k) =1050.; uv_ms_l (k) = 1.4;
      k= 1; Coord_P_mb_l (k) =1100.; uv_ms_l (k) = 1.4;


! 5. END
! =======
 
! 5.1 Turn off status error
!     ---------------------

      ierr = 0

   RETURN

   END SUBROUTINE err_default_upr
!------------------------------------------------------------------------------!

 SUBROUTINE err_default_sfc (err_levels, Coord_P_mb_l, Temp_K_l, Sp_Hum_gkg_l, &
                             uv_ms_l, P_mb_l, ierr)
!------------------------------------------------------------------------------!
!
! Hardcoded obs error for platforms 180 and 280 (surface) from: 
!
! http://www.emc.ncep.noaa.gov/gmb/bkistler/oberr/oberr.180.html
! and
! http://www.emc.ncep.noaa.gov/gmb/bkistler/oberr/oberr.280.html
! 
!------------------------------------------------------------------------------!
! Copyright UCAR (c) 2010.
! University Corporation for Atmospheric Research (UCAR),
! National Center for Atmospheric Research (NCAR),
! Research Applications Laboratory (RAL),
! P.O.Box 3000, Boulder, Colorado, 80307-3000, USA.
!
! Francois Vandenberghe, vandenb@ucar.edu, October 2010.
!------------------------------------------------------------------------------!
  IMPLICIT NONE

  INTEGER, INTENT (in) :: err_levels
  REAL,    DIMENSION (err_levels), INTENT (out) :: Coord_P_mb_l, Temp_K_l,  &
                                                   Sp_Hum_gkg_l, uv_ms_l, &
                                                   P_mb_l
  INTEGER, INTENT (out) :: ierr

  INTEGER :: k
  CHARACTER (LEN =  40), PARAMETER :: proc_name = 'err_default_sfc'
  CHARACTER (LEN = 200) :: error_message
!------------------------------------------------------------------------------!

! 1.  CHECK DIMENSIONS
! ====================

! 1.1 Turn error status on
!     --------------------

      ierr = -1

! 1.2 Check dimension
!     ---------------
 
      IF (err_levels /= 33) THEN
          WRITE (error_message,'(3A)') TRIM (proc_name),": ", &
                "Error in dimensions, err_levels must be 33."
          CALL wrf_message (error_message)
          RETURN
      ENDIF


! 2.  MASS OBS ERRORS
! ===================

    k=33; Coord_P_mb_l (k) =   0.; Temp_K_l (k) = 4.2; P_mb_l (k) = 1.6; Sp_Hum_gkg_l (k) = 2.0;
    k=32; Coord_P_mb_l (k) =   1.; Temp_K_l (k) = 4.2; P_mb_l (k) = 1.6; Sp_Hum_gkg_l (k) = 2.0;
    k=31; Coord_P_mb_l (k) =   2.; Temp_K_l (k) = 4.2; P_mb_l (k) = 1.6; Sp_Hum_gkg_l (k) = 2.0;
    k=30; Coord_P_mb_l (k) =   3.; Temp_K_l (k) = 4.2; P_mb_l (k) = 1.6; Sp_Hum_gkg_l (k) = 2.0;
    k=29; Coord_P_mb_l (k) =   4.; Temp_K_l (k) = 4.2; P_mb_l (k) = 1.6; Sp_Hum_gkg_l (k) = 2.0;
    k=28; Coord_P_mb_l (k) =   5.; Temp_K_l (k) = 4.2; P_mb_l (k) = 1.6; Sp_Hum_gkg_l (k) = 2.0;
    k=27; Coord_P_mb_l (k) =  10.; Temp_K_l (k) = 4.2; P_mb_l (k) = 1.6; Sp_Hum_gkg_l (k) = 2.0;
    k=26; Coord_P_mb_l (k) =  20.; Temp_K_l (k) = 4.2; P_mb_l (k) = 1.6; Sp_Hum_gkg_l (k) = 2.0;
    k=25; Coord_P_mb_l (k) =  30.; Temp_K_l (k) = 4.2; P_mb_l (k) = 1.6; Sp_Hum_gkg_l (k) = 2.0;
    k=24; Coord_P_mb_l (k) =  40.; Temp_K_l (k) = 4.2; P_mb_l (k) = 1.6; Sp_Hum_gkg_l (k) = 2.0;
    k=23; Coord_P_mb_l (k) =  50.; Temp_K_l (k) = 4.2; P_mb_l (k) = 1.6; Sp_Hum_gkg_l (k) = 2.0;
    k=22; Coord_P_mb_l (k) =  75.; Temp_K_l (k) = 4.2; P_mb_l (k) = 1.6; Sp_Hum_gkg_l (k) = 2.0;
    k=21; Coord_P_mb_l (k) = 100.; Temp_K_l (k) = 4.2; P_mb_l (k) = 1.6; Sp_Hum_gkg_l (k) = 2.0;
    k=20; Coord_P_mb_l (k) = 150.; Temp_K_l (k) = 4.2; P_mb_l (k) = 1.6; Sp_Hum_gkg_l (k) = 2.0;
    k=19; Coord_P_mb_l (k) = 200.; Temp_K_l (k) = 4.2; P_mb_l (k) = 1.6; Sp_Hum_gkg_l (k) = 2.0;
    k=18; Coord_P_mb_l (k) = 250.; Temp_K_l (k) = 4.2; P_mb_l (k) = 1.6; Sp_Hum_gkg_l (k) = 2.0;
    k=17; Coord_P_mb_l (k) = 300.; Temp_K_l (k) = 4.2; P_mb_l (k) = 1.6; Sp_Hum_gkg_l (k) = 2.0;
    k=16; Coord_P_mb_l (k) = 350.; Temp_K_l (k) = 4.2; P_mb_l (k) = 1.6; Sp_Hum_gkg_l (k) = 2.0;
    k=15; Coord_P_mb_l (k) = 400.; Temp_K_l (k) = 4.2; P_mb_l (k) = 1.6; Sp_Hum_gkg_l (k) = 2.0;
    k=14; Coord_P_mb_l (k) = 450.; Temp_K_l (k) = 4.2; P_mb_l (k) = 1.6; Sp_Hum_gkg_l (k) = 2.0;
    k=13; Coord_P_mb_l (k) = 500.; Temp_K_l (k) = 4.2; P_mb_l (k) = 1.6; Sp_Hum_gkg_l (k) = 2.0;
    k=12; Coord_P_mb_l (k) = 550.; Temp_K_l (k) = 4.2; P_mb_l (k) = 1.6; Sp_Hum_gkg_l (k) = 2.0;
    k=11; Coord_P_mb_l (k) = 600.; Temp_K_l (k) = 4.2; P_mb_l (k) = 1.6; Sp_Hum_gkg_l (k) = 2.0;
    k=10; Coord_P_mb_l (k) = 650.; Temp_K_l (k) = 4.2; P_mb_l (k) = 1.6; Sp_Hum_gkg_l (k) = 2.0;
    k= 9; Coord_P_mb_l (k) = 700.; Temp_K_l (k) = 4.2; P_mb_l (k) = 1.6; Sp_Hum_gkg_l (k) = 2.0;
    k= 8; Coord_P_mb_l (k) = 750.; Temp_K_l (k) = 4.2; P_mb_l (k) = 1.6; Sp_Hum_gkg_l (k) = 2.0;
    k= 7; Coord_P_mb_l (k) = 800.; Temp_K_l (k) = 4.2; P_mb_l (k) = 1.6; Sp_Hum_gkg_l (k) = 2.0;
    k= 6; Coord_P_mb_l (k) = 850.; Temp_K_l (k) = 4.2; P_mb_l (k) = 1.6; Sp_Hum_gkg_l (k) = 2.0;
    k= 5; Coord_P_mb_l (k) = 900.; Temp_K_l (k) = 4.2; P_mb_l (k) = 1.6; Sp_Hum_gkg_l (k) = 2.0;
    k= 4; Coord_P_mb_l (k) = 950.; Temp_K_l (k) = 4.2; P_mb_l (k) = 1.6; Sp_Hum_gkg_l (k) = 2.0;
    k= 3; Coord_P_mb_l (k) =1000.; Temp_K_l (k) = 4.2; P_mb_l (k) = 1.6; Sp_Hum_gkg_l (k) = 2.0;
    k= 2; Coord_P_mb_l (k) =1050.; Temp_K_l (k) = 4.2; P_mb_l (k) = 1.6; Sp_Hum_gkg_l (k) = 2.0;
    k= 1; Coord_P_mb_l (k) =1100.; Temp_K_l (k) = 4.2; P_mb_l (k) = 1.6; Sp_Hum_gkg_l (k) = 2.0;


! 3.  WIND OBS ERRORS
! ===================

      k=33; Coord_P_mb_l (k) =   0.; uv_ms_l (k) = 2.5; 
      k=32; Coord_P_mb_l (k) =   1.; uv_ms_l (k) = 2.5; 
      k=31; Coord_P_mb_l (k) =   2.; uv_ms_l (k) = 2.5; 
      k=30; Coord_P_mb_l (k) =   3.; uv_ms_l (k) = 2.5;
      k=29; Coord_P_mb_l (k) =   4.; uv_ms_l (k) = 2.5; 
      k=28; Coord_P_mb_l (k) =   5.; uv_ms_l (k) = 2.5; 
      k=27; Coord_P_mb_l (k) =  10.; uv_ms_l (k) = 2.5; 
      k=26; Coord_P_mb_l (k) =  20.; uv_ms_l (k) = 2.5; 
      k=25; Coord_P_mb_l (k) =  30.; uv_ms_l (k) = 2.5; 
      k=24; Coord_P_mb_l (k) =  40.; uv_ms_l (k) = 2.5; 
      k=23; Coord_P_mb_l (k) =  50.; uv_ms_l (k) = 2.5; 
      k=22; Coord_P_mb_l (k) =  75.; uv_ms_l (k) = 2.5;
      k=21; Coord_P_mb_l (k) = 100.; uv_ms_l (k) = 2.5; 
      k=20; Coord_P_mb_l (k) = 150.; uv_ms_l (k) = 2.5; 
      k=19; Coord_P_mb_l (k) = 200.; uv_ms_l (k) = 2.5; 
      k=18; Coord_P_mb_l (k) = 250.; uv_ms_l (k) = 2.5; 
      k=17; Coord_P_mb_l (k) = 300.; uv_ms_l (k) = 2.5; 
      k=16; Coord_P_mb_l (k) = 350.; uv_ms_l (k) = 2.5; 
      k=15; Coord_P_mb_l (k) = 400.; uv_ms_l (k) = 2.5; 
      k=14; Coord_P_mb_l (k) = 450.; uv_ms_l (k) = 2.5;
      k=13; Coord_P_mb_l (k) = 500.; uv_ms_l (k) = 2.5;
      k=12; Coord_P_mb_l (k) = 550.; uv_ms_l (k) = 2.5;
      k=11; Coord_P_mb_l (k) = 600.; uv_ms_l (k) = 2.5;
      k=10; Coord_P_mb_l (k) = 650.; uv_ms_l (k) = 2.5;
      k= 9; Coord_P_mb_l (k) = 700.; uv_ms_l (k) = 2.5;
      k= 8; Coord_P_mb_l (k) = 750.; uv_ms_l (k) = 2.5;
      k= 7; Coord_P_mb_l (k) = 800.; uv_ms_l (k) = 2.5;
      k= 6; Coord_P_mb_l (k) = 850.; uv_ms_l (k) = 2.5;
      k= 5; Coord_P_mb_l (k) = 900.; uv_ms_l (k) = 2.5;
      k= 4; Coord_P_mb_l (k) = 950.; uv_ms_l (k) = 2.5;
      k= 3; Coord_P_mb_l (k) =1000.; uv_ms_l (k) = 2.5;
      k= 2; Coord_P_mb_l (k) =1050.; uv_ms_l (k) = 2.5;
      k= 1; Coord_P_mb_l (k) =1100.; uv_ms_l (k) = 2.5;


! 5. END
! =======
 
! 5.1 Turn off status error
!     ---------------------

      ierr = 0

   RETURN

   END SUBROUTINE err_default_sfc
!------------------------------------------------------------------------------!

 SUBROUTINE err_height_upr (err_levels, Coord_P_mb_l, H_m_l, ierr)
!------------------------------------------------------------------------------!
! 
! ECMWF radiosonde height observational error. 
! Original ECMWF data were given on 15 pressure levels (Courtier et al. 1999, 
! QJR Metero. Soc., 123). They have been logarithmically interpolated on the 
! NCEP 33 observational error levels.
!
! Output is pressure levels in hPa and height errors in m from bottom to top.
!
!------------------------------------------------------------------------------!
! Copyright UCAR (c) 2010.
! University Corporation for Atmospheric Research (UCAR),
! National Center for Atmospheric Research (NCAR),
! Research Applications Laboratory (RAL),
! P.O.Box 3000, Boulder, Colorado, 80307-3000, USA.
!
! Francois Vandenberghe, vandenb@ucar.edu, October 2010.
!------------------------------------------------------------------------------!
  IMPLICIT NONE

  INTEGER, INTENT (in) :: err_levels
  REAL,    DIMENSION (err_levels), INTENT (out) :: Coord_P_mb_l, H_m_l
  INTEGER, INTENT (out) :: ierr

  INTEGER :: k
  CHARACTER (LEN =  40), PARAMETER :: proc_name = 'err_height_upr'
  CHARACTER (LEN = 200) :: error_message
!------------------------------------------------------------------------------!

! 1.  CHECK DIMENSIONS
! ====================

! 1.1 Turn error status on
!     --------------------
      ierr = -1

! 1.2 Check dimension
!     ---------------
 
      IF (err_levels /= 33) THEN
          WRITE (error_message,'(3A)') TRIM (proc_name),": ", &
                "Error in dimensions, err_levels must be 33."
          CALL wrf_message (error_message)
          RETURN
      ENDIF

! 2.  EIGHT ERRORS
! ================

      k=33; Coord_P_mb_l (k) =   0.; H_m_l (k) = 40.0; 
      k=32; Coord_P_mb_l (k) =   1.; H_m_l (k) = 40.0; 
      k=31; Coord_P_mb_l (k) =   2.; H_m_l (k) = 40.0; 
      k=30; Coord_P_mb_l (k) =   3.; H_m_l (k) = 40.0; 
      k=29; Coord_P_mb_l (k) =   4.; H_m_l (k) = 40.0; 
      k=28; Coord_P_mb_l (k) =   5.; H_m_l (k) = 40.0; 
      k=27; Coord_P_mb_l (k) =  10.; H_m_l (k) = 40.0; 
      k=26; Coord_P_mb_l (k) =  20.; H_m_l (k) = 32.0; 
      k=25; Coord_P_mb_l (k) =  30.; H_m_l (k) = 25.0; 
      k=24; Coord_P_mb_l (k) =  40.; H_m_l (k) = 23.7; 
      k=23; Coord_P_mb_l (k) =  50.; H_m_l (k) = 22.5; 
      k=22; Coord_P_mb_l (k) =  75.; H_m_l (k) = 19.3; 
      k=21; Coord_P_mb_l (k) = 100.; H_m_l (k) = 18.1; 
      k=20; Coord_P_mb_l (k) = 150.; H_m_l (k) = 15.2; 
      k=19; Coord_P_mb_l (k) = 200.; H_m_l (k) = 13.2; 
      k=18; Coord_P_mb_l (k) = 250.; H_m_l (k) = 11.8; 
      k=17; Coord_P_mb_l (k) = 300.; H_m_l (k) = 10.7; 
      k=16; Coord_P_mb_l (k) = 350.; H_m_l (k) = 10.2; 
      k=15; Coord_P_mb_l (k) = 400.; H_m_l (k) =  9.8; 
      k=14; Coord_P_mb_l (k) = 450.; H_m_l (k) =  9.1; 
      k=13; Coord_P_mb_l (k) = 500.; H_m_l (k) =  8.4; 
      k=12; Coord_P_mb_l (k) = 550.; H_m_l (k) =  7.5; 
      k=11; Coord_P_mb_l (k) = 600.; H_m_l (k) =  6.6; 
      k=10; Coord_P_mb_l (k) = 650.; H_m_l (k) =  5.9; 
      k= 9; Coord_P_mb_l (k) = 700.; H_m_l (k) =  5.2; 
      k= 8; Coord_P_mb_l (k) = 750.; H_m_l (k) =  4.9; 
      k= 7; Coord_P_mb_l (k) = 800.; H_m_l (k) =  4.7; 
      k= 6; Coord_P_mb_l (k) = 850.; H_m_l (k) =  4.4; 
      k= 5; Coord_P_mb_l (k) = 900.; H_m_l (k) =  4.4; 
      k= 4; Coord_P_mb_l (k) = 950.; H_m_l (k) =  4.3; 
      k= 3; Coord_P_mb_l (k) =1000.; H_m_l (k) =  4.3; 
      k= 2; Coord_P_mb_l (k) =1050.; H_m_l (k) =  4.3; 
      k= 1; Coord_P_mb_l (k) =1100.; H_m_l (k) =  4.3; 

! 3. END
! =======
 
! 3.1 Turn off status error
!     ---------------------
      ierr = 0

   RETURN

   END SUBROUTINE err_height_upr
!------------------------------------------------------------------------------!

 SUBROUTINE err_height_sfc (err_levels, Coord_P_mb_l, H_m_l, ierr)
!------------------------------------------------------------------------------!
! 
! ECMWF surface height observational error. 
!
! The surface value of the original ECMWF data given on 15 pressure levels 
! (Courtier et al. 1999, QJR Metero. Soc., 123) have been used to populate
! the NCEP 33 observational error levels.
!
! Output is pressure levels in hPa and height errors in m from bottom to top.
!
!------------------------------------------------------------------------------!
! Copyright UCAR (c) 2010.
! University Corporation for Atmospheric Research (UCAR),
! National Center for Atmospheric Research (NCAR),
! Research Applications Laboratory (RAL),
! P.O.Box 3000, Boulder, Colorado, 80307-3000, USA.
!
! Francois Vandenberghe, vandenb@ucar.edu, October 2010.
!------------------------------------------------------------------------------!
  IMPLICIT NONE

  INTEGER, INTENT (in) :: err_levels
  REAL,    DIMENSION (err_levels), INTENT (out) :: Coord_P_mb_l, H_m_l
  INTEGER, INTENT (out) :: ierr

  INTEGER :: k
  CHARACTER (LEN =  40), PARAMETER :: proc_name = 'err_height_sfc'
  CHARACTER (LEN = 200) :: error_message
!------------------------------------------------------------------------------!

! 1.  CHECK DIMENSIONS
! ====================

! 1.1 Turn error status on
!     --------------------

      ierr = -1

! 1.2 Check dimension
!     ---------------
 
      IF (err_levels /= 33) THEN
          WRITE (error_message,'(3A)') TRIM (proc_name),": ", &
                "Error in dimensions, err_levels must be 33."
          CALL wrf_message (error_message)
      ENDIF

! 2.  EIGHT ERRORS
! ================

      k=33; Coord_P_mb_l (k) =   0.; H_m_l (k) =  4.3; 
      k=32; Coord_P_mb_l (k) =   1.; H_m_l (k) =  4.3; 
      k=31; Coord_P_mb_l (k) =   2.; H_m_l (k) =  4.3; 
      k=30; Coord_P_mb_l (k) =   3.; H_m_l (k) =  4.3; 
      k=29; Coord_P_mb_l (k) =   4.; H_m_l (k) =  4.3; 
      k=28; Coord_P_mb_l (k) =   5.; H_m_l (k) =  4.3; 
      k=27; Coord_P_mb_l (k) =  10.; H_m_l (k) =  4.3; 
      k=26; Coord_P_mb_l (k) =  20.; H_m_l (k) =  4.3; 
      k=25; Coord_P_mb_l (k) =  30.; H_m_l (k) =  4.3; 
      k=24; Coord_P_mb_l (k) =  40.; H_m_l (k) =  4.3; 
      k=23; Coord_P_mb_l (k) =  50.; H_m_l (k) =  4.3; 
      k=22; Coord_P_mb_l (k) =  75.; H_m_l (k) =  4.3; 
      k=21; Coord_P_mb_l (k) = 100.; H_m_l (k) =  4.3; 
      k=20; Coord_P_mb_l (k) = 150.; H_m_l (k) =  4.3; 
      k=19; Coord_P_mb_l (k) = 200.; H_m_l (k) =  4.3; 
      k=18; Coord_P_mb_l (k) = 250.; H_m_l (k) =  4.3; 
      k=17; Coord_P_mb_l (k) = 300.; H_m_l (k) =  4.3; 
      k=16; Coord_P_mb_l (k) = 350.; H_m_l (k) =  4.3; 
      k=15; Coord_P_mb_l (k) = 400.; H_m_l (k) =  4.3; 
      k=14; Coord_P_mb_l (k) = 450.; H_m_l (k) =  4.3; 
      k=13; Coord_P_mb_l (k) = 500.; H_m_l (k) =  4.3; 
      k=12; Coord_P_mb_l (k) = 550.; H_m_l (k) =  4.3; 
      k=11; Coord_P_mb_l (k) = 600.; H_m_l (k) =  4.3; 
      k=10; Coord_P_mb_l (k) = 650.; H_m_l (k) =  4.3; 
      k= 9; Coord_P_mb_l (k) = 700.; H_m_l (k) =  4.3; 
      k= 8; Coord_P_mb_l (k) = 750.; H_m_l (k) =  4.3; 
      k= 7; Coord_P_mb_l (k) = 800.; H_m_l (k) =  4.3; 
      k= 6; Coord_P_mb_l (k) = 850.; H_m_l (k) =  4.3; 
      k= 5; Coord_P_mb_l (k) = 900.; H_m_l (k) =  4.3; 
      k= 4; Coord_P_mb_l (k) = 950.; H_m_l (k) =  4.3; 
      k= 3; Coord_P_mb_l (k) =1000.; H_m_l (k) =  4.3; 
      k= 2; Coord_P_mb_l (k) =1050.; H_m_l (k) =  4.3; 
      k= 1; Coord_P_mb_l (k) =1100.; H_m_l (k) =  4.3; 

! 3. END
! =======

! 3.1 Turn off status error
!     ---------------------
      ierr = 0


   RETURN

   END SUBROUTINE err_height_sfc
!------------------------------------------------------------------------------!

 SUBROUTINE err_rh_upr (err_levels, Coord_P_mb_l, RH_water_l, ierr)
!------------------------------------------------------------------------------!
! 
! Sounding relative humidity observational error on the 33 NCEP observational 
! error pressure levels.
!
! Output is pressure levels in hPa and rh error with respect to water in % 
! from bottom to top.
!
!------------------------------------------------------------------------------!
! Copyright UCAR (c) 2010.
! University Corporation for Atmospheric Research (UCAR),
! National Center for Atmospheric Research (NCAR),
! Research Applications Laboratory (RAL),
! P.O.Box 3000, Boulder, Colorado, 80307-3000, USA.
!
! Francois Vandenberghe, vandenb@ucar.edu, October 2010.
!------------------------------------------------------------------------------!
  IMPLICIT NONE

  INTEGER, INTENT (in) :: err_levels
  REAL,    DIMENSION (err_levels), INTENT (out) :: Coord_P_mb_l, RH_water_l
  INTEGER, INTENT (out) :: ierr

  INTEGER :: k
  CHARACTER (LEN =  40), PARAMETER :: proc_name = 'err_rh_upr'
  CHARACTER (LEN = 200) :: error_message
!------------------------------------------------------------------------------!

! 1.  CHECK DIMENSIONS
! ====================

! 1.1 Turn error status on
!     --------------------

      ierr = -1

! 1.2 Check dimension
!     ---------------
 
      IF (err_levels /= 33) THEN
          WRITE (error_message,'(3A)') TRIM (proc_name),": ", &
                "Error in dimensions, err_levels must be 33."
          CALL wrf_message (error_message)
      ENDIF

! 2.  EIGHT ERRORS
! ================

      k=33; Coord_P_mb_l (k) =   0.; RH_water_l (k) = 40.0;
      k=32; Coord_P_mb_l (k) =   1.; RH_water_l (k) = 40.0;
      k=31; Coord_P_mb_l (k) =   2.; RH_water_l (k) = 40.0;
      k=30; Coord_P_mb_l (k) =   3.; RH_water_l (k) = 40.0;
      k=29; Coord_P_mb_l (k) =   4.; RH_water_l (k) = 40.0;
      k=28; Coord_P_mb_l (k) =   5.; RH_water_l (k) = 40.0;
      k=27; Coord_P_mb_l (k) =  10.; RH_water_l (k) = 40.0;
      k=26; Coord_P_mb_l (k) =  20.; RH_water_l (k) = 40.0;
      k=25; Coord_P_mb_l (k) =  30.; RH_water_l (k) = 40.0;
      k=24; Coord_P_mb_l (k) =  40.; RH_water_l (k) = 40.0;
      k=23; Coord_P_mb_l (k) =  50.; RH_water_l (k) = 40.0;
      k=22; Coord_P_mb_l (k) =  75.; RH_water_l (k) = 40.0;
      k=21; Coord_P_mb_l (k) = 100.; RH_water_l (k) = 40.0;
      k=20; Coord_P_mb_l (k) = 150.; RH_water_l (k) = 40.0;
      k=19; Coord_P_mb_l (k) = 200.; RH_water_l (k) = 40.0;
      k=18; Coord_P_mb_l (k) = 250.; RH_water_l (k) = 40.0;
      k=17; Coord_P_mb_l (k) = 300.; RH_water_l (k) = 40.0;
      k=16; Coord_P_mb_l (k) = 350.; RH_water_l (k) = 40.0;
      k=15; Coord_P_mb_l (k) = 400.; RH_water_l (k) = 40.0;
      k=14; Coord_P_mb_l (k) = 450.; RH_water_l (k) = 40.0;
      k=13; Coord_P_mb_l (k) = 500.; RH_water_l (k) = 40.0;
      k=12; Coord_P_mb_l (k) = 550.; RH_water_l (k) = 40.0;
      k=11; Coord_P_mb_l (k) = 600.; RH_water_l (k) = 40.0;
      k=10; Coord_P_mb_l (k) = 650.; RH_water_l (k) = 40.0;
      k= 9; Coord_P_mb_l (k) = 700.; RH_water_l (k) = 40.0;
      k= 8; Coord_P_mb_l (k) = 750.; RH_water_l (k) = 30.0;
      k= 7; Coord_P_mb_l (k) = 800.; RH_water_l (k) = 30.0;
      k= 6; Coord_P_mb_l (k) = 850.; RH_water_l (k) = 30.0;
      k= 5; Coord_P_mb_l (k) = 900.; RH_water_l (k) = 30.0;
      k= 4; Coord_P_mb_l (k) = 950.; RH_water_l (k) = 30.0;
      k= 3; Coord_P_mb_l (k) =1000.; RH_water_l (k) = 20.0;
      k= 2; Coord_P_mb_l (k) =1050.; RH_water_l (k) = 20.0;
      k= 1; Coord_P_mb_l (k) =1100.; RH_water_l (k) = 20.0;

! 3. END
! =======

! 3.1 Turn off status error
!     ---------------------
      ierr = 0


   RETURN

   END SUBROUTINE err_rh_upr
!------------------------------------------------------------------------------!

 SUBROUTINE err_rh_sfc (err_levels, Coord_P_mb_l, RH_water_l, ierr)
!------------------------------------------------------------------------------!
! 
! Surface Relative humidity observational error on the 33 NCEP observational 
! error pressure levels.
!
! Output is pressure levels in hPa and rh error with respect to water in % 
! from bottom to top.
!
!------------------------------------------------------------------------------!
! Copyright UCAR (c) 2010.
! University Corporation for Atmospheric Research (UCAR),
! National Center for Atmospheric Research (NCAR),
! Research Applications Laboratory (RAL),
! P.O.Box 3000, Boulder, Colorado, 80307-3000, USA.
!
! Francois Vandenberghe, vandenb@ucar.edu, October 2010.
!------------------------------------------------------------------------------!
  IMPLICIT NONE

  INTEGER, INTENT (in) :: err_levels
  REAL,    DIMENSION (err_levels), INTENT (out) :: Coord_P_mb_l, RH_water_l
  INTEGER, INTENT (out) :: ierr

  INTEGER :: k
  CHARACTER (LEN =  40), PARAMETER :: proc_name = 'err_rh_sfc'
  CHARACTER (LEN = 200) :: error_message
!------------------------------------------------------------------------------!

! 1.  CHECK DIMENSIONS
! ====================
 
! 1.1 Turn error status on
!     --------------------

      ierr = -1

! 1.2 Check dimension
!     ---------------
      IF (err_levels /= 33) THEN
          WRITE (error_message,'(3A)') TRIM (proc_name),": ", &
                "Error in dimensions, err_levels must be 33."
          CALL wrf_message (error_message)
      ENDIF

! 2.  EIGHT ERRORS
! ================

      k=33; Coord_P_mb_l (k) =   0.; RH_water_l (k) = 20.0;
      k=32; Coord_P_mb_l (k) =   1.; RH_water_l (k) = 20.0;
      k=31; Coord_P_mb_l (k) =   2.; RH_water_l (k) = 20.0;
      k=30; Coord_P_mb_l (k) =   3.; RH_water_l (k) = 20.0;
      k=29; Coord_P_mb_l (k) =   4.; RH_water_l (k) = 20.0;
      k=28; Coord_P_mb_l (k) =   5.; RH_water_l (k) = 20.0;
      k=27; Coord_P_mb_l (k) =  10.; RH_water_l (k) = 20.0;
      k=26; Coord_P_mb_l (k) =  20.; RH_water_l (k) = 20.0;
      k=25; Coord_P_mb_l (k) =  30.; RH_water_l (k) = 20.0;
      k=24; Coord_P_mb_l (k) =  40.; RH_water_l (k) = 20.0;
      k=23; Coord_P_mb_l (k) =  50.; RH_water_l (k) = 20.0;
      k=22; Coord_P_mb_l (k) =  75.; RH_water_l (k) = 20.0;
      k=21; Coord_P_mb_l (k) = 100.; RH_water_l (k) = 20.0;
      k=20; Coord_P_mb_l (k) = 150.; RH_water_l (k) = 20.0;
      k=19; Coord_P_mb_l (k) = 200.; RH_water_l (k) = 20.0;
      k=18; Coord_P_mb_l (k) = 250.; RH_water_l (k) = 20.0;
      k=17; Coord_P_mb_l (k) = 300.; RH_water_l (k) = 20.0;
      k=16; Coord_P_mb_l (k) = 350.; RH_water_l (k) = 20.0;
      k=15; Coord_P_mb_l (k) = 400.; RH_water_l (k) = 20.0;
      k=14; Coord_P_mb_l (k) = 450.; RH_water_l (k) = 20.0;
      k=13; Coord_P_mb_l (k) = 500.; RH_water_l (k) = 20.0;
      k=12; Coord_P_mb_l (k) = 550.; RH_water_l (k) = 20.0;
      k=11; Coord_P_mb_l (k) = 600.; RH_water_l (k) = 20.0;
      k=10; Coord_P_mb_l (k) = 650.; RH_water_l (k) = 20.0;
      k= 9; Coord_P_mb_l (k) = 700.; RH_water_l (k) = 20.0;
      k= 8; Coord_P_mb_l (k) = 750.; RH_water_l (k) = 20.0;
      k= 7; Coord_P_mb_l (k) = 800.; RH_water_l (k) = 20.0;
      k= 6; Coord_P_mb_l (k) = 850.; RH_water_l (k) = 20.0;
      k= 5; Coord_P_mb_l (k) = 900.; RH_water_l (k) = 20.0;
      k= 4; Coord_P_mb_l (k) = 950.; RH_water_l (k) = 20.0;
      k= 3; Coord_P_mb_l (k) =1000.; RH_water_l (k) = 20.0;
      k= 2; Coord_P_mb_l (k) =1050.; RH_water_l (k) = 20.0;
      k= 1; Coord_P_mb_l (k) =1100.; RH_water_l (k) = 20.0;

! 3. END
! =======

! 3.1 Turn off status error
!     ---------------------
      ierr = 0


   RETURN

   END SUBROUTINE err_rh_sfc
!------------------------------------------------------------------------------!

 SUBROUTINE err_pres_upr (err_levels, Coord_P_mb_l, P_mb_l, ierr)
!------------------------------------------------------------------------------!
!
! Hardcoded obs error for pressure from Parrish and Derber (1992)
! The curve dP_mb = 0.091 * P_mb + 9 was fitted to Parrish and Derber 
! original 5 levels
!
!------------------------------------------------------------------------------!
! Copyright UCAR (c) 2010.
! University Corporation for Atmospheric Research (UCAR),
! National Center for Atmospheric Research (NCAR),
! Research Applications Laboratory (RAL),
! P.O.Box 3000, Boulder, Colorado, 80307-3000, USA.
!
! Francois Vandenberghe, vandenb@ucar.edu, October 2010.
!------------------------------------------------------------------------------!
  IMPLICIT NONE

  INTEGER, INTENT (in) :: err_levels
  REAL,    DIMENSION (err_levels), INTENT (out) :: Coord_P_mb_l, P_mb_l
  INTEGER, INTENT (out) :: ierr

  INTEGER :: k
  CHARACTER (LEN =  40), PARAMETER :: proc_name = 'err_pres_upr'
  CHARACTER (LEN = 200) :: error_message
!------------------------------------------------------------------------------!

! 1.  CHECK DIMENSIONS
! ====================

! 1.1 Turn error status on
!     --------------------

      ierr = -1

! 1.2 Check dimension
!     ---------------
 
      IF (err_levels /= 33) THEN
          WRITE (error_message,'(3A)') TRIM (proc_name),": ", &
                 "Error in dimensions, err_levels must be 33."
          CALL wrf_message (error_message)
          RETURN
      ENDIF

! 2.  CORRECT PRESSURE BASED ON  PARRISH S. AND J. DERBER 1992
! ============================================================
! Use expression dP_mb = 0.091 * P_mb + 9

      k=33; Coord_P_mb_l (k) =   0.; P_mb_l (k) =   9.0000; 
      k=32; Coord_P_mb_l (k) =   1.; P_mb_l (k) =   9.0910; 
      k=31; Coord_P_mb_l (k) =   2.; P_mb_l (k) =   9.1820; 
      k=30; Coord_P_mb_l (k) =   3.; P_mb_l (k) =   9.2730; 
      k=29; Coord_P_mb_l (k) =   4.; P_mb_l (k) =   9.3640; 
      k=28; Coord_P_mb_l (k) =   5.; P_mb_l (k) =   9.4550; 
      k=27; Coord_P_mb_l (k) =  10.; P_mb_l (k) =   9.9100; 
      k=26; Coord_P_mb_l (k) =  20.; P_mb_l (k) =  10.8200; 
      k=25; Coord_P_mb_l (k) =  30.; P_mb_l (k) =  11.7300; 
      k=24; Coord_P_mb_l (k) =  40.; P_mb_l (k) =  12.6400; 
      k=23; Coord_P_mb_l (k) =  50.; P_mb_l (k) =  13.5500; 
      k=22; Coord_P_mb_l (k) =  75.; P_mb_l (k) =  15.5500; 
      k=21; Coord_P_mb_l (k) = 100.; P_mb_l (k) =  18.1000; 
      k=20; Coord_P_mb_l (k) = 150.; P_mb_l (k) =  22.6500; 
      k=19; Coord_P_mb_l (k) = 200.; P_mb_l (k) =  27.2000; 
      k=18; Coord_P_mb_l (k) = 250.; P_mb_l (k) =  31.7500; 
      k=17; Coord_P_mb_l (k) = 300.; P_mb_l (k) =  36.3000; 
      k=16; Coord_P_mb_l (k) = 350.; P_mb_l (k) =  40.8500; 
      k=15; Coord_P_mb_l (k) = 400.; P_mb_l (k) =  45.4000; 
      k=14; Coord_P_mb_l (k) = 450.; P_mb_l (k) =  49.4000; 
      k=13; Coord_P_mb_l (k) = 500.; P_mb_l (k) =  54.5000; 
      k=12; Coord_P_mb_l (k) = 550.; P_mb_l (k) =  59.0500; 
      k=11; Coord_P_mb_l (k) = 600.; P_mb_l (k) =  63.6000; 
      k=10; Coord_P_mb_l (k) = 650.; P_mb_l (k) =  68.1500; 
      k= 9; Coord_P_mb_l (k) = 700.; P_mb_l (k) =  72.7000; 
      k= 8; Coord_P_mb_l (k) = 750.; P_mb_l (k) =  77.2500; 
      k= 7; Coord_P_mb_l (k) = 800.; P_mb_l (k) =  81.8000; 
      k= 6; Coord_P_mb_l (k) = 850.; P_mb_l (k) =  86.3500; 
      k= 5; Coord_P_mb_l (k) = 900.; P_mb_l (k) =  90.9000; 
      k= 4; Coord_P_mb_l (k) = 950.; P_mb_l (k) =  95.4500; 
      k= 3; Coord_P_mb_l (k) =1000.; P_mb_l (k) = 100.0000; 
      k= 2; Coord_P_mb_l (k) =1050.; P_mb_l (k) = 104.5500; 
      k= 1; Coord_P_mb_l (k) =1100.; P_mb_l (k) = 109.1000;

! 2.3 Convert into mb
!     ---------------
      P_mb_l = P_mb_l / 100.

! 3. END
! =======
 
! 3.1 Turn off status error
!     ---------------------

      ierr = 0

   RETURN

   END SUBROUTINE err_pres_upr
!------------------------------------------------------------------------------!

 SUBROUTINE err_pres_sfc (err_levels, Coord_P_mb_l, P_mb_l, ierr)
!------------------------------------------------------------------------------!
!
! Uper-air values are applied for surface.
!
! Hardcoded obs error for pressure from Parrish and Derber (1992)
! The curve dP_mb = 0.091 * P_mb + 9 was fitted to Parrish and Derber 
! original 5 levels
!
!------------------------------------------------------------------------------!
! Copyright UCAR (c) 2010.
! University Corporation for Atmospheric Research (UCAR),
! National Center for Atmospheric Research (NCAR),
! Research Applications Laboratory (RAL),
! P.O.Box 3000, Boulder, Colorado, 80307-3000, USA.
!
! Francois Vandenberghe, vandenb@ucar.edu, October 2010.
!------------------------------------------------------------------------------!
  IMPLICIT NONE

  INTEGER, INTENT (in) :: err_levels
  REAL,    DIMENSION (err_levels), INTENT (out) :: Coord_P_mb_l, P_mb_l
  INTEGER, INTENT (out) :: ierr

  INTEGER :: k
  CHARACTER (LEN =  40), PARAMETER :: proc_name = 'err_pres_sfc'
  CHARACTER (LEN = 200) :: error_message
!------------------------------------------------------------------------------!

! 1.  CHECK DIMENSIONS
! ====================

! 1.1 Turn error status on
!     --------------------

      ierr = -1

! 1.2 Check dimension
!     ---------------
 
      IF (err_levels /= 33) THEN
          WRITE (error_message,'(3A)') TRIM (proc_name),": ", &
                 "Error in dimensions, err_levels must be 33."
          CALL wrf_message (error_message)
          RETURN
      ENDIF

! 2.  CORRECT PRESSURE BASED ON  PARRISH S. AND J. DERBER 1992
! ============================================================
! Use expression dP_mb = 0.091 * P_mb + 9

      k=33; Coord_P_mb_l (k) =   0.; P_mb_l (k) =   9.0000; 
      k=32; Coord_P_mb_l (k) =   1.; P_mb_l (k) =   9.0910; 
      k=31; Coord_P_mb_l (k) =   2.; P_mb_l (k) =   9.1820; 
      k=30; Coord_P_mb_l (k) =   3.; P_mb_l (k) =   9.2730; 
      k=29; Coord_P_mb_l (k) =   4.; P_mb_l (k) =   9.3640; 
      k=28; Coord_P_mb_l (k) =   5.; P_mb_l (k) =   9.4550; 
      k=27; Coord_P_mb_l (k) =  10.; P_mb_l (k) =   9.9100; 
      k=26; Coord_P_mb_l (k) =  20.; P_mb_l (k) =  10.8200; 
      k=25; Coord_P_mb_l (k) =  30.; P_mb_l (k) =  11.7300; 
      k=24; Coord_P_mb_l (k) =  40.; P_mb_l (k) =  12.6400; 
      k=23; Coord_P_mb_l (k) =  50.; P_mb_l (k) =  13.5500; 
      k=22; Coord_P_mb_l (k) =  75.; P_mb_l (k) =  15.5500; 
      k=21; Coord_P_mb_l (k) = 100.; P_mb_l (k) =  18.1000; 
      k=20; Coord_P_mb_l (k) = 150.; P_mb_l (k) =  22.6500; 
      k=19; Coord_P_mb_l (k) = 200.; P_mb_l (k) =  27.2000; 
      k=18; Coord_P_mb_l (k) = 250.; P_mb_l (k) =  31.7500; 
      k=17; Coord_P_mb_l (k) = 300.; P_mb_l (k) =  36.3000; 
      k=16; Coord_P_mb_l (k) = 350.; P_mb_l (k) =  40.8500; 
      k=15; Coord_P_mb_l (k) = 400.; P_mb_l (k) =  45.4000; 
      k=14; Coord_P_mb_l (k) = 450.; P_mb_l (k) =  49.4000; 
      k=13; Coord_P_mb_l (k) = 500.; P_mb_l (k) =  54.5000; 
      k=12; Coord_P_mb_l (k) = 550.; P_mb_l (k) =  59.0500; 
      k=11; Coord_P_mb_l (k) = 600.; P_mb_l (k) =  63.6000; 
      k=10; Coord_P_mb_l (k) = 650.; P_mb_l (k) =  68.1500; 
      k= 9; Coord_P_mb_l (k) = 700.; P_mb_l (k) =  72.7000; 
      k= 8; Coord_P_mb_l (k) = 750.; P_mb_l (k) =  77.2500; 
      k= 7; Coord_P_mb_l (k) = 800.; P_mb_l (k) =  81.8000; 
      k= 6; Coord_P_mb_l (k) = 850.; P_mb_l (k) =  86.3500; 
      k= 5; Coord_P_mb_l (k) = 900.; P_mb_l (k) =  90.9000; 
      k= 4; Coord_P_mb_l (k) = 950.; P_mb_l (k) =  95.4500; 
      k= 3; Coord_P_mb_l (k) =1000.; P_mb_l (k) = 100.0000; 
      k= 2; Coord_P_mb_l (k) =1050.; P_mb_l (k) = 104.5500; 
      k= 1; Coord_P_mb_l (k) =1100.; P_mb_l (k) = 109.1000;

! 2.3 Convert into mb
!     ---------------
      P_mb_l = P_mb_l / 100.

! 3. END
! =======
 
! 3.1 Turn off status error
!     ---------------------

      ierr = 0

   RETURN

   END SUBROUTINE err_pres_sfc
!------------------------------------------------------------------------------!

  FUNCTION unit_observ_error_ncep (platform) RESULT (iunit)
!------------------------------------------------------------------------------!
!
! Given the report platform type, return the NCEP observational file
! unit number according to the table given at the end of this file.
!
!------------------------------------------------------------------------------!
! Copyright UCAR (c) 2010.
! University Corporation for Atmospheric Research (UCAR),
! National Center for Atmospheric Research (NCAR),
! Research Applications Laboratory (RAL),
! P.O.Box 3000, Boulder, Colorado, 80307-3000, USA.
!
! Francois Vandenberghe, vandenb@ucar.edu, October 2010.
!------------------------------------------------------------------------------!
  IMPLICIT NONE

  CHARACTER (len=40) :: platform
  INTEGER            :: iunit
  INTEGER            :: fm
  CHARACTER (len=40) :: platformo
  CHARACTER*200 :: error_message
!------------------------------------------------------------------------------!

  SELECT CASE (platform (7:11))

    CASE ('TEMP ')  
          iunit  = 20; ! RAWINS
          fm = 35
          platformo = 'FM-35 TEMP'

    CASE ('MWVR ')     ! WATER VAPOR RADIOMETER
          iunit  = 20; ! RAWINS
          fm = 35      !  Assing TEMP
          platformo = 'FM-35 TEMP'

    CASE ('NPN-P')     ! None WMO
          iunit  = 20; ! RAWINS
          fm = 32      !  Assing PILOT
          platformo = 'FM-32 PILOT'

    CASE ('PILOT')  
          iunit  = 21; ! PIBAL
          fm = 32
          platformo = 'FM-32 PILOT'

    CASE ('PIBAL')  
          iunit  = 21; ! PIBAL
          fm = 32
          platformo = 'FM-32 PILOT'

    CASE ('PROFI')     ! MAP WIND PROFILER  
          iunit  = 20; ! RAWINS
          fm = 32      !  Assing PILOT
          platformo = 'FM-32 PILOT'

    CASE ('SATWI')     ! None WMO
          iunit  = 34  ! GOES LOW
          fm = 88      ! ASSIGN SATOB
          platformo = 'FM-88 SATOB'

    CASE ('SATWN')     ! None WMO
          iunit  = 34  ! GOES LOW
          fm = 88      ! ASSIGN SATOB
          platformo = 'FM-88 SATOB'

    CASE ('AMDAR')  
          iunit  = 20; ! RAWINS by default
          fm = 42
          platformo = 'FM-42 AMDAR'

!         IF (index (name,'AIREP')  > 0)  THEN
!             iunit  = 30  ! AIREP/PIREP AIRCRAFT FLIGHT-LEVEL
!         ELSE IF (index (name,'ASDAR')  > 0)  THEN
!             iunit  = 31  ! ASDAR AIRCRAFD FLIGHT-LEVEL
!         ELSE IF (index (name,'ACARS')  > 0)  THEN
!             iunit  = 33  ! ACARS AIRCRAFT FLIGHT-LEVEL
!         ENDIF

    CASE ('TAMDA')  
          iunit  = 20; ! RAWINS by default
          fm = 97
          platformo = 'FM-97 TAMDAR'

    CASE ('ACARS')  
          iunit  = 20; ! RAWINS by default
          fm = 97
          platformo = 'FM-97 ACARS'

    CASE ('ACAMD')  
          iunit  = 20; ! RAWINS by default
          fm = 97
          platformo = 'FM-97 ACARS'

    CASE ('MDCRS')  
          iunit  = 20; ! RAWINS by default
          fm = 97
          platformo = 'FM-97 ACARS'

    CASE ('MADIS')  
          iunit  = 20; ! RAWINS by default
          fm = 97
          platformo = 'FM-97 ACARS'

    CASE ('METAR')  
          iunit  = 81  ! LAND SURFACE STATION WITH PRESSURE
          fm = 15 
          platformo = 'FM-15 METAR'

    CASE ('SPECI')  
          iunit  = 81  ! LAND SURFACE STATION WITH PRESSURE
          fm = 16
          platformo = 'FM-16 SPECI'

    CASE ('SHIP ')  
          iunit  = 81  ! LAND SURFACE STATION WITH PRESSURE
          fm = 13
          platformo = 'FM-13 SHIP'

    CASE ('QSCAT')  
          iunit  = 86  ! LAND SURFACE STATION WITH PRESSURE
          fm = 13
          platformo = 'FM-13 QSCAT'
!         WRITE (platformo,'(A,I2,1X,A)') "FM-", fm, platform (7:)

    CASE ('SYNOP')     ! INCLUDE GTS, SAMS, METNET, OKMESO AND PROFILER
          iunit  = 81  ! LAND SURFACE STATION WITH PRESSURE
          fm = 12      ! ASSIGN SYNOP BY DEFAULT  
          platformo = 'FM-12 SYNOP'

!         IF (index (name,'AIREP')  > 0)  THEN
!             iunit  = 30  ! AIREP/PIREP AIRCRAFT FLIGHT-LEVEL
!         ELSE IF (index (name,'ASDAR')  > 0)  THEN
!             iunit  = 31  ! ASDAR AIRCRAFD FLIGHT-LEVEL
!         ELSE IF (index (name,'ACARS')  > 0)  THEN
!             iunit  = 33  ! ACARS AIRCRAFT FLIGHT-LEVEL
!         ELSE IF (index (name,'PROFILER')  > 0)  THEN
!             iunit  = 20; ! RAWINS
!             fm = 32      ! ASSIGN PILOT
!             platformo = 'FM-32 PILOT'
!         ENDIF

    CASE ('AIREP')     ! AIREP/PIREP AIRCRAFT FLIGHT-LEVEL
          iunit  = 30  ! AIREP/PIREP AIRCRAFT FLIGHT-LEVEL
          fm = 12      ! ASSIGN SYNOP BY DEFAULT
          platformo = 'FM-12 SYNOP'
!! wuyh added 20130816
    CASE ('TEC  ')     ! SAMS ATEC
          iunit  = 30  ! 
          fm = 35      ! 
          platformo = 'FM-35 TEMP'
!! wuyh added 20130816 end 
    CASE DEFAULT
          iunit  = 20; ! RAWINS
          fm = 35      ! ASSIGN TEMP BY DEFAULT
          platformo = 'FM-35 TEMP'

          WRITE  (error_message,'(4A)') " Unknown platform: ",  &
                                          TRIM (platform),", ", &
                                        " assign RAWINS observational error."
          CALL wrf_message (error_message)

    END SELECT

  END FUNCTION unit_observ_error_ncep
!------------------------------------------------------------------------------!

  FUNCTION oberr_list (iunit) RESULT (platform)
!------------------------------------------------------------------------------!
! Associate NCEP error file id with the corresponding platform string as 
! given in the table at the end of this file.
!
!------------------------------------------------------------------------------!
! Copyright UCAR (c) 2010.
! University Corporation for Atmospheric Research (UCAR),
! National Center for Atmospheric Research (NCAR),
! Research Applications Laboratory (RAL),
! P.O.Box 3000, Boulder, Colorado, 80307-3000, USA.
!
! Francois Vandenberghe, vandenb@ucar.edu, October 2010.
!------------------------------------------------------------------------------!
  IMPLICIT NONE

  INTEGER            :: iunit
  CHARACTER (len=40) :: platform

!------------------------------------------------------------------------------!

  SELECT CASE (iunit)

    CASE (202); platform = "currently not used" 
    CASE (102); platform = "SSM/I 7-CHANNEL BRIGHTNESS TEMPERATURE"
    CASE (210); platform = "SYNTHETIC TROPICAL CYCLONE WIND"
    CASE (110); platform = "UPPER-AIR HEIGHT BOGUS"
    CASE (220); platform = "RAWINSONDE WIND"
    CASE (120); platform = "RAWINSONDE MASS"
    CASE (221); platform = "PIBAL WIND"
    CASE (121); platform = "currently not used"
    CASE (222); platform = "CLASS SOUNDING WIND" 
    CASE (122); platform = "CLASS SOUNDING MASS"
    CASE (223); platform = "PROFILER WIND"
    CASE (123); platform = "currently not used"
    CASE (224); platform = "VAD (NEXRAD) WIND"      
    CASE (124); platform = "currently not used"
    CASE (225); platform = "NEXRAD RADIAL WIND"      
    CASE (125); platform = "currently not used"
    CASE (230); platform = "AIREP/PIREP AIRCRAFT FLIGHT-LEVEL WIND"
    CASE (130); platform = "AIREP/PIREP AIRCRAFT FLIGHT-LEVEL MASS"
    CASE (231); platform = "ASDAR AIRCRAFT FLIGHT-LEVEL WIND"
    CASE (131); platform = "ASDAR AIRCRAFT FLIGHT-LEVEL MASS"
    CASE (232); platform = "RECONNAISSANCE: DROPWINSONDES WIND"
    CASE (132); platform = "RECONNAISSANCE: DROPWINSONDES MASS"
    CASE (233); platform = "ACARS AIRCRAFT FLIGHT-LEVEL WIND"
    CASE (133); platform = "ACARS AIRCRAFT FLIGHT-LEVEL MASS"
    CASE (240); platform = "NESDIS/NWS: GOES-ODD CLD-DRFT LOW-LVL"
    CASE (140); platform = "NESDIS/NWS:GOES-ODD SAT CLOUD-DRIFT"
    CASE (241); platform = "INDIA:INSAT CLOUD - DRFT L0W-LVL"
    CASE (141); platform = "INDIA:INSAT SATELLITE CLOUD-DRIFT"
    CASE (242); platform = "JAPAN: GMS SAT CLOUD-DRIFT LOW-LVL"
    CASE (142); platform = "JAPAN: GMS SATELLITE CLOUD-DRIFT"
    CASE (243); platform = "ESA: METEOSAT SAT CLD-DRIFT LOW-LVL"               
    CASE (143); platform = "ESA: METEOSAT SATELLITE CLOUD-DRIFT"
    CASE (244); platform = "NESDIS/NWS: GOES-EVN CLD-DRFT LOW-LVL"
    CASE (144); platform = "NESDIS/NWS:GOES-EVEN SAT CLOUD-DRIFT"
    CASE (245); platform = "NESDIS: METEOSAT SAT CLD-DRIFT LOW-LVL"
    CASE (145); platform = "NESDIS:METEOSAT SATELLITE CLOUD-DRIFT"
    CASE (246); platform = "NESDIS: GOES SAT VASGRADIENT LOW-LVL"
    CASE (146); platform = "NESDIS: GOES SATELLITE VAS GRADIENT"
    CASE (248); platform = "NESDIS: METEOSAT WATER VAPOR LOW-LVL"
    CASE (148); platform = "NESDIS:METEOSAT SATELLITE WATER VAPOR"
    CASE (249); platform = "NESDIS: GOES SAT WATER VAPOR LOW-LVL"
    CASE (149); platform = "NESDIS: GOES SATELLITE WATER VAPOR"
    CASE (250); platform = "NESDIS/NWS: GOES-ODD CLD-DRIFT HI-LVL"
    CASE (150); platform = "SATELLITE MOISTURE BOGUS"
    CASE (251); platform = "IND1A INSAT CLOUD-DRIFT HI-LVL"
    CASE (151); platform = "RAOB PRECIPITABLE WATER(CALCULATED)"
    CASE (252); platform = "JAPAN GMS SAT CLOUD - DRIFT HI-LEVEL"
    CASE (152); platform = "SSM/I PRECIPITABLE WATER RETRIEVAL"
    CASE (253); platform = "ESA : METEOSAT CLOUD-DRFT HI-LVL"               
    CASE (153); platform = "TOVS (NMCEDS) PRECIP. WATER RETRIEVAL"
    CASE (254); platform = "NESDIS/NWS : GOES-EVN CLD-DRIFT HI-LVL"
    CASE (154); platform = "currently not used"
    CASE (255); platform = "NESDIS: METEOSAT CLOUD-DRIFT HI-LEVEL"
    CASE (155); platform = "currently not used"
    CASE (256); platform = "NESDIS: GOES SAT VAS GRADIENT HIGH-LVL"
    CASE (156); platform = "LAND GOES-1 PREC. WATER RETRIEVAL-CLEAR"
    CASE (257); platform = "** currently not used **"
    CASE (157); platform = "LAND GOES-1 PREC. WATER RETRIEVAL-CLDY"
    CASE (258); platform = "NESDIS: METEOSAT WATER VAPOR HI-LEVEL"
    CASE (158); platform = "OCEAN GOES-1 PREC. WATER RETRIEVAL-CLEAR"
    CASE (259); platform = "NESDIS: GOES SAT WATER VAPOR HIGH-LVL"
    CASE (159); platform = " OCEAN GOES-1 PREC. WATER RETRIEVAL-CLDY"
    CASE (260); platform = "currently not used"      
    CASE (160); platform = "LAND TOVS SOUNDING TYPE UNKNOWN"
    CASE (261); platform = "currently not used"
    CASE (161); platform = "LAND TOVS SOUNDING CLEAR"
    CASE (262); platform = "currently not used"
    CASE (162); platform = "LAND TOVS SOUNDING NSTAR MASS"
    CASE (263); platform = "currently not used"
    CASE (163); platform = "LAND TOVS SOUNDING CLOUDY MASS"
    CASE (264); platform = "currently not used"
    CASE (164); platform = "LAND GOES-1 SOUNDINGS CLEAR MASS"
    CASE (265); platform = "currently not used"
    CASE (165); platform = "LAND GOES-1 SOUNDINGS CLOUDY MASS"
    CASE (269); platform = "currently not used"
    CASE (169); platform = "LAND DHSP SOUNDING MASS"
    CASE (270); platform = "CONSTANT-LEVEL BALLOON (PSEUDO) WIND"
    CASE (170); platform = "OCEAN TOVS SOUNDING TYPE UNKNOWN MASS"
    CASE (271); platform = "currently not used"
    CASE (171); platform = "OCEAN TOVS SOUNDING CLEAR MASS"
    CASE (272); platform = "currently not used WIND"
    CASE (172); platform = "OCEAN TOVS SOUNDING NSTAR MASS"
    CASE (273); platform = "currently not used"
    CASE (173); platform = "OCEAN TOVS SOUNDING CLOUDY MASS"
    CASE (274); platform = "currently not used"
    CASE (174); platform = "OCEAN GOES-1 SOUNDINGS CLEAR MASS"
    CASE (275); platform = "currently not used"
    CASE (175); platform = "OCEAN GOES-1 SOUNDINGS CLOUDY MASS"
    CASE (279); platform = "currently not used"
    CASE (179); platform = "OCEAN DMSP SOUNDING MASS"
    CASE (280); platform = "SURFACE MARINE WITH VALID STATION PRS. WIND"
    CASE (180); platform = "SURFACE MARINE WITH VALID STATION PRESSURE MASS"
    CASE (281); platform = "SURFACE LAND WITH VALID STATION PRS. WIND"
    CASE (181); platform = "SURFACE LAND WITH VALID STATION PRESSURE MASS"
    CASE (282); platform = "ATLAS BUOYS(SFC MARINE: MISSING STN. P) WIND"
    CASE (182); platform = "SPLASH-LVL DROPWINSONDE/RECCON/EST.MSLP MASS"
    CASE (283); platform = "SURFACE SSM/I WIND SPEED (DIRECTION=0)"
    CASE (183); platform = "SFC WITH MISSING STN PRESS & ELEV > 7.5M MASS)"
    CASE (284); platform = "SFC W/MISSING STN PRESS. & ELEV > 7.5M WINDS)"
    CASE (184); platform = "currently not used"
    CASE (286); platform = "SURFACE MARINE SCATTEROMETER (ERS1)"
    CASE (186); platform = "currently not used"
    CASE (290); platform = "currently not used"      
    CASE (190); platform = "OPC/NOS POINT MEAN SEA-LVL MASS"
    CASE (291); platform = "currently not used"
    CASE (191); platform = "AUSTRALIAN PAOBS (MSLP BOGUS) MASS"
    CASE (293); platform = "currently not used"
    CASE (193); platform = "DIGITIZED MEAN SEA-LEVEL PRESSURE BOGUS MASS"

    CASE DEFAULT

                platform = "unknown"

    END SELECT

 END FUNCTION oberr_list
!------------------------------------------------------------------------------!

 FUNCTION intplin (x,xx,yy) RESULT (val)
!------------------------------------------------------------------------------!
    IMPLICIT NONE

    REAL, DIMENSION (:) :: xx, yy
    REAL                :: x
    REAL                :: val

    INTEGER             :: n,m,jl
    CHARACTER*200 :: error_message
!------------------------------------------------------------------------------!
! Copyright UCAR (c) 2010.
! University Corporation for Atmospheric Research (UCAR),
! National Center for Atmospheric Research (NCAR),
! Research Applications Laboratory (RAL),
! P.O.Box 3000, Boulder, Colorado, 80307-3000, USA.
!
! Francois Vandenberghe, vandenb@ucar.edu, October 2010.
!------------------------------------------------------------------------------!

    n = size (xx)
    m = size (yy)

    IF (n .NE. m) THEN
        WRITE (error_message, '(A)' ) &
      " ERROR in intplin, arrays must have same size"
        CALL wrf_message (error_message)
        CALL ABORT
    ENDIF

    jl = locate (x,xx)

    IF (jl .LE. 0) THEN
        val = yy (1)
    ELSE IF (jl .GE. n) THEN
        val = yy (n)
    ELSE
        val = (xx (jl+1) - x) * yy (jl) + (x - xx (jl)) * yy (jl+1)
        val = val / (xx (jl+1) - xx (jl))
    ENDIF

 END FUNCTION intplin
!------------------------------------------------------------------------------!

 FUNCTION intplog (x,xx,yy) RESULT (val)
!------------------------------------------------------------------------------!
! Copyright UCAR (c) 2010.
! University Corporation for Atmospheric Research (UCAR),
! National Center for Atmospheric Research (NCAR),
! Research Applications Laboratory (RAL),
! P.O.Box 3000, Boulder, Colorado, 80307-3000, USA.
!
! Francois Vandenberghe, vandenb@ucar.edu, October 2010.
!------------------------------------------------------------------------------!
    IMPLICIT NONE

    REAL, DIMENSION (:) :: xx, yy
    REAL                :: x
    REAL                :: val

    INTEGER             :: n,m,jl
    CHARACTER*200 :: error_message
!------------------------------------------------------------------------------!

    n = size (xx)
    m = size (yy)

    IF (n .NE. m) THEN
        WRITE (error_message, '(A)' ) &
      " ERROR in intplog, arrays must have same size"
        CALL wrf_message (error_message)
        CALL ABORT
    ENDIF

    jl = locate (x,xx)

    IF (jl .LE. 0) THEN
        val = yy (1)
    ELSE IF (jl .GE. n) THEN
        val = yy (n)
    ELSE
        val = log (xx (jl+1) / x) * yy (jl) + log (x / xx (jl)) * yy (jl+1)
        val = val / log (xx (jl+1) / xx (jl))
    ENDIF

 END FUNCTION intplog
!------------------------------------------------------------------------------!

 FUNCTION locate (x,xx) RESULT (index)
!------------------------------------------------------------------------------!
    IMPLICIT NONE

    REAL, DIMENSION (:) :: xx
    REAL                :: x
    INTEGER             :: index

    INTEGER             :: n,jl,jm,ju
    LOGICAL             :: ascnd
!------------------------------------------------------------------------------!
! Copyright UCAR (c) 2010.
! University Corporation for Atmospheric Research (UCAR),
! National Center for Atmospheric Research (NCAR),
! Research Applications Laboratory (RAL),
! P.O.Box 3000, Boulder, Colorado, 80307-3000, USA.
!
! Francois Vandenberghe, vandenb@ucar.edu, October 2010.
!------------------------------------------------------------------------------!

    n = size (xx)
    ascnd = (xx (n) >= xx (1))   ! True if ascending order, false otherwise
    jl = 0                       ! Initialize lower limit
    ju = n+1                     ! Initialize upper limit

    DO

       IF (ju-jl <= 1) EXIT      ! Repeat until this condition is satisfied

       jm = (ju+jl) / 2.         ! Compute a mid point

       IF (ascnd .EQV. (x >= xx (jm))) THEN
           jl = jm               ! Replace mid point by lower limit
       ELSE
           ju = jm               ! Replace mid point by upper limit
       ENDIF

    ENDDO

    IF (x .EQ. xx (1)) THEN      ! Set the output, being carefull with endpoints
        index = 1
    ELSE IF (x .EQ. xx (n)) THEN
        index = n-1
    ELSE
        index = jl
    ENDIF

 END FUNCTION locate
!------------------------------------------------------------------------------!
 END MODULE module_observ_error_ncep

!------------------------------------------------------------------------------!
!
!                TABLE OF IDENTIFICATION OF REPORT TYPES USED
!
!                          CDAS/REANALYSIS SYSTEMS
!
!                                  X = Used
!
! Click on Report ID to view table of observation error profiles
! follow this link to the Forecast Error profiles
!
!        WINDS                           MASS
!
!  202    currently not used       102    SSM/I 7-CHANNEL BRIGHTNESS
!                                         TEMPERATURE
!  210    SYNTHETIC TROPICAL       110    UPPER-AIR HEIGHT BOGUS
!         CYCLONE WINDS
!  220  X RAWINSONDE               120  X RAWINSONDE
!  221  X PIBAL WINDS              121    currently not used
!  222    CLASS SOUNDING           122    CLASS SOUNDING
!  223    PROFILER WINDS           123    currently not used
!  224    VAD (NEXRAD) WINDS       124    currently not used
!  225    NEXRAD RADIAL WINDS      125    currently not used
!  230  X AIREP/PIREP AIRCRAFT     130  X AIREP/PIREP AIRCRAFT FLIGHT-LEVEL
!         FLIGHT-LEVEL
!  231  X ASDAR AIRCRAFT           131  X ASDAR AIRCRAFT FLIGHT-LEVEL
!         FLIGHT-LEVEL
!  232  X RECONNAISSANCE:          132  X RECONNAISSANCE: DROPWINSONDES
!         DROPWINSONDES
!  233  X ACARS AIRCRAFT           133  X ACARS AIRCRAFT FLIGHT-LEVEL
!         FLIGHT-LEVEL
!  240  X NESDIS/NWS: GOES-ODD     140    NESDIS/NWS:GOES-ODD SAT
!         CLD-DRFT LOW-LVL                CLOUD-DRIFT
!  241    INDIA:INSAT CLOUD - DRFT 141    INDIA:INSAT SATELLITE CLOUD-DRIFT
!         L0W-LVL
!  242  X JAPAN: GMS SAT           142    JAPAN: GMS SATELLITE CLOUD-DRIFT
!         CLOUD-DRIFT LOW-LVL
!  243  X ESA: METEOSAT SAT        143    ESA: METEOSAT SATELLITE
!         CLD-DRIFT LOW-LVL               CLOUD-DRIFT
!  244  X NESDIS/NWS: GOES-EVN     144    NESDIS/NWS:GOES-EVEN SAT
!         CLD-DRFT LOW-LVL                CLOUD-DRIFT
!  245    NESDIS: METEOSAT SAT     145    NESDIS:METEOSAT SATELLITE
!         CLD-DRIFT LOW-LVL               CLOUD-DRIFT
!  246    NESDIS: GOES SAT VAS     146    NESDIS: GOES SATELLITE VAS
!         GRADIENT LOW-LVL                GRADIENT
!  248    NESDIS: METEOSAT WATER   148    NESDIS:METEOSAT SATELLITE WATER
!         VAPOR LOW-LVL                   VAPOR
!  249    NESDIS: GOES SAT WATER   149    NESDIS: GOES SATELLITE WATER VAPOR
!         VAPOR LOW-LVL
!  250  X NESDIS/NWS: GOES-ODD     150    SATELLITE MOISTURE BOGUS
!         CLD-DRIFT HI-LVL
!  251    IND1A INSAT  CLOUD-DRIFT 151    RAOB PRECIPITABLE
!         HI-LVL                          WATER(CALCULATED)
!  252  X JAPAN GMS SAT CLOUD -    152    SSM/I PRECIPITABLEWATER RETRIEVAL
!         DRIFT HI-LEVEL
!  253  X ESA : METEOSAT           153    TOVS (NMCEDS) PRECIP. WATER
!         CLOUD-DRFT HI-LVL               RETRIEVAL
!  254  X NESDIS/NWS : GOES-EVN    154    currently not used
!         CLD-DRIFT HI-LVL
!  255    NESDIS: METEOSAT         155    currentlynot used
!         CLOUD-DRIFT HI-LEVEL
!  256    NESDIS: GOES SAT VAS     156    LAND GOES-1 PREC. WATER
!         GRADIENT HIGH-LVL               RETRIEVAL-CLEAR
!  257    ** currently not used    157    LAND GOES-1 PREC. WATER
!         **                              RETRIEVAL-CLDY
!  258    NESDIS: METEOSAT WATER   158    OCEAN GOES-1 PREC. WATER
!         VAPOR HI-LEVEL                  RETRIEVAL-CLEAR
!  259    NESDIS: GOES SAT WATER   159    OCEAN GOES-1 PREC. WATER
!         VAPOR HIGH-LVL                  RETRIEVAL-CLDY
!  260    currently not used       160    LAND TOVS SOUNDING TYPE UNKNOWN
!  261    currently not used       161  X LAND TOVS SOUNDING CLEAR
!  262    currently not used       162  X LAND TOVS SOUNDING NSTAR
!  263    currently not used       163  X LAND TOVS SOUNDING CLOUDY
!  264    currently not used       164    LAND GOES-1 SOUNDINGS CLEAR
!  265    currently not used       165    LAND GOES-1 SOUNDINGS CLOUDY
!  269    currently not used       169    LAND DHSP SOUNDING
!  270  X CONSTANT-LEVEL BALLOON   170    OCEAN TOVS SOUNDING TYPE UNKNOWN
!         (PSEUDO)
!  271    currently not used       171  X OCEAN TOVS SOUNDING CLEAR
!  272    currently not used       172  X OCEAN TOVS SOUNDING NSTAR
!  273    currently not used       173  X OCEAN TOVS SOUNDING CLOUDY
!  274    currently not used       174    OCEAN GOES-1 SOUNDINGS CLEAR
!  275    currently not used       175    OCEAN GOES-1 SOUNDINGS CLOUDY
!  279    currently not used       179    OCEAN DMSP SOUNDING
!  280  X SURFACE MARINE WITH      180  X SURFACE MARINE WITH VALID STATION
!         VALID STATION PRS.              PRESSURE
!  281    SURFACE LAND WITH VALID  181  X SURFACE LAND WITH VALID STATION
!         STATION PRS.                    PRESSURE
!  282  X ATLAS BUOYS(SFC MARINE:  182  X SPLASH-LVL
!         MISSING STN. P)                 DROPWINSONDE/RECCON/EST. MSLP
!  283    SURFACE SSM/I WIND SPEED 183    SFC WITH MISSING STN PRESS & ELEV
!         (DIRECTION=0)                   >7.5 M
!  284    SFC W/MISSING STN PRESS. 184    currently not used
!         & ELEV > 7.5M
!  286    SURFACE MARINE           186    currently not used
!         SCATTEROMETER (ERS1)
!  290    currently not used       190    OPC/NOS POINT MEAN SEA-LVL
!  291    currently not used       191  X AUSTRALIAN PAOBS (MSLP BOGUS)
!  293    currently not used       193    DIGITIZED MEAN SEA-LEVEL PRESSURE
!                                         BOGUS
!------------------------------------------------------------------------------!
