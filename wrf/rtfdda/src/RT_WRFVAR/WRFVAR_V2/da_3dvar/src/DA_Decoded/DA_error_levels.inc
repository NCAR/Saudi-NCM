
 SUBROUTINE error_levels (err_levels, Coord_mb, Temp_K, Sp_Hum_gkg, &
                          RH_water, uv_ms, P_mb, H_m, error_type, ldebug)
!------------------------------------------------------------------------------!
!
! Load error profiles based on the content of the string 
! error_type = 'avnfct'        => AVN forecast  
!            = 'avnobs'        => AVN observational
!            = 'etaobs'        => ETA observational
!            = 'avnobs+avnfct' => AVN innovation
!            = 'etaobs+avnfct' => AVN/ETA innovation 
!
! Errors are loaded on 33 pressure levels according to the instrumental
! platform code defined at: 
!
! http://wwwt.emc.ncep.noaa.gov/gmb/bkistler/oberr/reanl-obs.html
!
! Francois Vandenberghe, January 2004
!------------------------------------------------------------------------------!
  IMPLICIT NONE

! ARGUMENTS

  INTEGER, INTENT (in)  :: err_levels

  REAL, DIMENSION      (err_levels), INTENT (out) :: Coord_mb
  REAL, DIMENSION (0:99,err_levels), INTENT (out) :: Temp_K, Sp_Hum_gkg, &
                                                     RH_water, uv_ms, P_mb, H_m
  CHARACTER (LEN=80), INTENT (in)  :: error_type
  LOGICAL,            INTENT (in)  :: ldebug

!------------------------------------------------------------------------------!
! OBSERVATIONAL ERROR PROFILES

  REAL, DIMENSION (:,:), ALLOCATABLE :: Temp_K_fct,   Sp_Hum_gkg_fct, &
                                        uv_ms_fct,    P_mb_fct,       &
                                        H_m_fct

  REAL, DIMENSION (:,:), ALLOCATABLE :: Temp_K_obs,   Sp_Hum_gkg_obs, &
                                        uv_ms_obs,    P_mb_obs,       &
                                        H_m_obs

  REAL,DIMENSION (:), ALLOCATABLE :: ptmp
!------------------------------------------------------------------------------!
! OTHER VARIABLES

  REAL    :: www, xxx, yyy, zzz

  INTEGER :: iret
  INTEGER :: i, k, l
  INTEGER :: iunit, unit_err, unit_in
  INTEGER, DIMENSION (1) :: iii

  CHARACTER (LEN =  80) :: file_in
  CHARACTER (LEN =  80) :: line80
  CHARACTER (LEN = 100) :: error_message

! PARAMETERS

  REAL,    PARAMETER :: missing_read = 1.E+09

  CHARACTER (LEN =  32), PARAMETER :: proc_name = 'error_levels '

!------------------------------------------------------------------------------!

!     CALL prelog
!     WRITE  (log_unit,'(A)') &
!    "-----------------------------------------------------------------------"
      CALL prelog
      WRITE  (log_unit,'(A)') "Load forecast/observational errors:"

! 1.  INITIALIZATION
! ==================

! 1.1 Arguments
!     ---------

      IF (INDEX (error_type,"AVNFCT",.FALSE.) <= 0  .AND. &
          INDEX (error_type,"avnfct",.FALSE.) <= 0  .AND. &
          INDEX (error_type,"AVNOBS",.FALSE.) <= 0  .AND. &
          INDEX (error_type,"avnobs",.FALSE.) <= 0  .AND. &
          INDEX (error_type,"ETAOBS",.FALSE.) <= 0  .AND. &
          INDEX (error_type,"etaobs",.FALSE.) <= 0) THEN
          error_message = "Unknown error type argument "//TRIM (error_type)
          CALL error_handler (log_unit, 1, &
               proc_name//error_message, .TRUE., .TRUE.)
       ENDIF 

! 1.2 Allocate space
!     -------------
 
      ALLOCATE (Temp_K_obs   (0:99,err_levels),Sp_Hum_gkg_obs (0:99,err_levels))
      ALLOCATE (uv_ms_obs    (0:99,err_levels),P_mb_obs       (0:99,err_levels))

      ALLOCATE (Temp_K_fct   (0:99,err_levels),Sp_Hum_gkg_fct (0:99,err_levels))
      ALLOCATE (uv_ms_fct    (0:99,err_levels),P_mb_fct       (0:99,err_levels))

      ALLOCATE (ptmp (err_levels))

! 1.3 Reset arrays
!     -----------

      Temp_K_obs   = 0.; Sp_Hum_gkg_obs = 0.; uv_ms_obs = 0.; P_mb_obs   = 0.; 

      Temp_K_fct   = 0.; Sp_Hum_gkg_fct = 0.; uv_ms_fct = 0.; P_mb_fct   = 0.; 

      Temp_K    = 0.; Sp_Hum_gkg = 0.; uv_ms = 0.; P_mb = 0.; 
      RH_water  = 0.; H_m = 0.;

      ptmp = 0.;


! 2.  LOAD AVN FORECAST ERROR ON PRESSURE LEVELS
! ==============================================
 
avnfct:&
      IF (INDEX (error_type,"AVNFCT",.FALSE.) > 0 .OR. &
          INDEX (error_type,"avnfct",.FALSE.) > 0) THEN

! 2.1 Open file
!     ---------

      file_in = "errtable_avnfct.r3dv.txt"
      unit_in = 99

      OPEN (UNIT = unit_in, FILE = file_in, FORM = 'formatted', &
           ACTION = 'read', STATUS = 'old', IOSTAT = iret)

! 2.2 Use default values if file is missing
!     -------------------------------------

      IF (iret /= 0) THEN
          error_message = "Cannot find file: " // TRIM (file_in)
          CALL error_handler (log_unit, 2, proc_name//error_message, &
                             .TRUE., .TRUE.)
      ENDIF

! 2.3 Print details
!     -------------

      CALL prelog
      WRITE  (log_unit,'(2A)') &
      "Read forecast error profiles in file: ",TRIM (file_in)

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

             DO k = 1, err_levels
                l = l + 1

                READ (UNIT=unit_in,FMT='(1X,6E12.5)',IOSTAT=iret) &
                      Coord_mb (k), www,                          &
                      xxx, uv_ms_fct (unit_err,k),                &
                      yyy, zzz

                IF (iret /= 0) EXIT
             ENDDO

         ELSE IF (iunit >= 100) THEN

             unit_err = iunit - 100

             DO k = 1, err_levels
                l = l + 1

                READ (UNIT=unit_in,FMT='(1X,6E12.5)',IOSTAT=iret) &
                      Coord_mb (k), Temp_K_fct (unit_err,k),      &
                      Sp_Hum_gkg_fct (unit_err, k), xxx,          &
                      P_mb_fct (unit_err,k), yyy
                      
                IF (iret /= 0) EXIT
             ENDDO

         ELSE

             EXIT

         ENDIF
                       
! 2.8 If there is an read error, abort
!     --------------------------------

         IF (iret > 0) THEN
             WRITE  (error_message,'(3A,I5)') "Error reading file: ", &
 	                                       TRIM (file_in)," at line: ", l
             CALL error_handler (log_unit, 3, proc_name//error_message, &
	                        .TRUE., .TRUE.)
         ENDIF

! 2.9 Extrapolate pressure to higher level
!     ------------------------------------

         ptmp = P_mb_fct (unit_err,:) - missing_read

         iii = MAXLOC (ptmp) 

         IF (iii  (1) <= 1) THEN
             P_mb_fct (unit_err,1) =  1. ! hPa error
             iii  (1) =  2
         ENDIF

         iii (1) = iii (1) - 1

         WHERE (P_mb_fct (unit_err,:) >= missing_read) &
                P_mb_fct (unit_err,:)  = P_mb_fct (unit_err, iii (1))

      ENDDO

! 2.10 Make sure moisture is not too small
!      -----------------------------------

      WHERE (Sp_Hum_gkg_fct  < 0.1) Sp_Hum_gkg_fct = 0.1

! 2.11 Make sure pressure is not too small
!      -----------------------------------

      WHERE (P_mb_fct  < 1.) P_mb_fct = 1.

! 2.12 Make sure pressure is not too big
!      ---------------------------------

      WHERE (P_mb_fct  > 1.) P_mb_fct = 1.


! 2.13 Close input file
!      ----------------

      CLOSE (UNIT=unit_in)

      ENDIF avnfct


! 3.  LOAD AVN OBSERVATIONAL ERROR ON PRESSURE LEVELS
! ===================================================
 
avnobs:&
      IF (INDEX (error_type,"AVNOBS",.FALSE.) > 0 .OR. &
          INDEX (error_type,"avnobs",.FALSE.) > 0) THEN

! 3.1 Open file
!     ---------

      file_in = "errtable_avnobs.r3dv.txt"
      unit_in = 99

      OPEN (UNIT = unit_in, FILE = file_in, FORM = 'formatted', &
           ACTION = 'read', STATUS = 'old', IOSTAT = iret)

! 3.2 Use default values if file is missing
!     -------------------------------------

      IF (iret /= 0) THEN
          error_message = "Cannot find file: " // TRIM (file_in)
          CALL error_handler (log_unit, 2, proc_name//error_message, &
                             .TRUE., .TRUE.)
      ENDIF

! 3.3 Print details
!     -------------

      CALL prelog
      WRITE  (log_unit,'(2A)') &
      "Read observation error profiles in file: ",TRIM (file_in)

      l = 0

! 3.4 Start reading the file
!     ----------------------

      DO WHILE (iret == 0)

! 3.5 Read the header 
!     ---------------

         l = l + 1

         READ (UNIT=unit_in,FMT='(A)',IOSTAT=iret) line80

         IF (iret /= 0) EXIT

! 3.6 grab the platform code
!     ----------------------

         l = l + 1

         READ (line80 (2:4),'(I3)',IOSTAT=iret) iunit

         IF (iret /= 0) EXIT

! 3.7 Grab the data for the corresponding platform (from bottom to top)
!     --------------------------------------------

         IF (iunit >= 200) THEN

             unit_err = iunit - 200

             DO k = 1, err_levels
                l = l + 1

                READ (UNIT=unit_in,FMT='(1X,6E12.5)',IOSTAT=iret) &
                      Coord_mb (k), www,                          &
                      xxx, uv_ms_obs (unit_err,k),                &
                      yyy, zzz

                IF (iret /= 0) EXIT
             ENDDO

         ELSE IF (iunit >= 100) THEN

             unit_err = iunit - 100

             DO k = 1, err_levels
                l = l + 1

                READ (UNIT=unit_in,FMT='(1X,6E12.5)',IOSTAT=iret) &
                      Coord_mb (k), Temp_K_obs (unit_err,k),      &
                      Sp_Hum_gkg_obs (unit_err, k), xxx,          &
                      P_mb_obs (unit_err,k), yyy
                      
                IF (iret /= 0) EXIT
             ENDDO

         ELSE

             EXIT

         ENDIF
                       
! 3.8 If there is an read error, abort
!     --------------------------------

         IF (iret > 0) THEN
             WRITE  (error_message,'(3A,I5)') "Error reading file: ", &
 	                                       TRIM (file_in)," at line: ", l
             CALL error_handler (log_unit, 3, proc_name//error_message, &
	                        .TRUE., .TRUE.)
         ENDIF

! 3.9 Extrapolate pressure to higher level
!     ------------------------------------

         ptmp = P_mb_obs (unit_err,:) - missing_read

         iii = MAXLOC (ptmp) 

         IF (iii  (1) <= 1) THEN
             P_mb_obs (unit_err,1) =  1. ! hPa error
             iii  (1) =  2
         ENDIF

         iii (1) = iii (1) - 1

         WHERE (P_mb_obs (unit_err,:) >= missing_read) &
                P_mb_obs (unit_err,:)  = P_mb_obs (unit_err, iii (1))

      ENDDO

! 3.10 Make sure moisture is not too small
!      -----------------------------------

      WHERE (Sp_Hum_gkg_obs  < 0.1) Sp_Hum_gkg_obs = 0.1

! 3.11 Make sure pressure is not too small
!      -----------------------------------

      WHERE (P_mb_obs  < 1.) P_mb_obs = 1.

! 3.12 Make sure pressure is not too big
!      ---------------------------------

      WHERE (P_mb_obs  > 1.) P_mb_obs = 1.


! 3.13 Close input file
!      ----------------

      CLOSE (UNIT=unit_in)

      ENDIF avnobs


! 4.  LOAD OBSERVATIONAL ERROR ON PRESSURE LEVELS
! ================================================
 
etaobs:&
      IF (INDEX (error_type,"ETAOBS",.FALSE.) > 0 .OR. &
          INDEX (error_type,"etaobs",.FALSE.) > 0) THEN

! 4.1 Open file
!     ---------

      file_in = "errtable_etaobs.r3dv.txt"
      unit_in = 99

      OPEN (UNIT = unit_in, FILE = file_in, FORM = 'formatted', &
                ACTION = 'read', STATUS = 'old', IOSTAT = iret)

! 4.2 Use default values if file is missing
!     -------------------------------------

      IF (iret /= 0) THEN
          error_message = "Cannot find file: " // TRIM (file_in)
          CALL error_handler (log_unit, 2, proc_name//error_message, &
                              .TRUE., .TRUE.)
      ENDIF

! 4.3 Print details
!     -------------

      CALL prelog
      WRITE  (log_unit,'(2A)') &
      "Read observation error profiles in file: ",TRIM (file_in)

      l = 0

! 4.4 Start reading the file
!     ----------------------

      DO WHILE (iret == 0)

! 4.5 Read the header 
!     ---------------

         l = l + 1

         READ (UNIT=unit_in,FMT='(A)',IOSTAT=iret) line80

         IF (iret /= 0) EXIT

! 4.6 grab the platform code
!     ----------------------

         l = l + 1

         READ (line80 (2:4),'(I3)',IOSTAT=iret) iunit

         IF (iret /= 0) EXIT

! 4.7 Grab the data for the corresponding platform (from bottom to top)
!     --------------------------------------------

         IF (iunit >= 200) THEN

             unit_err = iunit - 200

             DO k = 1, err_levels
                l = l + 1

                READ (UNIT=unit_in,FMT='(1X,6E12.5)',IOSTAT=iret) &
                      Coord_mb (k), www,                          &
                      xxx, uv_ms_obs (unit_err,k),                &
                      yyy, zzz

                IF (iret /= 0) EXIT
             ENDDO

         ELSE IF (iunit >= 100) THEN

             unit_err = iunit - 100

             DO k = 1, err_levels
                l = l + 1

                READ (UNIT=unit_in,FMT='(1X,6E12.5)',IOSTAT=iret) &
                      Coord_mb (k), Temp_K_obs (unit_err,k),      &
                      Sp_Hum_gkg_obs (unit_err, k), xxx,          &
                      P_mb_obs (unit_err,k), yyy
                      
                IF (iret /= 0) EXIT
             ENDDO

         ELSE
             EXIT
         ENDIF
                       
! 4.8 If there is an read error, abort
!     --------------------------------

         IF (iret > 0) THEN
             WRITE  (log_unit,'(3A,I5)') "Error reading file: ",TRIM (file_in),&
                                         " at line: ", l
             CALL error_handler (log_unit, 5, proc_name//error_message, &
	                        .TRUE., .TRUE.)
         ENDIF

! 4.9 Extrapol pressure to higher level
!     ---------------------------------

         ptmp = P_mb_obs (unit_err,:) - missing_read

         iii = MAXLOC (ptmp) 

         IF (iii  (1) <= 1) THEN
             P_mb_obs (unit_err,1) =  1. ! hPa error
             iii  (1) =  2
         ENDIF

         iii (1) = iii (1) - 1

         WHERE (P_mb_obs (unit_err,:) >= missing_read) &
                P_mb_obs (unit_err,:)  = P_mb_obs (unit_err, iii (1))

      ENDDO

! 4.10 Make sure moisture is not too small
!     -----------------------------------

      WHERE (Sp_Hum_gkg_obs  < 0.1) Sp_Hum_gkg_obs = 0.1

! 4.11 Make sure pressure is not too small
!     -----------------------------------

      WHERE (P_mb_obs  < 1.) P_mb_obs = 1.

! 4.12 Make sure pressure is not too big
!     ---------------------------------

      WHERE (P_mb_obs  > 1.) P_mb_obs = 1.

! 4.13 Close input file
!     ----------------

      CLOSE (UNIT=unit_in)

      ENDIF etaobs


! 5.  OBS HEIGHT AND RELATIVE HUMIDITY ERROR SAME FOR ALL
! =======================================================

! 5.1 Get upper-air height error
!     --------------------------

       CALL err_height_upr (err_levels, Coord_mb, H_m (20,:))

       DO i = 0, 99
          H_m (i,:)  =  H_m (20,:)
       ENDDO

! 5.2 Get surface height error
!     ------------------------

       CALL err_height_sfc (err_levels, Coord_mb, H_m (81,:))

! 5.3 Get upper-air height RH
!     -----------------------

       CALL err_rh_upr (err_levels, Coord_mb, RH_water (20,:))

       DO i = 0, 99
          RH_water (i,:) = RH_water (20,:)
       ENDDO

! 5.4 Get surface height error
!     ------------------------

       CALL err_rh_sfc (err_levels, Coord_mb, RH_water (81,:))


! 6.  OUTPUT FORECAST, OBSERVER. OR INNOVATION ERROR ON PRESSURE LEVELS
! =====================================================================

      Temp_K     = SQRT (Temp_K_fct**2     + Temp_K_obs**2)
      uv_ms      = SQRT (uv_ms_fct**2      + uv_ms_obs**2)
      Sp_Hum_gkg = SQRT (Sp_Hum_gkg_fct**2 + Sp_Hum_gkg_obs**2)
      P_mb       = SQRT (P_mb_fct**2       + P_mb_obs**2)

! 7.  END
! =======

! 7.1 Free memory
!     -----------

      DEALLOCATE (Temp_K_obs,   Sp_Hum_gkg_obs )
      DEALLOCATE (uv_ms_obs,    P_mb_obs )

      DEALLOCATE (Temp_K_fct,   Sp_Hum_gkg_fct )
      DEALLOCATE (uv_ms_fct,    P_mb_fct )

      DEALLOCATE (ptmp)

 END SUBROUTINE error_levels
!------------------------------------------------------------------------------!

 SUBROUTINE err_height_upr (err_levels, Coord_mb, H_m)
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
  IMPLICIT NONE

  INTEGER, INTENT (in) :: err_levels
  REAL,    DIMENSION (err_levels), INTENT (out) :: Coord_mb, H_m

  INTEGER :: k
  CHARACTER (LEN =  32), PARAMETER :: proc_name = 'err_height_upr'
  CHARACTER (LEN = 100) :: error_message
!------------------------------------------------------------------------------!

! 1.  CHECK DIMENSIONS
! ====================
 
      IF (err_levels /= 33) THEN
          error_message = "Error in dimensions, err_levels must be 33."
          CALL error_handler (log_unit, err_levels, proc_name//error_message, &
	                      .TRUE.,.TRUE.)
      ENDIF

! 2.  EIGHT ERRORS
! ================

      k=33; Coord_mb (k) =   0.; H_m (k) = 40.0; 
      k=32; Coord_mb (k) =   1.; H_m (k) = 40.0; 
      k=31; Coord_mb (k) =   2.; H_m (k) = 40.0; 
      k=30; Coord_mb (k) =   3.; H_m (k) = 40.0; 
      k=29; Coord_mb (k) =   4.; H_m (k) = 40.0; 
      k=28; Coord_mb (k) =   5.; H_m (k) = 40.0; 
      k=27; Coord_mb (k) =  10.; H_m (k) = 40.0; 
      k=26; Coord_mb (k) =  20.; H_m (k) = 32.0; 
      k=25; Coord_mb (k) =  30.; H_m (k) = 25.0; 
      k=24; Coord_mb (k) =  40.; H_m (k) = 23.7; 
      k=23; Coord_mb (k) =  50.; H_m (k) = 22.5; 
      k=22; Coord_mb (k) =  75.; H_m (k) = 19.3; 
      k=21; Coord_mb (k) = 100.; H_m (k) = 18.1; 
      k=20; Coord_mb (k) = 150.; H_m (k) = 15.2; 
      k=19; Coord_mb (k) = 200.; H_m (k) = 13.2; 
      k=18; Coord_mb (k) = 250.; H_m (k) = 11.8; 
      k=17; Coord_mb (k) = 300.; H_m (k) = 10.7; 
      k=16; Coord_mb (k) = 350.; H_m (k) = 10.2; 
      k=15; Coord_mb (k) = 400.; H_m (k) =  9.8; 
      k=14; Coord_mb (k) = 450.; H_m (k) =  9.1; 
      k=13; Coord_mb (k) = 500.; H_m (k) =  8.4; 
      k=12; Coord_mb (k) = 550.; H_m (k) =  7.5; 
      k=11; Coord_mb (k) = 600.; H_m (k) =  6.6; 
      k=10; Coord_mb (k) = 650.; H_m (k) =  5.9; 
      k= 9; Coord_mb (k) = 700.; H_m (k) =  5.2; 
      k= 8; Coord_mb (k) = 750.; H_m (k) =  4.9; 
      k= 7; Coord_mb (k) = 800.; H_m (k) =  4.7; 
      k= 6; Coord_mb (k) = 850.; H_m (k) =  4.4; 
      k= 5; Coord_mb (k) = 900.; H_m (k) =  4.4; 
      k= 4; Coord_mb (k) = 950.; H_m (k) =  4.3; 
      k= 3; Coord_mb (k) =1000.; H_m (k) =  4.3; 
      k= 2; Coord_mb (k) =1050.; H_m (k) =  4.3; 
      k= 1; Coord_mb (k) =1100.; H_m (k) =  4.3; 

! 3. END
! =======

   RETURN
   END SUBROUTINE err_height_upr
!------------------------------------------------------------------------------!

 SUBROUTINE err_height_sfc (err_levels, Coord_mb, H_m)
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
  IMPLICIT NONE

  INTEGER, INTENT (in) :: err_levels
  REAL,    DIMENSION (err_levels), INTENT (out) :: Coord_mb, H_m

  INTEGER :: k
  CHARACTER (LEN =  32), PARAMETER :: proc_name = 'err_height_sfc'
  CHARACTER (LEN = 100) :: error_message
!------------------------------------------------------------------------------!

! 1.  CHECK DIMENSIONS
! ====================
 
      IF (err_levels /= 33) THEN
          error_message = "Error in dimensions, err_levels must be 33."
          CALL error_handler (log_unit, err_levels, proc_name//error_message,&
	                      .TRUE.,.TRUE.)
      ENDIF

! 2.  EIGHT ERRORS
! ================

      k=33; Coord_mb (k) =   0.; H_m (k) =  4.3; 
      k=32; Coord_mb (k) =   1.; H_m (k) =  4.3; 
      k=31; Coord_mb (k) =   2.; H_m (k) =  4.3; 
      k=30; Coord_mb (k) =   3.; H_m (k) =  4.3; 
      k=29; Coord_mb (k) =   4.; H_m (k) =  4.3; 
      k=28; Coord_mb (k) =   5.; H_m (k) =  4.3; 
      k=27; Coord_mb (k) =  10.; H_m (k) =  4.3; 
      k=26; Coord_mb (k) =  20.; H_m (k) =  4.3; 
      k=25; Coord_mb (k) =  30.; H_m (k) =  4.3; 
      k=24; Coord_mb (k) =  40.; H_m (k) =  4.3; 
      k=23; Coord_mb (k) =  50.; H_m (k) =  4.3; 
      k=22; Coord_mb (k) =  75.; H_m (k) =  4.3; 
      k=21; Coord_mb (k) = 100.; H_m (k) =  4.3; 
      k=20; Coord_mb (k) = 150.; H_m (k) =  4.3; 
      k=19; Coord_mb (k) = 200.; H_m (k) =  4.3; 
      k=18; Coord_mb (k) = 250.; H_m (k) =  4.3; 
      k=17; Coord_mb (k) = 300.; H_m (k) =  4.3; 
      k=16; Coord_mb (k) = 350.; H_m (k) =  4.3; 
      k=15; Coord_mb (k) = 400.; H_m (k) =  4.3; 
      k=14; Coord_mb (k) = 450.; H_m (k) =  4.3; 
      k=13; Coord_mb (k) = 500.; H_m (k) =  4.3; 
      k=12; Coord_mb (k) = 550.; H_m (k) =  4.3; 
      k=11; Coord_mb (k) = 600.; H_m (k) =  4.3; 
      k=10; Coord_mb (k) = 650.; H_m (k) =  4.3; 
      k= 9; Coord_mb (k) = 700.; H_m (k) =  4.3; 
      k= 8; Coord_mb (k) = 750.; H_m (k) =  4.3; 
      k= 7; Coord_mb (k) = 800.; H_m (k) =  4.3; 
      k= 6; Coord_mb (k) = 850.; H_m (k) =  4.3; 
      k= 5; Coord_mb (k) = 900.; H_m (k) =  4.3; 
      k= 4; Coord_mb (k) = 950.; H_m (k) =  4.3; 
      k= 3; Coord_mb (k) =1000.; H_m (k) =  4.3; 
      k= 2; Coord_mb (k) =1050.; H_m (k) =  4.3; 
      k= 1; Coord_mb (k) =1100.; H_m (k) =  4.3; 

! 3. END
! =======

   RETURN
   END SUBROUTINE err_height_sfc
!------------------------------------------------------------------------------!

 SUBROUTINE err_rh_upr (err_levels, Coord_mb, RH_water)
!------------------------------------------------------------------------------!
! 
! Sounding relative humidity observational error on the 33 NCEP observational 
! error pressure levels.
!
! Output is pressure levels in hPa and rh error with respect to water in % 
! from bottom to top.
!
!------------------------------------------------------------------------------!
  IMPLICIT NONE

  INTEGER, INTENT (in) :: err_levels
  REAL,    DIMENSION (err_levels), INTENT (out) :: Coord_mb, RH_water

  INTEGER :: k
  CHARACTER (LEN =  32), PARAMETER :: proc_name = 'err_rh_upr'
  CHARACTER (LEN = 100) :: error_message
!------------------------------------------------------------------------------!

! 1.  CHECK DIMENSIONS
! ====================
 
      IF (err_levels /= 33) THEN
          error_message = "Error in dimensions, err_levels must be 33."
          CALL error_handler (log_unit, err_levels, proc_name//error_message,&
	                     .TRUE.,.TRUE.)
      ENDIF

! 2.  EIGHT ERRORS
! ================

      k=33; Coord_mb (k) =   0.; RH_water (k) = 40.0;
      k=32; Coord_mb (k) =   1.; RH_water (k) = 40.0;
      k=31; Coord_mb (k) =   2.; RH_water (k) = 40.0;
      k=30; Coord_mb (k) =   3.; RH_water (k) = 40.0;
      k=29; Coord_mb (k) =   4.; RH_water (k) = 40.0;
      k=28; Coord_mb (k) =   5.; RH_water (k) = 40.0;
      k=27; Coord_mb (k) =  10.; RH_water (k) = 40.0;
      k=26; Coord_mb (k) =  20.; RH_water (k) = 40.0;
      k=25; Coord_mb (k) =  30.; RH_water (k) = 40.0;
      k=24; Coord_mb (k) =  40.; RH_water (k) = 40.0;
      k=23; Coord_mb (k) =  50.; RH_water (k) = 40.0;
      k=22; Coord_mb (k) =  75.; RH_water (k) = 40.0;
      k=21; Coord_mb (k) = 100.; RH_water (k) = 40.0;
      k=20; Coord_mb (k) = 150.; RH_water (k) = 40.0;
      k=19; Coord_mb (k) = 200.; RH_water (k) = 40.0;
      k=18; Coord_mb (k) = 250.; RH_water (k) = 40.0;
      k=17; Coord_mb (k) = 300.; RH_water (k) = 40.0;
      k=16; Coord_mb (k) = 350.; RH_water (k) = 40.0;
      k=15; Coord_mb (k) = 400.; RH_water (k) = 40.0;
      k=14; Coord_mb (k) = 450.; RH_water (k) = 40.0;
      k=13; Coord_mb (k) = 500.; RH_water (k) = 40.0;
      k=12; Coord_mb (k) = 550.; RH_water (k) = 40.0;
      k=11; Coord_mb (k) = 600.; RH_water (k) = 40.0;
      k=10; Coord_mb (k) = 650.; RH_water (k) = 40.0;
      k= 9; Coord_mb (k) = 700.; RH_water (k) = 40.0;
      k= 8; Coord_mb (k) = 750.; RH_water (k) = 30.0;
      k= 7; Coord_mb (k) = 800.; RH_water (k) = 30.0;
      k= 6; Coord_mb (k) = 850.; RH_water (k) = 30.0;
      k= 5; Coord_mb (k) = 900.; RH_water (k) = 30.0;
      k= 4; Coord_mb (k) = 950.; RH_water (k) = 30.0;
      k= 3; Coord_mb (k) =1000.; RH_water (k) = 20.0;
      k= 2; Coord_mb (k) =1050.; RH_water (k) = 20.0;
      k= 1; Coord_mb (k) =1100.; RH_water (k) = 20.0;

! 3. END
! =======

   RETURN
   END SUBROUTINE err_rh_upr
!------------------------------------------------------------------------------!

 SUBROUTINE err_rh_sfc (err_levels, Coord_mb, RH_water)
!------------------------------------------------------------------------------!
! 
! Surface Relative humidity observational error on the 33 NCEP observational 
! error pressure levels.
!
! Output is pressure levels in hPa and rh error with respect to water in % 
! from bottom to top.
!
!------------------------------------------------------------------------------!
  IMPLICIT NONE

  INTEGER, INTENT (in) :: err_levels
  REAL,    DIMENSION (err_levels), INTENT (out) :: Coord_mb, RH_water

  INTEGER :: k
  CHARACTER (LEN =  32), PARAMETER :: proc_name = 'err_rh_sfc'
  CHARACTER (LEN = 100) :: error_message
!------------------------------------------------------------------------------!

! 1.  CHECK DIMENSIONS
! ====================
 
      IF (err_levels /= 33) THEN
          error_message = "Error in dimensions, err_levels must be 33."
          CALL error_handler (log_unit, err_levels, proc_name//error_message,&
	                     .TRUE.,.TRUE.)
      ENDIF

! 2.  EIGHT ERRORS
! ================

      k=33; Coord_mb (k) =   0.; RH_water (k) = 20.0;
      k=32; Coord_mb (k) =   1.; RH_water (k) = 20.0;
      k=31; Coord_mb (k) =   2.; RH_water (k) = 20.0;
      k=30; Coord_mb (k) =   3.; RH_water (k) = 20.0;
      k=29; Coord_mb (k) =   4.; RH_water (k) = 20.0;
      k=28; Coord_mb (k) =   5.; RH_water (k) = 20.0;
      k=27; Coord_mb (k) =  10.; RH_water (k) = 20.0;
      k=26; Coord_mb (k) =  20.; RH_water (k) = 20.0;
      k=25; Coord_mb (k) =  30.; RH_water (k) = 20.0;
      k=24; Coord_mb (k) =  40.; RH_water (k) = 20.0;
      k=23; Coord_mb (k) =  50.; RH_water (k) = 20.0;
      k=22; Coord_mb (k) =  75.; RH_water (k) = 20.0;
      k=21; Coord_mb (k) = 100.; RH_water (k) = 20.0;
      k=20; Coord_mb (k) = 150.; RH_water (k) = 20.0;
      k=19; Coord_mb (k) = 200.; RH_water (k) = 20.0;
      k=18; Coord_mb (k) = 250.; RH_water (k) = 20.0;
      k=17; Coord_mb (k) = 300.; RH_water (k) = 20.0;
      k=16; Coord_mb (k) = 350.; RH_water (k) = 20.0;
      k=15; Coord_mb (k) = 400.; RH_water (k) = 20.0;
      k=14; Coord_mb (k) = 450.; RH_water (k) = 20.0;
      k=13; Coord_mb (k) = 500.; RH_water (k) = 20.0;
      k=12; Coord_mb (k) = 550.; RH_water (k) = 20.0;
      k=11; Coord_mb (k) = 600.; RH_water (k) = 20.0;
      k=10; Coord_mb (k) = 650.; RH_water (k) = 20.0;
      k= 9; Coord_mb (k) = 700.; RH_water (k) = 20.0;
      k= 8; Coord_mb (k) = 750.; RH_water (k) = 20.0;
      k= 7; Coord_mb (k) = 800.; RH_water (k) = 20.0;
      k= 6; Coord_mb (k) = 850.; RH_water (k) = 20.0;
      k= 5; Coord_mb (k) = 900.; RH_water (k) = 20.0;
      k= 4; Coord_mb (k) = 950.; RH_water (k) = 20.0;
      k= 3; Coord_mb (k) =1000.; RH_water (k) = 20.0;
      k= 2; Coord_mb (k) =1050.; RH_water (k) = 20.0;
      k= 1; Coord_mb (k) =1100.; RH_water (k) = 20.0;

! 3. END
! =======

   RETURN
   END SUBROUTINE err_rh_sfc
