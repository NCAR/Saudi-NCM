
      SUBROUTINE write_decoded (filename, iunit, number_obs, obs, grid, index)
!------------------------------------------------------------------------------!

      IMPLICIT NONE
      INTRINSIC MAX

      CHARACTER (LEN=80), INTENT (in) :: filename
      INTEGER,            INTENT (in) :: iunit
      INTEGER,            INTENT (in) :: number_obs
      TYPE (report),      DIMENSION (number_obs), INTENT (in) :: obs
      INTEGER,            DIMENSION (number_obs), INTENT (in) :: index

      TYPE (measurement), POINTER :: current
      TYPE(domain),       TARGET  :: grid

      INTEGER :: iost, n, nn, ns, l, k
      LOGICAL :: connected
      REAL    , PARAMETER                            ::  end_data_r    = -777777.

!----------------------------------------------------------------------------!

      CALL prelog
      WRITE  (log_unit,'(1A)') &
     "-----------------------------------------------------------------------"
      CALL prelog
      WRITE  (log_unit,'(2A)') "Write observations in file: ", TRIM (filename)


! 1.  OPEN FILE
! =============

! 1.1 Open file
!     ---------

      INQUIRE (UNIT = iunit, OPENED = connected)

      IF (connected ) CLOSE (iunit)

      OPEN (UNIT   = iunit,        &
            FILE   = filename,     &
            FORM   = 'FORMATTED',  &
            ACCESS = 'SEQUENTIAL', &
            ACTION = 'WRITE',      &
            STATUS = 'REPLACE',    &
            IOSTAT =  iost)

      REWIND (UNIT = iunit)

! 1.2 Loop over stations
!     ------------------

      ns = 0
      l  = 0

stations:&
      DO n = 1, number_obs

! 1.3 Write data sorted by location
!     -----------------------------

      nn = index (n)
      
! 1.4 Skip discarded data
!     -------------------

      IF (obs (nn) % info % discard) CYCLE stations

      ns = ns + 1

! 2.  WRITE STATION INFO
! ======================

      l = l + 1

      WRITE (UNIT = iunit, FMT = rpt_format )                            &
      obs (nn) % location % latitude ,  obs (nn) % location % longitude, &
      obs (nn) % location % id,         obs (nn) % location % name,      &
      obs (nn) % info % platform,       obs (nn) % info % source,        &
      obs (nn) % info % elevation,      obs (nn) % info % num_vld_fld,   &
      obs (nn) % info % num_error,      obs (nn) % info % num_warning,   &
      obs (nn) % info % seq_num,        obs (nn) % info % num_dups,      &
      obs (nn) % info % is_sound,       obs (nn) % info % bogus,         &
      obs (nn) % info % discard,                                         &
      obs (nn) % valid_time % sut,      obs (nn) % valid_time % julian,  &
      obs (nn) % valid_time % date_char,                                 &
      obs (nn) % ground % slp,          obs (nn) % ground % ref_pres,    &
      obs (nn) % ground % ground_t,     obs (nn) % ground % sst,         &    
      obs (nn) % ground % psfc,         obs (nn) % ground % precip,      & 
      obs (nn) % ground % t_max,        obs (nn) % ground % t_min,       & 
      obs (nn) % ground % t_min_night,  obs (nn) % ground % p_tend03,    &   
      obs (nn) % ground % p_tend24,     obs (nn) % ground % cloud_cvr,   &  
      obs (nn) % ground % ceiling


! 2.  WRITE UPPER LEVELS
! ======================

      current => obs (nn) % surface 
      k = 0

      DO WHILE (ASSOCIATED (current))

!        l = l + 1
         k = k + 1

         WRITE (UNIT = iunit,   FMT = meas_format )                  &
         current % meas % pressure,    current % meas % height,      &  
         current % meas % temperature, current % meas % dew_point,   &
         current % meas % speed,       current % meas % direction,   &   
         current % meas % u,           current % meas % v,           &
         current % meas % rh,          current % meas % thickness

         current => current % next

      ENDDO


! 3.  WRITE CLOSING UPPER LEVELS
! ==============================


      WRITE (UNIT = iunit,  FMT = meas_format ) &
             end_data_r, 0, end_data_r, 0, real (l+1), 0, &
             missing_r,  0, missing_r,  0, missing_r,  0, &
             missing_r,  0, missing_r,  0, missing_r,  0, &
             missing_r,  0

!     l = l + 1

! 4.  WRITE ENDING RECORD
! ======================

      WRITE (UNIT = iunit , FMT = end_format)  k,                             &
                                               obs (nn) % info % num_error,   &
                                               obs (nn) % info % num_warning

!     l = l + 1

      l = l + k + 3

      ENDDO stations

! 5. CLOSE FILE AND PRINT OUT
! ===========================

      CLOSE (iunit)

      CALL prelog 
      WRITE (log_unit, '(A,I5,A,1X,A,I6,A)')  &
             "Write ",ns," valid station(s)","(",l," lines)."

      END SUBROUTINE write_decoded
!------------------------------------------------------------------------------!

      SUBROUTINE write_decoded_single (iunit, obs, nlevels)
!------------------------------------------------------------------------------!
      IMPLICIT NONE

      INTEGER,       INTENT (in) :: iunit 
      INTEGER,       INTENT (in) :: nlevels
      TYPE (report), INTENT (in) :: obs

      INTEGER :: k, l, ns
      REAL    , PARAMETER                            ::  end_data_r    = -777777.

!------------------------------------------------------------------------------!

      CALL prelog
      WRITE  (log_unit,'(1A)')  &
     "-----------------------------------------------------------------------"
      CALL prelog
      WRITE  (log_unit,'(A,I3)') "Write observation in unit: ", iunit


! 1.  WRITE STATION INFO
! ======================

      ns = 1

      WRITE (UNIT = iunit, FMT = rpt_format)                   &
      obs % location % latitude ,  obs % location % longitude, &
      obs % location % id,         obs % location % name,      &
      obs % info % platform,       obs % info % source,        &
      obs % info % elevation,      obs % info % num_vld_fld,   &
      obs % info % num_error,      obs % info % num_warning,   &
      obs % info % seq_num,        obs % info % num_dups,      &
      obs % info % is_sound,       obs % info % bogus,         &
      obs % info % discard,                                    &
      obs % valid_time % sut,      obs % valid_time % julian,  &
      obs % valid_time % date_char,                            &
      obs % ground % slp,          obs % ground % ref_pres,    &
      obs % ground % ground_t,     obs % ground % sst,         &    
      obs % ground % psfc,         obs % ground % precip,      & 
      obs % ground % t_max,        obs % ground % t_min,       & 
      obs % ground % t_min_night,  obs % ground % p_tend03,    &   
      obs % ground % p_tend24,     obs % ground % cloud_cvr,   &  
      obs % ground % ceiling

      l = 1

! 2.  WRITE UPPER LEVELS
! ======================

      DO k = 1, nlevels

         WRITE (UNIT = iunit,   FMT = meas_format)                   &
         obs % each (k) % pressure,    obs % each (k) % height,      &   
         obs % each (k) % temperature, obs % each (k) % dew_point,   &
         obs % each (k) % speed,       obs % each (k) % direction,   &   
         obs % each (k) % u,           obs % each (k) % v,           &
         obs % each (k) % rh,          obs % each (k) % thickness

         l = l + 1

      ENDDO


! 3.  WRITE CLOSING UPPER LEVELS
! ==============================

      WRITE (UNIT = iunit,  FMT = meas_format) &
             end_data_r, 0, end_data_r, 0, real (nlevels), 0, &
             missing_r,  0, missing_r,  0, missing_r,      0, &
             missing_r,  0, missing_r,  0, missing_r,      0, &
             missing_r,  0, missing_r,  0

      l = l + 1


! 4.  WRITE ENDING RECORD
! ======================

      WRITE (UNIT = iunit , FMT = end_format) nlevels,                  &
                                              obs % info % num_error,   &
                                              obs % info % num_warning
      l = l + 1

! 5. PRINT OUT
! ============

      WRITE (log_unit, '(A,I5,A,1X,A,I6,A)')  &
             "Write ",ns," valid station(s)","(",l," lines)."

      END SUBROUTINE write_decoded_single
!------------------------------------------------------------------------------!

      SUBROUTINE plotobs_out (filename, iunit, number_obs, obs, mdl, err, index)
!------------------------------------------------------------------------------!

      IMPLICIT NONE
      INTRINSIC MAX

      CHARACTER (LEN=80), INTENT (in) :: filename
      INTEGER,            INTENT (in) :: iunit
      INTEGER,            INTENT (in) :: number_obs
      TYPE (report),      DIMENSION (number_obs), INTENT (in) :: obs, mdl, err
      INTEGER,            DIMENSION (number_obs), INTENT (in) :: index

      TYPE (measurement), POINTER :: current_obs, current_mdl, current_err
      TYPE (field)                :: obs_value, mdl_value, err_value

      CHARACTER (LEN=19) :: date_print, current_date
      INTEGER            :: iost, n, nn, ns, l, i, iq, ir, its
      INTEGER            :: tbegin, tend, tinc
      LOGICAL            :: connected, at_surface
      REAL               :: xjd, yid, elev

      CHARACTER (LEN=80)  :: title =   &
     "Number of Observations"
      CHARACTER (LEN=130) :: header1 = &
 " Variable  Press  Obs     Time  Station  Platform    at        Obs          Model         Error         X          Y         QC"
      CHARACTER (LEN=130) :: header2 = &
 "  Name     Level  Number  mnss    ID       name      sfc?     Value         Value         Value      Location   Location   Value"
      CHARACTER (LEN=80)  :: fmt_output = &
     "(3X,A2,6X,I4,1X,I6,4X,A4,2X,A5,3X,A60,3X,L1,4X,3(G13.6,1X),2(F8.3,2X),I7)"
!     "(3X,A2,6X,I4,1X,I6,4X,A4,2X,A5,3X,A11,3X,L1,4X,3(G13.6,1X),2(F8.3,2X),I7)"

!    "(1X,A8,6X,I4,3X,I6,3X,A4,3X,A5,3X,A20,2X,L1,1X,3(G13.6,1X),2(F8.3,1X),I7)"

      CHARACTER (LEN=3), DIMENSION (4) :: name_var
      DATA name_var / "U ", "V ", "T ", "RH " /
      INTEGER,           DIMENSION (4) :: ivar
      

!----------------------------------------------------------------------------!

      CALL prelog
      WRITE  (log_unit,'(1A)') &
     "-----------------------------------------------------------------------"
      CALL prelog
      WRITE  (log_unit,'(2A)') "Write observations in file: ", TRIM (filename)


! 1.  OPEN OUTPUT FILE
! ====================

! 1.1 Open file
!     ---------

      INQUIRE (UNIT = iunit, OPENED = connected)

      IF (connected ) CLOSE (iunit)

      OPEN (UNIT   = iunit,        &
            FILE   = filename,     &
            FORM   = 'FORMATTED',  &
            ACCESS = 'SEQUENTIAL', &
            ACTION = 'WRITE',      &
            STATUS = 'REPLACE',    &
            IOSTAT =  iost)

      REWIND (UNIT = iunit)

      l  = 0 
      ns = 0

! 1.2 Write format
!     ------------

      WRITE (UNIT=iunit, FMT = '(A)') TRIM (fmt_output)

      l = l + 1 


! 2.  LOOP OVER TIME (EVERY HOUR)
! ===============================


! 2.1 Begin at time_analysis - time_before (in mn)
!     --------------------------------------------

      iq = time_before * 60 / 3600   ! How many full hours
      ir = time_before*60 - iq* 3600 ! How many additional second

      IF (ir /= 0) iq = iq + 1       ! Round to the next hour

      tbegin = -iq*3600              ! Time increment in s

! 2.2 End at time_analysis + time_after (in mn) 
!     ----------------------------------------

      iq = time_after * 60 / 3600   ! How many full hours
      ir = time_after*60 - iq* 3600 ! How many additional second

      IF (ir /= 0) iq = iq + 1      ! Round to next hour

      tend = iq*3600                ! Time increment in s


! 2.3 Loop every hour from BEGIN_DATE to END_DATE
!     -------------------------------------------

loop_time:&
      DO tinc = tbegin, tend, 3600

! 2.4 Current date
!     -------------

      CALL GETH_NEWDATE (date_print, analysis_date (1:19), tinc)



! 3.  COUNT NONE MISSING OBSERVATIONS IN TIME WINDOW
! ==================================================

! 3.2 Loop over stations
!     ------------------

      ivar = 0

count_vars:&
      DO n = 1, number_obs

! 3.3 Write data sorted by location
!     -----------------------------

      nn = index (n)
      
! 3.4 Skip discarded reports
!     ----------------------

      IF (obs (nn) % info % discard) CYCLE count_vars
      IF (mdl (nn) % info % discard) CYCLE count_vars
      IF (err (nn) % info % discard) CYCLE count_vars

! 3.5 Skip reports without data
!     -------------------------

      current_obs => obs (nn) % surface
      current_mdl => mdl (nn) % surface
      current_err => err (nn) % surface

      IF (.NOT. ASSOCIATED (current_obs) .OR. &
          .NOT. ASSOCIATED (current_mdl) .OR. &
          .NOT. ASSOCIATED (current_err)) THEN
           CYCLE count_vars
      ENDIF

! 3.6 Obs time as CCYY-MM-DD_HH:MN:SS
!     -------------------------------

      WRITE (current_date,'(A4,"-",A2,"-",A2,"_",A2,":",A2,":",A2)') &
             obs (nn)%valid_time%date_char ( 1: 4), &
             obs (nn)%valid_time%date_char ( 5: 6), &
             obs (nn)%valid_time%date_char ( 7: 8), &
             obs (nn)%valid_time%date_char ( 9:10), &
             obs (nn)%valid_time%date_char (11:12), &
             obs (nn)%valid_time%date_char (13:14)

! 3.7 Time lag
!     --------

      CALL GETH_IDTS (current_date, date_print, its)

! 3.8 Discard obs already counted or to late
!     --------------------------------------

      IF (its < 0 .OR. its >= 3600) CYCLE count_vars

      ns = ns + 1

! 3.9 Loop over upper levels
!     ----------------------

      current_obs => obs (nn) % surface
      current_mdl => mdl (nn) % surface

      DO WHILE (ASSOCIATED (current_obs) .AND. ASSOCIATED (current_mdl) .AND. &
                ASSOCIATED (current_err))

! 3.10 Count
!      -----

         IF (EPS_NOT (current_obs % meas % u % data, missing_r, 1.) .AND. &
             EPS_NOT (current_mdl % meas % u % data, missing_r, 1.) .AND. &
             EPS_NOT (current_err % meas % u % data, missing_r, 1.)) THEN
             ivar (1) = ivar (1) + 1
         ENDIF

         IF (EPS_NOT (current_obs % meas % v % data, missing_r, 1.) .AND. &
             EPS_NOT (current_mdl % meas % v % data, missing_r, 1.) .AND. &
             EPS_NOT (current_err % meas % v % data, missing_r, 1.)) THEN
             ivar (2) = ivar (2) + 1
         ENDIF

         IF (EPS_NOT (current_obs % meas % temperature % data, missing_r, 1.) .AND.&
             EPS_NOT (current_mdl % meas % temperature % data, missing_r, 1.) .AND.&
             EPS_NOT (current_err % meas % temperature % data, missing_r, 1.)) THEN
             ivar (3) = ivar (3) + 1
         ENDIF

         IF (EPS_NOT (current_obs % meas % rh % data, missing_r, 1.) .AND. &
             EPS_NOT (current_mdl % meas % rh % data, missing_r, 1.) .AND. &
             EPS_NOT (current_err % meas % rh % data, missing_r, 1.)) THEN
             ivar (4) = ivar (4) + 1
         ENDIF

! 3.11 Go to next level
!      ----------------

         current_obs => current_obs % next 
         current_mdl => current_mdl % next 
         current_err => current_err % next 

      ENDDO

! 3.12 Go to next stations
!      --------------------

       ENDDO count_vars



! 4.  WRITE (NONE MISSING) VARIABLE INDIVIDUALLY
! ==============================================

! 4.1 Skip if there are no data in the time window
!     --------------------------------------------

      IF (MAXVAL (ivar) <= 0) CYCLE loop_time

! 4.2 Print current beginning date
!     ----------------------------

      WRITE (UNIT=iunit, FMT='(A)') "Date"
      WRITE (UNIT=iunit, FMT='(A)') date_print (1: 13)

      l = l + 2

! 4.3 Loop over variables
!     -------------------

loop_vars:&
      DO i = 1, 4

! 4.2 Write variable header
!     ---------------------

      WRITE (UNIT=iunit, FMT = '(A)')    TRIM (title)
      WRITE (UNIT=iunit, FMT = '(I8.8)')  ivar (i)
      WRITE (UNIT=iunit, FMT = '(A)') TRIM (header1)
      WRITE (UNIT=iunit, FMT = '(A)') TRIM (header2)

      l = l + 4 

! 4.3 Reset variable number 
!     ---------------------
      ivar (i) = 0

! 4.4 Loop over stations
!     ------------------

write_vars:&
      DO n = 1, number_obs

! 4.5 Write data sorted by location
!     -----------------------------

      nn = index (n)
      
! 4.6 Skip discarded reports
!     ----------------------

      IF (obs (nn) % info % discard) CYCLE write_vars
      IF (mdl (nn) % info % discard) CYCLE write_vars
      IF (err (nn) % info % discard) CYCLE write_vars

! 4.7 Skip reports without data
!     -------------------------

      current_obs => obs (nn) % surface
      current_mdl => mdl (nn) % surface
      current_err => mdl (nn) % surface

      IF  (.NOT. ASSOCIATED (current_obs) .OR. &
           .NOT. ASSOCIATED (current_mdl) .OR. &
           .NOT. ASSOCIATED (current_obs)) THEN
           CYCLE write_vars
      ENDIF

! 4.8 Obs time as CCYY-MM-DD_HH:MN:SS
!     -------------------------------

      WRITE (current_date,'(A4,"-",A2,"-",A2,"_",A2,":",A2,":",A2)') &
             obs (nn)%valid_time%date_char ( 1: 4), &
             obs (nn)%valid_time%date_char ( 5: 6), &
             obs (nn)%valid_time%date_char ( 7: 8), &
             obs (nn)%valid_time%date_char ( 9:10), &
             obs (nn)%valid_time%date_char (11:12), &
             obs (nn)%valid_time%date_char (13:14)

! 4.9 Time lag
!     --------

      CALL GETH_IDTS (current_date, date_print, its)

! 4.10 Discard obs already counted or to late
!      --------------------------------------

      IF (its < 0 .OR. its >= 3600) CYCLE write_vars

! 4.11 Get station coordinates
!      -----------------------

      CALL llxy (obs (nn) % location % latitude,  &
                 obs (nn) % location % longitude, &
                 xjd, yid)

! 4.12 Elevation
!      ---------

      elev = MAX (obs (nn) % info % elevation, mdl (nn) % info % elevation)

! 5.  LOOP OVER UPPER LEVELS
! ===========================

      current_obs => obs (nn) % surface
      current_mdl => mdl (nn) % surface
      current_err => err (nn) % surface

upper_levels:&
      DO WHILE (ASSOCIATED (current_obs) .AND. ASSOCIATED (current_mdl) .AND. &
                ASSOCIATED (current_err))

! 5.1 Select the appropriate variables
!     --------------------------------

         SELECT CASE (name_var (i)) 

                CASE ("U ")
                      obs_value = current_obs % meas % u
                      mdl_value = current_mdl % meas % u
                      err_value = current_err % meas % u
                CASE ("V ")
                      obs_value = current_obs % meas % v
                      mdl_value = current_mdl % meas % v
                      err_value = current_err % meas % v
                CASE ("T ")
                      obs_value = current_obs % meas % temperature
                      mdl_value = current_mdl % meas % temperature
                      err_value = current_err % meas % temperature
!                     err_value % data = current_err % meas % temperature % data
                CASE ("RH")
                      obs_value = current_obs % meas % rh
                      mdl_value = current_mdl % meas % rh
                      err_value = current_err % meas % rh
                CASE DEFAULT 
                      current_obs => current_obs % next 
                      current_mdl => current_mdl % next 
                      current_err => current_err % next 
                      CYCLE upper_levels

         END SELECT

! 5.2 Check for missing value
!     -----------------------

         IF (EPS_NOT (obs_value % data, missing_r, 1.) .AND. &
             EPS_NOT (mdl_value % data, missing_r, 1.) .AND. &
             EPS_NOT (err_value % data, missing_r, 1.)) THEN
             ivar (i) = ivar (i) + 1
         ELSE
             current_obs => current_obs % next 
             current_mdl => current_mdl % next 
             current_err => current_err % next 
             CYCLE upper_levels
         ENDIF

! 5.4 Determine if the observations is at the surface
!     -----------------------------------------------

      ! All surface stations come with the is_sound flag turned to FALSE
      at_surface = .NOT. obs (nn) % info % is_sound

      ! Very low sounding level can also be at the surface (+/-15m)
      IF (eps_equal (current_obs % meas % height % data, elev, 15.)) THEN
          at_surface = .TRUE.
      ENDIF
 
! 5.3 Write out
!     ---------

         WRITE (UNIT=iunit, FMT = fmt_output) &
         name_var (i),INT (current_obs % meas % pressure % data/100.),ivar (i),&
         obs (nn) % valid_time % date_char (11:14),                            &
         obs (nn) % location   % id (1:5), obs (nn) % info % platform (1:40),  &
         at_surface,                                                           &
         obs_value % data, mdl_value % data, err_value % data,                 &
         xjd, yid, obs_value % qc

         l = l + 1

! 5.4 Go to next level
!     ----------------

         current_obs => current_obs % next 
         current_mdl => current_mdl % next 
         current_err => current_err % next 

       ENDDO upper_levels

! 6.  END OF LOOPS
! ================

! 6.1 Go to next stations
!     -------------------

       ENDDO write_vars

! 6.2 Go to next variables
!     ---------------------

      ENDDO loop_vars

! 6.3 Go to next time period
!     -----------------------

      ENDDO loop_time

! 6.4 Close ouput file
!     -----------------

      CLOSE (UNIT=iunit)

! 6.5 Print out
!     ---------

      CALL prelog 
      WRITE (log_unit, '(A,I5,A,1X,A,I6,A)')  &
             "Write ",ns," valid station(s)","(",l," lines)."


      END SUBROUTINE plotobs_out
!------------------------------------------------------------------------------!
 SUBROUTINE write_3dvar (filename, iunit, &
                         number_of_obs, obs, err, index, time_analysis, &
                         xb, grid)

!-------------------------------------------------------------------------------
! Write observation at MM5 3D-VAR input ASCII format
!
! F. VANDENBERGHE, July 2003
!-------------------------------------------------------------------------------


  IMPLICIT NONE

  CHARACTER (LEN=80),                       INTENT (in) :: filename
  INTEGER,                                  INTENT (in) :: iunit
  INTEGER,                                  INTENT (in) :: number_of_obs
  TYPE (report), DIMENSION (number_of_obs), INTENT (inout) :: obs, err
  INTEGER,       DIMENSION (number_of_obs), INTENT (in) :: index
! REAL,                                     INTENT (in) :: missing_r
  CHARACTER (LEN =  19),                    INTENT (in) :: time_analysis
  TYPE (xb_type),                           INTENT (in) :: xb
  TYPE(domain),                             TARGET      :: grid


  TYPE (measurement ) , POINTER :: current_obs, current_err
  INTEGER                       :: loop_index
  INTEGER                       :: i, ii, n, ntotal, iost, ns
  INTEGER                       :: nmultis, nsingles, nlevels, nwrites
  INTEGER                       :: is_sound, fm
  LOGICAL                       :: connected
  CHARACTER (LEN =  19)         :: date19
  CHARACTER (LEN =  40)         :: platform
  CHARACTER (LEN = 120)         :: fmt_info, &
                                   fmt_srfc, &
                                   fmt_each
  REAL                          :: val_slp, val_pw
  REAL                          :: val_u,  val_v,  val_p, val_t
  REAL                          :: val_td, val_rh, val_qv
  REAL                          :: rew_cross, rns_cross
  LOGICAL                       :: change_qc

  INTEGER  :: nsynops,nmetars, nshipss, nsounds, npilots, naireps, &
              nsatems,nsatobs, ngpspws, nssmt1s, nssmt2s, nssmis,  &
              ntovss, namdars, ntamdars, nqscats, nprofls, nbuoyss, nothers

!------------------------------------------------------------------------------!

! rew_cross=real(nestjx(idd)-1)
! rns_cross=real(nestix(idd)-1)

!------------------------------------------------------------------------------!

      CALL prelog
      WRITE  (log_unit,'(1A)') &
     "-----------------------------------------------------------------------"
      CALL prelog
      WRITE  (log_unit,'(2A)') "Write observations in file: ", TRIM (filename)

! 1.  COUNT REPORTS PER PLATFORM
! ==============================

! 1.1 Count reports per platform
!     --------------------------

      CALL sort_platform (number_of_obs, obs, number_of_obs,       &
                          nsynops, nshipss, nmetars, npilots, nsounds, &
                          nsatems, nsatobs, naireps, ngpspws, nssmt1s, &
                          nssmt2s, nssmis,  ntovss,  namdars, ntamdars,&
                          nqscats, nprofls, nbuoyss, nothers)
      
! 1.2 Total number of reports
!     -----------------------

      ntotal =   nsynops + nmetars + nshipss + &
                 nsounds + npilots + naireps + &
                 nsatems + nsatobs + ngpspws + &
                 nssmt1s + nssmt2s + ntovss  + &
                 namdars + ntamdars+ nqscats + &
                 nprofls + nbuoyss + nothers  

! 2.  FORMAT
! ==========

     fmt_info = '(A12,1X,A19,1X,A40,1X,I6,2(F12.5,11X),F12.3,11X,6X,A5)'
!    fmt_info = '(A12,1X,A19,1X,A40,1X,I6,3(F12.3,11X),6X,A5)'
     fmt_srfc = '(F12.3,I4,F7.2,F12.3,I4,F7.2)'
     fmt_each = '(3(F12.3,I4,F7.2),11X,3(F12.3,I4,F7.2),11X,1(F12.3,I4,F7.2))'


! 3.  OPEN FILE
! =============

      INQUIRE (UNIT = iunit, OPENED = connected)

      IF (connected ) CLOSE (iunit)

      OPEN (UNIT   = iunit,        &
            FILE   = filename,     &
            FORM   = 'FORMATTED',  &
            ACCESS = 'SEQUENTIAL', &
            ACTION = 'WRITE',      &
            STATUS = 'REPLACE',    &
            IOSTAT =  iost)

      REWIND (UNIT = iunit)


! 4.  FILE HEADER
! ===============

! 4.1 Total number of stations contained in file
!     ------------------------------------------

      WRITE  (UNIT = iunit, FMT = '((A,I7),A)',ADVANCE='no' )    &
      "TOTAL =",nsynops + nmetars + nshipss + &
                nsounds + npilots + naireps + &
                nsatems + nsatobs + ngpspws + &
                nssmt1s + nssmt2s + namdars + &
                ntamdars+ ntovss  + nqscats + &
                nprofls + nbuoyss + nothers,", "


! 4.2. Missing value flag 
!      ------------------

       WRITE  (UNIT = iunit, FMT = '((A,F8.0),A)') "MISS. =",missing_r,","


! 4.3 Number of stations per platform
!      ------------------------------

      WRITE  (UNIT = iunit, FMT = '(5(5(A,I7,A),/,:))' )    &
      "SYNOP =",nsynops,", ", &
      "METAR =",nmetars,", ", &
      "SHIP  =",nshipss,", ", &
      "BUOY  =",nbuoyss,", ", &
      "TEMP  =",nsounds,", ", &
      "AMDAR =",namdars,", ", &
      "TAMDAR=",ntamdars,", ", &
      "AIREP =",naireps,", ", &
      "PILOT =",npilots,", ", &
      "SATEM =",nsatems,", ", &
      "SATOB =",nsatobs,", ", &
      "GPSPW =",ngpspws,", ", &
      "SSMT1 =",nssmt1s,", ", &
      "SSMT2 =",nssmt2s,", ", &
!     "SSMI  =",nssmis, ", ", &
      "TOVS  =",ntovss, ", ", &
      "QSCAT =",nqscats,", ", &
      "PROFL =",nprofls,", ", &
      "OTHER =",nothers,", "


! 4.4 Reference state info
!     --------------------

        WRITE (UNIT = iunit, FMT = '(4(A,F7.2),2(A,I7),A)') &
        "PHIC  =", grid%moad_cen_lat,", XLONC =", grid%cen_lon,", TRUE1 =", grid%truelat1,&
      ", TRUE2 =",grid%truelat2,", XIM11 =", xb % its,&
      ", XJM11 =",xb % jts,","

        WRITE (UNIT = iunit, FMT = '(2(A,F7.2),A,F7.0,A,F7.0,A)') &
        "TS0   =",  ts0, ", TLP   =", tlp, &
      ", PTOP  =",  ptop,", PS0   =", ps0,","  


! 4.5 Domains info
!      -----------

        WRITE (UNIT = iunit, FMT = '(5(A,I7,A))' )     &
        "IXC   =",  xb % jte - xb % jts + 1, ", ",  &
        "JXC   =",  xb % ite - xb % its + 1, ", ",  &
        "IPROJ =",  grid % map_proj, ", ",  &
        "IDD   =",  grid % id,  ", ",  &
        "MAXNES=",  1


        WRITE (UNIT = iunit, FMT = '(A,10(:,I7,A))')  &
        "NESTIX=",   xb % its 
        WRITE (UNIT = iunit, FMT = '(A,10(:,I7,A))')  &
        "NESTJX=",   xb % jts 
        WRITE (UNIT = iunit, FMT = '(A,10(:,I7,A))')  &
        "NUMC  =",   grid % id 
        WRITE (UNIT = iunit, FMT = '(A,10(:,F7.2,A))')&
        "DIS   =",   grid % dx/1000.
        !WRITE (UNIT = iunit, FMT = '(A,10(:,I7,A))')  &
        !"NESTI =",   xb % big_header % bhi (18,1)
        !WRITE (UNIT = iunit, FMT = '(A,10(:,I7,A))')  &
        !"NESTJ =",   xb % big_header % bhi (19,1)


! 4.6 Variable name and units
!     -----------------------

      WRITE (UNIT = iunit, FMT = '(A)' ) &
      "INFO  = PLATFORM, DATE, NAME, LEVELS, LATITUDE, LONGITUDE, ELEVATION, ID."
       WRITE (UNIT = iunit, FMT = '(A)' ) &
      "SRFC  = SLP, PW (DATA,QC,ERROR)."
       WRITE (UNIT = iunit, FMT = '(A)' ) &
      "EACH  = PRES, SPEED, DIR, HEIGHT, TEMP, DEW PT, HUMID (DATA,QC,ERROR)*LEVELS."

! 4.7 Formats
!     -------

      WRITE (UNIT = iunit, FMT = '(2A)' ) 'INFO_FMT = ', TRIM (fmt_info)
      WRITE (UNIT = iunit, FMT = '(2A)' ) 'SRFC_FMT = ', TRIM (fmt_srfc)
      WRITE (UNIT = iunit, FMT = '(2A)' ) 'EACH_FMT = ', TRIM (fmt_each)


! 4.8 End of header
!     -------------

        WRITE (UNIT = iunit, FMT = '(A)') &
"#------------------------------------------------------------------------------#"


! 5.  WRITE OBSERVATIONS
! ======================

      nmultis  = 0
      nsingles = 0
      nlevels  = 0
      nwrites  = 21 ! 21 lines of header
      ns = 0


! 5.1  Loop over stations
!      ------------------

stations: &
       DO n = 1, number_of_obs

! 5.2 Index of obs
!     ------------

      loop_index = index (n)

! 5.3 Check if station is valid
!     -------------------------

      IF (obs (loop_index)%info%discard ) CYCLE  stations

!     READ (obs (loop_index) % info % platform (4:6), '(I3)') fm
!     if ((fm == 125) .OR. (fm == 126))  CYCLE stations

      IF (obs (loop_index) % info % platform (4:6) == "125" .OR. &
          obs (loop_index) % info % platform (4:6) == "126")     &
          CYCLE stations

! SATEM reference pressure is assigned to slp:

!     if (fm == 86) then
      IF (obs (loop_index) % info % platform (4:5) == "86") THEN
           obs (loop_index) % ground % slp % data = &
           obs (loop_index) % ground % ref_pres % data
           obs (loop_index) % ground % slp % qc =   &
           obs (loop_index) % ground % ref_pres % qc
!          obs (loop_index) % ground % pw % data =  &
!          obs (loop_index) % ground % cloud_cvr % data
!          obs (loop_index) % ground % pw % qc =    &
!          obs (loop_index) % ground % cloud_cvr % qc
      endif

! 5.4 Date as ccyy-mm-dd_hh:mn:ss
!     ---------------------------

      WRITE (date19,'(A4,"-",A2,"-",A2,"_",A2,":",A2,":",A2)') &
             obs (loop_index) % valid_time % date_char ( 1: 4),&
             obs (loop_index) % valid_time % date_char ( 5: 6),&
             obs (loop_index) % valid_time % date_char ( 7: 8),&
             obs (loop_index) % valid_time % date_char ( 9:10),&
             obs (loop_index) % valid_time % date_char (11:12),&
             obs (loop_index) % valid_time % date_char (13:14)


! 5.5 Platform type for 3DVAR
!     -----------------------

       platform = platform_wmo (obs (loop_index) % location % name, &
                                obs (loop_index) % info     % platform)
! 5.6 Write station info
!     ------------------

      WRITE (UNIT = iunit, FMT = TRIM (fmt_info))       &
             platform, date19,                          &
             obs (loop_index) % location % name,        &
             obs (loop_index) % info     % num_vld_fld, &
             obs (loop_index) % location % latitude,    &
             obs (loop_index) % location % longitude,   &
             obs (loop_index) % info     % elevation,   &
             obs (loop_index) % location % id

             nwrites = nwrites + 1
             ns = ns + 1

! 5.7 Write surface info
!     ------------------

         change_qc = .false.

!        if(obs (loop_index) % location % xjc < 1.0 .or. &
!           obs (loop_index) % location % xjc > rew_cross .or. &
!           obs (loop_index) % location % yic < 1.0 .or. &
!           obs (loop_index) % location % yic > rns_cross) then
!           obs (loop_index) % ground % slp % qc = -88
!           obs (loop_index) % ground % pw  % qc = -88
!           change_qc = .true. 
!        end if


      WRITE (UNIT = iunit, FMT = TRIM (fmt_srfc))               &
             obs (loop_index) % ground % slp % data,  88, -88., &
             missing_r, -88, -88.

             nwrites = nwrites + 1

! 5.8 Initialise pointer
!     ------------------

      current_obs => obs (loop_index) % surface
      current_err => err (loop_index) % surface


! 5.9 Loop over levels
!     ----------------

      is_sound = -1

levels:&
      DO WHILE (ASSOCIATED (current_obs))

      nlevels  = nlevels  + 1
      is_sound = is_sound + 1

! SATEM thickness is assigned to temperature field:

!     if (fm == 86) &
      IF (obs (loop_index) % info % platform (4:5) == "86") &
         current_obs % meas % temperature = current_obs % meas % thickness

! 5.10 Keep only data with QC > 6 (1 sigma)
!      -----------------------------------

      if (current_obs % meas % pressure % qc < 6) THEN
          current_obs % meas % pressure % qc = 2
      ELSE if (current_obs % meas % pressure % qc > 10) THEN
          current_obs % meas % pressure % qc = 2
      ELSE
          current_obs % meas % pressure % qc = 0
      ENDIF

      if (current_obs % meas % height % qc < 6) THEN
          current_obs % meas % height % qc = 2
      ELSE if (current_obs % meas % height % qc > 10) THEN
          current_obs % meas % height % qc = 2
      ELSE
          current_obs % meas % height % qc = 0
      ENDIF

      if (current_obs % meas % temperature % qc   < 6) THEN
          current_obs % meas % temperature % data = missing_r
          current_err % meas % temperature % data = -88.
          current_obs % meas % temperature % qc   = -88
      ELSE
          current_obs % meas % temperature % qc   = 0
      ENDIF

      if (current_obs % meas % rh % qc   < 6) THEN
          current_obs % meas % rh % data = missing_r
          current_err % meas % rh % data = -88.
          current_obs % meas % rh % qc   = -88
      ELSE
          current_obs % meas % rh % qc   = 0
      ENDIF

      if (current_obs % meas % dew_point % qc   < 6) THEN
          current_obs % meas % dew_point % data = missing_r
          current_err % meas % dew_point % data = -88.
          current_obs % meas % dew_point % qc   = -88
      ELSE
          current_obs % meas % dew_point % qc   = 0
      ENDIF

!     if (current_obs % meas % speed % qc   < 6) THEN
!         current_obs % meas % speed % data = missing_r
!         current_err % meas % speed % data = -88.
!         current_obs % meas % speed % qc   = -88
!     ELSE
          current_obs % meas % speed % qc   = 0
!     ENDIF

!     if (current_obs % meas % direction % qc   < 6) THEN
!         current_obs % meas % direction % data = missing_r
!         current_err % meas % direction % data = -88.
!         current_obs % meas % direction % qc   = -88
!     ELSE
          current_obs % meas % direction % qc   = 0
!     ENDIF

! 5.10 Make sure that observational error is not missing
!      -------------------------------------------------

      IF (eps_equal (current_err % meas % pressure % data, missing_r,1.)) THEN
                  current_err % meas % pressure % data = -88.
!                 current_obs % meas % pressure % data = -88.
!                 current_obs % meas % pressure % qc   = -88.
      ENDIF

      IF (eps_equal (current_err % meas % height % data, missing_r,1.)) THEN
                  current_err % meas % height % data = -88.
!                 current_obs % meas % height % data = -88.
!                 current_obs % meas % height % qc   = -88.
      ENDIF

      IF (eps_equal (current_err % meas % temperature % data, missing_r,1.)) THEN
                  current_err % meas % temperature % data = -88.
                  current_obs % meas % temperature % data = -88.
                  current_obs % meas % temperature % qc   = -88.
      ENDIF

      IF (eps_equal (current_err % meas % dew_point % data, missing_r,1.)) THEN
                  current_err % meas % dew_point % data = -88.
                  current_obs % meas % dew_point % data = -88.
                  current_obs % meas % dew_point % qc   = -88.
      ENDIF

      IF (eps_equal (current_err % meas % direction % data, missing_r,1.)) THEN
                  current_err % meas % direction % data = -88.
                  current_obs % meas % direction % data = -88.
                  current_obs % meas % direction % qc   = -88.
      ENDIF

      IF (eps_equal (current_err % meas % speed % data, missing_r,1.)) THEN
                  current_err % meas % speed % data = -88.
                  current_obs % meas % speed % data = -88.
                  current_obs % meas % speed % qc   = -88.
      ENDIF

! 5.11 Write height, pressure, temp, mixing ratio, wind and model vertical coord
!      -------------------------------------------------------------------------

      if(change_qc) then
         current_obs % meas % temperature % qc = -88
         current_obs % meas % dew_point   % qc = -88
         current_obs % meas % rh          % qc = -88

         if(current_obs % meas % height   % qc >= 0) then
            current_obs % meas % pressure % qc = -88
         end if
      end if

      WRITE (UNIT = iunit, FMT = TRIM (fmt_each))     &
             current_obs % meas % pressure    % data, &
             current_obs % meas % pressure    % qc,   & 
             current_err % meas % pressure    % data, &
             current_obs % meas % speed       % data, & 
             current_obs % meas % speed       % qc,   & 
             current_err % meas % speed       % data, &
             current_obs % meas % direction   % data, &
             current_obs % meas % direction   % qc,   & 
             current_err % meas % direction   % data, &
             current_obs % meas % height      % data, &
             current_obs % meas % height      % qc,   &
             current_err % meas % height      % data, &
             current_obs % meas % temperature % data, &
             current_obs % meas % temperature % qc,   &
             current_err % meas % temperature % data, &
             current_obs % meas % dew_point   % data, &
             current_obs % meas % dew_point   % qc,   & 
             current_err % meas % dew_point   % data, &
             current_obs % meas % rh          % data, &
             current_obs % meas % rh          % qc,   & 
             current_err % meas % rh          % data

             nwrites = nwrites + 1


! 6.  GO TO NEXT OBS
!     ==============

! 6.1 Go to next level
!     -----------------

      current_obs => current_obs % next
      current_err => current_err % next

      ENDDO levels

! 6.2 Count surface and sounding
!     --------------------------

      if (is_sound .gt. 0) then
          nmultis = nmultis + 1
      else 
          nsingles  = nsingles + 1
      endif

! 6.3 Go to next record
!     -----------------

      ENDDO stations


! 7.   CLOSE OUTPUT FILES
! ========================

      CLOSE (UNIT = iunit) 


! 8.  PRINT DIAGNOSTIC
! =====================
 
      CALL prelog 
      WRITE (log_unit, '(A,I5,A,1X,A,I6,A)')  &
             "Write ",ns," valid station(s)","(",nwrites," lines)."



 END SUBROUTINE write_3dvar
!------------------------------------------------------------------------------!
