
 MODULE module_medoc
!-------------------------------------------------------------------------------
! Module containing the defintion of the MEDOC data structure.
! See SCIPUFF documentation section 14.2.2
!
! Francois Vandenberghe, September 2004                                        |
! Copyright UCAR (c) 1992 - 2004                                               |
! University Corporation for Atmospheric Research (UCAR)                       |
! National Center for Atmospheric Research (NCAR)                              |
! Research Applications Program (RAP)                                          |
! P.O.Box 3000, Boulder, Colorado, 80307-3000, USA                             |
!-------------------------------------------------------------------------------

  CHARACTER (LEN=80), PARAMETER :: medoc_fmt1 = "(6(a8,1x))"
  CHARACTER (LEN=80), PARAMETER :: medoc_fmt2 = "(6(i12,1x))"
  CHARACTER (LEN=80), PARAMETER :: medoc_fmt3 = "(6(f12.4,1x))"

  REAL, PARAMETER :: medoc_missing =-999999.

  TYPE medoc_type

! Variables read in file

   CHARACTER (LEN=8) :: fflag,codename,stagger,namdum
   CHARACTER (LEN=8), ALLOCATABLE, DIMENSION (:) :: nam3d,nam2d

   INTEGER :: iday,imonth,iyear,ihour,imin,isec
   INTEGER :: jday,jmonth,jyear,jhour,jmin,jsec
   INTEGER :: imax,jmax,kmax,nreper,nvar3d,nvar2d
   INTEGER :: idum

   REAL    :: dx,dy,xo,yo,lat,lon,ztop
   REAL    :: dum

   REAL, ALLOCATABLE, DIMENSION (:) :: sz
   REAL, ALLOCATABLE, DIMENSION (:,:,:) :: var2d
   REAL, ALLOCATABLE, DIMENSION (:,:,:,:) :: var3d

! Additional variables for convenience

   REAL, ALLOCATABLE, DIMENSION (:,:,:) :: height_msl_m
   REAL, ALLOCATABLE, DIMENSION (:,:)   :: terrain_msl_m
   CHARACTER (LEN=80) :: filename

  END TYPE medoc_type


CONTAINS
!-------------------------------------------------------------------------------

 SUBROUTINE read_medoc (medocfile, unitin, medoc)
!-------------------------------------------------------------------------------
! Read a MEDOC file, See SCIPUFF documentation Section 14.2.2
!-------------------------------------------------------------------------------
  IMPLICIT NONE

  CHARACTER (LEN=80), INTENT (in)    :: medocfile
  INTEGER,            INTENT (in)    :: unitin
  TYPE (medoc_type),  INTENT (inout) :: medoc
  
  INTEGER :: i,j,k,n, ier
  INTEGER :: idum
  REAL    :: dum
!-------------------------------------------------------------------------------

! 1.  READ HEADER
! ===============

! 1.0 Save input file name
!     --------------------

      medoc % filename = medocfile

! 1.1 Open MEDOC file
!     ---------------

      CALL openin (medocfile, unitin, .TRUE.)

! 1.2 Read header
!     -----------

      ! File format 'FFFFFFFF' or 'BBBBBBBB'
      READ (UNIT=unitin, FMT=medoc_fmt1,IOSTAT=ier) &
            medoc % fflag 

      IF (ier /= 0) THEN
          WRITE (*,'(2A)') "Error reading format ",TRIM (medoc_fmt1)
          CALL ABORT
      ENDIF

      ! Name of code and staggered grid flag
      READ (UNIT=unitin, FMT=medoc_fmt1,IOSTAT=ier) &
            medoc % codename, medoc % stagger 

      IF (ier /= 0) THEN
          WRITE (*,'(2A)') "Error reading format ",TRIM (medoc_fmt1)
          CALL ABORT
      ENDIF

      ! Time
      READ (UNIT=unitin, FMT=medoc_fmt2,IOSTAT=ier) &
            medoc % iday,  medoc % imonth, medoc % iyear, &
            medoc % ihour, medoc % imin,   medoc % isec 

      IF (ier /= 0) THEN
          WRITE (*,'(2A)') "Error reading format ",TRIM (medoc_fmt2)
          CALL ABORT
      ENDIF

      ! Initial Time of calculation
      READ (UNIT=unitin, FMT=medoc_fmt2,IOSTAT=ier) &
            medoc % jday, medoc % jmonth, medoc % jyear, &
            medoc % jhour, medoc % jmin, medoc % jsec 

      IF (ier /= 0) THEN
          WRITE (*,'(2A)') "Error reading format ",TRIM (medoc_fmt2)
          CALL ABORT
      ENDIF

      ! Number of grid points and variables
      READ (UNIT=unitin, FMT=medoc_fmt2,IOSTAT=ier) &
            medoc % imax, medoc % jmax, medoc % kmax, &
            medoc % nreper, medoc % nvar3d, medoc % nvar2d

      IF (ier /= 0) THEN
          WRITE (*,'(2A)') "Error reading format ",TRIM (medoc_fmt2)
          CALL ABORT
      ENDIF


! 2. READ DATA
! ============

! 2.1 Allocate space
!     --------------

      ALLOCATE (medoc % sz    (medoc % kmax))
      ALLOCATE (medoc % nam3d (medoc % nvar3d))
      ALLOCATE (medoc % nam2d (medoc % nvar2d))
      ALLOCATE (medoc % var3d (medoc % imax, medoc % jmax, medoc % kmax, medoc % nvar3d))
      ALLOCATE (medoc % var2d (medoc % imax, medoc % jmax, medoc % nvar2d))

      ALLOCATE (medoc % height_msl_m  (medoc % imax, medoc % jmax, medoc % kmax))
      ALLOCATE (medoc % terrain_msl_m (medoc % imax, medoc % jmax))


! 2.2 Reset
!     -----

      medoc % sz    = medoc_missing 
      medoc % var3d = medoc_missing 
      medoc % var2d = medoc_missing 

      DO n = 1, medoc % nvar3d
         medoc % nam3d (n) = ""
      ENDDO
      DO n = 1, medoc % nvar2d
         medoc % nam2d (n) = ""
      ENDDO

! 2.2 Skip 2 lines of integers
!     ------------------------

      ! Not used
      READ (UNIT=unitin, FMT=medoc_fmt2,IOSTAT=ier) idum,idum,idum,idum,idum,idum 
      IF (ier /= 0) THEN
          WRITE (*,'(2A)') "Error reading format ",TRIM (medoc_fmt2)
          CALL ABORT
      ENDIF

      ! Not used
      READ (UNIT=unitin, FMT=medoc_fmt2,IOSTAT=ier) idum,idum,idum 

      IF (ier /= 0) THEN
          WRITE (*,'(2A)') "Error reading format ",TRIM (medoc_fmt2)
          CALL ABORT
      ENDIF

! 2.3 Read vertical levels and grid size
!     ----------------------------------

      ! Grid and timing information 
      READ (UNIT=unitin, FMT=medoc_fmt3,IOSTAT=ier)             &
           (medoc % sz(k),k=1, medoc % kmax),                   &
            medoc % dx,  medoc % dy,  medoc % xo,  medoc % yo,  &
            medoc % lat, medoc % lon, medoc % dum, medoc % dum, &
            medoc % dum, medoc % dum, medoc % ztop 

      IF (ier /= 0) THEN
          WRITE (*,'(2A)') "Error reading format ",TRIM (medoc_fmt3)
          CALL ABORT
      ENDIF

! 2.4 Read arrays names and units
!     ---------------------------

      ! Names and units (NREPER+2*NVAR3D+2*NVAR2D values)
      READ (UNIT=unitin, FMT=medoc_fmt1,IOSTAT=ier) &
           (medoc % namdum,  n=1,medoc % nreper),   & 
           (medoc % nam3d(n),n=1,medoc % nvar3d),   &
           (medoc % namdum,  n=1,medoc % nvar3d),   &
           (medoc % nam2d(n),n=1,medoc % nvar2d),   &
           (medoc % namdum,  n=1,medoc % nvar2d) 

      IF (ier /= 0) THEN
          WRITE (*,'(2A)') "Error reading format ",TRIM (medoc_fmt1)
          CALL ABORT
      ENDIF

! 2.5 Skip (for some reasons, this record is absent from then VLAS/VDRAS file)
!     ----

      ! Not used
!     READ (UNIT=unitin, FMT=medoc_fmt3,IOSTAT=ier) &
!          (dum, n=1, 3*medoc % nreper)

!     IF (ier /= 0) THEN
!         WRITE (*,'(2A)') "Error reading format ",TRIM (medoc_fmt3)
!         CALL ABORT
!     ENDIF

! 2.6 Read 3D variables
!     -----------------

      ! 3-D Variables
      DO n = 1, medoc % nvar3d 
         READ (UNIT=unitin, FMT=medoc_fmt3,IOSTAT=ier) &
              (((medoc % var3d(i,j,k,n),i=1, medoc % imax),j=1, medoc % jmax),k=1,medoc % kmax) 
      ENDDO

      IF (ier /= 0) THEN
          WRITE (*,'(2A)') "Error reading format ",TRIM (medoc_fmt3)
          CALL ABORT
      ENDIF

! 2.7 Read 2D variables
!     -----------------

      ! 2-D Variables
      DO n = 1, medoc % nvar2d 
         READ (UNIT=unitin, FMT=medoc_fmt3,IOSTAT=ier) &
              ((medoc % var2d(i,j,n),i=1,medoc % imax),j=1, medoc % jmax) 
      ENDDO

      IF (ier /= 0) THEN
          WRITE (*,'(2A)') "Error reading format ",TRIM (medoc_fmt3)
          CALL ABORT
      ENDIF

! 2.6 Close input file
!     ----------------

      CLOSE (UNIT=unitin)

! 3.  CHECKS AND DIAGNOSTICS
! ==========================
! 
! 3.1 Can only handle spherical coordinates
!     -------------------------------------

      IF (medoc % xo > -999999. .OR. medoc % xo > -999999.) THEN
           WRITE (*,'(/,A)')   "Cannot handle non-spherical coordinates"
           WRITE (*,'(A,F12.4)') "xo = ", medoc % xo
           WRITE (*,'(A,F12.4)') "yo = ", medoc % yo
           CALL ABORT
      ENDIF

! 3.2 Make sure sz, terrain and wind have been loaded
!     ------------------------------------------------

      IF (ANY (medoc % sz < 0)) THEN
          WRITE (*,'(/,A)')   "Cannot find vertical levels data (variable SZ)"
          CALL ABORT
      ENDIF

      DO n = 1, medoc % nvar2d
         IF (medoc % nam2d (n) == "REL" .OR. medoc % nam2d (n) == "TOPO") EXIT
         IF (n == medoc % nvar2d) THEN
             WRITE (*,'(/,A)') "Cannot find terrain data (variable TOPO or REL)"
             CALL ABORT
         ENDIF
      ENDDO

      DO n = 1, medoc % nvar2d
         IF (medoc % nam3d (n) == "U")  EXIT
         IF (n == medoc % nvar3d) THEN
             WRITE (*,'(/,A)') "Cannot find wind data (U variable)"
             CALL ABORT
         ENDIF
      ENDDO

      DO n = 1, medoc % nvar2d
         IF (medoc % nam3d (n) == "V")  EXIT
         IF (n == medoc % nvar3d) THEN
             WRITE (*,'(/,A)') "Cannot find wind data (V variable)"
             CALL ABORT
         ENDIF
      ENDDO

! 3.2 Fill ztop if missing
!     --------------------

      IF (ABS (medoc % ztop) <= 0) THEN
          medoc % ztop = medoc % sz (medoc % kmax)
      ENDIF

! 3.3 Process SWIFT grid only
!     -----------------------

      IF (ABS (medoc % ztop - medoc % sz (medoc % kmax)) > 0) THEN
           WRITE (*,'(/,A)')   "Cannot handle this type of MEDOC file: ztop /= sz(kmax)"
           WRITE (*,'(A,F12.5)') "ztop = ", medoc % ztop
           WRITE (*,'(A,F12.5)') "sz   = ",(medoc % sz (k),k=1,medoc % kmax) 
      ENDIF

! 3.4 Extract terrain data
!     --------------------

      DO i = 1, medoc % nvar2d
         IF (medoc % nam2d (i) == "REL" .OR. medoc % nam2d (i) == "TOPO") THEN
             medoc % terrain_msl_m = medoc % var2d (:,:,i)
             EXIT
         ENDIF
      ENDDO

! 3.5 Compute upper-level heights
!     ---------------------------

      CALL height_medoc (medoc % imax, medoc % jmax, medoc % kmax, &
                         medoc % ztop, medoc % sz, medoc % terrain_msl_m,&
                         medoc % height_msl_m)

! 4.  PRINT-OUT
! =============

      WRITE (*,'(A)')
      WRITE (*,'(A,I2.2,":",I2.2,":",I2.2," on ",I2.2,"/",I2.2,"/",I4.4)') &
            "Data valid at:     ",                                        &
             medoc % ihour,  medoc % imin,  medoc % isec,                  &
             medoc % imonth, medoc % iday,  medoc % iyear
      WRITE (*,'(A,3I4)')       "Grid dimensions: ",medoc % imax, medoc % jmax, medoc % kmax
      WRITE (*,'(A,2(F9.4,A))') "Most South West: ",medoc % lon,"E,  ",medoc % lat,"N."
      WRITE (*,'(A,2(F9.4,A))') "Grid increment:  ",medoc % dx,"dg  ",medoc % dy,"dg"
      WRITE (*,'(A,10(I5,:))') "Height levels:  ",(NINT (medoc % sz(k)),k=1, MIN (10,medoc % kmax))
      IF (medoc % kmax > 10) &
      WRITE (*,'(16X,10(I5,:))') (NINT (medoc % sz(k)),k=11, MIN (20,medoc % kmax))
      IF (medoc % kmax > 20) &
      WRITE (*,'(16X,10(I5,:))') (NINT (medoc % sz(k)),k=21, MIN (30,medoc % kmax))
      IF (medoc % kmax > 30) &
      WRITE (*,'(16X,100(I5,:))') (NINT (medoc % sz(k)),k=31, medoc % kmax)
      WRITE (*,'(A)')

      WRITE (*,'(A,I2,A,22(A,:))') "Found  ",medoc % nvar2d," 2-D Field(s): ",&
                             (medoc % nam2d(n),n=1,medoc % nvar2d)
      WRITE (*,'(A,I2,A,22(A,:))') "Found  ",medoc % nvar3d," 3-D Field(s): ",&
                             (medoc % nam3d(n),n=1,medoc % nvar3d)
      WRITE (*,'(A)')


 END SUBROUTINE read_medoc
!------------------------------------------------------------------------------!

 SUBROUTINE height_medoc (imax, jmax, kmax, ztop, sz, topo, height_msl_m)
!------------------------------------------------------------------------------!
! Construct the 3-dimensional vertical level height above mean sea level
! from the MEDOC file.
!
! See SCIPUFF Documentation section 10.3.2
!------------------------------------------------------------------------------!

      IMPLICIT NONE

      INTEGER, INTENT (in) :: imax, jmax, kmax
      REAL,    INTENT (in) :: ztop

      REAL, DIMENSION (kmax),             INTENT (in)  :: sz 
      REAL, DIMENSION (imax, jmax),       INTENT (in)  :: topo
      REAL, DIMENSION (imax, jmax, kmax), INTENT (out) :: height_msl_m

      INTEGER :: i, j, k
      REAL    :: jj
!------------------------------------------------------------------------------!

      IF (ztop <= 0) THEN
          WRITE (*,'(/,A,F12.5)')   "Error ztop = ",ztop
          CALL ABORT
      ENDIF

      DO j = 1, jmax
         DO i = 1, imax
            jj = 1. - topo (i,j)/ztop  
            DO k = 1, kmax
               height_msl_m (i,j,k) = jj*sz(k) + topo (i,j)
            ENDDO
         ENDDO
      ENDDO
      
  END SUBROUTINE height_medoc
!------------------------------------------------------------------------------!

  SUBROUTINE free_medoc (medoc)
!------------------------------------------------------------------------------!
! Free MEDOC data structure
!------------------------------------------------------------------------------!
      IMPLICIT NONE

      TYPE (medoc_type) :: medoc
!------------------------------------------------------------------------------!

      DEALLOCATE (medoc % sz )
      DEALLOCATE (medoc % nam3d)
      DEALLOCATE (medoc % nam2d)
      DEALLOCATE (medoc % var3d)
      DEALLOCATE (medoc % var2d)

      DEALLOCATE (medoc % height_msl_m )
      DEALLOCATE (medoc % terrain_msl_m)

  END SUBROUTINE free_medoc
!------------------------------------------------------------------------------!
 END MODULE module_medoc
