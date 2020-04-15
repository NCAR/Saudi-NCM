
Program WRF_latlon

!=======================================================================
!BOP=======================================================================
!
! !MODULE: WRF_latlon - Print the lat and lon of the four corners of a WRF grid
!
! !DESCRIPTION:
!
!    This module reads the latitude and longitude arrays from a MM5 or WRF
!    input/output files and write in a file the latitude and longitude of
!    the four corners. Output latitude and longitude are rounded to 
!    the integer immediately greater, so that the actual MM5/WRF domain
!    is encompassed with the rectangle defined by the values of the
!    four corners written out.
!
!    The three dimensions (South/North,West/East,Bottom/Top) and the
!    grid box size in meters are also written in a second file.
!
! !OUPUT: 
!
!  File latlon.txt:
!  min_lat max_lat Format (2(1X,F10.6)
!  min_lon max_lon Format (2(1X,F10.6)
!
!  FILE dimensions.txt:
!  nSW/MIX nWE/MJX nBT/MKX DX (in m) Format (4I6)
!
!
!
! !USAGE:
!
!     WRF_latlon [-h-v-debug] -mm5/wrf MM5_OR_WRF_FILE \
!                [-loc latlon.txt -dim dimensions.txt]
!
! With;
!      MM5_OR_WRF_FILE: Name of the MM5V3 or WRFV2 input/output file to read.
!      latlon.txt:      Min and Max of latitcrs and longicrs.
!      dimensions.txt:  MIX, MJX, MKX and DX in meters.
!      -debug:          Additional print-out.
!       -v:             Print version and exit.
!       -h:             Print this message.
!
!
! !REVISION HISTORY:
!
! 2006-Ap-12 - F. Vandenberghe - first version
!
! !INTERFACE: ----------------------------------------------------------

!=======================================================================

! !USES:
   use WRF_kinds

   IMPLICIT NONE

   CHARACTER (char_long) :: flnm, flnms, flnmd
   LOGICAL :: ldebug, lmm5

   !---------------------------------------------------------------------------
   !--- Parse arguments
   !---------------------------------------------------------------------------

   CALL arguments (flnm, flnms, flnmd, lmm5, ldebug)

   IF (lmm5) THEN
       WRITE (*, '(/,"Read domain boundaries in MM5V3 file:   ",A)') TRIM (flnm)
   ELSE
       WRITE (*, '(/,"Read domain boundaries in WRFV2 file:   ",A)') TRIM (flnm)
   ENDIF

   WRITE (*, '(  "Write Lat and Lon Min and Max in file:  ",A)') TRIM (flnms)
   WRITE (*, '(  "Write dimensions and grid size in file: ",A)') TRIM (flnmd)

   !---------------------------------------------------------------------------
   !--- Process MM5
   !---------------------------------------------------------------------------

   IF (lmm5) THEN

       CALL process_mm5 (flnm, flnms, flnmd, ldebug)

   ELSE
   !---------------------------------------------------------------------------
   !--- Process WRF
   !---------------------------------------------------------------------------

       CALL process_wrf (flnm, flnms, flnmd, ldebug)

   ENDIF

   END PROGRAM WRF_latlon
!=======================================================================

   SUBROUTINE process_wrf (flnm, flnms, flnmd, ldebug)

!=======================================================================
! !USES:

   use WRF_kinds
   use WRF_ncread

!EOP

   implicit none

   CHARACTER (char_long) :: flnm, flnms, flnmd
   LOGICAL :: ldebug

   integer(INT) :: nTimeInd      ! index of matching Times variable
   integer(INT) :: nSN, nWE,nBT  ! north_south, west_east, bottom_top dimensions
   real(R8)     :: Dx            ! Grid box size in meters

   INTEGER :: ierr, ier
   LOGICAL :: lmm5

   LOGICAL :: found_lat  = .FALSE.
   LOGICAL :: found_lon  = .FALSE.
   INTEGER :: latmin, latmax
   INTEGER :: lonmin, lonmax
   REAL    :: xlatmin, xlatmax
   REAL    :: xlonmin, xlonmax

   REAL(R8), ALLOCATABLE :: work4d(:,:,:,:)  ! 4d work array

   INTEGER :: iunit  = 10
   INTEGER :: iunits = 11
   INTEGER :: iunitd = 12

   !---------------------------------------------------------------------------
   !--- Process MM5 file
   !---------------------------------------------------------------------------

   IF (lmm5) THEN
       WRITE (*, '(/,A,/)')  "MM5 read capability not implemented yet!"
       CALL ABORT
   ENDIF

   !---------------------------------------------------------------------------
   !--- Get dimensions from netCDF file
   !---------------------------------------------------------------------------

   CALL get_dimension(flnm, "south_north", nSN)
   CALL get_dimension(flnm, "west_east"  , nWE)
   call get_dimension(flnm, "bottom_top" , nBT)

   IF (ldebug) THEN
       WRITE (*,'(A)') 
       WRITE (UNIT=*, FMT='(3(A,I6,/))') "south_north = ",nSN, &
                                         "west_east   = ",nWE, &
                                         "bottom_top  = ",nBT
   ENDIF

   !---------------------------------------------------------------------------
   !--- Get grid box size in m
   !---------------------------------------------------------------------------

   CALL get_gl_real_att(flnm, "DX", Dx)

   IF (ldebug) THEN
       WRITE (UNIT=*, FMT='(A,F12.5,A)') "dx = ",Dx," meters"
   ENDIF

   !---------------------------------------------------------------------------
   !--- Read in model latitude and longitude: XLAT, XLONG at the desired time
   !---------------------------------------------------------------------------

   ALLOCATE (work4d(nWE, nSN, 1,1))    ! ncread_field4dG uses a 4d array

   ntimeInd = 1

   CALL ncread_field4dG(flnm, "XLAT", rfld=work4d, dim3i=nTimeInd)

   xlatmin = MINVAL (work4d)
   xlatmax = MAXVAL (work4d)


   CALL ncread_field4dG(flnm, "XLONG", rfld=work4d, dim3i=nTimeInd)

   xlonmin = MINVAL (work4d)
   xlonmax = MAXVAL (work4d)

   IF (ldebug) THEN
       WRITE (*,'(A)') 
       WRITE (*,'(2(A,F10.5))') "Min XLAT = ", xlatmin, " Max XLAT = ", xlatmax
       WRITE (*,'(2(A,F10.5))') "Min XLON = ", xlonmin, " Max XLON = ", xlonmax
   ENDIF

   DEALLOCATE (work4d)
 
   found_lat = .TRUE.
   found_lon = .TRUE.

   !---------------------------------------------------------------------------
   !--- Write dimensions and grid box size in m
   !---------------------------------------------------------------------------

   OPEN (iunitd, FILE=flnmd, FORM='formatted', STATUS='replace', &
         ACTION='write', IOSTAT=ier)

   IF (ier/=0) then
       WRITE (*,'(/,"Error cannot write file: ",A,/)') TRIM (flnmd)
       CALL ABORT
   ENDIF

   WRITE (*,'(/,"Output is file: ",A)') TRIM (flnmd)

   WRITE (UNIT=*, FMT='(3I6)') nSN, nWE, nBT
   WRITE (UNIT=*, FMT='(1I6)') NINT (Dx)

   WRITE (UNIT=iunitd, FMT='(3I6)') nSN, nWE, nBT
   WRITE (UNIT=iunitd, FMT='(1I6)') NINT (Dx)

   CLOSE (UNIT=iunitd)


   !---------------------------------------------------------------------------
   !--- Write four corners
   !---------------------------------------------------------------------------

   IF (found_lat .AND. found_lon) THEN

       latmin = FLOOR (xlatmin);
       lonmin = FLOOR (xlonmin);

       LATMAX = CEILING (xlatmax);
       LONMAX = ceiling (xlonmax);

      OPEN (iunits, FILE=flnms, FORM='formatted', STATUS='replace', &
            ACTION='write', IOSTAT=ier)

      IF (ier/=0) THEN
          WRITE (*,'(/,"Error cannot write file: ",A,/)') TRIM (flnms)
          CALL ABORT
      ENDIF

      WRITE (*,'(/,"Output is file: ",A)') TRIM (flnms)

      WRITE (*,'(2I5)') latmin, latmax
      WRITE (*,'(2I5)') lonmin, lonmax

      WRITE (iunits,'(2I5)') latmin, latmax
      WRITE (iunits,'(2I5)') lonmin, lonmax

      CLOSE (iunits)

   ELSE

     WRITE (*,'(/,"Error: did not find latitude and/or longitude array(s) in file: ",A,/)') TRIM (flnm)
     CALL ABORT

   ENDIF

   WRITE (*,'(A)') 

   END SUBROUTINE process_wrf
!=======================================================================

   SUBROUTINE process_mm5 (flnm, flnms, flnmd, ldebug)

!------------------------------------------------------------------------------!

! !USE
  use WRF_kinds

  implicit none

  CHARACTER (char_long) :: flnm, flnms, flnmd
  LOGICAL :: ldebug 

  integer, dimension(50,20) :: bhi
  real*4,  dimension(20,20) :: bhr
  character(len=80), dimension(50,20) :: bhic
  character(len=80), dimension(20,20) :: bhrc
  integer :: iunit  = 10
  integer :: iunits = 11
  integer :: iunitd = 12

  integer :: flag

  integer :: ndim
  real*4  :: time, sample
  integer, dimension(4) :: start_index, end_index
  character (len= 4) :: staggering
  character (len= 4) :: ordering
  character (len=24) :: start_date
  character (len=24) :: current_date
  character (len= 9) :: name
  character (len=25) :: units
  character (len=46) :: description

  real*4, allocatable, dimension(:,:,:,:) :: data

  integer :: ierr, ier

  logical :: newtime = .TRUE.
  logical :: found_lat  = .FALSE.
  logical :: found_lon  = .FALSE.
  integer :: latmin, latmax
  integer :: lonmin, lonmax
  real    :: xlatmin, xlatmax
  real    :: xlonmin, xlonmax

!------------------------------------------------------------------------------!

  open(iunit, file=flnm,  form='unformatted', status='old', action='read',iostat=ier)

  if (ier/=0) then
      WRITE(*,'(/,"Error cannot open file: ",A,/)') TRIM (flnm)
      CALL ABORT
  endif


  read (iunit, iostat=ierr) flag

  if(ier/=0) then
     write(*,'(/,"Error file must begin with flag (0,1,2) information",/)')
     CALL ABORT
  endif

  do while (ierr == 0)

     if (flag == 0) then

        read(iunit,iostat=ier) bhi, bhr, bhic, bhrc

        if(ier>0) then 
           write(*,'("Error reading big header")') 
           CALL ABORT
        elseif(ier<0) then 
           write(*,'("Encountered end of file while reading big header")') 
           exit
        endif

        !  Print out big header if requested

        IF (ldebug) call printout_big_header(bhi, bhr, bhic, bhrc)

        !  Write domains dimension and grid size in m

        open (iunitd, file=flnmd, form='formatted', status='replace', &
              action='write',iostat=ier)

        if(ier/=0) then
           write(*,'(/,"Error cannot write file: ",A,/)') TRIM (flnmd)
           CALL ABORT
        endif

        WRITE (*,'(/,"Output is file: ",A)') TRIM (flnmd)

        WRITE (UNIT=*, FMT='(3I6)') bhi (16,1), bhi (17,1), bhi (12,5)
        WRITE (UNIT=*, FMT='(1I6)') NINT (bhr(9,1))

        WRITE (UNIT=iunitd, FMT='(3I6)') bhi (16,1), bhi (17,1), bhi (12,5)
        WRITE (UNIT=iunitd, FMT='(1I6)') NINT (bhr(9,1))

        CLOSE (UNIT=iunitd)

     elseif (flag == 1) then

        READ (iunit,iostat=ier) ndim, start_index, end_index, time, &
                                staggering, ordering,&
                                current_date, name, units, description

        if(ier>0) then
           write(*,'("Error reading subheader")')
           CALL ABORT
        elseif(ier<0) then
           EXIT
        endif

        if (ldebug) then
            print*, 'ndim: ', ndim
            print*, 'start_index: ', start_index
            print*, 'end_index: ', end_index
            print*, 'time: ', time
            print*, 'staggering: #'//staggering//'#'
            print*, 'ordering: #'//ordering//'#'
            print*, 'date/time: #'//current_date//'#'
            print*, 'name: #'//name//'#'
            print*, 'units: #'//units//'#'
            print*, 'description: #'//description//'#'
        endif
 
        if (newtime) then
!          write(*,'(/,A,2x, I2.2," Hours"/)') &
!          current_date, INT (ANINT(time/60.))
           newtime = .FALSE.
        endif

        if (ndim == 1) then
           allocate(data(end_index(1), 1, 1, 1))
        elseif (ndim == 2) then
           allocate(data(end_index(1), end_index(2), 1, 1))
        elseif (ndim == 3) then
           allocate(data(end_index(1), end_index(2), end_index(3), 1))
        endif

        read(iunit) data

        if (ldebug) then
        if (ndim == 3) then
            sample = data( end_index(1)/2,end_index(2)/2,end_index(3)/2,1 )
        else if (ndim == 2) then
            sample = data( end_index(1)/2,end_index(2)/2,1,1)
        else if (ndim == 1) then
            sample = data( end_index(1)/2,1,1,1)
        end if

        write(*,'(A8,1x,I1,4(1x,I3),1x,A,1x,A," : ", F20.8,1x,A)')&
        name, ndim, end_index(1), end_index(2), end_index(3), end_index(4),&
        staggering, ordering, sample, trim(units)

        endif
        IF (TRIM (name) == 'LATITCRS') THEN
            xlatmin = MINVAL &
            (data(start_index(1):end_index(1)-1,start_index(2):end_index(2)-1,1,1))
            xlatmax = MAXVAL &
            (data(start_index(1):end_index(1)-1,start_index(2):end_index(2)-1,1,1))
            found_lat = .TRUE.
        ENDIF

        IF (TRIM (name) == 'LONGICRS') THEN
            xlonmin = MINVAL &
            (data (start_index(1):end_index(1)-1,start_index(2):end_index(2)-1,1,1))
            xlonmax = MAXVAL &
            (data (start_index(1):end_index(1)-1,start_index(2):end_index(2)-1,1,1))
            found_lon = .TRUE.
        ENDIF

        deallocate(data)

        IF (found_lat .AND. found_lon) EXIT

     ELSEIF (flag == 2) then

        newtime = .TRUE.

     ELSE
        WRITE (*,'("Error unknown flag = ",I6,/)') flag
        CALL ABORT
     ENDIF

     READ (iunit, iostat=ierr) flag

  ENDDO

  CLOSE (iunit)

  IF (ier < 0) WRITE (*,'("Hit the end of file unit: ", I3,/)') iunit


  IF (found_lat .AND. found_lon) THEN

      latmin = floor    (xlatmin);
      lonmin = floor    (xlonmin);

      latmax = ceiling  (xlatmax);
      lonmax = ceiling  (xlonmax);

      open (iunits, file=flnms, form='formatted', status='replace', &
           action='write',iostat=ier)

      if(ier/=0) then
         write(*,'(/,"Error cannot write file: ",A,/)') TRIM (flnms)
         CALL ABORT
      endif

      WRITE (*,'(/,"Output is file: ",A)') TRIM (flnms)

!     WRITE (*,'(2(1X,F10.6))') latmin, latmax
!     WRITE (*,'(2(1X,F10.6))') lonmin, lonmax
      WRITE (*,'(2I5)') latmin, latmax
      WRITE (*,'(2I5)') lonmin, lonmax

!     WRITE (*,'("Format: 2(1X,F10.6)",/)')

!     WRITE (iunits,'(2(1X,F10.6))') latmin, latmax
!     WRITE (iunits,'(2(1X,F10.6))') lonmin, lonmax
      WRITE (iunits,'(2I5)') latmin, latmax
      WRITE (iunits,'(2I5)') lonmin, lonmax


      WRITE (*,'(A)') ""
      
      CLOSE (iunits)

  ELSE

     WRITE (*,'(/,"Error: did not find latitude and/or longitude array(s) in file: ",A,/)') TRIM (flnm)
      CALL ABORT

  ENDIF

  END SUBROUTINE process_mm5
!=======================================================================

   SUBROUTINE printout_big_header(bhi, bhr, bhic, bhrc)
!=======================================================================

     implicit none
     integer, dimension(50,20) :: bhi
     real*4, dimension(20,20) :: bhr
     character(len=80), dimension(50,20) :: bhic
     character(len=80), dimension(20,20) :: bhrc
     integer :: i, j, v3j
   
     write(*,'(/)')
     v3j = bhi(1,1)
     if (bhi(1,1) == 11) v3j = v3j+5
     do j = 1, v3j
      if (j < 8 .or. j>10) then
        if (j == 1) write(*, '("TERRAIN Portion of big header:")')
        if (j == 2) write(*, '(/,"REGRID Portion of big header:")')
        if (j == 3) write(*, '(/,"RAWINS Portion of big header:")')
        if (j == 4) write(*, '(/,"SFC RAWINS Portion of big header:")')
        if (j == 5) write(*, '(/,"INTERP Portion of big header:")')
        if (j == 11) write(*, '(/,"MM5 Portion of big header:")')
        if (j == 6) write(*, '(/,"MM5 Substrate Temp File big header:")')
        if (j == 7) write(*, '(/,"MM5 Boundary File big header:")')
        if (j == 8) write(*, '(/,"Interpolated MM5 Portion of big header:")')
        write(*,'(/,"***Integers:"/)')
        do i = 1, size(bhi,1)
           if (bhi(i,j) /= -999) then
              write(*,'("BHI(",I3,",",I2,"):",I8," : ",A)')&
                   i, j, bhi(i,j),trim(bhic(i,j))
           endif
        enddo
   
        write(*,'(/,"***Floats:"/)')
        do i = 1, size(bhr,1)
           if (bhr(i,j) /= -999.) then
              write(*,'("BHR(",I3,",",I2,"):",F9.2," : ",A)')&
                   i, j, bhr(i,j),trim(bhrc(i,j))
           endif
        enddo
        write(*,'(/)')
      endif
     enddo

   END SUBROUTINE printout_big_header
!=======================================================================

   SUBROUTINE arguments(filein, fileou1, fileou2, lmm5, ldebug)

!=======================================================================

! !USE
     use WRF_kinds

     IMPLICIT NONE

     character(char_long) :: filein
     character(char_long) :: fileou1, fileou2
     logical :: ldebug, lmm5

     character(len=200) :: harg
     integer :: ierr, i, numarg
 

     logical :: present_mm5   = .FALSE., &
                present_wrf   = .FALSE., &
                present_ou1   = .FALSE., &
                present_ou2   = .FALSE., &
                print_version = .FALSE.

!=======================================================================

     filein  = " "
     fileou1 = " "
     fileou2 = " "
     lmm5    = .FALSE.

     numarg = command_argument_count()

     if (numarg <= 0) CALL help

     i = 1

   !---------------------------------------------------------------------------
   !--- Parse arguments
   !---------------------------------------------------------------------------

     DO WHILE ( i <= numarg)

        CALL Getarg(i, harg)

        IF (harg (1:6) == "-debug") THEN
                ldebug = .true.
        ELSEIF (harg (1:2) == "-h") THEN
                call help
        ELSEIF (harg (1:2) == "-v") THEN
                print_version = .true.
        ELSEIF (harg (1:4) == "-mm5" .OR. harg (1:2) == "-i") THEN
                i = i + 1
                call getarg(i, harg)
                filein = TRIM (harg)
                present_mm5 = .TRUE.
                lmm5 = .TRUE.
        ELSEIF (harg (1:4) == "-wrf") THEN
                i = i + 1
                call getarg(i, harg)
                filein = TRIM (harg)
                present_wrf = .TRUE.
        ELSEIF (harg (1:4) == "-loc") THEN
                i = i + 1
                call getarg(i, harg)
                fileou1 = TRIM (harg)
                present_ou1 = .TRUE.
        ELSEIF (harg (1:4) == "-dim") THEN
                i = i + 1
                call getarg(i, harg)
                fileou2 = TRIM (harg)
                present_ou2 = .TRUE.
        ENDIF

           i = i + 1

     ENDDO

   !---------------------------------------------------------------------------
   !--- Print version
   !---------------------------------------------------------------------------

     IF (print_version) THEN
          call getarg(0, harg)
          WRITE (*,'(/,3A,/)') "Program ",TRIM (harg)," version 1.1"
          CALL ABORT
     ENDIF

   !---------------------------------------------------------------------------
   !--- Check presence of arguments ans assign default values
   !---------------------------------------------------------------------------

     IF (.NOT. present_mm5 .AND. .NOT. present_wrf) THEN
          WRITE (*,'(/,A)') "Error: missing arguments -mm5/-wrf"
          CALL help
     ENDIF

     IF (present_mm5 .AND. present_wrf) THEN
          WRITE (*,'(/,A)') "Error: too much arguments must be -mm5 OR -wrf"
          CALL help
     ENDIF

     IF (.NOT. present_ou1) THEN
         fileou1 = "latlon.txt"
     ENDIF

     IF (.NOT. present_ou2) THEN
         fileou2 = "dimensions.txt"
     ENDIF

   END SUBROUTINE arguments

!=======================================================================

   SUBROUTINE help

!=======================================================================

   IMPLICIT NONE
   CHARACTER (len=120) :: cmd

!=======================================================================

     CALL getarg(0, cmd)

     WRITE (*,'(/,3A,/)') "Usage: ",trim(cmd)," [-h-v-debug] -mm5/-wrf input_file [-loc latlon.txt -dim dimensions.txt]"

     WRITE (*,'(A)') " input_file:     Name of the MM5 V3 or WRF V2 input/output file to read."
     WRITE (*,'(A)') " latlon.txt:     Min and Max of latitude and longitude arrays."
     WRITE (*,'(A)') " dimensions.txt: nSW/MIX nWE/MJX nBT/MKX DX (in m)."
     WRITE (*,'(A)') " -debug:         Additional print-out."
     WRITE (*,'(A)') " -v:             Print version and exit."
     WRITE (*,'(A)') " -h:             Print this message."
     WRITE (*,'(A)') " "

     CALL ABORT

!=======================================================================

   END SUBROUTINE help
