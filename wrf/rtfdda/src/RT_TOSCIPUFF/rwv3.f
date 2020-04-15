program rwv3

! This utility program is written in free-format Fortran 90.
!   It requires a Fortran 90 compiler to compile. On a DEC_Alpha
!   machine, type the following to compile:
!   
!   f90 -free -convert big_endian rwv3.f
!
  implicit none
!!! infnu -- # of input files, 
!!! iounu -- # of time levels in a spilt file
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  integer :: INFNU=1, IOUNU=100
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  integer, dimension(50,20) :: bhi
  real, dimension(20,20) :: bhr
  character(len=80), dimension(50,20) :: bhic
  character(len=80), dimension(20,20) :: bhrc
  character(len=120) :: flnm
  integer :: iunit = 10
  integer :: ounit = 45

  integer :: flag

  integer :: ndim
  real :: time, sample
  integer, dimension(4) :: start_index, end_index
  character (len= 4) :: staggering
  character (len= 4) :: ordering
  character (len=24) :: start_date
  character (len=24) :: current_date
  character (len= 9) :: name
  character (len=25) :: units
  character (len=46) :: description

  integer :: l,i,icount=0

  real, allocatable, dimension(:,:,:,:) :: data

  integer :: ierr, ier

  logical :: newtime = .TRUE.
  logical :: ifwrite = .TRUE.
  logical :: lmore = .FALSE.

!  call arguments(flnm, lmore)

  print*, 'flnm = ', trim(flnm)
! open(iunit, file=flnm, form='unformatted', status='old', action='read')

 do i = 1, INFNU
  write(*,'(/," -- read file at unit ", I3)') iunit
  iunit = iunit + (i-1)  ! yliu -- input files one by one

  read(iunit, iostat=ierr) flag
  if(ifwrite .and. i == 1) write(ounit) flag
  do while (ierr == 0)

     if (flag == 0) then
        read(iunit,iostat=ier) bhi, bhr, bhic, bhrc
        if(ifwrite .and. i==1) write(ounit) bhi, bhr, bhic, bhrc
        if(ier/=0) then
           write(*,'("Error reading big header")')
           call abort()
        endif
        call printout_big_header(bhi, bhr, bhic, bhrc)
     elseif (flag == 1) then

        read (iunit,iostat=ier) ndim, start_index, end_index, time, staggering, ordering,&
             current_date, name, units, description
        if(ifwrite) write (ounit) ndim, start_index, end_index,&
             time, staggering, ordering, current_date, name, units, description
        if(ier/=0) then
           write(*,'("Error reading subheader")')
           call abort()
        endif

        if (lmore) then
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
           write(*,'(/,A,2x, F15.5," Hours"/)') current_date, time/60.
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
        if(ifwrite) write (ounit) data

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

        deallocate(data)

     elseif (flag == 2) then
        newtime = .TRUE.
        icount = icount+1    ! yliu -- count time levels
        if(icount == IOUNU) then
        ounit = ounit + 1
        write(*,'(/," -- write to a new file at unit ", I3)') ounit
        write(ounit) 0
        write(ounit) bhi, bhr, bhic, bhrc
        write(ounit) 2
        icount = 0
        endif
     
     else
        stop
     endif
     read(iunit, iostat=ierr) flag
     if(ifwrite.and.ierr == 0) write(ounit) flag
  enddo

  write(*,'(/,"Hit the end of file of unit ", I3)') iunit
 
 enddo

end program rwv3

   subroutine printout_big_header(bhi, bhr, bhic, bhrc)

     implicit none
     integer, dimension(50,20) :: bhi
     real, dimension(20,20) :: bhr
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
   end subroutine printout_big_header
   
   subroutine arguments(v2file, lmore)
     implicit none
     character(len=*) :: v2file
     character(len=120) :: harg
     logical :: lmore
   
     integer :: ierr, i, numarg
     integer, external :: iargc
   
     numarg = iargc()
   
     i = 1
     lmore = .false.
   
     do while ( i < numarg) 
        call getarg(i, harg)
        print*, 'harg = ', trim(harg)
   
        if (harg == "-v") then
           i = i + 1
           lmore = .true.
        elseif (harg == "-h") then
           call help
        endif
   
     enddo
   
     call getarg(i,harg)
     v2file = harg
   end subroutine arguments
   
   subroutine help
     implicit none
     character(len=120) :: cmd
     call getarg(0, cmd)
   
     write(*,'(/,"Usage: ", A, " [-v] v2file ")') trim(cmd)
     write(*,'(8x, "-v     : Print extra info")')
     write(*,'(8x, "v3file : MM5v3 file name to read.")')
     write(*,'(8x, "-h     : print this help message and exit.",/)')
     stop
   end subroutine help

