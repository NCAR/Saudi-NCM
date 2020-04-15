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
  character(len=120) :: outfile
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

  integer :: l,ii,i,j,k,icount=0

  real, allocatable, dimension(:,:,:,:) :: data, datan
  real, allocatable, dimension(:,:) :: d2,d2n

  integer :: ierr, ier

  logical :: newtime = .TRUE.
  logical :: ifwrite = .TRUE.
  logical :: ifcut   = .TRUE.
  logical :: lmore = .FALSE.
  integer :: IX,JX
  integer :: IXN=165,JXN=165
  real    :: DXO=1.0,DXN=1.0
  integer :: III,JJJ,IID,JJD,IXC,JXC

!  call arguments(flnm, outfile)

! print*, 'flnm = ', trim(flnm)
! open(iunit, file=flnm, form='unformatted', status='old', action='read')

! print*, 'outfile = ', trim(outfile)
! open(iunit, file=outfile, form='unformatted', status='unknown', action='read')

 do ii = 1, INFNU
  write(*,'(/," -- read file at unit ", I3)') iunit
  iunit = iunit + (ii-1)  ! yliu -- input files one by one

  read(iunit, iostat=ierr) flag
  if(ifwrite .and. ii == 1) write(ounit) flag
  do while (ierr == 0)

     if (flag == 0) then
        read(iunit,iostat=ier) bhi, bhr, bhic, bhrc
        if(ier/=0) then
           write(*,'("Error reading big header")')
           call abort()
        endif
        IX=bhi(16,1)
        JX=bhi(17,1)
        write(*,'("IX,JX = ", I3, 2x, I3)') IX,JX
        if(ifcut) then
         IID=IXN/2 ! half s-n width of the subdomain
         JJD=JXN/2 ! half w-e width of the subdomain
         III=IID + 121  ! location of the center of the subdomain in the big domain
         JJJ=JJD   ! location of the center of the subdomain in the big domain
         if(III-IID<0.or.JJJ-JJD<0.or.III+IID>=IX.or.JJJ+JJD>=JX) then
          write(*,'("@@@ subdomain is outside of the main domain @@@")')
          write(*,'("@@@ Please reduce subdomain size or relocate it @@@")')
          write(*,'(," III, IID, JJJ,JJD ", I3,2X,I3,2X,I3,2X,I3)') III,IID,JJJ,JJD 
          stop
         endif
         bhi(40,1) = IID
         bhi(41,1) = JJD
         bhi(16,1) = IXN
         bhi(17,1) = JXN
         bhi(18,1) = bhi(18,1)+(III-IID)/float(bhi(21,1))
         bhi(19,1) = bhi(19,1)+(JJJ-JJD)/float(bhi(21,1))
         bhr(10,1) = bhr(10,1)+(III-IID-0.)/float(bhi(20,1))
         bhr(11,1) = bhr(11,1)+(JJJ-JJD-0.)/float(bhi(20,1))
         bhr(12,1) = bhr(12,1)-(IX-III-IID)/float(bhi(20,1))
         bhr(13,1) = bhr(13,1)-(JX-JJJ-JJD)/float(bhi(20,1))
        endif
        if(ifwrite .and. ii==1) write(ounit) bhi, bhr, bhic, bhrc
        call printout_big_header(bhi, bhr, bhic, bhrc)
     elseif (flag == 1) then

        read (iunit,iostat=ier) ndim, start_index, end_index, time, staggering, ordering,&
             current_date, name, units, description
        if(ier/=0) then
           write(*,'("Error reading subheader")')
           call abort()
        endif

        if(ifcut.and.(ndim.EQ.3.or.ndim.EQ.2)) then
         IX=end_index(1)
         JX=end_index(2)
         end_index(1)=IXN
         end_index(2)=JXN
        endif
        if(ifwrite) write (ounit) ndim, start_index, end_index,&
             time, staggering, ordering, current_date, name, units, description

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
           allocate(data(ix, jx, 1, 1))
           if(ifcut) then
            allocate(datan(end_index(1), end_index(2), 1, 1))
            allocate(d2(ix, jx))
            allocate(d2n(end_index(1), end_index(2)))
           endif
        elseif (ndim == 3) then
           allocate(data(ix, jx, end_index(3), 1))
           if(ifcut) then
            allocate(datan(end_index(1), end_index(2), end_index(3), 1))
            allocate(d2(ix, jx))
            allocate(d2n(end_index(1), end_index(2)))
           endif
        endif

        read(iunit) data
        if(ifcut.and.(ndim.EQ.3.or.ndim.EQ.2)) then
          IXC=III-IID+1
          JXC=JJJ-JJD+1
          DO K=1,end_index(3)
          do I=1,ix
          do J=1,jx
          d2(i,j)=data(i,j,k,1)
          enddo
          enddo
          call INTER2(d2,d2n,IX,JX,IXN,JXN,IXC,JXC,DXO,DXN,1.)
          do I=1,end_index(1)
          do J=1,end_index(2)
          datan(i,j,k,1)=d2n(i,j)
          enddo
          enddo
          enddo
        endif  
        if(ifwrite) then
          if(ifcut.and.(ndim.EQ.3.or.ndim.EQ.2)) then
            write (ounit) datan
          else
            write (ounit) data
          endif
        endif

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
        if(ifcut.and.(ndim.EQ.3.or.ndim.EQ.2)) then
         deallocate(datan)
         deallocate(d2)
         deallocate(d2n)
        endif

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

      SUBROUTINE INTER2(AORI,ANEW,MIO,MJO,MIN,MJN,IXNC,JXNC,DXO,DXN,CR)
!! 
!! From regular grid field AOLD to regular grid field ANEW
!! XX YY are normalized distances.
!! CR: =1.0 for cross point, =0.0 for dot point
!!
      DIMENSION AORI(MIO,MJO), ANEW(MIN,MJN)
! if simple extracting, not to interpolate
      IF(abs(DXN-DXO).LT.1.0E-5) THEN
      DO I=1,MIN
      DO J=1,MJN
      ANEW(I,J)=AORI(I+IXNC-1,J+JXNC-1)
      enddo
      enddo
      RETURN
      ENDIF
! if need interpolating do it
      XX=IXNC-DXN/DXO+CR*(0.5*DXN/DXO-0.5)
      DO I=1,MIN
      YY=JXNC-DXN/DXO+CR*(0.5*DXN/DXO-0.5)
      XX=XX+DXN/DXO
      DO J=1,MJN
      YY=YY+DXN/DXO
      ANEW(I,J)=BINT(XX,YY,AORI,MIO,MJO)
      enddo
      enddo
!     WRITE(51,'(10F8.2)') ANEW
   end subroutine inter2

      FUNCTION BINT(XX,YY,LIST,III,JJJ)
!
! --- BI-LINEAR INTERPOLATION AMONG FOUR GRID VALUES
!     SEE MANNING+HAAGENSON (1992) PAGES 9-10 FOR DETAILS.
!
      REAL LIST(III,JJJ),STL(4,4)
      BINT = 0.0
      N = 0
!c simply fill the value out domain of LIST(III,JJJ)----yliu
!c of course, the best way is to do extrapolation!!!!
      IF(XX.LT.1.0) XX=1.0
      IF(XX.GT.III) XX=III
      IF(YY.LT.1.0) YY=1.0
      IF(YY.GT.JJJ) YY=JJJ
!
      I = INT(XX+1.E-30)
      J = INT(YY+1.E-30)
      X = XX - I
      Y = YY - J
      IF(ABS(X).LT.0.0001.AND.ABS(Y).LT.0.0001) THEN
      BINT = LIST(I,J)
      RETURN
      ENDIF
      DO K = 1,4
         KK = I + K - 2
         DO L = 1,4
            STL(K,L) = 0.
            LL = J + L - 2
            IF(KK.GE.1.and.KK.LE.III.and.LL.GE.JJJ.and.LL.LE.1) THEN
            STL(K,L) = LIST(KK,LL)
            N = N + 1
            IF(STL(K,L).EQ.0.0) STL(K,L) = 1.E-30
            ENDIF
      ENDDO
      ENDDO
      A = ONED(X,STL(1,1),STL(2,1),STL(3,1),STL(4,1))
      B = ONED(X,STL(1,2),STL(2,2),STL(3,2),STL(4,2))
      C = ONED(X,STL(1,3),STL(2,3),STL(3,3),STL(4,3))
      D = ONED(X,STL(1,4),STL(2,4),STL(3,4),STL(4,4))
      BINT = ONED(Y,A,B,C,D)
      IF(N.EQ.16) RETURN
      E = ONED(Y,STL(1,1),STL(1,2),STL(1,3),STL(1,4))
      F = ONED(Y,STL(2,1),STL(2,2),STL(2,3),STL(2,4))
      G = ONED(Y,STL(3,1),STL(3,2),STL(3,3),STL(3,4))
      H = ONED(Y,STL(4,1),STL(4,2),STL(4,3),STL(4,4))
      BINT = (BINT+ONED(X,E,F,G,H))/2.
      RETURN
   end function BINT

      FUNCTION ONED(X,A,B,C,D)
      ONED = 0.
      IF(X.EQ.0.)ONED = B
      IF(X.EQ.1.)ONED = C
      IF(B*C.EQ.0.)RETURN
      IF(A*D.NE.0.) THEN
      ONED = (1.0-X)*(B+X*(0.5*(C-A)+X*(0.5*(C+A)-B)))+X*(C+(1.0-X)*(0.5&
           *(B-D)+(1.0-X)*(0.5*(B+D)-C)))
      ELSE
       ONED = B*(1.0-X)+C*X
       IF(A.NE.0.0)ONED = B+X*(0.5*(C-A)+X*(0.5*(C+A)-B))
       IF(D.NE.0.0)ONED=C+(1.0-X)*(0.5*(B-D)+(1.0-X)*(0.5*(B+D)-C))
      ENDIF
      if(a.ge.0.0.and.b.ge.0.0.and.c.ge.0.0.and.d.ge.0.0.&
           and.oned.lt.0.0)  oned=0.0
      if(a.le.0.0.and.b.le.0.0.and.c.le.0.0.and.d.le.0.0.&
           and.oned.gt.0.0)  oned=0.0

      RETURN
   end function ONED



!
!       compile and load
!
!  cft77 rwv3.f
!  segldr rwv3.o -f indef -o rwv3.exe
!  f90 -f free  rwv3.f -o rwv3.exe
!  f90  -freeform rwv3.f -o rwv3.exe
!   pgf90 -Mfreeform -byteswapio -tp p6 -pc 32 rwv3.f -o rwv3.exe
!  `pwd`/rwv3.exe*
!   rwv3.exe
!endif
!
