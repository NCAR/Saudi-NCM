#!/bin/csh -f

#set echo
set timestamp
setenv SUBSYSTEM NESTDOWNSST
setenv RM "rm -rf"
 
#
# ENVIRONMENT
#
set CFILE="$MM5HOME/cycle_code/CONFIG_FILES/cshrc_"
 
$CheckConfigFiles
set cfstat = $status
if ( $cfstat != 0 ) then
 echo "${SUBSYSTEM} -- Missing ConfigFile -> exiting"
 exit (2)
endif
source ${CFILE}user.mm5sys.${MM5HOST};
source ${CFILE}sizes.mm5sys.${MM5HOST}

#
#       $1 comes in as local file name, typically, mmout
#
   set mmout = $1
   set SMONTH = $2 
####################
   set FILNUM = 1
####################
   set LETTERS = (A B C D E F G H I J K L M N O P Q R S T U V W X Y Z)
#  set LETTERS = (a b c d e f g h i j k l m n o p q r s t u v w x y z)
#
#               set up fortran input files for rwv3
#

if ($FILNUM == 1) then
ln -s    $mmout            fort.10
else
set UNIT   = 0
set NUMFIL = 1
while ( $NUMFIL <= $FILNUM )
   set Local = $mmout$LETTERS[${NUMFIL}]
   @ UNIT = 9 + $NUMFIL
 ln -s     $Local         fort.$UNIT
   @ NUMFIL ++
end
endif
  ln -s TMP.DAT   fort.80
  ln -s TEM.DAT   fort.82
#
#               set up fortran output files for rwv3
#
set UNIT   = 0
set NUMFIL = 0
while ( $NUMFIL <= 9 )
   set Local = mmtmp${NUMFIL}
   @ UNIT = 45 + $NUMFIL
#  assign -a $Local             fort.$UNIT
   ln -s     $Local             fort.$UNIT
   @ NUMFIL ++
end
#
cat > cress.f << EOF
      PROGRAM CRETEST
      PARAMETER(MI=250,MJ=250, NSTA=4) 
c     PARAMETER(MI=61,MJ=61, NSTA=4 )            
c     PARAMETER(MI=31,MJ=31, NSTA=4, IDOMAIN=4)     
      REAL A(MI,MJ), ASTA(NSTA),XLAT(NSTA),XLON(NSTA)
      REAL XOBS(NSTA),YOBS(NSTA)
      REAL TSEACL(12,3) 
c     DATA XOBS/35.6,41.8,50.3,39.2/                ! Domain 2
c     DATA YOBS/31.2,9.5,26.5,37.7/                 ! Domain 2
c     DATA XOBS/23.7, 42.4, 67.8, 34.7/             ! Domain 3
c     DATA YOBS/13.5,-51.4, -0.4, 33.0/             ! Domain 3
c     DATA XOBS/-3.0, 53.2,129.5,30.2/              ! Domain 4
c     DATA YOBS/-33.3,-228.3,-75.3,24.9/            ! Domain 4
      DATA XLAT/ 38.90, 36.91, 38.46, 39.5/  
      DATA XLON/-76.44,-75.71,-74.70,-76.0/
      DATA TSEACL/
     &  3.1,3.1,5.5,10.8,16.9,22.4,25.8,25.8,23.2,17.2,11.5,5.8,
     &  6.9,5.9,7.1,11.0,16.1,20.9,24.3,24.6,23.2,19.3,14.9,10.1,
     &  6.1,4.7,5.4, 8.3,13.1,18.9,22.8,23.6,21.6,17.7,13.8,10.0/
      integer bhi(50,20)
      real bhr(20,20)
      character*80 bhic(50,20)
      character*80 bhrc(20,20)
      IDOMAIN=1
      ID = 2
      RIN = 150.0 
c Feb 10, 1998
c     ASTA(1) =  4.7
c     ASTA(2) =  6.5
c     ASTA(3) =  6.4
c May 29, 1999
c     ASTA(1) = 19.4
c     ASTA(2) = 18.3
c     ASTA(3) = 16.7
c June 8, 1999
c     ASTA(1) = 22.8
c     ASTA(2) = 21.0
c     ASTA(3) = 18.5
c July 6, 1999
c     ASTA(1) = 26.8
c     ASTA(2) = 24.0
c     ASTA(3) = 24.0
c Aug. 2, 1999
c     ASTA(1) = 27.5
c     ASTA(2) = 27.4
c     ASTA(3) = 26.0
c Aug. 6, 1999
c     ASTA(1) = 26.5
c     ASTA(2) = 26.2
c     ASTA(3) = 24.4
c July 17, 2001
c     ASTA(1) = 25.5
c     ASTA(2) = 24.5
c     ASTA(3) = 23.5
c Aug 21, 2001
      ASTA(1) = 26.5
      ASTA(2) = 25.0
      ASTA(3) = 23.5
c Mar 8, 2000
c     ASTA(1) =  8.5
c     ASTA(2) =  8.0
c     ASTA(3) =  6.0
c Sep 20, 2000
c     ASTA(1) = 22.5
c     ASTA(2) = 22.7
c     ASTA(3) = 21.3
c Sep 23, 2000
c     ASTA(1) = 22.5
c     ASTA(2) = 22.6
c     ASTA(3) = 21.0
c Sep 29, 2000
c     ASTA(1) = 19.5
c     ASTA(2) = 19.1
c     ASTA(3) = 19.1
c May 30, 2002
c     ASTA(1) = 20.5
c     ASTA(2) = 20.2
c     ASTA(3) = 20.0
c June 8, 2002
c     ASTA(1) = 22.5  !TPLM2
c     ASTA(2) = 21.6  !CHLV2
c     ASTA(3) = 18.2  !44009
c June 25, 2002
c     ASTA(1) = 26.0
c     ASTA(2) = 24.8
c     ASTA(3) = 20.0
c July 19, 2002
c     ASTA(1) = 26.0
c     ASTA(2) = 25.5
c     ASTA(3) = 20.0
c
c test 
c     ASTA(1) = -10
c     ASTA(2) = -20
c     ASTA(3) = -30
c     ASTA(4) = -40
c 10 year monthly mean
      read(*,"(i2)"), imonth
      if(imonth .lt. 1 .or. imonth .gt. 12) then
       print " wrong month in SST  adjusting "
       imonth = 3
      endif
      ASTA(1) = TSEACL(imonth,1)
      ASTA(2) = TSEACL(imonth,2)
      ASTA(3) = TSEACL(imonth,3)
      ASTA(4) = ASTA(1)+0.3*(ASTA(1)-ASTA(2))
      do i=1,mi
      do j=1,mj
      A(I,J)=0
      enddo
      enddo

      read(10, iostat=ierr) flag
      if (flag == 0) then
       read(10,iostat=ier) bhi, bhr, bhic, bhrc
       close(10)
      else 
       print *, "wrong model file"
       stop
      endif
      IDOMAIN = BHI( 13, 1)
      MIX=BHI( 16, 1)
      MJX=BHI( 17, 1)
     
      Do kk = 1, nsta
      call llxy(xlat(kk),xlon(kk),xobs(kk),yobs(kk),bhi,bhr) 
      xobs(kk) = xobs(kk) - 0.5  ! crs point
      yobs(kk) = yobs(kk) - 0.5  ! crs point
      print *, xobs(kk), yobs(kk), ASTA(KK)
      enddo

      if(IDOMAIN.EQ.2) then
      RIN = 30    
      elseif(IDOMAIN.EQ.3) then
      RIN = 60    
      elseif(IDOMAIN.EQ.4) then
      RIN = 180
      endif
      CALL CRESSAN(A,ASTA,XOBS,YOBS,MIX,MJX,NSTA,RIN,1)
      if(IDOMAIN.EQ.4) then
      do j=30,1,-1
      print 110, (IFIX(10*A(J,I)),I=1,30,1)
      enddo
      else
      do j=60,1,-2
      print 110, (IFIX(10*A(J,I)),I=2,60,2)
      enddo
      endif
      print *, '---------------------------------' 
c     write(82) ((A(J,I),J=1,MJX),I=1,MIX)
      write(82) A
      do j=61,1,-1
      enddo
c     do k =1,1
c     RIN=RIN * 2
c     CALL CRESSAN(A,ASTA,XOBS,YOBS,MI,MJ,NSTA,5.5,ID)
c     do j=60,1,-2
c     print 110, (IFIX(A(I,J)),I=2,60,2)
c     enddo
c     print *, '---------------------------------' 
c     if (RIN.LT.0.5) goto 999
c     enddo
 110  format (30I4)
 999  continue
      stop 9999
      end

      SUBROUTINE CRESSAN(A2,ASTA1,XOBS1,YOBS1,IMAX,JMAX,NSTA1,RIN,ID)
C
C   Purpose : Create the gridded fields on mesoscale grids by
C             using the Cressman-type objective analysis technique.
C
C   ASTA(NSTA): Station Data Source
C   XOBS(NSTA): J indices of the source data.
C   YOBS(NSTA): I indices of the source data.
C   NSTA      : Number of data points.
C   IMAX,JMAX : The dimension of the mesoscale domain.
C   RIN       : INFLUENCE RADIUM (in grid units)
C   A2(IMAX,JMAX): Gridded objective analysis field
C   BADVAL: bad observation
C   ID: =1, no first guess field; =2, A2 as first guess.
C
      PARAMETER (MX=250,NSTAT=100000)
      DIMENSION A2(IMAX,JMAX),XOBS1(NSTA1),YOBS1(NSTA1),ASTA1(NSTA1),
     1          COR(MX,MX),SUM(MX,MX),NS(MX,MX)
      DIMENSION XOBS(NSTAT),YOBS(NSTAT),ASTA(NSTAT) 
C
C     OBJECTIVE ANALYSIS TO FILL A GRID BASED ON OBSERVATIONS
C     XOBS AND YOBS ARE X AND Y POSITIONS ON OBSERVATIONS, NOT
C     NECESSARILY GRID POINTS.
C-----GRID LENGTHS IN X AND Y DIRECTIONS ARE UNITY.
C
       NSTA=NSTA1
       DO KK=1,NSTA
       XOBS(KK)=XOBS1(KK)
       YOBS(KK)=YOBS1(KK)
       ASTA(KK)=ASTA1(KK)
       ENDDO
      IF(ID.EQ.2) THEN
       IF(IMAX*JMAX.GT.NSTAT) then
       print *, 'need to increase NSTAT'
       stop 888
       ENDIF
       NSTA=NSTA1+IMAX*JMAX
       KK=NSTA1
       DO I=1,IMAX
       DO J=1,JMAX
       KK=KK+1
       XOBS(KK)=I
       YOBS(KK)=J
       ASTA(KK)=A2(I,J) 
       ENDDO
       ENDDO
      ENDIF
      

      BADVAL=-99.0
      IE     = IMAX
      JE     = JMAX
      NSCAN  = 1
      RIS   = RIN**2
      DO 30 I = 1,IMAX
      DO 30 J = 1,JMAX
      COR(I,J) = 0.0
      SUM(I,J) = 0.0
      NS(I,J)  = 0
   30 CONTINUE
C
C-----BEGIN TO PROCESS THE NSTA OBSERVATIONS:
C
      DO 80 KK = 1,NSTA
      IF (ASTA(KK) .EQ. BADVAL) GO TO 80
C
C-----DEFINE MAX AND MIN I AND J VALUES TO LIMIT THE NUMBER OF POINTS
C-----MUST BE CONSIDERED.
C
      RIOBS = YOBS(KK)
      RJOBS = XOBS(KK)
C
      IF(RJOBS.GT.JMAX+RIN .OR. RJOBS.LT.-RIN .OR.
     1   RIOBS.GT.IMAX+RIN .OR. RIOBS.LT.-RIN) GO TO 80
C
      YMAXI = RIOBS + RIN
      MAXI  = IFIX(YMAXI + 0.99)
      MAXI  = MIN0(MAXI,IE)
C
      YMINI = RIOBS - RIN
      MINI  = IFIX(YMINI)
      MINI  = MAX0(MINI,1)
C
      XMAXJ = RJOBS + RIN
      MAXJ  = IFIX(XMAXJ + 0.99)
      MAXJ  = MIN0(MAXJ,JE)
C
      XMINJ = RJOBS - RIN
      MINJ  = IFIX(XMINJ)
      MINJ  = MAX0(MINJ,1)
C
      DO 70 I=MINI,MAXI
      DO 70 J=MINJ,MAXJ
C
      RX = FLOAT(J) - RJOBS
      RY = FLOAT(I) - RIOBS
      RSQ = RX**2+RY**2
      IF (RSQ.GE.RIS) GOTO 70
C
      WT = (RIS - RSQ)/(RIS + RSQ)
C
C-----SAVE MAX. WEIGHTING FACTOR AND TERRAIN HEIGHT TO CHECK IF GRID
C-----POINT SHOULD BE TREATED AS A LAND OR SEA POINT.
C
      IF (WT.GT.0.0) THEN
         COR(I,J)   = COR(I,J) + WT*ASTA(KK)
         SUM(I,J)   = SUM(I,J) + WT
         NS(I,J)    = NS(I,J) + 1
      ENDIF
  70  CONTINUE
  80  CONTINUE
C
C-----NOW APPLY SUMMED WEIGHTS AND WEIGHTED OBSERVATIONS TO DETERMINE
C-----FIELD VALUE AT I,J POINTS
C
      DO 90 I = 1,IE
      DO 90 J = 1,JE
      IF (NS(I,J) .NE. 0) THEN
         COR(I,J) = COR(I,J)/SUM(I,J)
         A2(I,J)  = COR(I,J)
      ELSE
c       PRINT 26,RIN,I,J
c       STOP 26
      ENDIF
   90 CONTINUE
C
   26 FORMAT(' NO OBSERVATIONS ARE WITHIN RIN=',F7.2,
     1 ' GRID LENGTHS OF I=',I3,' J=',I3)
C-----MAY WANT TO SMOOTH FINAL FIELD A2 HERE
      RETURN
      END

      subroutine llxy(xlat,xlon,x,y,bhi,bhr)
c
c     CALCULATE X AND Y GIVEN LATITUDE AND LONGITUDE.
c
      integer bhi(50,20)
      real    bhr(20,20)
 
      conv = 57.29578
      a = 6370.0
      xlatc = BHR(2,1)
      xlonc = BHR(3,1)
      kproj = BHI(7,1)
      psi1  = BHR(5,1)
      psi2  = BHR(6,1)
      ds    = BHR(9,1)/1000.
      xn    = BHR(4,1)
      imax=(BHI(5,1)-1)*BHI(20,1)+1
      jmax=(BHI(6,1)-1)*BHI(20,1)+1
      imapst=(BHR(10,1)-1)*BHI(20,1)+1
      jmapst=(BHR(11,1)-1)*BHI(20,1)+1
      phi1 = 90.0-psi2
      pole = 90.0
 
      if ( xlatc.lt.0.0 ) then
        phi1 = -90.0-psi2
        pole = -pole
      endif
 
      if (kproj.eq.3) then
c MERCATOR PROJECTION
        C2     = A*COS(PSI1)
        XC     = 0.0
        PHICR  = XLATC/CONV
        CELL   = COS(PHICR)/(1.0+SIN(PHICR))
        YC     = - C2*ALOG(CELL)
        IF (XLAT.NE.-90.) THEN
           XLATR = XLAT/CONV
           CELL = COS(XLATR)/(1.0+SIN(XLATR))
           YY = -C2*ALOG(CELL)
           IF (XLONC.LT.0.0) THEN
             IF (XLON.GT.0.0) XLON=XLON-360.
           ELSE
             IF (XLON.LT.0.0) XLON=360.+XLON
           ENDIF
           XX = C2*(XLON-XLONC)/CONV
        ENDIF
 
      ELSE IF (KPROJ.LE.2) THEN
c LAMBERT-COMFORMAL or POLAR-STEREO PROJECTION
      PHIC = ( POLE - XLATC )/CONV
      PHI1 = PHI1/CONV
      XC = 0.0
      YC = -A/XN*SIN(PHI1)*(TAN(PHIC/2.0)/TAN(PHI1/2.0))**XN
c
c     CALCULATE X,Y COORDS. RELATIVE TO POLE
c
      YLON = XLON - XLONC
      IF(YLON.GT.180) YLON = YLON - 360.
      IF(YLON.LT.-180) YLON = YLON + 360.
      FLP = XN*YLON/CONV
      PSX = ( POLE - XLAT )/CONV
      R = -A/XN*SIN(PHI1)*(TAN(PSX/2.0)/TAN(PHI1/2.0))**XN
      IF ( XLATC.LT.0.0 ) THEN
         XX = R*SIN(FLP)
         YY = R*COS(FLP)
      ELSE
         XX = -R*SIN(FLP)
         YY = R*COS(FLP)
      END IF
      END IF
c
c  TRANSFORM (1,1) TO THE ORIGIN
c
      CENTRI = (IMAX + 1.)/2.0
      CENTRJ = (JMAX + 1.)/2.0
      X = ( XX - XC )/DS + CENTRJ  - jmapst + 1
      Y = ( YY - YC )/DS + CENTRI  - imapst + 1
      RETURN
      end 
EOF
#
cat > rwv3.f << EOF
program rwv3

! This utility program is written in free-format Fortran 90.
!   It requires a Fortran 90 compiler to compile. On a DEC_Alpha
!   machine, type the following to compile:
!   
!   f90 -free -convert big_endian rwv3.f
!
  implicit none
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

  integer :: l
  integer :: i
  integer :: j
  integer :: k

  real, allocatable, dimension(:,:,:,:) :: data

! yliu
  real, allocatable, dimension(:,:,:,:) :: latc
  real, allocatable, dimension(:,:,:,:) :: lonc
  real, allocatable, dimension(:,:,:,:) :: wattem 
  real, allocatable, dimension(:,:,:,:) :: lwmask 

  integer :: ierr, ier

  logical :: newtime = .TRUE.
  logical :: ifwrite = .TRUE.         !yliu
  logical :: lmore = .FALSE.
  integer, external :: iargc
!-------------------------------------
  integer :: ifchsst = 0
!-------------------------------------

  read(*,"(i1)"), ifchsst
  if(ifchsst == 1) ifwrite = .FALSE.
   
  if (ifchsst .eq. 0) then
   print*, 'have to have a argument (1 or 2) for ifchsst'
   stop
  else
   print*, 'ifchsst=',ifchsst
  endif

! print*, 'flnm = ', trim(flnm)
! open(iunit, file=flnm, form='unformatted', status='old', action='read')

  read(iunit, iostat=ierr) flag
  if(ifwrite) write(ounit) flag
  do while (ierr == 0)

     if (flag == 0) then
        read(iunit,iostat=ier) bhi, bhr, bhic, bhrc
        if(ifwrite) write(ounit) bhi, bhr, bhic, bhrc
        if(ier/=0) then
           write(*,'("Error reading big header")')
           call abort()
        endif
!       call printout_big_header(bhi, bhr, bhic, bhrc)
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

        if(ifchsst == 1) then
         if(name(1:8) == 'LATITCRS')  write(80) data
         if(name(1:8) == 'LONGICRS')  write(80) data
         if(name(1:8) == 'LANDMASK')  write(80) data
        else if(ifchsst==2.and.(name(1:8)=='TSEASFC '.or.name(1:8)=='GROUND T')) then
         allocate(latc(end_index(1), end_index(2), 1, 1))
         allocate(lonc(end_index(1), end_index(2), 1, 1))
         allocate(lwmask(end_index(1), end_index(2), 1, 1))
         allocate(wattem(end_index(1), end_index(2), 1, 1))
         read(80) lwmask
         read(80) latc
         read(80) lonc
         read(82) wattem
         print *, name(1:8), "changed with bay T"
         do i = 1,end_index(1)
         do j = 1,end_index(2)
         if(ABS(1.0-lwmask(i,j,1,1)) > 0.1 .and. lonc(i,j,1,1) >= -78.0 &
           .and. (latc(i,j,1,1) > 36.5 .and. latc(i,j,1,1) < 40.2 .and. &
                  lonc(i,j,1,1) > -78. .and. lonc(i,j,1,1) <-74.6) ) then
          if((latc(i,j,1,1) >= 37.5.and.lonc(i,j,1,1) <= -75.6) .or.   &
             (latc(i,j,1,1) >= 38.8.and.lonc(i,j,1,1) <= -74.9)) then
              print *, "test"
              print *, name(1:8), data(i,j,1,1),  wattem(i,j,1,1) 
             data(i,j,1,1) = wattem(i,j,1,1)+273.16
          end if
         end if
         enddo 
         enddo
         deallocate(latc)
         deallocate(lonc)
         deallocate(wattem)
         deallocate(lwmask)
         close(80)
         close(82)
        endif
        
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
     else
        stop
     endif
     read(iunit, iostat=ierr) flag
     if(ifwrite.and.ierr == 0) write(ounit) flag
  enddo

  write(*,'(/,"Hit the end of file of unit ", I3)') iunit

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

EOF
#
#       compile and load
#
   pgf90 cress.f -byteswapio -Wl,-Bstatic -o cress.exe
   echo $SMONTH > input
   $EXECUTABLE_ARCHIVE/cress.exe <input
   echo $SMONTH 
   echo $EXECUTABLE_ARCHIVE/cress.exe
   ls -l  $EXECUTABLE_ARCHIVE/cress.exe
#
#  cft77 rwv3.f
#  segldr rwv3.o -f indef -o rwv3sst.exe
    pgf90 -Mfreeform -byteswapio -Wl,-Bstatic rwv3.f -o rwv3sst.exe 
   echo "1" > input
   $EXECUTABLE_ARCHIVE/rwv3sst.exe  <input
   echo "2" > input
   $EXECUTABLE_ARCHIVE/rwv3sst.exe < input 
#
rm rwv3.o assign.rwv3 fort.*
rm cress.o 
ls -l 
#
#
echo "finished SST replacement "
#
