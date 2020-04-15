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
