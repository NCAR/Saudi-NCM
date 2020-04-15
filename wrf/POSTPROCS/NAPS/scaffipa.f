C$DEBUG
      PROGRAM SCAFFIPA
C           MASTER DRIVER
C-----------------------------------------------------------------------
C           THIS PROGRAM CONTROLS THE FLOW OF PROPAGATION MODEL
C-----------------------------------------------------------------------
      IMPLICIT NONE
C
      include 'interface.inc'
      include 'geometry.inc'
      include 'ground.inc'
      INTEGER ncdir,nodir
      CHARACTER*60 DATADIR,OUTDIR,DBFLNAM,FILMET,FILOUT,met_dsn
      COMMON /filenm/ DATADIR,OUTDIR,DBFLNAM,FILMET,FILOUT,
     1        met_dsn,ncdir,nodir
      REAL    ALTN,TC,RH,WN,WE,WU,WV,WNDSPD,WNDDRT,p_mb,cs,cseff
      integer nlevels
      COMMON /WEATHR/ALTN(maxint),TC(maxint),RH(maxint),WNDSPD(maxint),
     1        WNDDRT(maxint),WN(maxint),WE(maxint),cseff(maxint),
     2        WU(maxint),WV(maxint),cs(maxint),p_mb(maxint),nlevels
      integer KI,KT,MT1,MT2,MT3,MT4,MT8
      COMMON  /LUNT/ KI,KT,MT1,MT2,MT3,MT4,MT8
      DATA    KT/9/ , KI/0/ , MT1/3/ , MT2/4/ , MT3/2/ , MT8/8/
      CHARACTER METLABEL*80, FILLABEL*80, SITE*10
      COMMON /IDTAG/ METLABEL, FILLABEL, SITE
      REAL    freq, src_level, back_level
      REAL    theta, tatt
      REAL    azimuth1, azimuth2, dazimuth
      REAL    XB, YB, ZBAG, WB, RMAX, DELPSI,DELRES
      logical PLOTIT, REPLOT
      logical cntfill
      INTEGER ntheta, IEL, nmdir
      integer nmaxr
      parameter (nmaxr=1024)
      integer ndelsi, nranges, ipsi, output
      real    Aza, rangea, dBa, max_range
      common /savea/  ndelsi, ipsi, nranges(361), Aza(361),
     1        max_range, rangea(nmaxr,361), dBa(nmaxr,361), output
      integer i, j, k, ios
      real    zk, press, temp, ws, wd, csk, heading, alt_agl, xdist
      integer nxg, nyg
c     parameter (nxg=201, nyg=161)
      parameter (nxg=201, nyg=201)
      real    dBxy(nxg,nyg), xg(nxg), yg(nyg)
      CHARACTER*60 METDIR
      REAL    DEGPRAD
      DATA    DEGPRAD/57.29578/
c
c-----------------------------------------------------------------------
c
c     --- Set up airplane orientation.  For now assume airplane
c     --- takes off and climbs along the runway heading without
c     --- deviation.
      heading = 30.  ! deg
      alt_agl = 10.  ! m
      xdist   =  0.  ! distance from break release point
c
c     --- set up the run
      output=0            ! output flag
      PLOTIT=.true.
      REPLOT=.false.
      Zs = alt_agl        ! source ht, m
      Zr = 1.             ! receiver ht, m
      max_range = 1.0E3   ! m
c     max_range = 100.   ! m
      azimuth1 = 0.       ! deg cc from true east
      azimuth2 = 360.      ! deg
      dazimuth = 90.       ! deg
ctk      dazimuth = 10.       ! deg
c
      freq = 1000.        ! Hz
      src_level = 114.3+19.6   ! dB
      back_level = 1.0E-4 ! dB
c     --- INPUT GROUND DATA
      n_ground_layers=1
      Sigma1=366.0        ! top layer flow resistivity cgs
      Om1=0.27            ! top layer porosity
      Pn1=0.5             ! top layer grain shape factor
      Sf1=0.5             ! top layer pore shape factor
      D=0.                ! depth of top layer
      Sigma2=0.           ! bottom layer flow resistivity cgs
      Om2=0.              ! bottom layer porosity
      Pn2=0.              ! bottom layer grain shape factor
      Sf2=0.              ! bottom layer pore shape factor
c
c     --- specify data directory and output directory
c     OUTDIR = '/home/sharman/airport/'
ctk      OUTDIR = 'c:\NCAR\scaffip\scaffip_new\airport\'
      OUTDIR = '/duku/users/hallb/tk/sound/'
      nodir = index(OUTDIR,' ')
      nodir = nodir-1
      print *,'OUTDIR =',OUTDIR
c
c     --- specify the input meteorolgy file
      FILMET='010119980000dulles.dat'
c-----------------------------------------------------------------------
c
c     --- open the output files
      FILOUT = OUTDIR(1:nodir)//'output.dat'     ! unit=KT
      OPEN(UNIT=KT, FILE=FILOUT, STATUS='UNKNOWN', FORM ='FORMATTED',
     1  iostat=ios)
      if(ios.ne.0) then
        print *,'error opening output file:',FILOUT
        stop
      endif
c
      print*,' replot = ', replot

      if(REPLOT) then
        DBFLNAM= OUTDIR(1:nodir)//'decibel.dat'    ! unit=15
        OPEN(UNIT=15, FILE=DBFLNAM, STATUS='OLD', ACCESS='SEQUENTIAL',
     1    FORM ='FORMATTED',iostat=ios)
        if(ios.ne.0) then
          print *,'error opening input file:',DBFLNAM
          stop
        endif
        READ(15,1000) FILLABEL
        READ(15,1000) METLABEL
        READ(15,1001) XB, YB, ZBAG, WB, RMAX, DELPSI,DELRES
        READ(15,*) DELPSI,NDELSI
 
        do j=1,ndelsi
          READ(15,*) Aza(j), nranges(j)
          do i=1,nranges(j)
            READ(15,*) rangea(i,j),dBa(i,j) 
          enddo
        enddo
        close(unit=15)
        go to 600
      endif
c
c     --- initialize save common
      ndelsi=0
      do i=1,nmaxr
        do j=1,361
          rangea(i,j)=0.
          dBa(i,j)=0.
        enddo
      enddo
      do j=1,361
        nranges(j)=0
        Aza(j)=0.
      enddo
c 
c     --- fill the geometry common
      theta1 = azimuth1/DEGPRAD
      theta2 = azimuth2/DEGPRAD
      dtheta = dazimuth/DEGPRAD
      range = max_range
c
c     --- read the input sounding data
      IEL =0
      CALL MET(IEL)
c##### special case of standard atmsophere
c     --- overwrite data with stand atmsophere distribution,
c     --- and constant wind and direction
      do k=1,nlevels
        zk=ALTN(k)
        call STDATM(zk,press,D,Temp,csk)
        p_mb(k) = press/100.
        patm(k) = p_mb(k)/1013.25  ! p in atmospheres
        TK(k)   = temp           ! deg K
        TC(k)=TK(k)-273.15
        RH(k)=0.
        WS=4.           ! m/s
        WD=30.          ! deg (headwind)
        WNDSPD(k) = WS  ! M/S
        WNDDRT(k) = WD
        WE(k) = WS * SIN(WD/DEGPRAD)  ! m/s
        WN(k) = WS * COS(WD/DEGPRAD)  ! m/s
        wspeed(k) = WS                ! m/s
        wtheta(k) = WD/DEGPRAD        ! radians
      enddo
      print *,'overwriting stdatm data'
c##### end special case of standard atmsophere
c
c     -----PRINT THE HEADER FOR THE OUTPUT
      CALL Header(freq)
c
c     --- Scan through azimuths
c     --- For convenience azimuth is measured counterclockwise
c     --- from true EAST
      ntheta = NINT((azimuth2-azimuth1)/dazimuth) + 1
      if(ABS((azimuth2-azimuth1)-360.).LT.1.) ntheta=ntheta-1
      ndelsi = ntheta
      do ipsi=1,ntheta
        theta = theta1 + dtheta*(ipsi-1)
        print *,'in theta loop: theta=',theta*DEGPRAD
        Aza(ipsi)=theta*DEGPRAD
c
c       -----CALCULATE THE SOUND SPEED PROFILE
        call sound(theta)
c
c       --- compute gain as a function of range for this azimuth
        call Ffp3a(freq,src_level,back_level,Tatt)
c
        PRINT 110
        WRITE(6,105) theta
        WRITE(6,100) range,Tatt
        PRINT 110
        write(KT,110)
        WRITE(KT,105) theta
        WRITE(KT,100) range,Tatt
        write(KT,110)
      enddo
c     
      DBFLNAM= OUTDIR(1:nodir)//'decibel.dat'    ! unit=15
      OPEN(UNIT=15, FILE=DBFLNAM, STATUS='UNKNOWN', ACCESS='SEQUENTIAL',
     1  FORM ='FORMATTED',iostat=ios)
      if(ios.ne.0) then
        print *,'error opening output file:',DBFLNAM
        stop
      endif
      WRITE(15,1002) FILLABEL
      METLABEL=FILLABEL
      WRITE(15,1002) METLABEL
      XB=0.
      YB=0.
	ZBAG=0.
      WB=0.
      RMAX=max_range
      DELPSI=dazimuth
      DELRES=0.
      WRITE(15,1001) XB, YB, ZBAG, WB, RMAX, DELPSI,DELRES
      write(15,1003) DELPSI,NDELSI
      do j=1,ndelsi
        WRITE(15,1003) Aza(j), nranges(j)
        do i=1,nranges(j)
          WRITE(15,1004) rangea(i,j),dBa(i,j) 
        enddo
      enddo
      close(unit=15)
      close(unit=KT)
c
  600 continue
      if(PLOTIT) then
        CALL INITPLT
        cntfill = .true.
        CALL INTRP   (dBxy,xg,yg,nxg,nyg)
        CALL PLOTDBXY(dBxy,xg,yg,nxg,nyg,cntfill)
        CALL ENDPLT
ctk        CALL NEWPLT
      endif
 1000 FORMAT(A)
 1002 FORMAT(A80)
 1001 format(7F12.2)
 1003 format(F12.2,I5)
 1004 format(2F14.4)
c
      STOP
c
c     -----LOSS TABLE
  100 FORMAT(4X,F9.2,8(2X,F9.4))
  105 FORMAT(4X,'theta =',F9.2)
C
C     -----A LINE
 110  FORMAT (X,80('-'))
C
5     FORMAT(1A64)
      END
c
      SUBROUTINE Sound(azimuth)
C-----------------------------------------------------------------------
C          CALCULATE THE SOUND SPEED PROFILE
c     --- For convenience, azimuth (assummed input as radians) is
c     --- measured counterclockwise from true EAST
C-----------------------------------------------------------------------
      IMPLICIT NONE
      REAL    azimuth
C
      include 'interface.inc'
      include 'layer.inc'
      include 'geometry.inc'
      REAL    ALTN,TC,RH,WN,WE,WU,WV,WNDSPD,WNDDRT,p_mb,cs,cseff
      integer nlevels
      COMMON /WEATHR/ALTN(maxint),TC(maxint),RH(maxint),WNDSPD(maxint),
     1        WNDDRT(maxint),WN(maxint),WE(maxint),cseff(maxint),
     2        WU(maxint),WV(maxint),cs(maxint),p_mb(maxint),nlevels
      INTEGER i
      REAL    SI, gamma
      REAL*8  PIBY2
      PARAMETER (PIBY2=1.570796326794897D0)
c
C      SI is measured counterclockwise from true EAST
      si = azimuth
c  Compute effective speed of sound for each altitude increment
      DO i=1,n_interfaces
        WU(i)=0.3048*(WE(i)*SIN(SI-PIBY2)-WN(i)*COS(SI-PIBY2))
        WV(i)=0.3048*(WE(i)*COS(SI-PIBY2)+WN(i)*SIN(SI-PIBY2))
C        SV(N)=331.48*SQRT(1.+T(N)/273.15)

C**  Modification done on speed of sound calculation Aug 1991
C  See Wong and Embleton-- Published 7 Jan 1985
        gamma = 0.04833 + (RH(i)/100. - 0.023) *       ! gamma=cp/cv
     &            ( 9.2E-5 + 5.5E-6 * TC(i) + 4.25E-7 * TC(i)**2 ) 
        cs(i) = SQRT(gamma * 8314.34 * (TC(i)+273.15) )
C**
        cseff(i)=SQRT(cs(i)**2-WV(i)**2)+WU(i)
        c(i) = cseff(i)  ! in layer common
      enddo
      RETURN
      END
c
      subroutine ffp3a(freq,src_level,back_level,tatt)
C-----------------------------------------------------------------------
C     FAST FIELD PROGRAM 3.
C-----------------------------------------------------------------------
      IMPLICIT NONE
      REAL    freq, src_level,back_level, tatt
      include 'interface.inc'
      include 'layer.inc'
      include 'geometry.inc'
      include 'gp.inc'
      integer KI,KT,MT1,MT2,MT3,MT4,MT8
      COMMON  /LUNT/ KI,KT,MT1,MT2,MT3,MT4,MT8
      integer nmaxr
      parameter (nmaxr=1024)
      integer ndelsi, nranges, ipsi, output
      real    Aza, rangea, dBa, max_range
      common /savea/  ndelsi, ipsi, nranges(361), Aza(361),
     1        max_range, rangea(nmaxr,361), dBa(nmaxr,361), output
c
      LOGICAL First,Last
      INTEGER I,N,Ipan,Npan,Points,Nyq 
      COMPLEX A,P(2048),Field(2048),Voltge     
      REAL    omega, kmax, kwidth, deltar, deltak, kmin, k, r, d, 
     1        Src, Det, extra, level,gain(2048)
      REAL    Zcre,Zcim,Kcre,Kcim
      LOGICAL SMOOTHF
      data    SMOOTHF/.TRUE./
C
C     -----Pyramid needs Ftype = UNDEF, DOS needs Ftype = UNKNOWN.
      CHARACTER*7 Ftype
      PARAMETER (Ftype = 'UNDEF')
C
C     --- z0 = (1-i)/SQRT(4 Pi)
      COMPLEX z0
      DATA z0 /(0.282094791,-0.282094791)/
      REAL E, twopi
      DATA E /1.E-20/
      DATA TwoPi /6.283185307/
C
C     -----Open output files (.o) and error output files (.e).        
      if(output.eq.1) then
        OPEN (10,FILE = 'wavnum.out',STATUS = 'unknown')
c       OPEN (11,FILE = 'levels.out',STATUS = 'unknown')
      endif
C
      IF (Output.EQ.2) THEN
        OPEN (12,FILE = 'profil.err',STATUS = 'unknown')
        OPEN (13,FILE = 'npan.err',STATUS = 'unknown')
        OPEN (14,FILE = 'clip.err',STATUS = 'unknown')
        OPEN (15,FILE = 'wavnum.err',STATUS = 'unknown')
        OPEN (16,FILE = 'press.err',STATUS = 'unknown')
      ENDIF
C
C     -----Preliminary setup
      omega = Twopi*freq
      Src = zs
      Det = zr
C
C     -----Read in the problem geometry.
      CALL Setup (Npan,Points,Extra)
C
C     -----Impedance and wavenumber of half-spaces #1 & #N.
      CALL Zeff (Freq,Zcre,Zcim,Kcre,Kcim,C(1))
      Zc1 = Cmplx(Zcre,-Zcim)
      Kc1 = Cmplx(Kcre,-Kcim)
      ZcN = Cmplx(1.,0.)
      KcN = Cmplx(1.,0.)
C
C     -----Read in the profile.
      CALL Profil(Src,Det,freq,N)
C
C     -----Squares of intrinsic wave numbers within each layer.
      CALL Wavnum(Extra,N,omega)
C
C     -----Upper cut-off wave number.
      CALL Cutoff(DeltaK,DeltaR,freq,Kmax,Kwidth,NYq,Npan,omega,Points,
     1  Range)
C
C     -----Zero the pressure array.
      DO 10 I = 1,Points
        P(I) = (0.0,0.0)
 10   CONTINUE 

C     -----Write out panel values.
      WRITE (6,100) Kmax,Nyq,Npan,Kwidth,DeltaR,DeltaK
      WRITE (KT,100) Kmax,Nyq,Npan,Kwidth,DeltaR,DeltaK
      IF (Output.ge.1) WRITE (13,100) Kmax,Nyq,Npan,Kwidth,DeltaR,DeltaK
 100  FORMAT ('Kmax =',1PE10.4,'Nyq =',I5,/,'Npan =',I5,/,'Kwidth =',
     >        1PE10.4,/,'DeltaR =',1PE10.4,/,'DeltaK =',1PE10.4)
C
C     -----Overlap and add field from each panel.
      DO 40 Ipan = 1,Npan
C       -----Starting wave number of each panel.
        Kmin = (Ipan-1) * KWidth
C
C       -----Find the pressure field in the transform domain.
        DO 20 I = 1,Points
C         -----Wave number and pressure amplitude.
          K = (I - 1)*DeltaK + Kmin
          A = Voltge(K,N,omega)
          Field(I) = CONJG(z0*A)/SQRT(K + E)
C
C         -----Write out the horizontal wave number spectrum.
          IF (Output.ge.2) THEN
            WRITE(10,*) K,CABS(A)
            WRITE(15,*) K,REAL(A),AIMAG(A)
          ELSE IF (Output.EQ.1) THEN
c           WRITE(10,*) K,CABS(A)
            First = (Ipan.EQ.1) .AND. (I.EQ.1)
            Last  = (Ipan.EQ.Npan) .AND. (I.EQ.Points)
            CALL Smoothp(First,Last,K,CABS(A))
          ENDIF
 20     CONTINUE
C
C       -----Fourier Transform the Wave number Amplitudes.
        CALL Four1(Field,Points,1)
        print *,'after four1 ipan=',ipan
C
C       -----Retrieve the incremental Pressure Amplitudes.
        DO 30 I = 2, Points
C         -----Range and Pressure Amplitude.
          R = (I-1) * DeltaR
          D = SQRT(R**2 + (Src - Det)**2)
          A = CONJG(Field(I))/SQRT(R)*DeltaK*
     >        CEXP(CMPLX(EXTRA * D, -Kmin * R))
c
c         -----Accumulate the total pressure in P(I).
          P(I) = P(I) + A
c        print *,'i,r,d,a,p(i)=',i,r,d,a,p(i)
          IF((Ipan.EQ.Npan).and.(R.gt.Range)) GOTO 40
 30     CONTINUE
 40   CONTINUE
c
c     --- Find gain re 0 db at 1m (transmission loss).
      do i=2, Points
        Gain(i) = 20.0 * ALOG10(CABS(p(i)))
      enddo
c
c     --- smooth the gain
      if(SMOOTHF) then
        call smooth(gain,points)
      endif
c     print *,'after smooth'
c     do i=1, Points
c        print *,'i,gain(i)=',i,gain(i)
c     enddo
c       
c     --- add in the source level and noise floor, then write out the
c     --- levels versus range.
      write(KT,110)
      write(6,110)
      write(KT,*) '     x(m)         gain(dB)'
      write(6,*)  '     x(m)         gain(dB)'
      write(KT,110)
      write(6,110)
 110  FORMAT (X,80('-'))
      do i=2,Points
c        gain(i) = gain(i) + src_level
        if((src_level.gt.0.) .and. (ABS(gain(i)).lt.back_level)) then
          gain(i) = SIGN(back_level,gain(i))
        endif
        tatt = gain(i)
c
c       -----Find level re free field no boundaries.
        R = (I-1) * DeltaR
        D = SQRT(R**2 + (Src - Det)**2)
        Level = 20.0 * ALOG10(CABS(P(I))*D)
c         print *,'i,r,d,p(i),level=',i,r,d,p(i),level
        WRITE(KT,*) R,Gain(i),gain(i)+src_level
        WRITE(6,*)  R,Gain(i),gain(i)+src_level
        nranges(IPSI) = i
        rangea(I,IPSI) = R 
        dBa(I,IPSI) = gain(i)+src_level 
c        WRITE(KT,*) R,p(i),level,Gain(i)
c        WRITE(6,*)  R,p(i),level,Gain(i)
        IF(Output.EQ.2) WRITE(16,*)R,REAL(P(I)),AIMAG(P(I))
        IF(R.ge.Range) GOTO 50
      enddo
   50 CONTINUE
C
      return
      end
c
      SUBROUTINE Zeff(Fr,Zeffre,Zeffim,K1re,K1im,C0)
C-----------------------------------------------------------------------
c      CALCULATES EFFECTIVE GROUND IMPEDANCE .  IT CALLS Z4par 
C      FOR ONE LAYER AND Z2lay FOR TWO LAYERS.
C-----------------------------------------------------------------------
      IMPLICIT NONE
      REAL    fr,Zeffre,Zeffim,K1re,K1im,c0
      include 'ground.inc'
      INTEGER output
      COMMON /Debug/output
      REAL    Ztre,Ztim,K2re,K2im,Zbre,Zbim
C
C     ------FIND IMPEDANCE FOR TOP GROUND INTERFACE
      CALL Z4par(Fr,Sigma1,Sf1,Om1,Pn1,Ztre,Ztim,K1re,K1im,C0)
      Zeffre = Ztre
      Zeffim = Ztim
      IF(n_ground_layers.eq.2) THEN
C
C       -----PUT IN THE CONTRIBUTION FROM THE BOTTOM GROUND INTERFACE
        CALL Z4par(Fr,Sigma2,Sf2,Om2,Pn2,Zbre,Zbim,K2re,K2im,C0)
        CALL Z2lay(Ztre,Ztim,Zbre,Zbim,K1re,K1im,D,Zeffre,Zeffim)
      ENDIF
c
      IF(output.GE.2) THEN
        PRINT*,"Impedance  = (",Zeffre,",",Zeffim,")"
        PRINT*,"Wavenumber = (",K1re,",",K1im,")"
      ENDIF
      RETURN
      END
c
      COMPLEX FUNCTION Admit(Last,V)
C-----------------------------------------------------------------------
C     FINDS ADMITTANCE OF ONE BRANCH OF THE TRANSMISSION LINE.
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER Toggle,Last,I2L,Inc,L
      include 'interface.inc'
      include 'layer.inc'
      include 'gp.inc'
      COMPLEX V,RefL2,RefL,Phase,Y
C
      Toggle = 0
C
C     -----Direction to source from last (+1=Up, -1=Down).
      Inc = ISIGN (1,Isrc-Last)
C
C     -----From interface to section beyond interface (+1=Up, 0=Down).
      I2L = (1+Inc)/2
C
C     -----Admittance of last significant line (p.632).
      Y = Y0(Last)
C
C     -----Step through network to section in front of source. 
      DO 10 L = (Last+Inc), (Isrc+I2L-Inc), Inc
C
C          -----If beyond the receiver, set toggle and voltage.
           IF(L .EQ. Idet+I2L) THEN
             Toggle = 1
             V = (1.0,0.0)
           ENDIF
C
C          -----Electrical phase factor (gamma = J*Kz).
           Phase = CEXP(CMPLX(0.0,-thickness(L)) * Kz(L))
C
C          -----Reflection coefficient at the end of section L (eq. 22).
           RefL = (Y0(L)-Y) / (Y0(L)+Y)
C
C          -----Reflection coefficient at the beginning of section L.
           RefL2 = RefL * Phase**2
C
C          -----Total voltage at the beginning of section L (eq. 21).
           IF (Toggle.EQ.1) V = V * (1+RefL2)/((1+RefL)*Phase)
C
C          -----Admittance at the beginning of section L (eq. 22).
           Y = Y0(L) * (1-RefL2)/(1+RefL2)
C
 10   CONTINUE
C
C     -----Admittance at the source.
      Admit = Y
C
      RETURN
      END
c
      FUNCTION Airab(Fr,T,Rh,P)
C-----------------------------------------------------------------------
C                    AIR ABSORPTION ROUTINE 
C
C      References:
C                   (1)   ANSI STANDARD S1.26-198X
C                   (2)   H.E.Bass. et al, "Absorption of Sound by the
C                           Atmosphere" in 'Physical Acoustics' (1984)
C
C-----------------------------------------------------------------------
      IMPLICIT REAL (A-Z)
C-----------------------------------------------------------------------
C     Variables:
C          Fr   -  Frequency (Hz)
C          T    -  Temperature (deg K)
C          Rh   -  Relative Humidity (%) (0 < Rh < 100)
C          P    -  Pressure (Atm)
C-----------------------------------------------------------------------
C
C     -----Reference Ambient Temperature : 20 deg C
      T0 = 293.15
      T20ovt = T0 / T
C
C     -----Saturation Pressure (Ref 2, p.169, Eq.72)
      Psatpo = 8.422 - 10.05916*T20ovt + 5.023*ALOG10(T20ovt)
      Psatpo = 10. ** Psatpo
C
C     -----Percent Mole Fraction of Water Vapor (Ref 1, p.19, Eq.D10)
      H = Rh * Psatpo / P
C
C     -----Oxygen Relaxation Frequency (Ref 1, p.7, Eq.9)
      Fro = P * (24. + 4.04E4 * H * (0.02+H)/(0.391+H) )
C
C     -----Nitrogen Relaxation Frequency (Ref 1, p.7, Eq.10)
      Frn = P * SQRT(T20ovt) * 
     >     (9. + 280. * H * EXP(-4.170 * (T20ovt**.3333-1.)))
C
C     -----Absorption Coefficient, Alpha (Nepers/m) (Ref 1, p.7, Eq.8)
      Alpha = .01275 * EXP(-2239.1/T) / (Fro+Fr*Fr/Fro)
      Alpha = .10680 * EXP(-3352.0/T) / (Frn+Fr*Fr/Frn) + Alpha
      Alpha = Fr*Fr*(1.84E-11/SQRT(T20ovt)/P + T20ovt**2.5 * Alpha)
C
      Airab = Alpha
C
      RETURN
      END
c
      SUBROUTINE Clip (K,Load,Last,N,omega)
C----------------------------------------------------------------------- 
C     SIMPLIFY T-LINE BY CLIPPING TECHNIQUE OF Lee, et al.
C----------------------------------------------------------------------- 
      IMPLICIT NONE
      REAL    k,omega
      INTEGER L,Last,Toggle,Load,Inc,I2L,N
      include 'interface.inc'
      include 'layer.inc'
      include 'gp.inc'
      REAL    Decay
C
      Last = -1
      Decay = 0.0
      Toggle = 0
C
C     -----If detector not between source and load, set toggle.
      IF ((Idet .GT. Isrc) .EQV. (Isrc .GT. Load)) Toggle = 1
C
C     -----Direction to load from source (+1=Up, 0=Down).
      Inc = ISIGN (1,Load-Isrc)
C
C     -----Conversion from interface to opposite layer (+1=Up, 0=Down).
      I2L = (1+Inc)/2
C
C     -----Step from source to layer nearest load.
      DO 10 L = (Isrc+I2L), (Load-Inc), Inc
C          -----Find admittance and wave number for section L.
           CALL Parms(K,L,N,omega)
C
C          -----Sum the exponential decay coefficients (eq. 17).
           Decay = Decay - AIMAG(Kz(L))*thickness(L)
C
C          -----Check if beyond the detector.
           IF (Toggle.EQ.1) THEN
C               -----If decay too large, replace L with infinite section.
             IF (Decay .GE. Kzmax/2.0) GOTO 20
           ELSE
C               -----If decay too large, V=0 anyway, so return "-1".
             IF (Decay .GE. Kzmax) GOTO 30
C
C            -----If detector is reached, set toggle.
             IF (L+Inc .EQ. Idet+I2L) Toggle = 1
           ENDIF
 10   CONTINUE
C
C     -----Find parameters for the load layer.
      CALL Parms(K, Load, N, omega)
      L = Load
C
C     -----Last significant layer in the network.
 20   Last = L
 30   RETURN
      END
c
      SUBROUTINE Cutoff(DeltaK,DeltaR,f,Kmax,Kwidth,Nyq,Npan,omega,
     1  Points, Range)
C-----------------------------------------------------------------------
C     Upper cutoff wave number,Kmax.
C-----------------------------------------------------------------------
      IMPLICIT NONE
      REAL    DeltaK,DeltaR,f,Kmax,Kwidth,omega,Range
      INTEGER Index,Lmin,Lmax,L,Points,Nyq,Npan
      REAL    Gamma,Differ,Cmin
C
      include 'interface.inc'
      include 'layer.inc'
      include 'gp.inc'
C
      REAL TwoPi
      DATA TwoPi /6.283185307/
C
C     -----Lowest and highest of layers between src and det.
      Lmin = MIN0(Isrc,Idet) + 1
      Lmax = MAX0(Isrc,Idet)
C
C     -----Smallest speed from src to det.
      Cmin = C(Lmin)
      DO 10 L = Lmin,Lmax
        Cmin = AMIN1(Cmin,C(L))
 10   CONTINUE
C
C     -----Total altitude difference between source and detector.
      Differ = 0.0
      DO 20 L = Lmin,Lmax
        Differ = thickness(L)+Differ
 20   CONTINUE
C
C     -----Find Kmax based on an empirical relationship.
      Gamma = 7.5E-3*F + (2.5-6.25E-4*F)/Differ
      Kmax = SQRT(Gamma**2 + (Omega/Cmin)**2)
C
C     -----Determine the number of panels.
      Nyq = INT(Kmax*Range*2.0/Points/TwoPi) + 1
      IF (Npan.EQ.0) Npan = Nyq
      IF (Npan.LT.0) Npan = Nyq * IABS(Npan)
C
C     -----Index of the point nearest "Range" based on Nyquist criteria.
      Index = INT(Range*Kmax/TwoPi/Npan + 0.5) + 1
C
C     -----Adjust Range separations so that the "Index" point is at "Range"
      DeltaR = Range/(Index-1)
C
C     -----Adjust Upper cut-off wave number according to adjusted "DeltaR".
      Kmax = (TwoPi*Npan)/DeltaR
C
C     -----Wave number band-width per panel.
      KWidth = Kmax/Npan
      DeltaK = KWidth/Points
C     
      RETURN
      END
c
      SUBROUTINE Four1(Data,Nn,Isign)
C-----------------------------------------------------------------------
C     Replaces DATA by its discrete Fourier transform, if ISIGN is
C     input as 1; or replaces DATA by NN times its inverse discrete
C     Fourier transform, if ISIGN is input as -1. DATA is a complex
C     array of length NN or, equivalently, a real array of length
C     2*NN. NN must be an integer power of 2 (this is not checked for!).
C-----------------------------------------------------------------------
C     W.H.Press et al., Numerical Recipes (Cambridge Univ., 1986) p.394
C-----------------------------------------------------------------------
      INTEGER Nn,Isign,N,J,I,M,Mmax,Istep
      REAL    Tempr,Tempi
      REAL    Data(2*Nn)
C
C     -----Double precision for trigonometric recurrences.
      REAL*8  Wr,Wi,Wpr,Wpi,Wtemp,Theta
C
      N =2*Nn
      J = 1
C
C     -----This is the bit-reversal section of the routine.
      DO 20 I=1,N,2 
           IF (J.GT.I) THEN
C               -----Exchange two complex numbers
                Tempr = Data(J)
                Tempi = Data(J+1)
                Data(J) = Data(I)
                Data(J+1) = Data(I+1)
                Data(I) = Tempr
                Data(I+1) = Tempi
           ENDIF
           M=N/2
 10        IF ((M.GE.2).AND.(M.LT.J)) THEN
                J = J-M
                M = M/2
                GOTO 10
           ENDIF
           J = J+M
 20   CONTINUE
C
C     -----Here begins the Danielson-Lanczos section of the routine.
      Mmax=2
C
C     -----Outer loop executed log2(NN) times.
 30   IF (N.GT.Mmax) THEN
C
C          -----Initialize for trigonometric recurrence.
           Istep = 2*Mmax
           Theta = 6.28318530717959D0/(Isign*Mmax)
           Wpr = -2.D0*DSIN(0.5D0*Theta)**2
           Wpi = DSIN(Theta)
           Wr = 1.D0
           Wi = 0.D0
C
C          -----Here are the two nested inner loops.
           DO 40 M=1,Mmax,2 
                DO 50 I=M,N,Istep 
C                    -----This is the Danielson-Lanczos formula.
                     J = I+Mmax
                     Tempr = SNGL(Wr)*Data(J)-Sngl(Wi)*Data(J+1)
                     Tempi = SNGL(Wr)*Data(J+1)+SNGL(Wi)*Data(J)
                     Data(J) = Data(I)-Tempr
                     Data(J+1) = Data(I+1)-Tempi
                     Data(I) = Data(I)+Tempr
                     Data(I+1) = Data(I+1)+Tempi
 50             CONTINUE
C
C               -----Trigonometric recurrence.
                Wtemp = Wr
                Wr = Wr*Wpr-Wi*Wpi+Wr
                Wi = Wi*Wpr+Wtemp*Wpi+Wi
 40        CONTINUE
           Mmax=Istep 
           GOTO 30
      ENDIF
      RETURN
      END
c
      SUBROUTINE Header(freq)
C-----------------------------------------------------------------------
C        PRINTS THE HEADER FOR THE OUTPUT
C-----------------------------------------------------------------------
      IMPLICIT NONE
      REAL    freq
C
      include 'interface.inc'
      include 'geometry.inc'
      include 'ground.inc'
      integer KI,KT,MT1,MT2,MT3,MT4,MT8
      COMMON  /LUNT/ KI,KT,MT1,MT2,MT3,MT4,MT8
      INTEGER Int
      CHARACTER*2  Cvar
C
      Cvar = 'R'
C
      write(KT,1070)
C
      write(KT,*) '                     SINGLE FREQUENCIES'
C
C     --- PRINT THE CONSTANT PART OF GEOMETRY
      write(KT,1000)
      write(KT,1002) Zr,Zs,range,Theta1,freq
C
C     -----PRINT THE INTERFACE INFORMATION
      write(KT,1000)
      write(KT,1010)
      DO 60 Int = n_interfaces, 1, -1
        write(KT,1020) Int,Z(Int),Tk(Int),Rhi(Int),patm(Int),
     >                 wspeed(Int),WTheta(Int)
60    CONTINUE
C
C     -----PRINT THE GROUND PARAMETERS
      write(KT,1000)
      IF (n_ground_layers.EQ.1) THEN
C       -----ONE LAYER
        write(KT,1035)
        write(KT,1040) 1,Sigma1,Om1
      ELSE
C       -----TWO LAYERS
        write(KT,1030)
        write(KT,1040) 1,Sigma1,Om1,D
        write(KT,1040) 2,Sigma2,Om2
      END IF
      write(KT,1000)
C
C     -----PRINT FREQUENCY
c      write(KT, 1000
c      write(KT, 1055
c      write(KT, 1050,Cvar,freq
C
      RETURN
C
C     -----A LINE
1000  FORMAT (X,80('-'))
C
C     -----PRINT Zs, Zr, Rhz, AND Theta1
1002  FORMAT (X,' RECEIVER HEIGHT(Zr,m)  = ',F8.1,2X,
     >          ' SOURCE HEIGHT(Zs,m)    = ',F8.1,/,X,
     >          ' RANGE(R,m)             = ',F8.1,2X,
     >          ' BEARING OF SOURCE(deg) = ',F8.1,/,X,
     >          ' FREQUENCY (Hz)         = ',F8.1)
C
C     -----INTERFACES
1010  FORMAT (X,' INTERF   HEIGHT   TEMP   REL_HUM   PRES',
     >          '     WIND_SP    WIND_DIR')
1020  FORMAT (6X,I2,F9.1,F8.2,F9.2,F7.2,F10.1,F11.2,F11.2)
C
C     -----GROUND LAYERS
1030  FORMAT (X,' GROUND     SIGMA     POROSITY     DEPTH') 
1035  FORMAT (X,' GROUND     SIGMA     POROSITY')
1040  FORMAT (X,5X,I2,F10.2,F11.2,F10.3)
C
C     -----FREQUENCIES
1050  FORMAT (7X,A,"(m)",8(5X,F6.1))
1055  FORMAT (10X,'FREQUENCY (Hz)')
C
C     -----SKIP 2 LINES
1070  FORMAT (2(X,/,X))
C
      END
c
      SUBROUTINE Insert (Znew,Z,N,Index)
C-----------------------------------------------------------------------
C     Insert a new layer with top interface height = Znew.
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER N,Index
      INTEGER maxint
      PARAMETER (maxint=70)
      include 'layer.inc'
      REAL    Znew, z(maxint)
      INTEGER i
C
C     -----Find Index of Znew in Z(*) (if it exists).
      do i=1,N-1 
        Index=i
        IF(Z(i).EQ.Znew) GOTO 40
        IF(Z(i).GT.Znew) GOTO 20
      enddo
C
C     -----Shift array Z(*) up to open a slot for Znew.
   20 do i=N,2,-1
        Index=i
        Z(Index+1) = Z(Index)
        C(Index+1) = C(Index)
        Rho(Index+1) = Rho(Index)
        Mu(Index+1) = Mu(Index)
C
C       -----Check if Znew belongs above the next Z().
        IF (Znew.GT.Z(Index-1)) GOTO 30
      enddo
C
C     -----Extend the array length and insert Znew.
 30   N = N+1
      Z(Index) = Znew
C
C     -----Return the Index of Z(*) that yields Znew.
 40   RETURN
      END
c
      SUBROUTINE Parms(K,L,N,omega)
C----------------------------------------------------------------------- 
C     T-LINE PARAMETERS FOR "L" IN THE NETWORK.
C----------------------------------------------------------------------- 
      IMPLICIT NONE
      REAL    K,omega
      INTEGER L,N
      include 'interface.inc'
      include 'layer.inc'
      include 'gp.inc'
C
C     -----Electrical wave number Kz=(-j*gamma), (eq. 4b).
      Kz(L) = CSQRT(Ki2(L)-K**2)
C
C     -----Correct branch forces re(gamma) positive.
      IF (AIMAG(Kz(L)).GT.0.0) Kz(L) = -Kz(L)
C
C     -----Electrical characteristic admittance (eq. 4d).
      Y0(L) = Kz(L)/(Omega*Rho(L))
C
C     -----Admittance for half-spaces.
      IF (L.EQ.1)  Y0(L) = Y0(L)/(Zc1*Kc1)
      IF (L.EQ.N)  Y0(L) = Y0(L)/(ZcN*KcN)
C
      RETURN
      END
c
      SUBROUTINE Profil (Src,Det,f,n)
C-----------------------------------------------------------------------
C     READ IN THE PROBLEM ATMOSPHERIC PROFILE.
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER n
      REAL    Src,Det,f
C
      INTEGER Output
      COMMON /Out/ Output
      include 'interface.inc'
      include 'layer.inc'
      include 'gp.inc'
      REAL    Top, Height
      INTEGER L
      REAL    Airab
C
C     -----Read the problem profile (z increases).
      DO 10 L = 1,n_interfaces
        Rho(L) = 1.2
        Mu(L) = 0.
c       Mu(L) = Airab(F,TK(L),RHI(L),patm(L))
 10   CONTINUE
      n = n_interfaces
C
C     -----Insert source and detector interfaces (lowest first).
      IF(Src.LT.Det) THEN
        CALL Insert(Src,Z,n,Isrc)
        CALL Insert(Det,Z,n,Idet)
      ELSE
        CALL Insert(Det,Z,n,Idet)
        CALL Insert(Src,Z,n,Isrc)
      ENDIF
C
C     -----Convert interface heights to layer thicknesses.
      Top = Z(n-1)
      DO 30 L = 2,n-1
        thickness(L) = Z(L) - Z(L-1)
 30   CONTINUE
C
C     -----Write out the values for confirmation.
      IF (Output.GE.2) THEN
        WRITE (12,*) ' n_layers = ', n
        WRITE (12,*) ' Isrc = ',Isrc, '  Src = ',Src
        WRITE (12,*) ' Idet = ',Idet, '  Det = ',Det
        WRITE (12,*) ' ------------------------------------------'   
        WRITE (12,100)
        WRITE (12,110) n,C(n),Rho(n),Mu(n)
        Height = Top
        DO 40 L = n-1,2,-1
          WRITE (12,120) L,Height,TK(L),C(L),Rho(L),Mu(L)
          Height = Height - thickness(L)
 40     CONTINUE
        WRITE (12,110) 1,C(1),Rho(1),Mu(1) 
      ENDIF
C
 100  FORMAT(1x,'Layer',5x,'Z',8x,'Thick',8x,'C',6x,'Rho',5x,'Mu')
 110  FORMAT(1x,I3,25x,F8.3,3x,F3.1,3x,F6.4)
 120  FORMAT(1x,I3,3x,F8.3,3x,F8.3,3x,F8.3,3x,F3.1,3x,F6.4)
      RETURN
      END
c
      SUBROUTINE Setup(Npan,Points,Extra)
C--------------------------------------------------------------------
C     READS INPUT FILE FOR PROGRAM INPUT VALUES.
C--------------------------------------------------------------------
      IMPLICIT NONE
      REAL    extra
      INTEGER Npan,Points
      include 'geometry.inc'
      include 'interface.inc'
      include 'layer.inc'
      include 'gp.inc'
      INTEGER output
      COMMON /Out/ Output
C
      REAL default_extra_loss
      parameter (default_extra_loss=1.0E-4)
C
C     -----Number of panels and points.
      Npan = -2
      Points = 1024
C
C     -----Extra Loss.
c     IF (Code.EQ.6) THEN
c       OPEN (30,FILE='extra.loss')
c       READ (30,*) Extra
c       CLOSE(30)
c     ELSE
        Extra = default_extra_loss
c     ENDIF
C
C     -----Exponential of Kzmax.
      kzmax = 1.0E8
      Kzmax = ALOG(kzmax)
C
C     -----Output flag.
      Output = 0
C
      RETURN
      END
c
      subroutine smooth(u,n)
      IMPLICIT NONE
      integer n
      real u(n)
      real*8 F0,F1,F2,F3,F4,S0,S1,S2,S3,S4,P1,P2,P3,P4
      integer j,nm1, nm2, nm3, nm4
c
C    SET WEIGHTS OF FILTER
      F0 = .7265625
      F1 = .21875
      F2 =-.109375
      F3 = .03125
      F4 =-.00390625
c
      IF(N.LE.4) RETURN
      NM4=N-4
      NM3=N-3
      NM2=N-2
      NM1=N-1
      P4=U(8)
      P3=U(7)
      P2=U(6)
      S0=U(5)
      S1=U(4)
      S2=U(3)
      S3=U(2)
      S4=U(1)
      U(1)=.5*(S4+S3)
      U(2)=.25*(S4+S3+S3+S2)
      U(3)=.25*(S3+S2+S2+S1)
      U(4)=.25*(S2+S1+S1+S0)
      IF(NM4.LT.5) GO TO 16
      DO 15 J=5,NM4
        P1=P2
        P2=P3
        P3=P4
        P4=U(J+4)
        S0=U(J)
        U(J) = F0*S0+F1*(P1+S1)+F2*(P2+S2)+F3*(P3+S3)+F4*(P4+S4)
        S4=S3
        S3=S2
        S2=S1
        S1=S0
   15 CONTINUE
   16 S1=U(NM3)
      S2=U(NM2)
      S3=U(NM1)
      S4=U(N)
      U(NM3)=.25*(S2+S1+S1+S0)
      U(NM2)=.25*(S3+S2+S2+S1)
      U(NM1)=.25*(S4+S3+S3+S2)
      U(N)=.5*(S4+S3)
c
      RETURN
      END
c
      SUBROUTINE Smoothp (First,Last,Xnew,Ynew)
C----------------------------------------------------------------------
C     Print only (X,Y) pairs necessary to represent a smooth curve.
C     Requires equally spaced x values.
C----------------------------------------------------------------------
      LOGICAL First,Last,Second
      REAL Xold,Yold,Xnew,Ynew,DeltaY,Ynext,Tol
      PARAMETER (Tol = 0.025)
      COMMON /Crv/ Xold,Yold,DeltaY,Ynext,Second
      integer KI,KT,MT1,MT2,MT3,MT4,MT8
      COMMON  /LUNT/ KI,KT,MT1,MT2,MT3,MT4,MT8
C
      IF(First) THEN
C       -----Always print the first point.
        WRITE(10,*) Xnew,Ynew
        Second = .TRUE.
      ELSEIF (Second) THEN
C       -----From the second point we predict the third.
        DeltaY = Ynew - Yold
        Ynext = Ynew + DeltaY
        Second = .FALSE.
      ELSE
C       -----Prediction is OK if it falls within Tol
C            of the actual value.
        IF (ABS(Ynew-Ynext).GT.Tol) THEN
C         -----Prediction failed; Write out the previous pair.
          WRITE(10,*) Xold,Yold
          DeltaY = Ynew - Yold
          Ynext = Ynew + DeltaY
        ELSE
C         ----Prediction succeeded; Predict the next pair.
          Ynext = Ynext + DeltaY
        ENDIF
      ENDIF
C     ----Always print the final pair.
      IF (Last) WRITE(10,*) Xnew,Ynew
C
C     -----Save the newest values.
      Xold = Xnew
      Yold = Ynew
C
      RETURN
      END
c
      COMPLEX FUNCTION Voltge(K,N,omega)
C-----------------------------------------------------------------------
C     FINDS THE VOLTAGE AT Idet DUE TO CURRENT SOURCE Isourc AT Isrc.
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER N,Last1,LastN
      REAL    K,omega
      COMPLEX Isourc,Vsourc,Ysourc,Y1,YN,V,Admit
      include 'interface.inc'
      include 'layer.inc'
      include 'gp.inc'
      INTEGER Output
      COMMON /Out/ Output
C
C     -----Default voltage zero.
      Voltge = (0.0,0.0)
      V      = (0.0,0.0)
C
C     -----Find last significant line toward the N half-space.
      CALL Clip(K,N,LastN,N,omega)
C
C     -----If V=0 already, we are through (return zero volts).
      IF (LastN.NE.-1) THEN
C          -----Find last significant line toward the 1 half-space.
           CALL Clip(K,1,Last1,N,omega)
C
C          -----If V=0 already, we are through (return zero volts).
           IF (Last1.NE.-1) THEN
C               -----Admittance that the source sees looking toward #N.
                YN = Admit(LastN,V)
C
C               -----Admittance that the source sees looking toward #1.
                Y1 = Admit(Last1,V)
C
C               -----Total source admittance (add in parallel).
                Ysourc = Y1+YN
C
C               -----Source current (eq. 14).
                Isourc = 2.0*K/(Omega*Rho(Isrc))
C
C               -----Source voltage (eq. 20).
                Vsourc = Isourc/Ysourc
C
C               -----Detector voltage (eq. 23).
                Voltge = Vsourc/V
           ENDIF
      ENDIF
C
C     -----Write out cut-off layer (if different from load).
      IF(Output.GE.2) THEN
        IF (LastN.NE.N) WRITE(14,*) 'For K =',K,'LastN =',LastN
        IF (Last1.NE.1) WRITE(14,*) 'For K =',K,'Last1 =',Last1
      ENDIF
C
      RETURN
      END
c
      SUBROUTINE Wavnum(Extra,N,omega)
C-----------------------------------------------------------------------
C     SQUARES OF COMPLEX INTRINSIC WAVENUMBERS FOR EACH LINE SECTION
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER N, L
      REAL    Extra,Omega
      include 'interface.inc'
      include 'layer.inc'
      include 'gp.inc'
C
      COMPLEX j
      DATA j/(0.0,1.0)/
C
C     -----Finite thickness slabs.
      DO 10 L = 2,N - 1 
        Ki2(L) = CMPLX(Omega/C(L),-Mu(L)-Extra)**2
 10   CONTINUE
C
C     -----Half-spaces #1 and #N.
      Ki2(1) = ((Omega/C(1))*Kc1 - j*(Extra+Mu(1)))**2
      Ki2(N) = ((Omega/C(N))*KcN - j*(Extra+Mu(N)))**2
C
      RETURN
      END
c
      SUBROUTINE Z4par(Fr,Sigma,Sf,Om,Pn,Zcre,Zcim,Akre,Akim,C0)
C---------------------------------------------------------------------
C          FOUR-PARAMETER APPROXIMATE MODEL FOR GROUND IMPEDANCE
C
C           Reference:
C                     K. Attenborough, "Acoustical Impedance Models
C                        For Outdoor Ground Surfaces", J. Sound Vib
C                        99, 521 - 544 (1985).
C
C          rho0 = 1.21(kg/m^3), gamma = 1.4, Prandtl#= .724
C---------------------------------------------------------------------
      IMPLICIT NONE
      real    Fr,Sigma,Sf,Om,Pn,Zcre,Zcim,Akre,Akim,C0
      complex kb,zc
      real    alpha, beta, x, q2, Kbre, Kbim
      real    gamma, rho0, twopi
      parameter (gamma=1.4 , rho0=1.21)
      DATA TwoPi /6.283185307/
C
      alpha = Twopi*Fr
      beta  = alpha/C0
c
C     -----NOTE: THE FACTOR OF 1000 CONVERTS FROM cgs TO mks
      X = Sf*Sf*(Sigma*1000.)/(rho0*alpha)
C
C     -----q^2 DEFINED AFTER Eq.8, p.524
      Q2 = Om**(-Pn)
C
C     -----kb, DEFINED BY Eq.11, p.524
C          Kb = (1.4 OM)^.5 * [1.126 q2/Om + i X]^.5
C             = [1.576 q2 + 1.4 i Om X ]^.5
c     CALL Zroot (1.5764 *Q2, gamma*Om*X, Kbre, Kbim)
c     print *,'after zroot: kb =',Kbre, Kbim
      kb = CSQRT(CMPLX(1.5764 *Q2, gamma*Om*X))
      Kbre = REAL(kb)
      Kbim = AIMAG(kb)
C
C     -----Zc, DEFINED BY Eq.12, p.524
C          Zc = [(4/3) q2/Om + i X ] / Kb
c     CALL Zdiv ((4./3.)*Q2/Om,X, Kbre,Kbim, Zcre,Zcim)
c     print *,'after zdiv: zc =',Zcre,Zcim
      zc = CMPLX((4./3.)*Q2/Om,X)/kb
      Zcre = REAL(zc)
      Zcim = AIMAG(zc)
C
C     -----Kb HAS NO UNITS, Ak IS Kb * k1
      Akre = beta*Kbre
      Akim = beta*kbim
C
      RETURN
      END
c
      SUBROUTINE Z2lay(Ztre,Ztim,Zbre,Zbim,K1re,K1im,D,Zre,Zim)
C-----------------------------------------------------------------------
C      FIND THE IMPEDANCE FOR ONE OR TWO GROUND LAYERS, See Ref 1,
C      p.527, Eq.15.
C
C          Reference:
C                    K. Attenborough, "Acoustical Impedance Models
C                       For Outdoor Ground Surfaces", J. Sound Vib.
C                       99, 521 - 544 (1985).
C
C-----------------------------------------------------------------------
      IMPLICIT NONE
      real    Ztre,Ztim,Zbre,Zbim,K1re,K1im,D,Zre,Zim
      real    a,b,expb,tanhb,tga,x,y,u,v,r,s,tgre,tgim
      complex tg, Q
C-----------------------------------------------------------------------
C         Zb-iZt tan A+iB         X+iY
C     Z = ---------------- * Zt = ---- * Zt = (R+iS)*Zt
C         Zt-iZb tan A+iB         U+iV
C-----------------------------------------------------------------------
      A = K1re*D
      B = K1im*D
C
C     -----HYPERBOLIC TANGENT OF B (PREVENT OVERFLOW)
      B = AMIN1(85.,B)
      Expb = EXP(B)
      Tanhb = (Expb-1/Expb)/(Expb+1/Expb)
C
C     -----TAN A+iB = (tanA + i tanhB) / (1 - i tanA tanhB)
      Tga = TAN(A)
c     CALL Zdiv(Tga,Tanhb,1.,-Tga*Tanhb,Tgre,Tgim)
      tg = CMPLX(Tga,Tanhb)/CMPLX(1.,-Tga*Tanhb)
      Tgre = REAL(tg)
      Tgim = AIMAG(tg)
C
C     -----PRE-FACTOR, R + i S
      X = Zbre+Ztre*Tgim+Ztim*Tgre
      Y = Zbim-Ztre*Tgre+Ztim*Tgim
      U = Ztre+Zbre*Tgim+Zbim*Tgre
      V = Ztim-Zbre*Tgre+Zbim*Tgim
c     CALL Zdiv(X,Y,U,V,R,S)
      Q = CMPLX(x,y)/CMPLX(u,v)
      R = REAL(Q)
      S = AIMAG(Q)
C
C     -----COMPLEX IMPEDANCE , Z(d)
      Zre = R*Ztre-S*Ztim
      Zim = R*Ztim+S*Ztre
      RETURN
      END
c
      SUBROUTINE MET(IEL)
C-------------------------------------------------------------------
C
C  FUNCTION:Read in the data from the MET PROFILE file.  First read
C  the header lines then read in the actual profile data.
C  Common/DATA/ holds the arrays to be passed to the plotting routines
C  that will plot the various MET profile curves.
C  VARIABLES:
C             YMETRS,ALTN  - Altitude arrays adjusted to MSL (meters)
C             TEMPER,TN    - Temperatures arrays (degrees C.)
C             RELHUM       - Relative humidity arrays (percent)
C             WNDDRT,WD    - Wind direction (met angles, north=0)
C             WNDSPD       - Wind speed (knots)
C             WS           - Wind speed (m/sec)
C             KOUNT        - Number of points in each array.
C             FTPERM       - Feet to  meters.       3.280833
C             DEGPRAD      - radians to degrees C. 57.29578
C             FPSTOKN      - Feet/sec to knots      0.5925
C             IEL          - Flag to indicate elevation data available
C--------------------------------------------------------------------
      include 'interface.inc'
      include 'geometry.inc'
      INTEGER ncdir,nodir 
      CHARACTER*60 DATADIR,OUTDIR,DBFLNAM,FILMET,FILOUT,met_dsn
      COMMON /filenm/ DATADIR,OUTDIR,DBFLNAM,FILMET,FILOUT,
     1        met_dsn,ncdir,nodir
      REAL    ALTN,TC,RH,WN,WE,WU,WV,WNDSPD,WNDDRT,p_mb,cs,cseff
      integer nlevels
      COMMON /WEATHR/ALTN(maxint),TC(maxint),RH(maxint),WNDSPD(maxint),
     1        WNDDRT(maxint),WN(maxint),WE(maxint),cseff(maxint),
     2        WU(maxint),WV(maxint),cs(maxint),p_mb(maxint),nlevels
      integer KI,KT,MT1,MT2,MT3,MT4,MT8
      COMMON /LUNT/ KI,KT,MT1,MT2,MT3,MT4,MT8
      CHARACTER METLABEL*80, FILLABEL*80, SITE*10
      COMMON /IDTAG/ METLABEL, FILLABEL, SITE
      REAL NORTH,KNOTS
      CHARACTER*80 IDLAB1,IDLAB2
      DATA FTPERM/3.280833/,DEGPRAD/57.29578/,FPSTOKN/0.5925/
C
C  Call up a menu to get the user's options for met data input to the
C  model.  The data file will be opened on Unit 3 which is used as the
C  input unit throughout the program. (thus no file name necessary here.
      OPEN( UNIT = MT3, FILE = FILMET, STATUS ='OLD', IOSTAT=IOS )
      write(kt,*) 'opening FILMET=',FILMET
      if(ios.ne.0) then
        print *,'error opening input metfile=',FILMET
        stop
      endif
c
C "KOUNT" IS THE COUNTER FOR THE ARRAYS HAVING THE MET DATA
crds  KOUNT = 1
      KOUNT = 0
      II = 0

C  Read in the file id label and the surface conditions.
      READ(MT3,1111)FILLABEL
      print *, 'FILLABEL=',FILLABEL
      READ(MT3,1111)IDLAB1
      print *, 'IDLAB1=',IDLAB1
 1111 FORMAT (A)
      READ(MT3,*)SURFHGT, SFPRES
      print *, 'SURFHGT, SFPRES=',SURFHGT, SFPRES
c following is an attempt to make sure the pressure is really a pressure,
c as we have had 2 different formats for this line...
        if (sfpres .lt. 500.0) then                !bogus looking pressure?
          backspace(mt3)                   !yes. go back and try other format
          read(mt3,*) surfhgt,sftemp,sfrh,sfwspd,sfwdir,sfpres
        end if
      READ(MT3,1111) IDLAB2
      print *, 'IDLAB2=',IDLAB2
c
c     --- write data to output file
      WRITE(KT,1111) FILLABEL
C     WRITE(KT,1111) IDLAB1
C     WRITE(KT,*)SURFHGT, SFPRES
      WRITE(KT,*) 'SURFHGT(M) =',SURFHGT,' SFPRES(MB) =',SFPRES
      WRITE(KT,1112) 
 1112 FORMAT('  LINE  MSL(m)     T(C)    RH(%)   WSPD(m/s)',
     1  '  WDD    WE(ft/s)  WN(ft/s)  P(mb)')
c
C  If elevation data not available (IEL = 0) do not adjust the surface altitude.
C  But if the elevation data has been read in:
C  Set the surface height to the first reading of the elevation data.
C  ALTN(1) was previously assigned the value of ELEV(1)
      IF( IEL .EQ. -1 ) SURFHGT = ALTN(1)        

C  Initialize altitude accumulator with surface height.
      HEIGHT=SURFHGT

C   Read set of input data (id number,altitude,temperature ordinate,
C   relative humidity ordinate, wind speed, wind direction )
C
  700 CONTINUE
C
      do kread=1,maxint
        READ(MT3,*,END=5000)NBR,AGLALT,t,RO,WS,WD,PRESS
        HEIGHT = AGLALT + SURFHGT
        KNOTS  = WS * FTPERM * FPSTOKN
        EAST   = WS * SIN( WD/DEGPRAD )  ! m/s
        NORTH  = WS * COS( WD/DEGPRAD )  ! m/s
C
C  Put met data in arrays in common /data/ to be displayed
        KOUNT = KOUNT + 1
cc      YMETRS(KOUNT) = HEIGHT
cc      TEMPER(KOUNT) = t
cc      RELHUM(KOUNT) = RO
c       WNDSPD(KOUNT) = KNOTS
        WNDSPD(KOUNT) = WS  ! M/S
        WNDDRT(KOUNT) = WD  ! deg
C
C  Fill the Weathr common with the read in data 
        II=II+1
c       --- make heigths relative to surface
c        ALTN(II)=HEIGHT
        ALTN(II)=AGLALT
        TC(II)=t        ! deg C
        RH(II)=RO
        WE(II)=EAST     ! m/s
        WN(II)=NORTH    ! m/s
        p_mb(II) = press
C
C  Fill the interface common with the read in data 
        z(II)      = AGLALT         ! m
        patm(II)   = press/1013.25  ! p in atmospheres
        TK(ii)     = t + 273.15     ! deg K
        wspeed(ii) = WS             ! m/s
        wtheta(ii) = WD/DEGPRAD     ! radians
        nlevels=II
        n_interfaces=ii
c
c       --- write data to output file
        WRITE(KT,3000) II,HEIGHT,t,RO,WS,WD,EAST,NORTH,press
        WRITE(* ,3000) II,HEIGHT,t,RO,WS,WD,EAST,NORTH,press
c
      enddo
 5000 CONTINUE
 3000 format(i5,f9.1,6f9.2,f11.2)
c
      RETURN
      END
c
      subroutine INM(xdist,heading,alt_agl,dBxy,xg,yg,nxg,nyg)
c----------------------------------------------------------------------
c     --- This subroutine provides airplane noise computations based on
c     --- algorithms specified in SAE-AIR 1845.  Specific computations
c     --- are based on two engine jet aircraft given in Appendix E.
c----------------------------------------------------------------------
      implicit none
      real    xdist,heading,alt_agl
      integer nxg, nyg
      real    dBxy(nxg,nyg), xg(nxg), yg(nyg)
      integer ix, jy, i, j, NP, NX
      real    xlg, ylg, dxg, dyg, dBxymax, dBxymin, delx, dely, r, rsq,
     1        theta, Azj, Ps, sg, vtg_min, vtg, V, deltaV, beta, Gb, Gr,
     2        Gbr, deltaL, atheta, LAEj, LAEjp1, LAEPD, rp, rx
      real    PA(20), XA(20), dB(20,20)
      integer nmaxr
      parameter (nmaxr=1024)
      integer ndelsi, nranges, ipsi, output
      real    Aza, rangea, dBa, max_range
      common /savea/  ndelsi, ipsi, nranges(361), Aza(361),
     1        max_range, rangea(nmaxr,361), dBa(nmaxr,361), output
      real*8     DEGREES_PER_RADIAN 
      parameter (DEGREES_PER_RADIAN = 57.29577951308232D0 )
c
c     --- Build a 201X201 rectangular grid.  This grid follows the 
c     --- convention of the AIR and places the origin at the brake
c     --- release point, with x axis along the runway, y to the right
c     --- and z upward.
c
c     --- Case 1.  Start of takeoff roll.
      xlg=-max_range
      ylg=-max_range
      dxg=2.*max_range/FLOAT(nxg-1)
      dyg=dxg
      do ix=1,nxg
        xg(ix)=(ix-1)*dxg + xlg
      enddo
      do jy=1,nyg
        yg(jy)=(jy-1)*dyg + ylg
      enddo
      dBxymax = -1.0E30
      dBxymin = +1.0E30
      do ix=1,nxg
        do jy=1,nyg
          dBxy(ix,jy) = -1.
          delx = xg(ix)
          dely = yg(jy)
          rsq = delx**2 + dely**2
          r = SQRT(rsq)
          if(ABS(r).LT.80.) then
            dBxy(ix,jy) = 150.
            go to 603
          endif
          theta = ATAN2(dely,delx)
          Azj=theta*DEGREES_PER_RADIAN
          if(Azj.lt.0.) Azj=Azj+360.
c         --- Assign aircraft performance parameters
          Ps = 8519.    ! power setting, lbs
          sg = 4408./3.208  ! equivalent takeoff ground roll dist, m
          vtg_min = 32.  ! minimum ground speed, kts
c         --- compute speed adjustment for duration, dB
          vtg = 0.      ! ground speed, kts
          V = SQRT(vtg_min**2 + (vtg**2-vtg_min**2)*xdist/sg)
          deltaV = 10.*ALOG10(160./V) ! dB
          beta = 0.     ! elevation angle, deg
c         --- get lateral attenuations
          if(beta.lt.60.) then  ! deg
            Gb = 3.96 - 0.66*beta + 9.9*EXP(-0.13*beta)  ! dB
          else
            Gb = 0.  ! dB
          endif
          if(r.lt.914.) then  ! m
            Gr = 15.09*(1.-EXP(-0.00274*r))  ! dB
            Gbr = Gr*Gb/13.86
          else
            Gbr = Gb  ! dB
          endif
c         --- directivity adjustment on ground
          deltaL = 0.
          if(beta.le.0.) then  ! deg
            if(xg(ix).le.0.) then ! compute only behind aircraft
              atheta=ABS(Azj)
              if(atheta.le.148.4) then
                deltaL = 51.44 - 1.533*atheta + 0.015147*atheta**2 -
     1                   4.7173E-5*atheta**3			  	 
              else
                deltaL =339.18 - 2.5802*atheta - 4.5545E-3*atheta**2 +
     1                  4.4193E-5*atheta**3			  	 
              endif
            endif
          endif
c         --- Use interpolation into the NPD data
c         --- isolate indices corresponding to distance and power
          call locate(XA, NX, r, j)
          call locate(PA, NP, Ps, i)
c         --- use linear interpolation for power at dist j
          rp = (Ps-PA(i))/(PA(i+1)-PA(i))
          LAEj = dB(i,j) + rp*(dB(i+1,j)-dB(i,j))
          LAEjp1 = dB(i,j+1) + r*(dB(i+1,j+1)-dB(i,j+1))
c         --- use logarithmic interpolation for distance
          rx = (ALOG10(r)-ALOG10(XA(j)))/(ALOG10(XA(j+1))-ALOG10(XA(j)))
          LAEPD = LAEj + rx*(LAEjp1-LAEj)
c         --- add the various contributions
          dBxy(ix,jy) = LAEPD + deltaV - Gbr + deltaL
  603     continue
c         print *,'ix,jy,r,theta,dB=',ix,jy,r,Azj,dBxy(ix,jy)
          dBxymax = MAX(dBxy(ix,jy),dBxymax)
          dBxymin = MIN(dBxy(ix,jy),dBxymin)
        enddo
      enddo
      print *,'in INM: dBxymax,dBxymin=',dBxymax,dBxymin
c      do j=1,nyg
c        write(6,*) (dBxy(i,j),i=1,nxg)
c      enddo
      return
      end
c
      subroutine PLOTDBXY(dBxy,xg,yg,nxg,nyg,cntfill)
c     --- uses ncar graphics to contur xy grid
      implicit none
      integer nxg,nyg
      real    dBxy(nxg,nyg),xg(nxg),yg(nyg)
      logical cntfill
      integer ICLORS,NCONTS
      real    CONTRS
      COMMON /CLRMAP/ ICLORS(20),CONTRS(51),NCONTS
      INTEGER ncdir,nodir
      CHARACTER*60 DATADIR,OUTDIR,DBFLNAM,FILMET,FILOUT,met_dsn
      COMMON /filenm/ DATADIR,OUTDIR,DBFLNAM,FILMET,FILOUT,
     1        met_dsn,ncdir,nodir
      character*20 text
      logical white_backgrnd
      real    X1, Y1, X2, DX, DY, Y2, UL, UR, UB, UT, cmx, cmn, cnt,
     1        xint, yint     
      integer iset, ihi, idash, ncl, ic, mjrx, mjry, mnrx, mnry, ixlab,
     1        iylab, igph, lc , l, ls
      real    xx1,xX2,yY1,yY2,xUL,xUR,yUB,yUT, TRXB, TRYB
c###### new code for NCAR graphics fill routines ######
      external fillc
      integer lwrk,liwk
      integer lmap
      integer nwrk, ngrps
      parameter (lwrk=2500, liwk=1000)
      parameter (lmap=500000)
      parameter (nwrk=250000,ngrps=100)
      real    rwrk(lwrk)
      integer iwrk(liwk)
      integer map(lmap)
      integer iarea(ngrps), igrp(ngrps)
      real    xwrk(nwrk), ywrk(nwrk)
c######################
c
      white_backgrnd = .true.
c     --- use ncar graphics contouring facility
      X1=0.075
      Y1=0.075
      X2=(1.-X1)
      DX=X2-X1
      DY=DX
      Y2=Y1+DY
      UL=1.0
      UR=FLOAT(nxg)
      UB=1.0
      UT=FLOAT(nyg)
      print *,'calling set: UL,UR,UB,UT=',UL,UR,UB,UT
      print *,'             X1,X2,Y1,Y2=',X1,X2,Y1,Y2
      call set(X1,X2,Y1,Y2,UL,UR,UB,UT,1)
      print*,' white_backgrnd = ',white_backgrnd

      if(white_backgrnd) then
        CALL COLOR(0)    ! black text
        CALL GSPLCI(0)   ! black lines
ctk!no!        CALL COLOR(1)    ! black text
ctk!no!        CALL GSPLCI(1)   ! black lines
c           --- flush the background white
ctktk            call wflush
      else
ctk!no!        CALL COLOR(0)    ! white text
ctk!no!        CALL GSPLCI(0)   ! white lines
        CALL COLOR(1)    ! color text
        CALL GSPLCI(1)   ! color lines
        CALL GSPMCI(1)   ! color polymarker

      endif

corig      if(white_backgrnd) then
corig        CALL COLOR(1)    ! black text
corig        CALL GSPLCI(1)   ! black lines
corig      else
corig        CALL COLOR(0)    ! white text
corig        CALL GSPLCI(0)   ! white lines
corig      endif
c
      CALL LEVELQ

c      CALL REDFIN(RMAX)
c      CALL MPCNVT
c      call setusv('LW',1000)
c     call frame
c      CALL BLASTMAP
      iset=1  ! viewport already set
      ihi=-1  ! don't label data or show H or L
      idash = -155  ! solid for + values, dashed for - values
c      idash = +1  ! solid
      cmx = CONTRS(NCONTS)
      cmn = CONTRS(1)
      cnt = CONTRS(2)-CONTRS(1)
      ncl = NINT((cmx-cmn)/cnt)+1
      print *,'cmx,cmn,cnt,ncl=',cmx,cmn,cnt,ncl
c     call cpsetc('ILT',' ') ! suppress info label
      call cpseti('LLP',0)   ! suppress contour labels
      call cpsetr('CIS',cnt)   ! contour interval specifier
      call cpsetr('CMN',cmn)   ! contour minimum
      call cpsetr('CMX',cmx)   ! contour maximum
      call cpseti('CLS',ncl)   ! draw ncl contours
c     call cpseti('CLS',-10)   ! draw ncl contours
      call cpseti('LIS',1)     ! label interval specifier
c.      call cpseti('SET',iset)  ! call set flag - 1=no
c########  contours lines
      if(.not.cntfill) then
        call setusv('LW',2000)
c       if(white_backgrnd) then
c         --- flush the background white
c         call wflush
c       endif
c       --- draw contours lines each of a color specified in the 
c       --- dbcolor file

        print*,' ***  nconts = ',nconts

        do ic=1,NCONTS
          CALL GSPLCI(ICLORS(ic))
c          print*,' ICLORS(ic) = ',ICLORS(ic)

          cmn = CONTRS(ic)
          cnt = CONTRS(2)-CONTRS(1)
          cmx = cmn + cnt/2.

          print*,' cmn,cmx, cnt = ',cmn,cmx, cnt

          call cpcnrc(dBxy,nxg,nxg,nyg,cmn,cmx,cnt,iset,ihi,idash)
        enddo
        call setusv('LW',1000)
c######### contour fill
      else
c       --- use input contour interval, max,min
        cmx = CONTRS(NCONTS)
        cmn = CONTRS(1)
        cnt = CONTRS(2)-CONTRS(1)
        ncl = NINT((cmx-cmn)/cnt)+1
c       print *,'cmx,cmn,cnt,ncl=',cmx,cmn,cnt,ncl
        call arinam(map,lmap)
cc      print *,'in PLOTDB2: calling cprect: nxg,nyg=',nxg,nyg
        call cprect(dBxy,nxg,nxg,nyg,rwrk,lwrk,iwrk,liwk)
        call setusv('LW',1000)   ! light lines
        call getset(xx1,xX2,yY1,yY2,xUL,xUR,yUB,yUT,l)
cc    print *,'in PLOTDB2: calling cpcldr: X1,X2,Y1,Y2,UL,UR,UB,UT=',
cc   1            xx1,xX2,yY1,yY2,xUL,xUR,yUB,yUT
        call set(X1,X2,Y1,Y2,UL,UR,UB,UT,1)
        call getset(xx1,xX2,yY1,yY2,xUL,xUR,yUB,yUT,l)
cc    print *,'in PLOTDB2: calling cpcldr: X1,X2,Y1,Y2,UL,UR,UB,UT=',
cc   1            xx1,xX2,yY1,yY2,xUL,xUR,yUB,yUT
cc    print *,'in PLOTDB2: calling cpcnrc: cmn,cmx,cnt=',cmn,cmx,cnt
        call cpcnrc(dBxy,nxg,nxg,nyg,cmn,cmx,cnt,iset,ihi,idash)
        call cpgeti('SET',iset)
        call getset(xx1,xX2,yY1,yY2,xUL,xUR,yUB,yUT,l)
cc    print *,'in PLOTDB2: calling cpcldr: X1,X2,Y1,Y2,UL,UR,UB,UT=',
cc   1            xx1,xX2,yY1,yY2,xUL,xUR,yUB,yUT
c       call cpcldr(dBxy,rwrk,iwrk)
        call cpclam(dBxy,rwrk,iwrk,map)
        call gsfais(1)   ! solid fill
        call arscam(map, xwrk, ywrk, nwrk, iarea, igrp, ngrps, fillc)

cend option contourfill:
      endif
c      CALL BLASTMAP
c      CALL GETLABEL
corig      mjrx=10
corig      mjry=10
corig      mnrx=0
corig      mnry=0

      mjrx=2
      mjry=2
      mnrx=4
      mnry=4
      ixlab=1
      iylab=1
      igph=5
      xint=0.
      yint=0.
   
ctk      CALL GSPLCI(ICLORS(4))

      call gridal(mjrx,mnrx,mjry,mnry,ixlab,iylab,igph,xint,yint)

      REWIND(15)
c###############################################
c     --- Place a mark at the center.
      if(white_backgrnd) then
         lc=0
      else
        lc=1
      endif
      CALL COLOR(lc)
      call setusv('LW',2000)
      TRXB=0.
      TRYB=0.
      call wtstr(TRXB,TRYB,'+',2,0,0)
      call setusv('LW',1000)
c
ctkadd:
      call frame
      call conrec(dBxy,nxg,nxg,nyg,0.,0.,0.,0.,-1.,idash)
      call frame

      RETURN
      END
C
      SUBROUTINE LOCATE(XA, N, X, JLOWER)
      IMPLICIT NONE
      INTEGER J, N, JLOWER
      REAL*4  X, XA(N)
C
      INTEGER JMID, JUPPER
C
C     --- RETURN END POINTS IF OUT OF RANGE
      IF(X.LE.XA(1)) THEN
        JLOWER = 1
      ELSEIF(X.GE.XA(N)) THEN
        JLOWER = N
      ELSE
C       --- LOCATE INDICES JLOWER, JLOWER+1 SURROUNDING X VALUE.
C       --- USE BINARY SEARCH
        JLOWER = 0
        JUPPER = N+1
        DO 10 J=1,JUPPER/2
          IF(JUPPER-JLOWER.GT.1) THEN
            JMID=(JUPPER+JLOWER)/2
            IF(X.GT.XA(JMID)) THEN
              JLOWER=JMID
            ELSE
              JUPPER=JMID
            ENDIF
          ELSE
            GO TO 11
          ENDIF
   10   CONTINUE
   11   CONTINUE
      ENDIF
      RETURN
      END
C
      SUBROUTINE STDATM(Z,P,D,T,CS)
C
C    THIS ROUTINE COMPUTES THE 1962 STANDARD ATMOSPHERE PRESSURE (P) -
C    N/M**2, DENSITY (D) - KG/M**3, ABSOLUTE TEMPERATURE (T) - DEG K,
C    AND SOUND SPEED (CS) - M/S IN TERMS OF GEOMETRIC ALTITUDE (Z) IN M.
C
      REAL*4 Z,P,D,T,CS
      REAL*4 A,C1,C2,DH,G0,GAMMA,DTDZ,H,HB,M0,P0,PB,R,RSTAR,T0,TB
C
C    ASSIGN CONSTANTS
      RSTAR = 8.31432
      M0 = 28.9644E-3
      R = RSTAR/M0
      G0 = 9.80665
      A = 6.378178E6
      C1 = G0*M0/RSTAR
      GAMMA =1.4
C
C    CONVERT TO GEPOTENTIAL ALTITUDE
      H = Z/(1.0+Z/A)
C
C    TROPOSPHERE
      IF(H.GT.11.0E3) GO TO 10
      T0 = 288.15
      DTDZ = -6.5E-3
      P0 = 1.01325E5
      C2 = C1/DTDZ
      T = T0 + DTDZ*H
      P = P0*(T0/T)**C2
      D = P/(R*T)
      CS = SQRT(GAMMA*R*T)
      RETURN
C
C    STRATOSPHERE
   10 IF(H.GT.20.E3) GO TO 20
      HB = 11.0E3
      T = 216.65
      DH = H - HB
      PB = 2.2632E4
      P = PB*EXP(-C1*DH/T)
      D = P/(R*T)
      CS = SQRT(GAMMA*R*T)
      RETURN
   20 IF(H.GT.32.E3) GO TO 30
      HB = 20.E3
      TB = 216.65
      PB = 5.47487E3
      DTDZ = 1.0E-3
      DH = H - HB
      T = TB + DTDZ*DH
      C2 = C1/DTDZ
      P = PB*(TB/T)**C2
      D = P/(R*T)
      CS = SQRT(GAMMA*R*T)
      RETURN
   30 IF(H.GT.47.E3) GO TO 40
      HB = 32.E3
      TB = 228.65
      PB = 8.68014E2
      DTDZ = 2.8E-3
      DH = H - HB
      T = TB + DTDZ*DH
      C2 = C1/DTDZ
      P = PB*(TB/T)**C2
      D = P/(R*T)
      CS = SQRT(GAMMA*R*T)
      RETURN
C
C    MESOSPHERE
   40 IF(H.GT.52.E3) GO TO 50
      HB = 47.0E3
      T = 270.65
      DH = H - HB
      PB = 1.10905E2
      P = PB*EXP(-C1*DH/T)
      D = P/(R*T)
      CS = SQRT(GAMMA*R*T)
      RETURN
   50 IF(H.GT.61.E3) GO TO 60
      HB = 52.E3
      TB = 270.65
      PB = 5.90005E1
      DTDZ = -2.0E-3
      DH = H - HB
      T = TB + DTDZ*DH
      C2 = C1/DTDZ
      P = PB*(TB/T)**C2
      D = P/(R*T)
      CS = SQRT(GAMMA*R*T)
      RETURN
   60 IF(H.GT.79.E3) GO TO 70
      HB = 61.E3
      TB = 252.650
      PB = 1.82099E1
      DTDZ = -4.0E-3
      DH = H - HB
      T = TB + DTDZ*DH
      C2 = C1/DTDZ
      P = PB*(TB/T)**C2
      D = P/(R*T)
      CS = SQRT(GAMMA*R*T)
      RETURN
   70 IF(H.GT.88.743E3) GO TO 80
      HB = 79.0E3
      T = 180.65
      DH = H - HB
      PB = 1.0377E0
      P = PB*EXP(-C1*DH/T)
      D = P/(R*T)
      CS = SQRT(GAMMA*R*T)
      RETURN
C
C   EXIT WITH ERROR MESSAGE FOR GEOMETRIC ALTITUDES GT 90 KM
   80 WRITE(6,100) Z
  100 FORMAT(1H0,'ERROR - ALTITUDE = ',1PE15.7,' GREATER THAN 90 KM')
C
      RETURN
      END
c
      subroutine shftlft(string)
c***********************************************************************
c*
c* Purpose:        This routine will left justify a character string,
c*                 i.e. remove leading blanks.
c*
c* Description:    The routine will search the string for the first
c*                 non-blank character and save that index within the
c*                 string.  If a non-blank character is found, the
c*                 string will be shifted to the left, otherwise the
c*                 string is left as is.
c*
c* Calling interface:
c*   Input:
c*     string - a character string of indeterminate length.
c*   Output:
c*     string - The same string, except that the non-blank characters
c*              in the string will be shifted to the left.  an example
c*              is the following: '  AAAA' ==> 'AAAA  '.
c***********************************************************************
c
      implicit none
      character*(*) string
      character*1  blank
      parameter   (blank = ' ' )
      integer i, first_idx, last_idx
      character*120 temp
c
c     ---- initializations
      first_idx = 0
      last_idx  = 0
      temp      = ' '
      first_idx = 1
      last_idx = LEN(string)
c
c     --- Search the string for the first non-blank character
      do 10 i = 1, last_idx, 1
        if(string(i:i) .ne. blank) then
c         --- found first non-blank character
          first_idx = i
          go to 20
        endif
   10 continue
   20 continue
c
c     --- Determine results of search for a non-blank character
      if ( (first_idx.gt.1) .and. (first_idx.le.last_idx) ) then
c
c       --- string is non-blank
        do 30 i = first_idx+1, last_idx, 1
          if(string(i:i) .eq. blank) then
c           --- found last character
            last_idx = last_idx - 1
            go to 40
          endif
   30   continue
   40   continue
c       --- Shift the string left and blank out the
c       --- remainder of the string.
        temp = ' '
        temp(1:) = string(first_idx:last_idx)
        string = ' '
        string(1:) = temp(1:)
      end if
      return
      end
c
      SUBROUTINE INTRP(dBxy,xg,yg,nxg,nyg)
C----------------------------------------------------------------------
      implicit none
      integer nxg, nyg
      real xg(nxg), yg(nyg), dBxy(nxg,nyg)
      integer nmaxr
      parameter (nmaxr=1024)
      integer ndelsi, nranges, ipsi, output
      real    Aza, rangea, dBa, max_range
      common /savea/  ndelsi, ipsi, nranges(361), Aza(361),
     1        max_range, rangea(nmaxr,361), dBa(nmaxr,361), output
      integer i,j, ix,jy,i1,i2, kt
      real    dBamax, dBamin, xlg, ylg, dxg, dyg, dBxymax, dBxymin,
     1        delx, dely, r, rsq, theta, Azj, psi1, psi2, r1, r2,
     2        drange, dr2, xn, dB1, dB2, dBdpsi
      real*8     DEGREES_PER_RADIAN 
      parameter (DEGREES_PER_RADIAN = 57.29577951308232D0 )
C
      dBamax = -1.0E30
      dBamin = +1.0E30
      do j=1,ndelsi
        do i=1,nranges(j)
          dBamax = MAX(dBamax,dBa(i,j))
          dBamin = MIN(dBamin,dBa(i,j))
        enddo
      enddo
      print *,'in PLOTDB2: dBamax,dBamin=',dBamax,dBamin
c     ---- build a 201X201 rectangular grid
      xlg=-max_range
      ylg=-max_range
      dxg=2.*max_range/FLOAT(nxg-1)
      dyg=dxg
      do ix=1,nxg
        xg(ix)=(ix-1)*dxg + xlg
      enddo
      do jy=1,nyg
        yg(jy)=(jy-1)*dyg + ylg
      enddo
      dBxymax = -1.0E30
      dBxymin = +1.0E30
c     --- wrap 360 deg data to 0 deg data
      Aza(ndelsi+1) =360.
      nranges(ndelsi+1)=nranges(1)
      do i=1,nranges(ndelsi+1)
        rangea(i,ndelsi+1)=rangea(i,1)
        dBa   (i,ndelsi+1)=dBa(i,1)
      enddo
      do ix=1,nxg
        do jy=1,nyg
          dBxy(ix,jy) = -1.
          delx = xg(ix)
          dely = yg(jy)
          rsq = delx**2 + dely**2
          r = SQRT(rsq)
          if(ABS(r).LT.dxg) then
            dBxy(ix,jy) = dBamax
            go to 603
          endif
          theta = ATAN2(dely,delx)
          Azj=theta*DEGREES_PER_RADIAN
          if(Azj.lt.0.) Azj=Azj+360.
c         --- bracket theta between j and j+1 azimuths
          do j=1,ndelsi
            if((Azj.ge.Aza(j)).and.(Azj.lt.Aza(j+1))) then
c             --- heading is between the these two angles (j and j+1)
c             --- interpolate db in range at outer azimuth (j+1)
              psi1=Aza(j)
              psi2=Aza(j+1)
              if( (r.gt.rangea(nranges(j),j)).or.
     &            (r.gt.rangea(nranges(j+1),j+1)) ) then
                dBxy(ix,jy) = 0.
                go to 603
              endif
              do i=1,nranges(j+1)-1
                r1=rangea(i,j+1)
                r2=rangea(i+1,j+1)
                if((r.ge.r1).and.(r.lt.r2)) then
                  i2=i
                  drange = r2-r1
                  if(ABS(drange).LE. 0.00001) then
                    dB2 = (dBa(i+1,j+1)+dBa(i,j+1))/2
                  else
                    XN = (dBa(i,j+1) - dBa(i+1,j+1)) /
     &                LOG(r1/r2)
                    dB2 = dBa(i+1,j+1) + XN*LOG(r/r2)
                  endif
                  go to 601
                endif
              enddo
              write(kt,*) 'error-outer range not bracketed'
              write(kt,*) 'j,Azj,psi1,psi2,r,rmin,rmax=',j,Azj,
     &          psi1,psi2,r,rangea(1,j+1),rangea(nranges(j+1),j+1)
              dBxy(ix,jy) = 0.
              go to 603
c             --- interpolate db in range at inner azimuth (j)
  601         do i=1,nranges(j)-1
                if((r.ge.rangea(i,j)).and.(r.lt.rangea(i+1,j)))
     1          then
                  i1=i
                  drange = rangea(i+1,j)-rangea(i,j)
                  if(ABS(drange).LE. 0.00001) THEN
                    dB1 = (dBa(i+1,j)+dBa(i,j))/2
                  else
                    XN = (dBa(i,j) - dBa(i+1,j)) /
     &                LOG(rangea(i,j)/rangea(i+1,j))
                    dB1 = dBa(i+1,j) + XN*LOG(r/rangea(i+1,j))
                  endif
                  go to 602
                endif
              enddo
              write(kt,*) 'error-inner range not bracketed'
c             --- interpolate dB in azimuth
  602         dBdpsi = (dB2-dB1)/(psi2-psi1)
              dBxy(ix,jy) = dB1 + dBdpsi*(Azj-psi1)
              go to 603
            endif
          enddo ! j loop
          write(kt,*) 'error-azimuth not bracketed'
          dBxy(ix,jy) = 0.
  603     continue
c         print *,'ix,jy,r,theta,dB=',ix,jy,r,Azj,dBxy(ix,jy)
          dBxymax = MAX(dBxy(ix,jy),dBxymax)
          dBxymin = MIN(dBxy(ix,jy),dBxymin)
        enddo
      enddo
      print *,'in INTRP: dBxymax,dBxymin=',dBxymax,dBxymin
c      do j=1,nyg
c        write(6,*) (dBxy(i,j),i=1,nxg)
c      enddo
      RETURN
      END
C
      SUBROUTINE LEVELQ
C-----------------------------------------------------------------------
C   Function: This subroutine gives the user the option of changing
C             the contour levels.  Enter the desired levels or accept
C             the present default values stored in the file "DBcolor.DAT"
C   Version 5.0 took this option out of the users domain and the levels
C             and/or colors can be change by editing the file.
c
C-----------------------------------------------------------------------
      INTEGER ncdir,nodir
      CHARACTER*60 DATADIR,OUTDIR,DBFLNAM,FILMET,FILOUT,met_dsn
      COMMON /filenm/ DATADIR,OUTDIR,DBFLNAM,FILMET,FILOUT,
     1        met_dsn,ncdir,nodir
      integer KI,KT,MT1,MT2,MT3,MT4,MT8
      COMMON /LUNT/ KI,KT,MT1,MT2,MT3,MT4,MT8
      COMMON /CLRMAP/ ICLORS(20),CONTRS(51),NCONTS
      CHARACTER*60 DUMMY
 1100 FORMAT (A)
C
  10  CONTINUE
C Check to see which contour the user requested.
C If itype = 1  open a DB contour file, If itype =2 open elevation contour.
        IUNIT = 18
C  Open the file containing the default DB levels. 
C
        open (unit=18, file='dbcolor.dat',
     1  status='old',access='sequential',form='formatted',
     2  iostat=ios)
        if(ios.ne.0) then
          print *,'error opening dbcolor file'
          stop
        endif
C Read in the default levels.      
c       write(*,*)' Reading in the db levels and colors'
        read(iunit,1100) dummy
        read(iunit,*) NCONTS
        do 50 j = 1,NCONTS       
          read(iunit,*,err=910)iclors(j),contrs(j)
c         print *,'j,color,conrt=',j,iclors(j),contrs(j)
 50     CONTINUE
        GO TO 15
910     WRITE(KT,*)' SORRY, ERROR READING DBLEVEL AND COLOR DATA FILE'
 15     CONTINUE
      CLOSE (IUNIT)
c
      RETURN
      END
c
      subroutine fillc(xwrk, ywrk, nwrk, iarea, igrp, ngrps)
c     --- Fills dB contours.
c     --- This routine is required by ncar graphics to assign areas
c     --- and colors to fill.
      integer iarea(*), igrp(*)
      real    xwrk(*), ywrk(*)
c     integer nscr
c     parameter (nscr=350000)
c     integer iscr(nscr)
c     real    rscr(nscr)
      INTEGER isnd, ICHOICE, ISITE, IOPT, ICONTR, nsnds, nlevels,
     1        DBPLOTOPT
      REAL    PSIMIN, PSIMAX, LATS, LONS, LATB, LONB, input_delpsi,
     1        xminp,xmaxp,yminp,ymaxp
      LOGICAL DOELV, DOATT, white_backgrnd
      COMMON  /INPUTS/ ICHOICE, ISITE, IOPT, ICONTR, PSIMIN, PSIMAX,
     1        LATS, LONS, LATB, LONB, input_delpsi, isnd, DOELV, DOATT,
     2        white_backgrnd, xminp,xmaxp,yminp,ymaxp, nsnds, nlevels,
     3        DBPLOTOPT
      COMMON /CLRMAP/ ICLORS(20),CONTRS(51),NCONTS
c
c     print *,'in fillc: ngrps=',ngrps
c     print *,'in fillc: ICLORS=',ICLORS
      ifill=1
c     ---- If any area identifier is negative don't fill
      do j=1,ngrps
        if(iarea(j).lt.0) ifill=0
c       print *,'j,iarea(j),igrp(j),ifill=',j,iarea(j),igrp(j),ifill
      enddo
c     ---- Otherwise fill in the color according to its area identifier
c     ---- relative to edge group 3 (contour lines)
c     call gsfais(3)  ! hatch fill
      call gsfais(1)  ! solid fill
      call sfsetr('spacing',.005)
      call sfsetr('angle',45.)
      call setusv('LW',3000)
      if(ifill.ne.0) then
        ifill=0
        do j=1,ngrps
          if(igrp(j).eq.3) ifill=iarea(j)
        enddo
        if(ifill.gt.1 .and. ifill.lt.NCONTS) then
c         --- use for solid fill
          call gsfaci(ICLORS(ifill-1))   ! set color to fill
          call gfa(nwrk-1,xwrk,ywrk)
c         --- to here
c         --- use this for cross-hatch fill from here
c         call gsplci(ICLORS(ifill-1))   ! set color to fill
c         call sfsetr('angle',45.)
c         call sfnorm(xwrk,ywrk,nwrk-1,rscr,nscr,iscr,nscr)
c         call sfsetr('angle',135.)
c         call sfnorm(xwrk,ywrk,nwrk-1,rscr,nscr,iscr,nscr)
c         --- to here
        endif
      else 
ctk- uncomment:
       if(white_backgrnd) then
         call gsfaci(1)   ! white background
         call gfa(nwrk-1,xwrk,ywrk)
       endif
      endif
      return
      end
c
      subroutine INITPLT
      INTEGER isnd, ICHOICE, ISITE, IOPT, ICONTR, nsnds, nlevels,
     1        DBPLOTOPT
      REAL    PSIMIN, PSIMAX, LATS, LONS, LATB, LONB, input_delpsi,
     1        xminp,xmaxp,yminp,ymaxp
      LOGICAL DOELV, DOATT, white_backgrnd
      COMMON  /INPUTS/ ICHOICE, ISITE, IOPT, ICONTR, PSIMIN, PSIMAX,
     1        LATS, LONS, LATB, LONB, input_delpsi, isnd, DOELV, DOATT,
     2        white_backgrnd, xminp,xmaxp,yminp,ymaxp, nsnds, nlevels,
     3        DBPLOTOPT
c     --- Open GKS, open workstation, activate workstation.
      CALL GOPKS (6, 0)
      CALL GOPWK (1, 2, 1)
c                     R   G   B
c     if(white_backgrnd) then
c       --- use this pair for black on white background
c       CALL GSCR (1,0,1.0,1.0,1.0) ! 0=background=white
c       CALL GSCR (1,1,0.0,0.0,0.0) ! 1=foreground=black
c     else
c       --- use this pair for white on black background
        CALL GSCR (1,0,0.0,0.0,0.0) ! 0=background=black
        CALL GSCR (1,1,1.0,1.0,1.0) ! 1=foreground=white
c     endif
c     --- use these colors for dB contours
c                     rd  gr   bl
      CALL GSCR (1,2, 1.0,0.0, 0.0)  ! 2=
      CALL GSCR (1,3, 1.0,0.12,0.0) ! 3=
      CALL GSCR (1,4, 1.0,0.25,0.0) ! 4= 
      CALL GSCR (1,5, 1.0,0.50,.75) ! 5= light pink
      CALL GSCR (1,6, 1.0,0.00,0.5) ! 6= bright pink
      CALL GSCR (1,7, 1.0,0.00,0.0) ! 7= red
      CALL GSCR (1,8, 1.0,0.35,0.0) ! 8= red-orange
      CALL GSCR (1,9, 1.0,0.50,0.0) ! 9= orange-read
      CALL GSCR (1,10, 1.0,0.63,0.0) ! 
      CALL GSCR (1,11, 1.0,0.88,0.0) ! 
      CALL GSCR (1,12,1.0,1.0, 0.0) ! 
      CALL GSCR (1,13,0.6,1.0, 0.0) !
      CALL GSCR (1,14,0.0,1.0, 0.0) !
      CALL GSCR (1,15,0.10,0.1, 1.0) !

c                      rd   gr  bl

      CALL GSCR (1,16,0.4,1.0,1.0) ! 16=light blue
      CALL GSCR (1,17,0.7,1.0,0.7) ! 17=light green

c
c     --- use these colors for dB contours
c.      CALL GSCR (1,2, 1.0,0.0, 0.0)  ! 2=red
c.      CALL GSCR (1,3, 1.0,0.12,0.0) ! 3=red-red-orange
c.      CALL GSCR (1,4, 1.0,0.25,0.0) ! 4=red-orange
c.      CALL GSCR (1,5, 1.0,0.38,0.0) ! 5=red-orange
c.      CALL GSCR (1,6, 1.0,0.50,0.0) ! 6=orange
c.      CALL GSCR (1,7, 1.0,0.63,0.0) ! 7=red-orange
c.      CALL GSCR (1,8, 1.0,0.75,0.0) ! 8=red-orange
c.      CALL GSCR (1,9, 1.0,0.88,0.0) ! 9=red-orange
c.      CALL GSCR (1,10,1.0,1.0, 0.0) ! 10=yellow
c.      CALL GSCR (1,11,1.0,1.0, 0.5) ! 11=yellow
c.      CALL GSCR (1,12,0.0,0.5, 0.5) ! 5=blue-green
c.      CALL GSCR (1,13,0.0,1.0, 0.0) ! 3=green
c.      CALL GSCR (1,14,0.0,0.0, 0.5) ! 4=dark blue
c.      CALL GSCR (1,15,0.0,0.8, 0.0) ! 15=dark green

c     --- use for background map
corig      CALL GSCR (1,16,0.0,0.7,1.0) ! 16=light blue
corig      CALL GSCR (1,17,0.3,1.0,0.5) ! 17=light green

      CALL GACWK (1)
      CALL GSELNT(0)
      CALL GSCLIP(0)
c     print *,'return from INITPLT'
c
      return
      end
c
      subroutine ENDPLT
c     --- Deactivate and close workstation, close GKS.
      CALL GDAWK (1)
      CALL GCLWK (1)
      CALL GCLKS
      return
      end
c
      subroutine NEWPLT
      call frame
      return
      end
c
      SUBROUTINE COLOR(i)
      integer i
      call gstxci(i)
      return
      end
c      include 'ncarg_stub.f'
