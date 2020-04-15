C$DEBUG
      PROGRAM NAPSMM5
c     --- Reads in sounding data, runs the naps sound model, and plots
c     --- the results using ncar graphics
c     --- ### Note this version plots rays along individual azumithal spokes,
c     --- but which spokes are to be plotted are presently hardcoded in 
c     --- subroutine BLAST, with plotting done in PLOTRAY.  The spokes
c     --- to plot should eventually be user specified. ####
c-----------------------------------------------------------------------
      INTEGER KI,KT,MT1,MT2,MT3,MT4,MT8
      COMMON /LUNT/ KI,KT,MT1,MT2,MT3,MT4,MT8
c-----------------------------------------------------------------------
      INTEGER isnd, ICHOICE, ISITE, IOPT, ICONTR, nsnds, nlevels,
     1        DBPLOTOPT
      REAL    PSIMIN, PSIMAX, LATS, LONS, LATB, LONB, input_delpsi,
     1        xminp,xmaxp,yminp,ymaxp
      LOGICAL DOELV, DOATT, white_backgrnd
      CHARACTER*60 snd_label
      COMMON  /INPUTS/ snd_label, ICHOICE, ISITE, IOPT, ICONTR, PSIMIN,
     1        PSIMAX, LATS, LONS, LATB, LONB, input_delpsi, isnd, DOELV,
     2        DOATT, white_backgrnd, xminp,xmaxp,yminp,ymaxp, nsnds,
     3        nlevels, DBPLOTOPT
c-----------------------------------------------------------------------
      CHARACTER METLABEL*80, FILLABEL*80, SITE*10
      COMMON /IDTAG/  METLABEL, FILLABEL, SITE
c-----------------------------------------------------------------------
      CHARACTER*60 DBFLNAM
      COMMON /DBFILE/ DBFLNAM
c-----------------------------------------------------------------------
      integer IGUN
      real    XB,YB,ZB,WB,SFPRES,DIRANG,ZBAG
      COMMON /SOURCE/ XB,YB,ZB,WB,SFPRES,DIRANG,IGUN,ZBAG
c-----------------------------------------------------------------------
      real    X,Y,RGARRY,DBARRY,ATOTRY
      COMMON /DEDAT/ X(751),Y(361),RGARRY(751),DBARRY(751),ATOTRY(751)
c-----------------------------------------------------------------------
      integer ncdir,nodir,nmdir 
      CHARACTER*60 DATADIR,OUTDIR,METDIR,FILELV,FILEAT,FILMET,FILSUM,
     1  FILOUT,met_dsn
      COMMON /filenm/ DATADIR,OUTDIR,METDIR,FILELV,FILEAT,FILMET,FILSUM,
     1  FILOUT,met_dsn,ncdir,nodir,nmdir
c-----------------------------------------------------------------------
      real    XFAC,YFAC,DELRES,RXL,RYL,RXH,RYH,X1,Y1,X2,Y2,
     1  UB,UT,UR,UL,TRXB,TRYB
      COMMON /MAPCOR/ XFAC,YFAC,DELRES,RXL,RYL,RXH,RYH,X1,Y1,X2,Y2,
     1  UB,UT,UR,UL,TRXB,TRYB
c-----------------------------------------------------------------------
      real    A,CONTRS,RMAX,DELPSI,RRATIO
      integer M,N,IRAMAX,NRO,NCONTS
      COMMON /SOUND/ A(751,361),M,N,CONTRS(51),RMAX,DELPSI,IRAMAX,
     &               NRO,NCONTS,RRATIO
c-----------------------------------------------------------------------
      integer ISUR,IMAX,JCOAST,JFOCAL
      real    RANGE,MAGNIF,TIMEOA,HEIGHT,REFLCT,TBRATO,DRA,DRB
      COMMON /SKIP/ISUR,IMAX,RANGE(10),MAGNIF(10),TIMEOA(10),HEIGHT(10),
     &  REFLCT(10),TBRATO(10),JCOAST(10),JFOCAL(10),DRA(10),DRB(10)
c-----------------------------------------------------------------------
      REAL    RMAXD, DELRT, DELS, ALTGMX
      INTEGER ATT, ELEV, NPPSPK
      COMMON/WHEELS/ ATT(751,73),ELEV(751,73),RMAXD,NPPSPK,DELRT,DELS,
     &               ALTGMX
c-----------------------------------------------------------------------
      REAL    ALT,T,RH,WN,WE,V,WU,WV,SV,p_mb
      INTEGER NP
      COMMON /WEATHR/ ALT(70),T(70),RH(70),WN(70),WE(70),V(70),WU(70),
     &                WV(70),SV(70),p_mb(70),NP
c-----------------------------------------------------------------------
      LOGICAL DEFLT
      COMMON /DEFAULT/ DEFLT
c-----------------------------------------------------------------------
      LOGICAL DBFLG
      COMMON /FLAGS/ DBFLG
c-----------------------------------------------------------------------
      real   XLAT,XLONG,RMXE
      COMMON /AUX/ XLAT,XLONG,RMXE
c-----------------------------------------------------------------------
      integer max_ranges, max_azs
      parameter (max_ranges=751, max_azs=361) 
      integer ndelsi, nranges, nray
      real    Aza, rangea, dBa, xray, yray
      common /savea/  ndelsi, nranges(361), Aza(361), rangea(751,361),
     1                dBa(751,361), xray(751,101), yray(751,101), nray
      character*80 record
      LOGICAL PLOTIT
C
      real*8     DEGREES_PER_RADIAN, REARTH 
      parameter (DEGREES_PER_RADIAN = 57.29577951308232D0 )
      parameter (REARTH =6370999.D0)
      DATA KT/9/ , KI/0/ , MT1/3/ , MT2/4/ , MT3/2/ , MT8/8/
      DATA DEFLT/.false./
c
c     --- Open and read the input file
c     --- Note blast site codes are:
c     1. BRIAR POINT
c     2. FUSE RANGE
c     3. H-FIELD
c     4. I-FIELD
c     5. MAIN FRONT
c     6. NEW BOMBING FLD.
c     7. OLD BOMBING FLD.
c     8. PLATE RANGE
c     9. POVERTY ISLAND
c    10. TRENCH WARFARE
c    11. RECOILESS RANGE B
c    12. ABBEY POINT
c    13. CARROLL ISLAND
c    14. C-FIELD
c    15. 9600 IMPACT
c     --- Note valid gun type codes are:
c     0. UNIFORM BLAST        ## DEFAULT ##
c     1. 105 mm HOWITZER M102
c     2. 105 mm TANK M60
c     3. 8 INCH HOWITZER M110
c     4. 8 INCH SELF PROPELLED M110A1
c     5. 120 mm TANK (HEAT_TPT)  use for "older ammo"
c     6. 120 mm TANK (SABOT)     ## DEFAULT ##
      OPEN(UNIT=15,FILE='input.dat',STATUS='OLD', FORM ='FORMATTED',
     1  iostat=ios)
c     print *, 'opening input file'
      if(ios.eq.0) then
        read(15,*) ICHOICE        ! default=1, NAPS model run
        read(15,*) IOPT           ! default=0, computes dB contours
        read(15,*) PLOTIT         ! set to true to invoke plotting
        read(15,*) white_backgrnd ! true for white background on plots
        read(15,*) DBPLOTOPT      ! dB contour plot option (1-5)
        read(15,*) DOELV          ! true to use elevation data
        read(15,*) DOATT          ! true to use land attribute data
        read(15,*) input_delpsi   ! azimuth interval for contours (deg)
        read(15,*) METDIR         ! directory containing met dat
        read(15,*) DATADIR        ! data directrory
        read(15,*) OUTDIR         ! output directory
        read(15,*) ISITE          ! blast site
        read(15,*) ZBAG           ! blast height above surface (meters)
        read(15,*) WB             ! blast weight (lbs)
        read(15,*) IGUN           ! gun type (default=0, uniform blast)
        read(15,*) met_dsn        ! input meteorolgy file
        close(unit=15)
        print *, ICHOICE
        print *, IOPT
        print *, PLOTIT
        print *, white_backgrnd
        print *, DBPLOTOPT
        print *, DOELV
        print *, DOATT
        print *, input_delpsi
        print *, METDIR
        print *, DATADIR
        print *, OUTDIR
        print *, ISITE
        print *, ZBAG
        print *, WB
        print *, IGUN
        print *, met_dsn
      else
        print *,'error opening input file'
        stop
      endif
c
c     --- Limit azimuth interval to 5 deg because terrain and land
c     --- attribute data are along 5 deg spokes
c     --- NOTE: azimuth here is measured counterclockwise from east!!!
      if(input_delpsi.lt.5.) input_delpsi=5.
      DELPSI = input_delpsi
      IDELPSI = NINT(DELPSI)
c
c     --- form the names of the output files
      ncdir = index(DATADIR,' ')
      ncdir = ncdir-1
      nmdir = index(METDIR,' ')
      nmdir = nmdir-1
      nodir = index(OUTDIR,' ')
      nodir = nodir-1
c     print *,'METDIR =',METDIR
c     print *,'DATADIR=',DATADIR
c     print *,'OUTDIR =',OUTDIR
      FILOUT = OUTDIR(1:nodir)//'output.dat'     ! unit=KT
      FILSUM = OUTDIR(1:nodir)//'summary.dat'    ! unit=MT8
      DBFLNAM= OUTDIR(1:nodir)//'decibel.dat'    ! unit=15
      FILMET = metdir(1:nmdir)//met_dsn
      print *,'FILMET=',FILMET
c
c     --- open the MM5 met file, and read the first two lines
      OPEN( UNIT = MT3, FILE = FILMET, STATUS ='OLD', IOSTAT=IOS )
      print *, 'opening MM5filmet=',FILMET
      if(ios.ne.0) then
        print *,'error opening input MM5filmet:',FILMET
        stop
      endif
  199 format(A80)
      do irec=1,10
        read(MT3,199,end=21) record
        CALL shftlft(record)
        if(record(1:1).ne.' ') then 
          N = INDEX(record,' ') - 1
          nlevels=0
          read(record(1:N), '(I30)', IOSTAT = IOS) nlevels
          record=record(N+1:)
          CALL shftlft(record)
          N = INDEX(record,' ') - 1
          nsnds=0
          read(record(1:N), '(I30)', IOSTAT = IOS) nsnds
          go to 21
        endif
      enddo
  21  continue
c     print *,'nsnds,nlevels=',nsnds,nlevels
      if((nsnds.le.0).or.(nlevels.le.0)) then
        print *,'error reading MM5 met file'
        stop
      endif
c
c     --- open the output files
      OPEN(UNIT=KT,  FILE=FILOUT, STATUS='UNKNOWN', FORM ='FORMATTED',
     1  iostat=ios)
c     --- Open a file to store a summary of the calculations made during
c     --- the running of the blast model.This provides a tabular output for
c     --- the user.
      if(MT8.ne.KT) then
        OPEN(UNIT=MT8, FILE=FILSUM, STATUS='UNKNOWN', FORM ='FORMATTED',
     1    iostat=ios)
      endif
c
      WRITE(KT,*) '   '
      WRITE(KT,*)
      WRITE(KT,112)
      WRITE(KT,113)
      WRITE(KT,114)
      WRITE(KT,115)
      WRITE(KT,116)
      WRITE(KT,117)
      WRITE(KT,118)
      WRITE(KT,119)
      WRITE(KT,120)
      WRITE(KT,121)
 112  FORMAT(10X,' NN       NN        AAA        PPPPPP        SSSSSS')
 113  FORMAT(10X,' NNN      NN       AA AA       PP    PP    SS     S')
 114  FORMAT(10X,' NN NN    NN      AA   AA      PP     PP   SS'      )
 115  FORMAT(10X,' NN NN    NN      AA   AA      PP     PP   SS'      )
 116  FORMAT(10X,' NN  NN   NN     AA     AA     PP    PP      SS'    )
 117  FORMAT(10X,' NN  NN   NN     AA     AA     PPPPPP          SS  ')
 118  FORMAT(10X,' NN    NN NN    AAAAAAAAAAA    PP                SS')
 119  FORMAT(10X,' NN    NN NN   AA         AA   PP                SS')
 120  FORMAT(10X,' NN      NNN   AA         AA   PP          S     SS')
 121  FORMAT(10X,' NN       NN   AA         AA   PP          SSSSSS'  )
      WRITE(KT,*) '   '
      WRITE(KT,*) '   '
      WRITE(KT,*) '   '
      WRITE(KT,*)'                 NOISE ASSESSMENT PREDICTION SYSTEM'
      WRITE(KT,*)' '
      WRITE(KT,*)'                      VERSION 5.1  APRIL 1999 '
      WRITE(KT,*)' '
      WRITE(KT,*)'                   MICROSOFT FORTRAN 5.0 VERSION'
      WRITE(KT,*)'  '
      WRITE(KT,*)'  '
c     WRITE(KT,*)'  '
c     WRITE(KT,*)'  '
c     WRITE(KT,*)'      Please press return key to continue'
c     READ(KI,150) ANSR
c     CALL NEWPAG
C
C  Run/Display Menu to get user's choice.
C
c 5   WRITE(KT,*)'                  MAIN MENU            '
c     WRITE(KT,*)'  '
c     WRITE(KT,*)'             RUN/MET DISPLAY MENU     '
c     WRITE(KT,*)' '
c     WRITE(KT,*)'           1..  RUN NAPS MODEL    '
c     WRITE(KT,*)' '
c     WRITE(KT,*)'           2..  DISPLAY METEOROLOGICAL DATA '
c     WRITE(KT,*)' '
c     WRITE(KT,*)'           3..  PLOT DB CONTOUR (ARCHIVED FILE)'
c     WRITE(KT,*)' '
c     WRITE(KT,*)'           99.. STOP TERMINATE PROGRAM  '
c     WRITE(KT,*)'  '
c     WRITE(KT,100 )'        ENTER THE NUMBER OF YOUR CHOICE.    '
c     READ (KI,*,ERR=5) ICHOICE
c
      IF( ICHOICE .NE. 1 .AND. ICHOICE .NE. 2 .AND.
     &    ICHOICE .NE. 3 .AND. ICHOICE .NE.99 ) THEN
          WRITE(KT,*)' NOT A VALID CHOICE'
          stop
c         CALL NEWPAG
c         GO TO 5
      END IF
c
      if(PLOTIT) then
        CALL INITPLT
      endif
c      
c     ---- read in a sounding for each MM5 pseudo-sounding
      do isnd=1,nsnds
c     do isnd=1,2
      call initc
c
      IF( ICHOICE .EQ. 1 ) CALL GETBLAST
          IDBFLG = 1                      !blast data was generated.
c     IF( ICHOICE .EQ. 2 ) THEN
c         CALL GETMET
c         CALL METDISPLAY
c         GO TO 5
c     END IF
c     IF( ICHOICE .EQ. 3 ) then 
c         CALL PLOTDB
c         CALL NEWPAG
c         GO TO 5
c     END IF
c
c     --- bring in call to BLAST to get the range/azimuth intensity pattern.
      CALL USER(IOPT,IDBFLG)
      DELPSI = input_delpsi
      CALL BLAST(DELPSI,IRAMAX,RMAX,NRO,ICONTR,IOPT,RRATIO)
      CLOSE(15)
cmm5  --- Plot only dB contours
      if(PLOTIT) then
        DBFLG=.true.
cmm5    CALL PLOTOPTS(1,ISITE)  ! plot sonding
cmm5    CALL NEWPLT
        if(white_backgrnd) then
c         --- flush the background white
          call wflush
        endif
cmm5    CALL PLOTOPTS(2,ISITE)  ! plot background
cmm5    CALL NEWPLT
cmm5    CALL PLOTOPTS(5,ISITE)  ! plot elevation
cmm5    CALL NEWPLT
        CALL PLOTOPTS(3,ISITE)  ! plot dB map
        CALL NEWPLT
      endif
c
      enddo
      if(PLOTIT) CALL ENDPLT
      CLOSE(UNIT=MT3)
      CLOSE(UNIT=KT)
      CLOSE(UNIT=MT8)
c
      STOP
      END
c
      SUBROUTINE initc
c     --- Initialize commons to zero
c-----------------------------------------------------------------------
      integer IGUN
      real    XB,YB,ZB,WB,SFPRES,DIRANG,ZBAG
      COMMON /SOURCE/ XB,YB,ZB,WB,SFPRES,DIRANG,IGUN,ZBAG
c-----------------------------------------------------------------------
      real    X,Y,RGARRY,DBARRY,ATOTRY
      COMMON /DEDAT/ X(751),Y(361),RGARRY(751),DBARRY(751),ATOTRY(751)
c-----------------------------------------------------------------------
      real    XFAC,YFAC,DELRES,RXL,RYL,RXH,RYH,X1,Y1,X2,Y2,
     1  UB,UT,UR,UL,TRXB,TRYB
      COMMON /MAPCOR/ XFAC,YFAC,DELRES,RXL,RYL,RXH,RYH,X1,Y1,X2,Y2,
     1  UB,UT,UR,UL,TRXB,TRYB
c-----------------------------------------------------------------------
      real    A,CONTRS,RMAX,DELPSI,RRATIO
      integer M,N,IRAMAX,NRO,NCONTS
      COMMON /SOUND/ A(751,361),M,N,CONTRS(51),RMAX,DELPSI,IRAMAX,
     &               NRO,NCONTS,RRATIO
c-----------------------------------------------------------------------
      integer ISUR,IMAX,JCOAST,JFOCAL
      real    RANGE,MAGNIF,TIMEOA,HEIGHT,REFLCT,TBRATO,DRA,DRB
      COMMON /SKIP/ISUR,IMAX,RANGE(10),MAGNIF(10),TIMEOA(10),HEIGHT(10),
     &  REFLCT(10),TBRATO(10),JCOAST(10),JFOCAL(10),DRA(10),DRB(10)
c-----------------------------------------------------------------------
      REAL    RMAXD, DELRT, DELS, ALTGMX
      INTEGER ATT, ELEV, NPPSPK
      COMMON/WHEELS/ ATT(751,73),ELEV(751,73),RMAXD,NPPSPK,DELRT,DELS,
     &               ALTGMX
c-----------------------------------------------------------------------
      COMMON /RAYMAP/ISK(751), RANGES(751,5), FACTOR(751,5),
     &  TIMES(751,5), HIGHS(751,5), REFLTS(751,5), TBRATS(751,5),
     &  DECIBL(751,5), ANGLE(751), IFOCAL(751,5), JRAY(751,5),
     &  ATOT(751,5)
c-----------------------------------------------------------------------
      REAL    ALT,T,RH,WN,WE,V,WU,WV,SV,p_mb
      INTEGER NP
      COMMON /WEATHR/ ALT(70),T(70),RH(70),WN(70),WE(70),V(70),WU(70),
     &                WV(70),SV(70),p_mb(70),NP
c-----------------------------------------------------------------------
      integer max_ranges, max_azs
      parameter (max_ranges=751, max_azs=361) 
      integer ndelsi, nranges, nray
      real    Aza, rangea, dBa, xray, yray
      common /savea/  ndelsi, nranges(361), Aza(361), rangea(751,361),
     1                dBa(751,361), xray(751,101), yray(751,101), nray
c
c     --- /DEDAT/
      do i=1,751
        X(i)=0.
      enddo
      do j=1,361
        Y(j)=0.
      enddo
      do j=1,751
        RGARRY(j)=0.
        DBARRY(j)=0.
        ATOTRY(j)=0.
      enddo
c
c     --- /SOUND/ 
      do i=1,751
        do j=1,361
          A(i,j)=0.
        enddo
      enddo
      do j=1,51
        CONTRS(j)=0.
      enddo
c
c     --- /SKIP/
      ISUR=0
      IMAX=0
      do j=1,10
        RANGE(j)=0.
        MAGNIF(j)=0.
        TIMEOA(j)=0.
        HEIGHT(j)=0.
        REFLCT(j)=0.
        TBRATO(j)=0.
        JCOAST(j)=0
        JFOCAL(j)=0
        DRA(j)=0.
        DRB(j)=0.
      enddo
c
c     --- /WHEELS/
      NPPSPK=0
      ALTGMX=0.
      do i=1,751
        do j=1,73
          ATT(i,j)=0
          ELEV(i,j)=0
        enddo
      enddo
c
c     --- /RAYMAP/
      do i=1,751
	  ISK(i)=0
        ANGLE(i)=0.
        do j=1,5
          RANGES(i,j)=0.
          FACTOR(i,j)=0.
          TIMES(i,j)=0.
          HIGHS(i,j)=0.
          REFLTS(i,j)=0.
          TBRATS(i,j)=0.
          DECIBL(i,j)=0.
          IFOCAL(i,j)=0.
          JRAY(i,j)=0.
          ATOT(i,j)=0.
        enddo
      enddo
c
c     --- /WEATHR/
      NP=0
      SFPRES=0.
      do i=1,70
        ALT(i)=0.
        T(i)=0.
        RH(i)=0.
        WN(i)=0.
        WE(i)=0.
        V(i)=0.
        WU(i)=0.
        WV(i)=0.
        SV(i)=0.
        p_mb(i)=0.
      enddo
c
c     --- /savea/
      ndelsi=0
      do i=1,751
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
      return
      end
c
      SUBROUTINE GETBLAST
C----------------------------------------------------------------------
C
C   VARIABLES:
C             ALT       - array of altitudes for a given MET profile
C             ATT(I)    - data array from land attritute file.
C             DELPSI    - azimuth increments.       default = 15.
C             DELRT     - data base range increment default = 200M
C             DELS      - resolution of the spokes data  default = 5 deg.
C             ELEV(I)   - data array from elevation spokes file
C             FILELV    - name of the elevation spokes data file.
C             FILEAT    - name of the land attribute data file.
C             IMAX      - number of skiprays allowed    default = 5
C             IGUN      - type of weapon used.
C             METLABEL  - label used as ID on the Met Profile graphs
C             NPPSPK    - number of points per spoke.
C             NRO       - number of ray traces allowed. default = 55
C             RMAX,RMAXD- max range of the data available default = 38650.
C             RRATIO    - reflection ratio       default = 0.1
C             SFPRES    - surface press at blast (MB)
C             SITE      - string, name of blast site.
C             T,TN      - array of temperatures for a given MET profile
C             WE,WN     - the east and north wind components
C             XB,YB     - UTM's of the blast site.
C             XLAT,XLONG- lat. and long. of the blast site.
C             ZBAG      - altitude of the blast. (AGL)
C             ZB        - altitude of the blast. (MSL)
C     Variables of COMMON /SKIP/ defined in subroutine BLAST
C
C----------------------------------------------------------------------
      integer ncdir,nodir,nmdir 
      CHARACTER*60 DATADIR,OUTDIR,METDIR,FILELV,FILEAT,FILMET,FILSUM,
     1  FILOUT,met_dsn
      COMMON /filenm/ DATADIR,OUTDIR,METDIR,FILELV,FILEAT,FILMET,FILSUM,
     1  FILOUT,met_dsn,ncdir,nodir,nmdir
c-----------------------------------------------------------------------
      real    XFAC,YFAC,DELRES,RXL,RYL,RXH,RYH,X1,Y1,X2,Y2,
     1  UB,UT,UR,UL,TRXB,TRYB
      COMMON /MAPCOR/ XFAC,YFAC,DELRES,RXL,RYL,RXH,RYH,X1,Y1,X2,Y2,
     1  UB,UT,UR,UL,TRXB,TRYB
c-----------------------------------------------------------------------
      INTEGER isnd, ICHOICE, ISITE, IOPT, ICONTR, nsnds, nlevels,
     1        DBPLOTOPT
      REAL    PSIMIN, PSIMAX, LATS, LONS, LATB, LONB, input_delpsi,
     1        xminp,xmaxp,yminp,ymaxp
      LOGICAL DOELV, DOATT, white_backgrnd
      CHARACTER*60 snd_label
      COMMON  /INPUTS/ snd_label, ICHOICE, ISITE, IOPT, ICONTR, PSIMIN,
     1        PSIMAX, LATS, LONS, LATB, LONB, input_delpsi, isnd, DOELV,
     2        DOATT, white_backgrnd, xminp,xmaxp,yminp,ymaxp, nsnds,
     3        nlevels, DBPLOTOPT
c-----------------------------------------------------------------------
      INTEGER KI,KT,MT1,MT2,MT3,MT4,MT8
      COMMON /LUNT/ KI,KT,MT1,MT2,MT3,MT4,MT8
      CHARACTER METLABEL*80, FILLABEL*80, SITE*10
      COMMON /IDTAG/  METLABEL, FILLABEL, SITE
c-----------------------------------------------------------------------
      integer IGUN
      real    XB,YB,ZB,WB,SFPRES,DIRANG,ZBAG
      COMMON /SOURCE/ XB,YB,ZB,WB,SFPRES,DIRANG,IGUN,ZBAG
c-----------------------------------------------------------------------
      real    A,CONTRS,RMAX,DELPSI,RRATIO
      integer M,N,IRAMAX,NRO,NCONTS
      COMMON /SOUND/ A(751,361),M,N,CONTRS(51),RMAX,DELPSI,IRAMAX,
     &               NRO,NCONTS,RRATIO
c-----------------------------------------------------------------------
      integer ISUR,IMAX,JCOAST,JFOCAL
      real    RANGE,MAGNIF,TIMEOA,HEIGHT,REFLCT,TBRATO,DRA,DRB
      COMMON /SKIP/ISUR,IMAX,RANGE(10),MAGNIF(10),TIMEOA(10),HEIGHT(10),
     &  REFLCT(10),TBRATO(10),JCOAST(10),JFOCAL(10),DRA(10),DRB(10)
c-----------------------------------------------------------------------
      REAL    RMAXD, DELRT, DELS, ALTGMX
      INTEGER ATT, ELEV, NPPSPK
      COMMON/WHEELS/ ATT(751,73),ELEV(751,73),RMAXD,NPPSPK,DELRT,DELS,
     &               ALTGMX
c-----------------------------------------------------------------------
      REAL    ALT,T,RH,WN,WE,V,WU,WV,SV,p_mb
      INTEGER NP
      COMMON /WEATHR/ ALT(70),T(70),RH(70),WN(70),WE(70),V(70),WU(70),
     &                WV(70),SV(70),p_mb(70),NP
c-----------------------------------------------------------------------
      LOGICAL DEFLT
      COMMON /DEFAULT/ DEFLT
c-----------------------------------------------------------------------
      real   XLAT,XLONG,RMXE
      COMMON /AUX/ XLAT,XLONG,RMXE
c-----------------------------------------------------------------------
      CHARACTER CHRZB*5,CHRWB*5
      LOGICAL NOGUN
C     real    altin(70), TCin(70), RHin(70), WEin(70), WNin(70)
c
c     --- Retrieve lat,lons on specified site
C         XB,YB     - UTM's of the blast site.
C         XLAT,XLONG- lat. and long. of the blast site.
      CALL GETSITE(ISITE,NOGUN)
      WRITE(KT,*) 'ISITE = ',ISITE
      WRITE(KT,*) 'IGUN  = ',IGUN
      WRITE(KT,*) 'FILMET= ',FILMET
      WRITE(KT,*) 'FILELV= ',FILELV
      WRITE(KT,*) 'FILEAT= ',FILEAT
C
C  Read in blast source information.
      CALL NEWPAG
C
      CALL BLTMENU(ZBAG, WB, IGUN,DIRANG,NOGUN)
C
C  Build the ID label for the ray traces.
      WRITE(CHRZB,'(F4.0)') ZBAG
      WRITE(CHRWB,'(F4.0)') WB
      METLABEL = SITE//'; '//CHRZB//'M; '//CHRWB//' LBS.'
      WRITE(KT,1000) METLABEL
c     WRITE(KT,*)'XUTM, YUTM, BLT HT, BLT WT '
c     WRITE(KT,1100) XB, YB, ZBAG,   WB
      WRITE(KT,*) 'Blast site                 = ',SITE
      WRITE(KT,*) 'Blast lat,long coordinates =',XLAT,XLONG
      WRITE(KT,*) 'Blast UTM coordinates      =',XB,YB
      WRITE(KT,*) 'Blast height(agl m)        =',ZBAG
      WRITE(KT,*) 'Blast weight(lbs)          =',WB
c
C  Get MSL of the blast. (Ht of blast + alt. of the site.)
      ZB = ZBAG + ELEV(1,1)
C
C  Assign the parameters for common MAPCOR. XFAC and YFAC are the factors
c  needed to convert the UTMs to the map scaling.  These will change with
C  the various locations running NAPS. These values are used in the routines
C  concerned with drawing the contour maps. (BLASTMAP, PLOTOPTS, etc,)
      DELRES = DELRT
      RMAX   = RMAXD
      XFAC   =  200000.
      YFAC   = 4300000.
C
C  Set some default values to guard against input data that would exceed
C  limits set by the program.
      NRO = 55
      IMAX = 5
      RRATIO = 0.1
      DELPSI = 10.
C
c  Obtain temperature, east wind, and west wind versus altitude
c  from Daiosonde.  Assume sounder released at the same ground
c  altitude as the blast source, or close by.
      ALT(1) = ELEV(1,1)
      IEL =-1
      CALL MET(IEL)
c     --- MM5 change: close met file in main
c     CLOSE(MT3)
c
c  Write data to the summary file for a tabular record of the run.
      WRITE(MT8,1000)FILLABEL
      WRITE(MT8,*)'XUTM, YUTM, BLT HT, BLT WT, SURF PRESS '
      WRITE(MT8,1100) XB, YB, ZBAG,   WB,     SFPRES
      WRITE(MT8,*)
     &  'RANGE, MAX/SKIP RAYS, MAX/RAYS, AZI INCR, REFL RRATIO'
      WRITE(MT8,1200) RMAX,IMAX,NRO,DELPSI,RRATIO
      WRITE(KT,*)'RANGE, MAX/SKIP RAYS, MAX/RAYS, AZI INCR, REFL RRATIO'
      WRITE(KT,1200) RMAX,IMAX,NRO,DELPSI,RRATIO
      WRITE(6 ,1200) RMAX,IMAX,NRO,DELPSI,RRATIO
 1000 FORMAT(A)
 1100 FORMAT( 2(5X,F8.0), 3(5X,F7.2) )
 1200 FORMAT( F7.0,3X,I5,3X,I5,3X,F7.2,3X,F5.3)
c     --- end of GETBLAST
      RETURN
      END
C
      SUBROUTINE GETSITE(JSITE,NOGUN)
c     --- Retrieve lat,lons on specified site
C         XB,YB     - UTM's of the blast site.
C         XLAT,XLONG- lat. and long. of the blast site.
      IMPLICIT NONE
      INTEGER JSITE
      LOGICAL NOGUN
c-----------------------------------------------------------------------
      INTEGER isnd, ICHOICE, ISITE, IOPT, ICONTR, nsnds, nlevels,
     1        DBPLOTOPT
      REAL    PSIMIN, PSIMAX, LATS, LONS, LATB, LONB, input_delpsi,
     1        xminp,xmaxp,yminp,ymaxp
      LOGICAL DOELV, DOATT, white_backgrnd
      CHARACTER*60 snd_label
      COMMON  /INPUTS/ snd_label, ICHOICE, ISITE, IOPT, ICONTR, PSIMIN,
     1        PSIMAX, LATS, LONS, LATB, LONB, input_delpsi, isnd, DOELV,
     2        DOATT, white_backgrnd, xminp,xmaxp,yminp,ymaxp, nsnds,
     3        nlevels, DBPLOTOPT
c-----------------------------------------------------------------------
      integer ncdir,nodir,nmdir 
      CHARACTER*60 DATADIR,OUTDIR,METDIR,FILELV,FILEAT,FILMET,FILSUM,
     1  FILOUT,met_dsn
      COMMON /filenm/ DATADIR,OUTDIR,METDIR,FILELV,FILEAT,FILMET,FILSUM,
     1  FILOUT,met_dsn,ncdir,nodir,nmdir
c-----------------------------------------------------------------------
      CHARACTER METLABEL*80, FILLABEL*80, SITE*10
      COMMON /IDTAG/  METLABEL, FILLABEL, SITE
c-----------------------------------------------------------------------
      INTEGER KI,KT,MT1,MT2,MT3,MT4,MT8
      COMMON /LUNT/ KI,KT,MT1,MT2,MT3,MT4,MT8
c-----------------------------------------------------------------------
      INTEGER IGUN
      REAL    XB,YB,ZB,WB,SFPRES,DIRANG,ZBAG
      COMMON /SOURCE/ XB,YB,ZB,WB,SFPRES,DIRANG,IGUN,ZBAG
c-----------------------------------------------------------------------
      REAL    RMAXD, DELRT, DELS, ALTGMX
      INTEGER ATT, ELEV, NPPSPK
      COMMON/WHEELS/ ATT(751,73),ELEV(751,73),RMAXD,NPPSPK,DELRT,DELS,
     &               ALTGMX
c-----------------------------------------------------------------------
      real   XLAT,XLONG,RMXE
      COMMON /AUX/ XLAT,XLONG,RMXE
c-----------------------------------------------------------------------
      INTEGER I, J, IOS, NPSPKE, NPSPKA, TEMP(751)
      REAL    XSE, YSE, DELRE
      REAL    XSA, YSA, DELRA, RMXA
C
      print *,'enter GETSITE: JSITE=',JSITE
      NOGUN = .TRUE.
      IF(JSITE .EQ. 1) THEN
        FILELV = DATADIR(1:ncdir)//'elev1.dat'
        FILEAT = DATADIR(1:ncdir)//'att1.dat'
        XB     =  395061.
        YB     = 4359138.
        XLAT   = 39.377
        XLONG  = 76.218
        SITE   = 'BRIAR PT. '
      ELSEIF(JSITE .EQ. 2) THEN
        FILELV = DATADIR(1:ncdir)//'elev2.dat'
        FILEAT = DATADIR(1:ncdir)//'att2.dat'
        XB     =  406950.
        YB     = 4365950.
        XLAT   = 39.444
        XLONG  = 76.010
        SITE   = 'FUSE RNG. '
        DIRANG = 224.
        NOGUN = .FALSE.
      ELSEIF(JSITE .EQ. 3) THEN
        FILELV = DATADIR(1:ncdir)//'elev3.dat'
        FILEAT = DATADIR(1:ncdir)//'att3.dat'
        XB     =  388508.
        YB     = 4354419.
        XLAT   = 39.334
        XLONG  = 76.294
        SITE   = 'H-FIELD   '
        DIRANG = 28.
        NOGUN = .FALSE.
      ELSEIF(JSITE .EQ. 4) THEN
        FILELV = DATADIR(1:ncdir)//'elev4.dat'
        FILEAT = DATADIR(1:ncdir)//'att4.dat'
        XB     =  389112.
        YB     = 4353177.
        XLAT   = 39.323
        XLONG  = 76.286
        SITE   = 'I-FIELD   '
      ELSEIF(JSITE .EQ. 5) THEN
        FILELV = DATADIR(1:ncdir)//'elev5.dat'
        FILEAT = DATADIR(1:ncdir)//'att5.dat'
        XB     =  404162.
        YB     = 4369504.
        XLAT   = 39.472
        XLONG  = 76.114
        SITE   = 'MAIN FRONT'
        DIRANG = 237.
        NOGUN  = .FALSE.
      ELSEIF(JSITE .EQ. 6) THEN
        FILELV = DATADIR(1:ncdir)//'elev6.dat'
        FILEAT = DATADIR(1:ncdir)//'att6.dat'
        XB     =  393090.
        YB     = 4358579.
        XLAT   = 39.372
        XLONG  = 76.241
        SITE   = 'NEW B FLD.'
      ELSEIF(JSITE .EQ. 7) THEN
        FILELV = DATADIR(1:ncdir)//'elev7.dat'
        FILEAT = DATADIR(1:ncdir)//'att7.dat'
        XB     =  393286.
        YB     = 4360611.
        XLAT   = 39.390
        XLONG  = 76.239
        SITE   = 'OLD B FLD.'
      ELSEIF(JSITE .EQ. 8) THEN
        FILELV = DATADIR(1:ncdir)//'elev8.dat'
        FILEAT = DATADIR(1:ncdir)//'att8.dat'
        XB     =  404773.
        YB     = 4366752.
        XLAT   = 39.447
        XLONG  = 76.107
        SITE   = 'PLATE RNG.'
        DIRANG = 221.
        NOGUN  = .FALSE.
      ELSEIF(JSITE .EQ. 9) THEN
        FILELV = DATADIR(1:ncdir)//'elev9.dat'
        FILEAT = DATADIR(1:ncdir)//'att9.dat'
        XB     =  394957.
        YB     = 4362069.
        XLAT   = 39.403
        XLONG  = 76.220
        SITE   = 'PVRTY IS. '
      ELSEIF(JSITE .EQ.10) THEN
        FILELV = DATADIR(1:ncdir)//'elev10.dat'
        FILEAT = DATADIR(1:ncdir)//'att10.dat'
        XB     =  403155.
        YB     = 4369208.
        XLAT   = 39.469
        XLONG  = 76.126
        SITE   = 'TRENCH WAR'
        DIRANG = 244.5
        NOGUN  = .FALSE.
      ELSEIF(JSITE .EQ.11) THEN
        FILELV = DATADIR(1:ncdir)//'elev11.dat'
        FILEAT = DATADIR(1:ncdir)//'att11.dat'
        XB     =  400439. 
        YB     = 4366128.
        XLAT   = 39.882
        XLONG  = 76.300
        SITE   = 'RECOILESS RANGE B'
        DIRANG = 321.
        NOGUN  = .FALSE.
      ELSEIF(JSITE .EQ.12 ) THEN
        FILELV = DATADIR(1:ncdir)//'elev12.dat'
        FILEAT = DATADIR(1:ncdir)//'att12.dat'
        XB     =  394483.
        YB     = 4358899.
        XLAT   = 39.375
        XLONG  = 76.225
        SITE   = 'ABBEY PT'
      ELSEIF(JSITE .EQ.13 ) THEN
        FILELV = DATADIR(1:ncdir)//'elev13.dat'
        FILEAT = DATADIR(1:ncdir)//'att13.dat'
        XB     =  382925.
        YB     = 4352620.
        XLAT   = 39.317
        XLONG  = 76.358
        SITE   = 'CARROLL IS'
      ELSEIF(JSITE .EQ.14 ) THEN
        FILELV = DATADIR(1:ncdir)//'elev14.dat'
        FILEAT = DATADIR(1:ncdir)//'att14.dat'
        XB     =  390176.
        YB     = 4358959.
        XLAT   = 39.375
        XLONG  = 76.275
        SITE   = 'C-FIELD'
        DIRANG = 144.
        NOGUN  = .FALSE.
      ELSEIF(JSITE .EQ.15 ) THEN
        FILELV = DATADIR(1:ncdir)//'elev15.dat'
        FILEAT = DATADIR(1:ncdir)//'att15.dat'
        XB     =  397367.
        YB     = 4361636.
        XLAT   = 39.400
        XLONG  = 76.192
        SITE   = '9600 IMPACT'
      END IF 
      xlong=-xlong
      LATB=XLAT
      LONB=XLONG
      if(NOGUN) IGUN=0
c
c     --- Open the elevation and attribute data files.
c     print *,'FILELV,FILAT=',FILELV,FILEAT
c     print *,'MT1,MT2=',MT1,MT2
      OPEN(UNIT=MT1,FILE=FILELV,STATUS='OLD',FORM='FORMATTED',
     1 iostat=ios)
      if(ios.ne.0) then
        write(kt,*) 'error opening input elevation file:',filelv
        stop
      endif
      OPEN(UNIT=MT2,FILE=FILEAT,STATUS='OLD',FORM='FORMATTED',
     1 iostat=ios)
      if(ios.ne.0) then
        write(kt,*) 'error opening input attribute file:',fileat
        stop
      endif
c
      WRITE(KT,*)'READING ELEVATION AND LAND USE DATA FILES.'
C
C  Read in headers for the elevation and land use/land cover data.
      READ(MT1,*) XSE,YSE,RMXE,NPSPKE,DELRE
      READ(MT2,*) XSA,YSA,RMXA,NPSPKA,DELRA
C
C  Check if the two data file are for the same location.
      IF (XSE.NE.XSA.OR.YSE.NE.YSA.OR.RMXE.NE.RMXA.OR.
     &   NPSPKE.NE.NPSPKA.OR.DELRE.NE.DELRA) THEN
         WRITE(KT,*)
         WRITE(KT,*) 'INCONSISTENT DATA FOR ELEV AND ATT SPOKES DB '
         WRITE(KT,*) 'XSE, YSE, RMXE, NPSPKE, DELRE :'
         WRITE(KT,*)  XSE, YSE, RMXE, NPSPKE, DELRE
         WRITE(KT,*) 'XSA, YSA, RMXA, NPSPKA, DELRA :'
         WRITE(KT,*)  XSA, YSA, RMXA, NPSPKA, DELRA
         STOP
      ENDIF
c
      RMAXD=RMXE
      NPPSPK=NPSPKE
      DELRT=DELRE
      DELS=5.0
C
C  Read in elevation data.
      write(kt,*) 'DOELV=',DOELV
c     write(6 ,*) 'DOELV=',DOELV
c     print *,'NPSPKE=',NPSPKE
      if(DOELV) then
        DO 33 I=1,72
          READ(MT1,*,iostat=ios) (TEMP(J), J=1,NPSPKE)
          DO 330 J=1,NPSPKE
            ELEV(J,I)=TEMP(J)
 330      CONTINUE
 33     CONTINUE
        WRITE(KT,*)'    COMPLETED READING ELEVATION DATA FILE. '
        WRITE(6 ,*)'    COMPLETED READING ELEVATION DATA FILE. '
      else
        write(kt,*) 'setting elevation data to zero'
        write(6 ,*) 'setting elevation data to zero'
        DO I=1,72
          DO J=1,NPSPKE
            ELEV(J,I)=0.
          ENDDO
        ENDDO
      endif
C
C  Read in land use/land cover data.
      write(kt,*) 'DOATT=',DOATT
      write(6 ,*) 'DOATT=',DOATT
      if(DOATT) then
        DO 44 I = 1,72
          READ(MT2,*) ( TEMP(J), J=1,NPSPKA)
          DO 440 J = 1, NPSPKA
  440        ATT(J,I)=TEMP(J)
   44   CONTINUE
        WRITE(KT,*)'    COMPLETED READING LAND USE DATA FILE. '
        WRITE(6 ,*)'    COMPLETED READING LAND USE DATA FILE. '
      else
        write(kt,*) 'setting attribute data to zero'
        write(6 ,*) 'setting attribute data to zero'
        DO I=1,72
          DO J=1,NPSPKA
            ATT(J,I)=0.
          ENDDO
        ENDDO
      endif
c
      CLOSE (UNIT=MT1)
      CLOSE (UNIT=MT2)
C
C  Set up element no. 73 for plotting purposes.
      DO 50 I = 1,NPPSPK
        ATT(I,73) = ATT(I,1)
        ELEV(I,73) = ELEV(I,1)
50    CONTINUE
C
C  Does the blast site match the data files?
      IF ((XSE.NE.XB).OR.(YSE.NE.YB)) THEN
        WRITE(KT,*) 'ERROR, DATA SOURCE DOESN''T MATCH BLAST SOURCE.'
        WRITE(KT,*) 'XB  = ', XB ,'    YB  = ', YB
        WRITE(KT,*) 'XSE = ', XSE,'    YSE = ', YSE
        STOP
      ENDIF
      RETURN
      END
C
      SUBROUTINE NEWPAG
C----------------------------------------------------------------------
C  The IBM graphics uses this call to clear the screen.
cstub CALL GMODE(3)
      RETURN
      END
C----------------------------------------------------------------------
C
      SUBROUTINE BLTMENU (ZBAG, WB, IGUN, DIRANG, NOGUN)
C----------------------------------------------------------------------
C  This subroutine ask the user to input the data concerning
C  the blast site.
C  OUTPUT VARIABLES:
C             ZBAG  - height of blast above  ground level.(M)
C             WB    - blast weight in pounds
C             IGUN  - type of weapon used for blast
C             DIRANG- firing direction of the weapon (Cartesian angle)
C
C----------------------------------------------------------------------
      integer KI,KT,MT1,MT2,MT3,MT4,MT8
      COMMON /LUNT/ KI,KT,MT1,MT2,MT3,MT4,MT8
c-----------------------------------------------------------------------
      CHARACTER *1 ANSR
      LOGICAL NOGUN
C
 1000 FORMAT( A, $ )
 1200 FORMAT( A)
C
c 125 WRITE(KT,*)'     '
c     WRITE(KT,*)'    BLAST DATA INPUT SCREEN '
c     WRITE(KT,*)'   '
c     WRITE(KT,1000)' ENTER BLAST HEIGHT ABOVE SURFACE IN METERS.  '
c     READ (KI,*,ERR=125) ZBAG
C
c 150 WRITE(KT,*)'     '
c     WRITE(KT,1000)' ENTER BLAST WEIGHT IN POUNDS.  '
c     READ (KI,*,ERR=150) WB
      IF(WB. LT. 0. ) THEN
         WRITE(KT,*)' NEGATIVE BLAST WEIGHT'
         stop
      END IF
C
C  If not an assigned gun site skip query for weapon type.
      IF (NOGUN) GO TO 900
C     
c 175 WRITE(KT,*) '     '
c     WRITE(KT,*) '     '
c     WRITE(KT,*)'       WEAPON TYPE MENU    '
c     WRITE(KT,*) '     '
c     WRITE(KT,*)'    0.. UNIFORM BLAST     '
c     WRITE(KT,*)'    1.. 105 HOWITZER  M102'
c     WRITE(KT,*)'    2.. 105 TANK  M60 '
c     WRITE(KT,*)'    3.. 8 INCH HOWITZER  M110'
c     WRITE(KT,*)'    4.. 8 INCH SELF PROPELLED  M110A1'
c     WRITE(KT,*)'    5.. 120 TANK  (HEAT_TPT)  '
c     WRITE(KT,*)'    6.. 120 TANK  (SABOT)  '
c     WRITE(KT,*)'     '
c     WRITE(KT,1000)'    ENTER THE NUMBER OF YOUR CHOICE:  '
c     READ (KI,*,ERR=175) IGUN
C
      IF (IGUN .LT.0 .OR. IGUN .GT. 6 ) THEN
          WRITE(KT,*)' INVALID GUN TYPE'
          stop
c         READ(KI,1200) ANSR
c         CALL NEWPAG
c         GO TO 175
      END IF
C
C  If uniform blast exit.
      IF(IGUN .LT. 1 ) GO TO 900
C
C  Keep this code for the present.  May want to revert to user
C  input of weapon type and direction.
C***********************************************************
C 200  WRITE(KT,*)'    '
C      WRITE(KT,*)'    '
C      WRITE(KT,*)' THE WEAPON FIRING DIRECTION IS MEASURED'
C      WRITE(KT,*)' IN DEGREES CLOCKWISE FROM NORTH. '
C      WRITE(KT,*)'    '
C      WRITE(KT,1000)' PLEASE ENTER THE FIRING DIRECTION:  '
C      READ (KT,*,ERR=200) DIR
C  Convert the met direction to standard x,y axis.
C      IF(DIR .GE. 0. .OR. DIR .LE. 90.) THEN
C         DIRANG = 90. - DIR
C      ELSE
C          DIRANG = 450.- DIR
C      END IF
C  *********************************************************
 900  RETURN
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
c-----------------------------------------------------------------------
      INTEGER isnd, ICHOICE, ISITE, IOPT, ICONTR, nsnds, nlevels,
     1        DBPLOTOPT
      REAL    PSIMIN, PSIMAX, LATS, LONS, LATB, LONB, input_delpsi,
     1        xminp,xmaxp,yminp,ymaxp
      LOGICAL DOELV, DOATT, white_backgrnd
      CHARACTER*60 snd_label
      COMMON  /INPUTS/ snd_label, ICHOICE, ISITE, IOPT, ICONTR, PSIMIN,
     1        PSIMAX, LATS, LONS, LATB, LONB, input_delpsi, isnd, DOELV,
     2        DOATT, white_backgrnd, xminp,xmaxp,yminp,ymaxp, nsnds,
     3        nlevels, DBPLOTOPT
c-----------------------------------------------------------------------
      integer ncdir,nodir,nmdir 
      CHARACTER*60 DATADIR,OUTDIR,METDIR,FILELV,FILEAT,FILMET,FILSUM,
     1  FILOUT,met_dsn
      COMMON /filenm/ DATADIR,OUTDIR,METDIR,FILELV,FILEAT,FILMET,FILSUM,
     1  FILOUT,met_dsn,ncdir,nodir,nmdir
c-----------------------------------------------------------------------
      real    YMETRS,TEMPER,RELHUM,WNDSPD,WNDDRT
      integer KOUNT
      COMMON /DATA/ YMETRS(70),TEMPER(70),RELHUM(70),WNDSPD(70),
     &              WNDDRT(70),KOUNT
c-----------------------------------------------------------------------
      REAL    ALTN,TN,RH,WN,WE,V,WU,WV,SV,p_mb
      INTEGER II
      COMMON /WEATHR/ALTN(70),TN(70),RH(70),WN(70),WE(70),V(70),WU(70),
     &                WV(70),SV(70),p_mb(70),II
c-----------------------------------------------------------------------
      integer KI,KT,MT1,MT2,MT3,MT4,MT8
      COMMON /LUNT/ KI,KT,MT1,MT2,MT3,MT4,MT8
c-----------------------------------------------------------------------
      CHARACTER METLABEL*80, FILLABEL*80, SITE*10
      COMMON /IDTAG/  METLABEL, FILLABEL, SITE
c-----------------------------------------------------------------------
      INTEGER IGUN
      REAL    XB,YB,ZB,WB,SFPRES,DIRANG,ZBAG
      COMMON /SOURCE/ XB,YB,ZB,WB,SFPRES,DIRANG,IGUN,ZBAG
c-----------------------------------------------------------------------
      REAL NORTH,KNOTS
      logical EXTRAPOLATE
      parameter (EXTRAPOLATE=.FALSE.)
c     parameter (EXTRAPOLATE=.TRUE.)
      character*80 record
      character*2  cnlevels
      CHARACTER*80 IDLAB1,IDLAB2
      DATA FTPERM/3.280833/,DEGPRAD/57.29578/,FPSTOKN/0.5925/
C
C  Call up a menu to get the user's options for met data input to the
C  model.  The data file will be opened on Unit 3 which is used as the
C  input unit throughout the program. (thus no file name necessary here.
c     --- MM5 change: read file already opened in main
c     OPEN( UNIT = MT3, FILE = FILMET, STATUS ='OLD', IOSTAT=IOS )
c     write(kt,*) 'opening FILMET=',FILMET
c     if(ios.ne.0) then
c       print *,'error opening input metfile=',FILMET
c       stop
c     endif
c
C "KOUNT" IS THE COUNTER FOR THE ARRAYS HAVING THE MET DATA
crds  KOUNT = 1
      KOUNT = 0
      II = 0
cmm5  --- for mm5 files without surface data, extrapolate to the surface
      if(EXTRAPOLATE) then
        KOUNT = 1
        II = 1
      endif
C
C  Read in the file id label and the surface conditions.
cmm5  --- MM5 change.  Check for blank records
      write(cnlevels,222) nlevels
  222 format(I2)
      do kread=1,5
        read(MT3,199) record
        CALL shftlft(record)
        if(record(1:1).eq.' ') go to 277
        if(record(1:2).eq.cnlevels) go to 277
        FILLABEL=record
        go to 278
  277   continue
      enddo
  278 continue
cmm5  READ(MT3,1111)FILLABEL
      print *, 'FILLABEL=',FILLABEL
cmm5  --- isolate sounding lat,lon
      record=FILLABEL
      CALL shftlft(record)
      NL=index(record,'i,j=')
      snd_label=record(NL:)
      FILLABEL=record(1:NL-2)
      NL=index(record,'long')
      record=record(NL+6:)
      CALL shftlft(record)
      NL = INDEX(record,',') - 1
      if(NL.LE.0) go to 287
      LATS=0.
      read(record(1:NL), '(E30.0)', IOSTAT = IOS) LATS
      record = record(NL+2: )
      CALL shftlft(record)
      NL = INDEX(record,')') - 1
      LONS=0.
      read(record(1:NL), '(E30.0)', IOSTAT = IOS) LONS
      print *,'in MET: LATS,LONS=',LATS,LONS
  287 continue
      do kread=1,5
        read(MT3,199) record
        CALL shftlft(record)
        if(record(1:1).eq.' ') go to 288
        IDLAB1=record
        go to 289
  288   continue
      enddo
  289 continue
cmm5  READ(MT3,1111)IDLAB1
cmm5  print *, 'IDLAB1=',IDLAB1
 1111 FORMAT (A)
      do kread=1,5
        read(MT3,199) record
        CALL shftlft(record)
        if(record(1:1).eq.' ') go to 298
        N = INDEX(record,' ') - 1
        SURFHGT=0.
        read(record(1:N), '(E30.0)', IOSTAT = IOS) SURFHGT
        record = record(N+1: )
        CALL shftlft(record)
        N = INDEX(record,' ') - 1
        SFPRES=0.
        read(record(1:N), '(E30.0)', IOSTAT = IOS) SFPRES
        go to 299
  298   continue
      enddo
  299 continue
cmm5  READ(MT3,*)SURFHGT, SFPRES
c following is an attempt to make sure the pressure is really a pressure,
c as we have had 2 different formats for this line...
        if (sfpres .lt. 500.0) then                !bogus looking pressure?
          backspace(mt3)                   !yes. go back and try other format
          read(mt3,*) surfhgt,sftemp,sfrh,sfwspd,sfwdir,sfpres
        end if
      do kread=1,5
        read(MT3,199) record
        CALL shftlft(record)
        if(record(1:1).eq.' ') go to 268
        IDLAB2=record
        go to 269
  268   continue
      enddo
  269 continue
cmm5  READ(MT3,1111) IDLAB2
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
      write(kt,*) 'SURFHGT=',SURFHGT

C  Initialize altitude accumulator with surface height.
      HEIGHT=SURFHGT

C   Read set of input data (id number,altitude,temperature ordinate,
C   relative humidity ordinate, wind speed, wind direction )
C
  700 CONTINUE
C
  199 format(A80)
      do kread=1,1000
        read(MT3,199,end=5000) record
        CALL shftlft(record)
        N = INDEX(record,' ') - 1
        if(record(1:1).eq.' ') go to 499
        nbr=0
        read(record(1:N), '(I30)', IOSTAT = IOS) nbr
        record = record(N+1: )
        CALL shftlft(record)
        N = INDEX(record,' ') - 1
        AGLALT=0.
        read(record(1:N), '(E30.0)', IOSTAT = IOS) AGLALT
        record = record(N+1: )
        CALL shftlft(record)
        N = INDEX(record,' ') - 1
        TC=0.
        read(record(1:N), '(E30.0)', IOSTAT = IOS) TC
        record = record(N+1: )
        CALL shftlft(record)
        N = INDEX(record,' ') - 1
        RO=0.
        read(record(1:N), '(E30.0)', IOSTAT = IOS) RO
        record = record(N+1: )
        CALL shftlft(record)
        N = INDEX(record,' ') - 1
        WS=0.
        read(record(1:N), '(E30.0)', IOSTAT = IOS) WS
        record = record(N+1: )
        CALL shftlft(record)
        N = INDEX(record,' ') - 1
        WD=0.
        read(record(1:N), '(E30.0)', IOSTAT = IOS) WD
        record = record(N+1: )
        CALL shftlft(record)
        PRESS=0.
        read(record, '(E30.0)', IOSTAT = IOS) PRESS
cmm5    READ(MT3,*,END=5000)NBR,AGLALT,TC,RO,WS,WD,PRESS  ! usual
c       write(*,3000)NBR,AGLALT,TC,RO,WS,WD,PRESS
        HEIGHT = AGLALT + SURFHGT
        KNOTS  = WS * FTPERM * FPSTOKN
        EAST   = WS * FTPERM * SIN( WD/DEGPRAD )  ! ft/s
        NORTH  = WS * FTPERM * COS( WD/DEGPRAD )  ! ft/s
 2000   HGTFT=HEIGHT*FTPERM
C
C  Put met data in arrays in common /data/ to be displayed
        KOUNT = KOUNT + 1
        YMETRS(KOUNT) = HEIGHT
        TEMPER(KOUNT) = TC
        RELHUM(KOUNT) = RO
c       WNDSPD(KOUNT) = KNOTS
        WNDSPD(KOUNT) = WS  ! M/S
        WNDDRT(KOUNT) = WD
C
C  Fill the Weathr common with the read in data 
        II=II+1
        ALTN(II)=HEIGHT
        TN(II)=TC
        RH(II)=RO
        WE(II)=EAST
        WN(II)=NORTH
        p_mb(II) = press
c
c       --- write data to output file
        WRITE(KT,3000) II,HEIGHT,TC,RO,WS,WD,EAST,NORTH,press
c       WRITE(* ,3000) II,HEIGHT,TC,RO,WS,WD,EAST,NORTH,press
  499   CONTINUE
        if(KOUNT.eq.nlevels) go to 5000
c
      enddo
 5000 CONTINUE
 3000 format(i5,f9.1,6f9.2,f11.2)
c
cmm5  --- for mm5 files without surface data, extrapolate to the surface
      if(EXTRAPOLATE) then
c     --- set surface values, k=1
c     sigma(1)= 1.0
      p_mb(1) = SFPRES
      z1 = SURFHGT
      ALTN(1) = SURFHGT
c     z2 = ALTN(2)
c     z3 = ALTN(3)
c     rdz = (z2-z1)/(z3-z2)
cc    TN(1) = TN(2) - (TN(3)-TN(2))*rdz
cc    RH(1) = RH(2) - (RH(3)-RH(2))*rdz
cc    WE(1) = WE(2) - (WE(3)-WE(2))*rdz
cc    WN(1) = WN(2) - (WN(3)-WN(2))*rdz
      TN(1) = TN(2)
      RH(1) = RH(2)
      WE(1) = WE(2)
      WN(1) = WN(2)
      YMETRS(1) = SURFHGT
      TEMPER(1) = TN(1)
      RELHUM(1) = RH(1)
      WS = SQRT(WE(1)**2+WN(1)**2)  ! ft/s
      WNDSPD(1) = WS/FTPERM     ! m/s
      ZMETANG = WNDDRT(1)
cc    DEGX = ATAN2(WN(1),WE(1))*DEGPRAD
cc    IF ( DEGX .LE. 90.) THEN
cc       ZMETANG = 90. - DEGX
cc    ELSE
cc       ZMETANG = 450. - DEGX 
cc    END IF
      WNDDRT(1)=ZMETANG
      write(KT,*) 'extrapolated mm5 sounding'
      WRITE(*,1112)
      do k=1,KOUNT
        AGLALT = ALTN(k) - SURFHGT
        WRITE(KT,3000) k,AGLALT,TN(k),RH(k),Wndspd(k),WNDDRT(k),
     1    p_mb(k)
        WRITE(6 ,3000) k,AGLALT,TN(k),RH(k),Wndspd(k),WNDDRT(k),
     1    p_mb(k)
      enddo
      endif
c
      RETURN
      END
C
crds  SUBROUTINE USER(IOPT,IDBFLG)
      SUBROUTINE USER(JOPT,IDBFLG)
C-----------------------------------------------------------------------
C      WRITTEN WITH THE HELP FROM CODE IN TEKGRS WRITTEN BY RCLAIR
C
C      FUNCTION: To provide the user with menus for selecting the MET
C      profile to be displayed.
C-----------------------------------------------------------------------
c-----------------------------------------------------------------------
      INTEGER isnd, ICHOICE, ISITE, IOPT, ICONTR, nsnds, nlevels,
     1        DBPLOTOPT
      REAL    PSIMIN, PSIMAX, LATS, LONS, LATB, LONB, input_delpsi,
     1        xminp,xmaxp,yminp,ymaxp
      LOGICAL DOELV, DOATT, white_backgrnd
      CHARACTER*60 snd_label
      COMMON  /INPUTS/ snd_label, ICHOICE, ISITE, IOPT, ICONTR, PSIMIN,
     1        PSIMAX, LATS, LONS, LATB, LONB, input_delpsi, isnd, DOELV,
     2        DOATT, white_backgrnd, xminp,xmaxp,yminp,ymaxp, nsnds,
     3        nlevels, DBPLOTOPT
c-----------------------------------------------------------------------
      integer KI,KT,MT1,MT2,MT3,MT4,MT8
      COMMON /LUNT/ KI,KT,MT1,MT2,MT3,MT4,MT8
      CHARACTER*2 ANSR
c-----------------------------------------------------------------------
      real    XFAC,YFAC,DELRES,RXL,RYL,RXH,RYH,X1,Y1,X2,Y2,
     1  UB,UT,UR,UL,TRXB,TRYB
      COMMON /MAPCOR/ XFAC,YFAC,DELRES,RXL,RYL,RXH,RYH,X1,Y1,X2,Y2,
     1  UB,UT,UR,UL,TRXB,TRYB
c-----------------------------------------------------------------------
      INTEGER IGUN
      REAL    XB,YB,ZB,WB,SFPRES,DIRANG,ZBAG
      COMMON /SOURCE/ XB,YB,ZB,WB,SFPRES,DIRANG,IGUN,ZBAG
c-----------------------------------------------------------------------
      CHARACTER METLABEL*80, FILLABEL*80, SITE*10
      COMMON /IDTAG/  METLABEL, FILLABEL, SITE
c-----------------------------------------------------------------------
      real    A,CONTRS,RMAX,DELPSI,RRATIO
      integer M,N,IRAMAX,NRO,NCONTS
      COMMON /SOUND/ A(751,361),M,N,CONTRS(51),RMAX,DELPSI,IRAMAX,
     &               NRO,NCONTS,RRATIO
c-----------------------------------------------------------------------
      CHARACTER*60 DBFLNAM
      COMMON /DBFILE/ DBFLNAM
c-----------------------------------------------------------------------
      integer ncdir,nodir,nmdir 
      CHARACTER*60 DATADIR,OUTDIR,METDIR,FILELV,FILEAT,FILMET,FILSUM,
     1  FILOUT,met_dsn
c-----------------------------------------------------------------------
      COMMON /filenm/ DATADIR,OUTDIR,METDIR,FILELV,FILEAT,FILMET,FILSUM,
     1  FILOUT,met_dsn,ncdir,nodir,nmdir
c-----------------------------------------------------------------------
      LOGICAL DEFLT
      COMMON /DEFAULT/ DEFLT
c-----------------------------------------------------------------------
      real   XLAT,XLONG,RMXE
      COMMON /AUX/ XLAT,XLONG,RMXE
c-----------------------------------------------------------------------
C
 1000 FORMAT(A)
 1010 FORMAT(A,$)
      IF (DEFLT) GO TO 200
C
c      WRITE(KT,*)'           SCREEN DISPLAYS     '
c      WRITE(KT,*)'     '
c      WRITE(KT,*)' THE PROGRAM GENERATES RAYTRACE DATA AND (OPTIONAL)'
c      WRITE(KT,*)' SCREEN PLOTS. THE COMPLETION OF A PLOT IS INDICATED'
c      WRITE(KT,*)' BY THE APPEARANCE OF AN IDENTIFICATION LABEL IN THE'
c      WRITE(KT,*)' UPPER LEFT HAND CORNER OF THE DISPLAY.'
c      WRITE(KT,*)'     '
c      WRITE(KT,*)' TO OBTAIN A HARD COPY OF A SCREEN DISPLAY, PRESS'
c      WRITE(KT,*)' THE PRINT SCREEN KEY.  ONCE THE SCREEN HAS BEEN'
c      WRITE(KT,*)' ERASED THE PLOT CAN NOT BE RECOVERED DURING THIS'
c      WRITE(KT,*)' RUN. TO CLEAR THE SCREEN AND PROCEED TO THE NEXT'
c      WRITE(KT,*)' PLOT, PRESS THE RETURN KEY.'
c      WRITE(KT,*)'     '
c      WRITE(KT,*)' THE DECIBEL DATA GENERATED FOR THE CONTOUR PLOT'
c      WRITE(KT,*)' IS STORED IN THE FILE, "DECIBEL.DAT."  '
c      WRITE(KT,*)' THE PC RETAINS ONLY ONE VERSION OF A FILE SO'
c      WRITE(KT,*)' THIS DATA FILE IS OVERWRITTEN EACH TIME THE NAPS'
c      WRITE(KT,*)' PROGRAM IS EXECUTED. BUT YOU MAY RENAME THE DECIBEL'
c      WRITE(KT,*)' FILE , THUS SAVING IT FOR FUTURE PLOTTING.'
c      WRITE(KT,*)' A SUMMARY FILE IS ALSO GENERATED AND A HARD COPY'
c      WRITE(KT,*)' CAN BE OBTAINED BY EXECUTING A PRINT COMMAND AT'
c      WRITE(KT,*)' THE CONCLUSION OF THE PROGRAM.'
c      WRITE(KT,*)'     '
c      WRITE(KT,*)' PRESS THE RETURN KEY TO CONTINUE.'
c      READ (KI,1000)ANSR
C
 200  CONTINUE
      CALL NEWPAG
C
      IF(DEFLT) THEN
         IOPT = 0
         IDELPSI = 15
         IDBFLG = 1
         GO TO 210
      END IF
 209  CALL MENU(IOPT,IDBFLG)
C
C 99 WILL END THE PROGRAM AND 4 WILL RETURN TO MAIN MENU
C
       IF(IOPT .EQ. 99 .OR. IOPT .EQ. 4 ) THEN
          RETURN
       END IF
C
  210  CONTINUE
C IF IOPT = 0,1,OR 2 Set the flag to run the blast routine to generate
C data for the contour plots.
       
       IF(IOPT .EQ. 0) ICONTR = 2
       IF(IOPT .EQ. 1 .OR. IOPT .EQ. 2) ICONTR = 1
       IF(IOPT .EQ. 0 .OR. IOPT .EQ. 1 .OR. IOPT .EQ. 2 ) THEN
         IF(DEFLT) GO TO 230
          WRITE(KT,*)'        SELECT AZIMUTH INCREMENT  '
          WRITE(KT,*)'  '
          WRITE(KT,*)
     &  ' THE AZIMUTH ANGLES ARE MEASURED CLOCKWISE FROM NORTH.'
          WRITE(KT,*)' INCREMENTS GREATER THAN 15 WILL NOT GENERATE A'
          WRITE(KT,*)' RELIABLE CONTOUR PLOT.'
c         WRITE(KT,*)
c    &  ' THE INCREMENTS MUST BE A MULTIPLE OF 5 AND A FACTOR OF 360. '
c         WRITE(KT,*)'    '
c 220   WRITE(KT,1010)'  ENTER YOUR CHOICE OF AZIMUTH ANGLE INCREMENT '
c         READ(KI,*,ERR=220)DELPSI
c         IDELPSI = INT(DELPSI+.0001)
c         IF(MOD(IDELPSI,5) .NE. 0)THEN
c            WRITE(KT,*)' INCREMENT NOT A MULTIPLE OF 5.'
c            WRITE(KT,*)' PRESS RETURN KEY AND TRY AGAIN.'
c            READ(KI,1000) ANSR
c            GO TO 220
c         END IF
c         IF(MOD(360,IDELPSI) .NE. 0)THEN
c            WRITE(KT,*)' INCREMENT NOT A FACTOR OF 360.'
c            WRITE(KT,*)' PRESS RETURN KEY AND TRY AGAIN.'
c            READ(KI,1000) ANSR
c            GO TO 220
c         END IF
          DELPSI = input_delpsi      ! rds
          WRITE(KT,*)' '
          WRITE(KT,*)
     &      ' THE RAYTRACE PLOTS WILL BEGIN AT 90 DEGREES (EAST),'
          WRITE(KT,1111) DELPSI
 1111     FORMAT('  AND WILL OCCUR EVERY',F5.0,' DEGREES')
          WRITE(KT,*)' COUNTERCLOCKWISE FROM EAST. '
          WRITE(KT,*)' '
c         WRITE(KT,*)' PLEASE PRESS THE RETURN KEY TO CONTINUE. '
c         READ(KI,1000)ANSR
c230      CALL NEWPAG
 230      CONTINUE
C
C   Open a file to store the DB data for the contour plots.
c   Add 3 header lines to the file for identification reasons.
          OPEN (UNIT = 15, FILE = DBFLNAM, FORM = 'FORMATTED',
     &          STATUS = 'UNKNOWN' )
          WRITE(15,1000) FILLABEL
          WRITE(15,1000) METLABEL
c         WRITE(15,*)    XB, YB, ZBAG, WB, RMAX, DELPSI,DELRES
          WRITE(15,1001) XB, YB, ZBAG, WB, RMAX, DELPSI,DELRES
 1001     format(7F12.2)
C
crds ######### moved call to BLAST to main routine
c         CALL BLAST(DELPSI,IRAMAX,RMAX,NRO,ICONTR,IOPT,RRATIO)
c         CALL NEWPAG

C  Call a menu to get other plot options from the user.
C
c         CALL PLOTOPTS(IMAIN,IDBFLG)
c         IF(IMAIN .EQ. 1)THEN
c            CALL NEWPAG
c            RETURN
c         END IF
       ENDIF
C
c      IF(IOPT .EQ. 3 ) THEN
c        ICONTR = 0
c        WRITE(KT,*)
c    *   ' THE AZIMUTH ANGLES ARE MEASURED CLOCKWISE FROM NORTH.'
c        WRITE(KT,*)' THE INCREMENTS MUST BE A MULTIPLE OF 5.'
c        WRITE(KT,*)'    '
c240     WRITE(KT,1010)
c    *      '  ENTER YOUR CHOICE FOR AZIMUTH ANGLE INCREMENT: '
c        READ(KI,*,ERR=240)DELPSI
c        IDELPSI = INT(DELPSI+.0001)
c        IF(MOD(IDELPSI,5) .NE. 0)THEN
c            WRITE(KT,*)' INCREMENT NOT A MULTIPLE OF 5.'
c            WRITE(KT,*)' PRESS RETURN KEY AND TRY AGAIN.'
c            READ(KI,1000) ANSR
c            GO TO 240
c        END IF
c        CALL NEWPAG
crds ######### moved call to BLAST to main routine
c        CALL BLAST(DELPSI,IRAMAX,RMAX,NRO,ICONTR,IOPT,RRATIO)
c        CALL NEWPAG
c      ENDIF
C
c      GO TO 200
c      --- end of subroutine USER
       return
       END
C
       SUBROUTINE MENU(IOPT,IDBFLG)
C---------------------------------------------------------------------------
C
C     FUNCTION: TO DISPAY THE MENU WHEN IOPT = 0
C
C             IDBFLG  -  Flag indicating the DB data was calculated.
C                        e.i. the file "DECIBEL.DAT" was created.
C---------------------------------------------------------------------------
       integer KI,KT,MT1,MT2,MT3,MT4,MT8
       COMMON /LUNT/ KI,KT,MT1,MT2,MT3,MT4,MT8
       CHARACTER *1 ANSWR
 1000  FORMAT(A)
 1500  FORMAT(A,$)
C
  100  CALL NEWPAG
c      WRITE(KT,*)'         CONTOUR/RAYTRACE  DISPLAY MENU   '
c      WRITE(KT,*)'    '
c      WRITE(KT,*)'    '
c      WRITE(KT,*)
c    *      '  0....GENERATE CONTOUR DATA, RAYTRACES NOT DISPLAYED.'
c      WRITE(KT,*)'    '
c      WRITE(KT,*)
c    *      '  1....GENERATE CONTOUR DATA, RAYTRACES WITH NO PAUSES.'
c      WRITE(KT,*)'    '
c      WRITE(KT,*)
c    *      '  2....GENERATE CONTOUR DATA, PAUSE AT RAYTRACES.'
c      WRITE(KT,*)'    '
c      WRITE(KT,*)'  3....DRAW RAYTRACE PLOTS ONLY, NO CONTOURS. '
c      WRITE(KT,*)'     '
c      WRITE(KT,*)'  4....RETURN TO THE MAIN MENU'
c      WRITE(KT,*)'     '
c      WRITE(KT,*)' 99....STOP - TERMINATE PROGRAM'
c      WRITE(KT,*)'     '
C      WRITE(KT,1500)' ENTER THE NUMBER OF YOUR CHOICE.    '
C      READ(KI,*, ERR=100) IOPT
       IF(IOPT .GT. 4 .AND. IOPT .NE. 99 ) THEN
          WRITE(KT,*)' INVALID ENTRY'
C         READ(KI,1000) ANSWR
C         GO TO 100
          STOP
       END IF
C
       IF(IOPT .LT. 3 ) IDBFLG = 1
       CALL NEWPAG
C
       RETURN
       END
c
      SUBROUTINE BLAST(DELPSI,IRAMAX,RMAX,NROSAV,ICONTR,IOPT,RRATIO)
C----------------------------------------------------------------------
C
C  FUNCTION: BLAST controls computation of air blast propagation.
C
C  VARIABLES:
C             ANGLE  - Raytrace initial elevation angle from blast source
C             DECIBL - Peak pressure in decibels at a ground location
C             DRA,DRB- Ground range increments between adjacent raytraces (m)
C             FACTOR - Magnification factor for focusing of a downward 
C                      reflecting ray.
C             HEIGHT - Apogee height of raytraces. (m)
C             HIGHS  - Apogee height record of multiple raytraces.(m)
C             IFOCAL - Raytrace type: 0=never reached ground, 1=struck ground,
C                      2=focal point.
C             ISUR   - raytrace status: neg=invalid trace, 0=never reached
C                      the ground, pos.= counter for skip rays.
C             ISK    - Number of skip rays + primary ray.
C             JCOAST - Number of shadow or coastal boundaries in a given azimuth.
C             JFOCAL - Number of focal points in a given azimuths.
C             JRAY   - Number of interfering rays at a single raytrace location
C             MAGNIF - Recond of magnification factors for all raytraces in a
C                      azimuth direction.
C             NROSHD - Number of raytraces allotted for the upward refracting
C                      zone
C             PLOTIT - Logical flag indicatiing first call to Raytrace
C                      so axes will be drawn only one time.
C             RANGE  - ground dietance of raytrace (M)
C             RANGES - record of ground distances of all raytraces in a given
C                      azimuth direction
C             REFLCT - ground reflection coefficient at location of reflected ray
C             REFLTS - record of coefficients of all raytraces in given az. direction
C             SI     - current azimuth angle (radians)
C             TBRATO - relative change in pulse duration due to ground reflection.
C             TBRATS - record of relative changes in pulse duration of all 
C                      raytraces in a given az. direction.                      
C             TIMES  - tine of arrival on the ground of a raytrace(including
C                      skip rays)
C             TIMEOA - record of time of arrival of all raytraces in a given
C                      az. direction
C                
C---------------------------------------------------------------------
      CHARACTER *1 ANSWR
      DIMENSION IDEX(500),JDEX(500)
c-----------------------------------------------------------------------
      LOGICAL*1 PLOTIT
      COMMON /PLTRT/PLOTIT
c-----------------------------------------------------------------------
      integer KI,KT,MT1,MT2,MT3,MT4,MT8
      COMMON /LUNT/ KI,KT,MT1,MT2,MT3,MT4,MT8
c-----------------------------------------------------------------------
      INTEGER IGUN
      REAL    XB,YB,ZB,WB,SFPRES,DIRANG,ZBAG
      COMMON /SOURCE/ XB,YB,ZB,WB,SFPRES,DIRANG,IGUN,ZBAG
c-----------------------------------------------------------------------
      CHARACTER METLABEL*80, FILLABEL*80, SITE*10
      COMMON /IDTAG/  METLABEL, FILLABEL, SITE
c-----------------------------------------------------------------------
      integer ISUR,IMAX,JCOAST,JFOCAL
      real    RANGE,MAGNIF,TIMEOA,HEIGHT,REFLCT,TBRATO,DRA,DRB
      COMMON /SKIP/ISUR,IMAX,RANGE(10),MAGNIF(10),TIMEOA(10),HEIGHT(10),
     &      REFLCT(10),TBRATO(10),JCOAST(10),JFOCAL(10),DRA(10),DRB(10)
c-----------------------------------------------------------------------
      COMMON /TRCINP/ IPLOT, XPLOT(751), YPLOT(751), APLOT(751),
     &                        XTEMP(751), YTEMP(751), ISCALE
c-----------------------------------------------------------------------
      REAL    ALT,T,RH,WN,WE,V,WU,WV,SV,p_mb
      INTEGER NP
      COMMON /WEATHR/ALT(70),T(70),RH(70),WN(70),WE(70),V(70),WU(70),
     &               WV(70),SV(70),p_mb(70),NP
c-----------------------------------------------------------------------
      COMMON /RAYMAP/ISK(751), RANGES(751,5), FACTOR(751,5),
     &  TIMES(751,5), HIGHS(751,5), REFLTS(751,5), TBRATS(751,5),
     &  DECIBL(751,5), ANGLE(751), IFOCAL(751,5), JRAY(751,5),
     &  ATOT(751,5)
c-----------------------------------------------------------------------
      REAL    RMAXD, DELRT, DELS, ALTGMX
      INTEGER ATT, ELEV, NPPSPK
      COMMON/WHEELS/ ATT(751,73),ELEV(751,73),RMAXD,NPPSPK,DELRT,DELS,
     &               ALTGMX
c-----------------------------------------------------------------------
      integer ndelsi, nranges, nray
      real    Aza, rangea, dBa, xray, yray
      common /savea/  ndelsi, nranges(361), Aza(361), rangea(751,361),
     1                dBa(751,361), xray(751,101), yray(751,101), nray
c-----------------------------------------------------------------------
      CHARACTER*80 TEXT
      logical plotray1
C
      PI = ACOS(-1.)
      DATA REARTH /6370999./,NROSHD/5/
c
c     write(*,*) 'on entry to BLAST: NPPSPK=',NPPSPK
c     write(*,*) 'ELEV='
c     DO I=1,73
c       write(kt,222) (ELEV(j,i),j=1,NPPSPK)
c     ENDDO
c     write(kt,*) 'ATT='
c     DO I=1,73
c       write(kt,222) (ATT(j,i),j=1,NPPSPK)
c     ENDDO
c 222 format(20I4)
C
C   Call menu option
      IMENU = 1
C   Compute ray data option
      ICALL = 0
C   Hard copy option
      ICOPY = 0
C
      E=1.0E-06
C conversion factor for deg to radians
      ACC=180./PI
      IRAMAX=0
      NDELSI=360./DELPSI+E
      IF (DELPSI .GT. 180.) NDELSI = 2
C
C Determine maximum elevation angle variation for all azimuth angles.  
C Dividing this angle by the maximum number of raytraces will give typical
C elevation angle decrement.
      SINML = 0.0
c     write(6,*) 'initial call to trange'
      DO 1050 IPSI = 1,NDELSI
         SI = (IPSI-1) * DELPSI/ACC
         CALL TRANGE(SI,THETM,THETL,IREFRT)
         IF( IREFRT .EQ. 0 ) THEN
           IF( (SIN(THETM) - SIN(THETL)) .GT. SINML )
     &          SINML = SIN(THETM)-SIN(THETL)
         END IF
c        write(6,*)'ipsi,si,thetl,thetm,irefrt=',
c    1            ipsi,si,thetl,thetm,irefrt
1050  CONTINUE
C
      WRITE(mt8,*)' '
      WRITE(mt8,*)' THE RAYTRACE PLOTS WILL BEGIN AT 90 DEGREES (EAST),'
      WRITE(mt8,1111) DELPSI
 1111 FORMAT('  AND WILL OCCUR EVERY',F5.0,' DEGREES')
      WRITE(mt8,*)' COUNTERCLOCKWISE FROM EAST. '
c
      do iii=1,500
        IDEX(III)=0
        JDEX(III)=0
      enddo	   
c
      DO 1000 IPSI=1,NDELSI
C
C  Check to see if a hard copy is desired before erasing the screen.
c     IF(ICOPY .EQ. 1) THEN
c        CALL GCLIP(0)
c        CALL CJUST(-1.0,0.0)
c        CALL WORLD(0.0,10.0,0.0,10.0)
c        CALL VUPORT(0.0,1.0,0.0,1.0)
c        CALL COLOR(15)
c        WRITE(TEXT,650)  METLABEL(1:35)
c650  FORMAT(' FIRING DATA; ', A)
c        CALL GTX(0.0,6.9,TEXT)
c        CALL ENDPLT
c        IF(IOPT .EQ. 1) GO TO 675
c        WRITE(*,*) CHAR(7)
c        READ(KI,1003) ANSWR
c675     CALL NEWPAG
c     END IF
c
C      SI is measured counterclockwise from true EAST
      SI=(IPSI-1)*DELPSI/ACC
      DEGX = SI * ACC
      nray=0
      print *,'in BLAST: IPSI,DEGX=',IPSI,DEGX
      plotray1=.false.
c####### temporary change #############
c### uncomment the "cplot" commented code and change the desired 
c### azimuths (relative to east) to plot individual rays
cplot if((NINT(DEGX).eq.0).or.(NINT(DEGX).eq.90).or.
cplot1   (NINT(DEGX).eq.180).or.(NINT(DEGX).eq.270)) then
cplot   plotray1=.true.
cplot   plotray1=.true.
cplot endif
c####### end temporary change #############

C   Generate contour plot data? ICONTR=1, yes and ICONTR=0, no.
C   iF ICONTR = 2, generate contour data but do not display traces.
c   Set IMENU to skip menu and MOPT to automatically generate raytraces
c
         IF( ICONTR .GE. 1 ) THEN
            IMENU = 0
            MOPT  = 1
            IF(ICONTR .EQ. 2 ) THEN
               PLOTIT = .FALSE.
            ELSE
               PLOTIT = .TRUE.
            END IF
            GO TO 6
         ELSE
            IF (ICONTR .EQ. 0 ) PLOTIT = .TRUE.
         END IF
c                                           !june 1986
c  Draw the pictorial display of the ray trace
c  use a menu for input of user options for the plots.
c  If IMENU = 0 then skip menu
c     IF( IMENU .EQ. 0) GO TO 4
c Convert the math angle DEGX to a met angle for display purposes.
c     IF ( DEGX .LE. 90.) THEN
c        ZMETANG = 90. - DEGX
c     ELSE
c        ZMETANG = 450. - DEGX 
c     END IF
c
c  3  CALL NEWPAG
c     WRITE(KT,*) ' '
c     WRITE(KT,*) '        RAYTRACE  PLOT  MENU          '
c     WRITE(KT,*) ' '
c     WRITE(KT,*) 'Azimuth angles are measured clock wise from NORTH.'
c     WRITE(KT,*) ' '
c     WRITE(KT,*) 'The Raytrace plots will begin at 90 degrees and'
c     WRITE(KT,*) 'continue counterclock wise.'
c     WRITE(KT,*) ' '
c     WRITE(KT,*) 'The CURRENT azimuth angle is ', ZMETANG
c     WRITE(KT,*) ' '
c     WRITE(KT,*) '            PLOT  OPTIONS '
c     WRITE(KT,*) ' '
c     WRITE(KT,*) ' 1 - PLOT RAYTRACES for current azimuth angle.'
c     WRITE(KT,*) ' '
c     WRITE(KT,*) ' 2 - DO NOT plot raytrace for current azimuth.'
c     WRITE(KT,*) '     Increment to the next azimuth.'
c     WRITE(KT,*) ' '
c     WRITE(KT,*) ' 3 - NO FURTHER  Raytrace plots are desired. '
c     WRITE(KT,*) '  '
c     WRITE(KT,*) '     Please enter number of your choice. '
c     READ(KI,*,ERR=3) MOPT
c    Check if a valid response to options.
c
c     IF( MOPT .LT. 1 .OR. MOPT .GT. 3 ) THEN
c         WRITE(KT,*)'Invalid option number, try again.'
c         READ(KI,1003)ANSWR
c         GO TO 3
c     END IF
c
c     CALL NEWPAG
c
  6   CONTINUE
c
c  If MOPT = 1 then call raytrace plot routines
      IF( MOPT .EQ. 1) THEN
          IF((ICONTR .EQ. 1) .OR. 
     *      ((ICONTR .EQ. 0) .AND. (IOPT .EQ.3) )) THEN 
             ICOPY = 1
             IMENU = 1
             ICALL = 1
c            CALL NEWPLT
c            CALL GCLIP(0)
c            CALL CJUST(-1.0,0.0)
c            CALL WORLD(0.0,10.0,0.0,10.0)
c            CALL VUPORT(0.0,1.0,0.0,1.0)
c            CALL COLOR(15)
c Convert the angle DEGX to a met angle for display on the graph.
             IF ( DEGX .LE. 90.) THEN
                ZMETANG = 90. - DEGX
             ELSE
                ZMETANG = 450. - DEGX 
             END IF
             WRITE(TEXT,3100) ZMETANG
c            CALL GTX(0.0,7.4,TEXT)
 3100 FORMAT(' SOUND RAYTRACE PLOT FOR AZIMUTH ANGLE = ',F4.0)
             WRITE(TEXT,111) FILLABEL(1:20)
c            CALL GTX(0.0,7.15,TEXT)
  111 FORMAT(' MET:  ', A20 )
             GO TO 7
         ELSE
             IF (ICONTR .EQ. 2 ) THEN
               IF ( DEGX .LE. 90.) THEN
                  ZMETANG = 90. - DEGX
               ELSE
                  ZMETANG = 450. - DEGX 
               END IF
 3200 FORMAT(' COMPUTING RAYTRACE FOR AZIMUTH ANGLE =' F4.0)
             END IF
         END IF 
      END IF
c
C  If MOPT = 2 then clear, increment azimuth and go again
      IF( MOPT .EQ.2) THEN
          ICOPY = 0
          IMENU = 1
          ICALL = 0
          GO TO 1000
      END IF
c
c  If MOPT = 3 then skip the raytrace call and return.
      IF( MOPT .EQ. 3) THEN
          ICOPY = 0
          IMENU = 0
          ICALL = 0
          RETURN
      END IF
c
   4  CONTINUE
C
C  Set IMENU to show menu on next pass thru loop
   5  IMENU = 1
   7  CONTINUE
C
C  Obtain generalized Snell Constant, maximum and minimum elevation angles,
C  and type of refraction (up or down) for a given azimuth angle.
      CALL TRANGE(SI,THETM,THETL,IREFRT)
C
C  Reset NRO to 5 for upward refraction
      IF(IREFRT .EQ. 1)  NRO =  NROSHD

C  Reset NRO to a flexible number within the limits
c  Minimun value of NRO at 10 insures sound contour resolution.
      IF( IREFRT .EQ. 0) THEN
         IF(SINML .NE. 0.0 ) THEN
           NRO = (SIN(THETM) - SIN(THETL))/(SINML/NROSAV)
         ELSE 
           NRO = 0
         END IF
         NRO = MIN(NRO,NROSAV)
         NRO = MAX(NRO, NROSAV/5,10 )
      END IF 
c        write(6,*)'after trange: si,thetl,thetm,irefrt,NRO=',
c    1            si,thetl,thetm,irefrt,NRO
C
C  Set flag so new axis will be drawn for the raytrace plots for this
C  azimuth. The terrain values to be plotted  were determined in TRANGE.
      ISCALE = 0
  8   CONTINUE
C
      DELSIT=(SIN(THETM)-SIN(THETL))/NRO
c     print *,'at 8: STARTING OVER: THETM,THETL,NRO,DELSIT=',
c    1                              THETM,THETL,NRO,DELSIT
      DO 10 J=1,10
         DRA(J)=1.0
   10 DRB(J)=1.0
      IRANGE=0
      ISHADE=0

C      Counters for the number of times COAST, and FOCAL are called.
      ICOCNT=0
      IFOCNT=0
C      Compute all ray traces in the SI direction
      SINMAX=SIN(THETM)
      IPOINT=1
      II=0
 2000 CONTINUE
c      print *,'after 2000: II,NRO=',II,NRO
      IF(II.GE.NRO) GO TO 2100
      II=II+1

C      Angle is initial elevation angle of acoustic ray trace
      IF (IRANGE .EQ. 750) THEN
         GO TO 2000
      END IF
      IRANGE=IRANGE+1
      ANGLE(IRANGE)=ASIN(SINMAX-(II-IPOINT)*DELSIT)
      IF(IRANGE.GT.1.AND.ANGLE(IRANGE).NE.0.0) THEN
         DELANG=0.1* ABS(ANGLE(IRANGE-1)/ANGLE(IRANGE)-1.0)
      ELSE
         DELANG=0.001
      END IF
c      print *,'IRANGE,ANGLE(IRANGE),DELANG=',
c    1          IRANGE,ANGLE(IRANGE),DELANG

C    Calculate the ray trace and associated parameters
c     print *,'calling TRACE: RMAX,THETM,ANGLE(IRANGE),SI=',
c    1           RMAX,THETM,ANGLE(IRANGE),SI
      CALL TRACE(RMAX,THETM,ANGLE(IRANGE),SI)

c   Start over if too few raytraces are valid, increase no. of rays.
c     print *,'IRANGE,II,ISUR,NRO,NROSAV=',IRANGE,II,ISUR,NRO,NROSAV
        IF( IRANGE.LE.6 .AND. II.GE.NRO .AND. NRO.LT.NROSAV) THEN
           NRO = NROSAV
           IREFRT = 0
           IF(IRANGE .GT. 1 ) THETM = ANGLE(1)
           GO TO 8
        END IF
C   Calculate the shadow zone parameters if no remaining downward refracting
C   ray can be found.
c     print *,'II,ISUR,NRO,ISHADE=',II,ISUR,NRO,ISHADE
        IF( II.EQ.NRO .AND. ISHADE.NE.0 .AND. ISUR .LE.0)GO TO 321

C    If ray trace is invalid , skip to next elevation angle decrement
C
      IF(ISUR.LE.-1) THEN
         IF(IRANGE .NE. 0) THEN
           DEGANG = ANGLE(IRANGE)* 180./PI
c           WRITE(6,*)'BLAST,IRANGE= ',IRANGE,'  ANGLE= ' ,DEGANG,
c    &               ' ISUR= ',ISUR
          ELSE
c           WRITE(6,*)'BLAST,IRANGE=0,  NO ANGLE   ISUR= ',ISUR
         END IF
        IRANGE = IRANGE-1

C  Start over if all ray traces are invalid, assume all upward raytraces.
        IF(IRANGE .LE. 1 .AND. II .GE. NRO) THEN
           IREFRT = 1
           NRO    = NROSHD
           THETM  = MAX(THETM,THETL+0.05)
	   print *,'all raytraces invalid: THETM=',THETM
           GO TO 8
        END IF
        GO TO 2000
      ENDIF
c
C  Throw out extra downward raytraces that are too close to the blast source.
      IF(ISUR.GT.0 .AND. RANGE(ISUR).LT.100. .AND. IRANGE.GT.1 .AND.
     &   ISK(IRANGE-1).GT.0 .AND.
     &   RANGES(IRANGE-1,ISK(IRANGE-1) ) .LT.100. ) THEN
         IRANGE = IRANGE -1
         GO TO 2000
      END IF
C
C  Start over if ray trace hits ground while "upward ray trace"
      IF(IREFRT.EQ.1.AND.ISUR.GT.0) THEN
         NRO = NROSAV
         IREFRT = 0
         IF(IRANGE.GT.1) THETM = ANGLE(IRANGE-1)
c        print *,'raytrace hits ground: THETM=',THETM
         GO TO 8
      ENDIF
C  Check for excessive number of upward ray traces and reset THETM
      IF(IREFRT.EQ.0.AND.ISUR.GT.0.AND.IRANGE.EQ.ISHADE+1.AND.
     &     ISHADE.GT.NROSHD) THEN
         IF(IRANGE.GT.2) THETM = ANGLE(IRANGE-1)
c   print *,'excessive number of up rays: THETM=',THETM
         GO TO 8
      ENDIF

C      Store values for a simple refracted ray
      IRESHD = 0
      IF(IRANGE .GT.3 .AND. ZB .GT. ALT(1) ) THEN
        IF(ISHADE .EQ. 0 ) THEN
C  Very large range increment for a relatively constant elevation angle
C  decrement inplies a defocussing or shadow region.  The very large range
C  increment also present problems for sound contour plotting, thus shadow
C  point are added.
          IF(  RANGES(IRANGE,1)+10000.  .LT. RANGES(IRANGE-1,1) 
     &      .and. II .EQ. NRO )THEN
             ISUR= 0
             IRESHD = 1
          END IF
        END IF
      END IF
C
      ISK(IRANGE)=ISUR
      JSKIP=MAX(ISUR,1)
      DO 100 J=1,JSKIP
         RANGES(IRANGE,J)=RANGE(J)
         FACTOR(IRANGE,J)=MAGNIF(J)
         TIMES(IRANGE,J)=TIMEOA(J)
         HIGHS(IRANGE,J)=HEIGHT(J)
         REFLTS(IRANGE,J)=REFLCT(J)
         TBRATS(IRANGE,J)=TBRATO(J)
         IF(ISUR.EQ.0) IFOCAL(IRANGE,J)=0
         IF(ISUR.GT.0) IFOCAL(IRANGE,J)=1
         IF(IRANGE .EQ.2 )THEN
             DRA(J) = RMAX - RANGES(1,J)
         ELSE
             DRA(J) = DRB(J)
         END IF
         IF(IRANGE.LE.1.OR.ISK(IRANGE).EQ.0)  GO TO 100
         IF(J.GT.ISK(IRANGE-1).OR.ISK(IRANGE-1).EQ.0)GOTO100
         DRB(J)=RANGES(IRANGE-1,J)-RANGES(IRANGE,J)
  100 CONTINUE
C
C  If ICALL = 0 then skip raytrace calculations
      SINCRT = 0.05* (SIN(THETM) - SIN(THETL) ) + SIN(THETL)
      If( ICALL .EQ. 0 .OR. SIN(ANGLE(IRANGE)) .LT. SINCRT ) GO TO 110
c     CALL RAYTRACE(SI)

  110 CONTINUE
c####### temporary change #############
      if(plotray1) then
c       print *,'in BLAST: CALLING RAYTRACE: DEGX,nray=',DEGX,nray
        nray=nray+1
        CALL RAYTRACE(SI,nray)
      endif
c####### end temporary change #############

C   Search for coastal, focal, or shaded initial/skip rays
C   must process any shaded rays before allowing checks on
C   focal or coastal regions.
      IF(ISK(IRANGE).EQ.0 .OR. IRANGE.EQ.1 ) GO TO 300
      DO 200 J=1,JSKIP
         JCOAST(J)=0
         JFOCAL(J)=0
         IF(IRANGE.LE.1 ) GO TO 200
         IF(J.GT.ISK(IRANGE-1).OR.ISK(IRANGE-1).EQ.0) GO TO 200
         RREF=ABS(REFLCT(J)-REFLTS(IRANGE-1,J))
         IF(RREF.GT.1.-RRATIO) JCOAST(J)=J
         IF(J .GT.MAX(1, ISK(IRANGE-2)) .AND.IRANGE.GT.2 ) GO TO 200
         IF(DRA(J)*DRB(J).LE.0.0) JFOCAL(J)=J
 200  CONTINUE
C  Interval halving to obtain focal or coastal range
      JMAX=0
      DO 250 J=1,JSKIP
 250     IF(JFOCAL(J).GT.JMAX) JMAX=JFOCAL(J)
C
C  Count number of times focal is called.
C
      IF(JMAX.EQ.0 ) GO TO 260
      IFOCNT = IFOCNT + 1

      CALL FOCAL(IRANGE,JMAX,RMAX,THETM,SI,DELANG,RRATIO,IFOCNT)

C  If ICALL = 0 then skip raytrace calculations
      SINCRT = 0.05* (SIN(THETM) - SIN(THETL) ) + SIN(THETL)
      If( ICALL .EQ. 0 .OR. SIN(ANGLE(IRANGE)) .LT. SINCRT ) GO TO 255
c     CALL RAYTRACE(SI)
 255  CONTINUE
c####### temporary change #############
      if(plotray1) then
c       print *,'in BLAST: CALLING RAYTRACE: DEGX,nray=',DEGX,nray
        nray=nray+1
        CALL RAYTRACE(SI,nray)
      endif
c####### end temporary change #############
C
      GO TO 280
 260  DO 270 J=1,JSKIP
 270     IF(JCOAST(J).GT.JMAX) JMAX=JCOAST(J)
C
C  Count number of times coast is called.
      IF( JMAX.EQ.0 .AND. ISHADE.EQ.0) GO TO 280
      ICOCNT = ICOCNT + 1
C
      CALL COAST(IRANGE,JMAX,RMAX,THETM,SI,DELANG,RRATIO,ISHADE,
     +ICOCNT)

C  If ICALL = 0 then skip calculations for raytrace
      SINCRT = 0.05* (SIN(THETM) - SIN(THETL) ) + SIN(THETL)
      If( ICALL .EQ. 0 .OR. SIN(ANGLE(IRANGE)) .LT. SINCRT ) GO TO 280
c     CALL RAYTRACE(SI)
c####### temporary change #############
      if(plotray1) then
c       print *,'in BLAST: CALLING RAYTRACE: DEGX,nray=',DEGX,nray
        nray=nray+1
        CALL RAYTRACE(SI,nray)
      endif
c####### end temporary change #############
C
 280  CONTINUE
      IF(IRANGE .GT. 1 .AND. 
     &  (RANGES(IRANGE,1) + 10000.) .LT. RANGES(IRANGE-1,1) )THEN
        IRESHD = 1
        GO TO 300
      END IF
C
      IF( ISHADE .NE. 0 ) GO TO 310
C  Go to next angle decrement after processing any focal/coastal regions
      GO TO 2000

C       Calculate range, time of arrival, and magnification factor for
C       shaded rays
 300  IF(ISHADE.NE.0) GO TO 310
C  Save value of parameters of unshaded rays to be used later
      IF(IRANGE.EQ.1) RSHADE=RMAX
      IF(IRANGE.GT.1) RSHADE=MAX(RANGES(IRANGE-1,1),1.0)
      IF(IRANGE.EQ.1) FSHADE=1.0
      IF(IRANGE.GT.1) FSHADE=FACTOR(IRANGE-1,1)
      NSHADE=IRANGE
C
C  Need shadow points at least every 5 KM. for when ireshd is = 1.
c  Check ISHADE, no negative value accepted.
      IF(IRESHD .EQ. 1) THEN
         ISHADE = INT( (RANGES(IRANGE-1,1) - RANGES(IRANGE,1) )/5000.)
         ISHADE = MIN(ISHADE,(105-NRO+II) )
         ISHADE = MAX(0,ISHADE)
         IRANGE = IRANGE + ISHADE
         ISK(IRANGE) = 0
      END IF 
C         
C  Check if unshaded rays have been determined again
 310  IF(ISK(IRANGE).NE.0.OR.II.EQ.NRO) GO TO 320

C   Check for complete upward refracting zone.
C   Value for IREFRT defined in TRANGE and NRO is 5 if IREFRT is 1
      IF(IREFRT.EQ.1 .AND. ISHADE .GE. NRO) THEN
         II = NRO
         ISHADE = NRO - 1
         ANGLE(NRO) = THETL
         IRANGE = NRO
         GO TO 321
      END IF
      ISHADE=ISHADE+1

C  Go to next angle decrement to find any unshaded rays.
      GO TO 2000
C
C  Process the shaded rays after finding an unshaded ray and
C  define parameters in case unshaded rays has not been determined again
 320  IF(II.NE.NRO.OR.ISK(IRANGE).NE.0) GO TO 325
 321  CONTINUE
      RANGES(IRANGE,1)=1.0
      FACTOR(IRANGE,1)=1.0
      HIGHS(IRANGE,1)=ZB
      REFLTS(IRANGE,1)=0.0
      TBRATS(IRANGE,1)=1.0
      IFOCAL(IRANGE,1)=0

C      Define range increments for shaded rays
 325  DELRSH=(RANGES(IRANGE,1)-RSHADE)/(ISHADE+1)

C******************
C  Define an effective barrier range to assure continuous magnifi-
C  cation factor values.
C      IF(RSHADE.LT.RANGES(IRANGE,1))REFF=RSHADE*(1.+ALOG10(FSHADE)
C     1                                          /0.5)
C      IF(RSHADE.GE.RANGES(IRANGE,1))REFF=RANGES(IRANGE,1)*
C     1         (1.+ALOG10(FACTOR(IRANGE,1))/0.5)
C******************

C       Compute ranges and times of shaded rays at equal range increments
 330  IF(NSHADE.EQ.1) GO TO 340
      RANGES(NSHADE,1)=RANGES(NSHADE-1,1)+DELRSH
      RHALF=RANGES(NSHADE-1,1)+DELRSH/2.
      NB = 0
      CALL TERAIN(RHALF,SI,ANGLE(IRANGE),NB,1,0.0,INB,ALT1,VO1,
     &            VGRAD,RG,RTBG)
      TIMES(NSHADE,1)=TIMES(NSHADE-1,1)+DELRSH/VO1
      GO TO 350
 340  IF(II.EQ.NRO) GO TO 345

C  Redefine controlling parameters to fill in quiet zone area and
C  relieve crowding in the downward refraction area.
      ISHOLD=ISHADE
      ISHNEW=NRO*(RMAX-RANGES(IRANGE,1))/RMAX
      ISHNEW = MIN(ISHNEW,5)
      IPLUS=IRANGE+(ISHNEW-ISHOLD)

C  Insure at least five ray traces in the downward refraction area.
      IF(IPLUS.GT.NRO - 5) ISHNEW =MAX(NRO+ISHOLD-IRANGE - 5,ISHOLD )
      IF(ISHNEW.LE.ISHOLD) GO TO 345
      IF(IPLUS.GT.NRO - 5) IPLUS= MAX(NRO - 5, IRANGE )
C
C  Reset index of valid raytrace from IRANGE to IPLUS
      ANGLE(IPLUS)=ANGLE(IRANGE)
      ISK(IPLUS)=ISK(IRANGE)
      JSKIP=MAX(ISK(IRANGE),1)
      DO 355 J=1,JSKIP
         RANGES(IPLUS,J)=RANGES(IRANGE,J)
         FACTOR(IPLUS,J)=FACTOR(IRANGE,J)
         TIMES(IPLUS,J)=TIMES(IRANGE,J)
         HIGHS(IPLUS,J)=HIGHS(IRANGE,J)
         REFLTS(IPLUS,J)=REFLTS(IRANGE,J)
         TBRATS(IPLUS,J)=TBRATS(IRANGE,J)
         IFOCAL(IPLUS,J)=IFOCAL(IRANGE,J)
355   CONTINUE
c
C   Reset index and angle increment parameters after calculation of the 
C   shadow zone.
      IRANGE=IPLUS
      ISHADE=ISHNEW
      DELRSH=(RANGES(IRANGE,1)-RSHADE)/(ISHADE+1)
      II=IRANGE
      SINMAX=SIN(ANGLE(IRANGE))
      IPOINT=II
      DELSIT=(SINMAX-SIN(THETL))/(NRO-IPOINT+1)
 345  RANGES(NSHADE,1)=RSHADE+DELRSH
      RHALF=RANGES(NSHADE,1)/2.
      NB = 0
      CALL TERAIN(RHALF,SI,ANGLE(IRANGE),NB,1,0.0,INB,ALT1,VO1,
     &            VGRAD,RG,RTBG)
      TIMES(NSHADE,1)=RANGES(NSHADE,1)/VO1
 350  CONTINUE

C  Formula to assure a smooth transition to the maximum 
C  excess attenuation in the exterior quiet zone.
      FACMIN = 1.0
      IF( RANGES(NSHADE,1) .GT. 68.06) THEN
         FACMIN = 1.0/ ( 0.187*(3.2808*RANGES(NSHADE,1))**0.31 )
      END IF

      IF(RSHADE .EQ.RMAX) THEN
C  Do exterior shadow zone attenuation.        
         FACTOR(NSHADE,1) = FACTOR(IRANGE,1)
         IF(RANGES(NSHADE,1) .GT. RANGES(IRANGE,1) + 68.06) THEN
            FACTOR(NSHADE,1) = FACTOR(IRANGE,1) /( 0.187 *
     &         (3.2808*(RANGES(NSHADE,1)-RANGES(IRANGE,1)))**0.31 )
         END IF
         FACTOR(NSHADE,1) = MAX(FACTOR(NSHADE,1),FACMIN)
         FACTOR(IRANGE,1) = MAX(FACTOR(IRANGE,1),FACMIN)
      ELSE   
C  Do interior shadow zone attenuation.
         FACTR1 = FACTOR(IRANGE,1)
         DELR1 = RANGES(NSHADE,1)-RANGES(IRANGE,1)
         IF( ABS(DELR1) .GT. 68.06) THEN
           FACTR1=FACTR1/( 0.187 *(3.2808*ABS(DELR1))**0.31)
         END IF
         FACTR2 = FSHADE
         DELR2 = RSHADE - RANGES(NSHADE,1)
         IF( ABS(DELR2) .GT. 68.06) THEN
           FACTR2=FACTR2/(0.187 *(3.2808*ABS(DELR2))**0.31) 
         END IF
         IF( (DELR2+DELR1) .EQ. 0.0 ) THEN
            FACTOR(NSHADE,1) = (FACTR1+FACTR2)/2.
         ELSE 
            FACTOR(NSHADE,1) = MAX((FACTR1*DELR2+FACTR2*DELR1)/
     &                      (DELR2+DELR1),FACMIN)
         END IF
      END IF
      NB = 0
C
      CALL TERAIN(RANGES(NSHADE,1),SI,ANGLE(IRANGE),NB,1,0.0,INB,
     &            ZGR,VO1,VGRAD,RG,RTBG)
      ZGR = ZGR + REARTH -
     &  SQRT (REARTH*REARTH-RANGES(NSHADE,1)*RANGES(NSHADE,1))
      HIGHS(NSHADE,1) = ZGR
      REFLTS(NSHADE,1)= RG
      TBRATS(NSHADE,1)=RTBG
      IFOCAL(NSHADE,1)=0
      ISK(NSHADE)=0
      ANGLE(NSHADE)=ANGLE(IRANGE)
      NSHADE=NSHADE+1
      ISHADE=0
C   Go to next angle decrement when processing of shaded rays is done
      IF(NSHADE.GE.IRANGE) GO TO 2000
      GO TO 330
 2100 CONTINUE

C  If the minimum range is the focal point, then calculate
C  excess attenuation in Central quiet zone.
      RMIN=RMAX
      DO 700 I=1,IRANGE
        JSKIP=MAX(ISK(I),1)
        DO 700 J=1,JSKIP
           IF(RANGES(I,J).LT.RMIN .AND. RANGES(I,J) .GT. 1.0 ) THEN
              RMIN= MAX(RANGES(I,J),1.0)
              IMIN=I
              JMIN=J
           END IF
  700 CONTINUE
C
      IMINP = IMIN + 1
      IF(IMINP .GE. IRANGE) IMINP = IMIN
      IMING = IMIN - 1
      IF(IMIN .EQ. 1) IMING = IMIN
C
C  Check for existance of central shadow zone.
      IF( ( ( (IFOCAL(IMIN,JMIN).EQ.2 .OR. IFOCAL(IMINP,JMIN) .EQ. 2 
     &         .OR. IFOCAL(IMING,JMIN) .EQ. 2) 
     &       .AND. IMIN .NE. IRANGE ) 
     &     .OR.(IREFRT .EQ. 0 .AND. V(2).LE.V(1) .AND. ZB .EQ. ALT(1) )
     &     .OR.(RMIN.GT.10000..AND.IMIN.EQ.IRANGE.AND.ISK(IRANGE).GT.0)
     &     .OR.(ZB.GT.ALT(1)  .AND. ANGLE(IRANGE).LT.0.0
     &       .AND. RANGES(IRANGE,1) .GT. 10000.) )
     &   .AND. IRANGE .LT. 750 .AND. RMIN .GT. 1.0 )  THEN
         IF(RANGES(IRANGE,1) .EQ. 1.0 ) IRANGE = IRANGE - 1
         NRANGE=IRANGE+1
         NTR=MIN(IRANGE+10,750)
      ELSE
         IF(RANGES(IRANGE,1) .EQ. 1.0 ) IRANGE = IRANGE - 1
         NRANGE=MIN(IRANGE+1,750)
         NTR=NRANGE
      END IF
C
C  Calculate parameters for the shaded rays.
      DRANGE=(RMIN-1.0)/(NTR-NRANGE+1)
      DO 750 I=NRANGE,NTR
         ANGLE(I)=THETL
         ISK(I)=0
         RANGES(I,1)=RMIN-(I-NRANGE+1)*DRANGE
         FACTR1 = 1.0
         DELR1 = RANGES(I,1)
         IF(RANGES(I,1) .GT. 68.06 ) THEN
           FACTR1 = 1.0/( 0.187*(3.2808*DELR1)**0.31)
         END IF
         FACTR2 = FACTOR(IMIN,JMIN)
         DELR2  = RMIN - RANGES(I,1)
         IF(RMIN .GT. RANGES(I,1) + 68.06 ) THEN         
            FACTR2 = FACTR2/(0.187*(3.2808*DELR2)**0.31 )
         END IF
C
         IF( (DELR1+DELR2) .EQ.0.0 ) THEN
            FACTOR(I,1) = (FACTR1+FACTR2)/2.
         ELSE
            FACTOR(I,1) =MAX( (FACTR1*DELR2+ FACTR2*DELR1)/
     &                 (DELR1+DELR2),FACTR1 )
         END IF
         IF(I.EQ.NTR) FACTOR(I,1)=1.0
         TIMES(I,1)=TIMES(IMIN,JMIN)*RANGES(I,1)/RMIN
         NB = 0
         CALL TERAIN(RANGES(I,1),SI,ANGLE(I),NB,1,0.0,INB,ZGR,VO1,
     &              VGRAD,RG, RTBG)
         ZGR = ZGR + REARTH -
     &    SQRT( REARTH*REARTH - RANGES(I,1)*RANGES(I,1) )
         HIGHS(I,1)= ZGR
         REFLTS(I,1)= RG
         TBRATS(I,1)=RTBG
         IFOCAL(I,1)=0
         IRANGE=I
  750 CONTINUE

C  Calculate sound intensities at all ground locations of ray impact,
C  including wave interferences of separate rays at same location
      SIP=SI*ACC
      CALL NOISE(IRANGE,SIP,ISORT,IDEX,JDEX)
C
C  Write the DB data out to a file to be used for the contour plotting.
      if(IPSI.eq.1) then
        write(15,*) DELPSI,NDELSI
        write(6,*) DELPSI,NDELSI
      endif
      WRITE(15,*) SIP, ISORT
c     WRITE(6,*) SIP, ISORT
C
C  Display the met angle so all print outs are consistant.
      IF(SIP .LE. 90) ZMETANG = 90. - SIP
      IF(SIP .GT. 90) ZMETANG = 450.- SIP
      WRITE(MT8,600) ZMETANG
 600  FORMAT(////38X,'PEAK SOUND INTENSITIES IN DB ALONG AZIMUTH OF'
     1,F6.2,' DEG',//)
       WRITE(MT8,611)
 611  FORMAT(1H ,3X,' RAY NO.  ELEV ',5X,'RANGE (M)',4X,'RAY APOGEE(M)',
     1 2X,'ARRIVAL',3X,'FREE AIR',3X,'CHARGE',3X,'ONE RAY',3X,'MANY RAYS
     2',2X,'TOTAL',4X,'TOTAL',4X,'NO OF',/,13X,'ANGLE',34X,'TIME(S)',4X,
     3'(DB)'5X,'WT.(DB)',3X,'FOCUS(DB) FOCUS(DB) 1LB.(DB)',3X,'(DB)',4X,
     $'RAYS',3X,'TYPE',/)
      DO 400 I=1,IRANGE
         JSKIP=MAX(ISK(I),1)
         DO 400 J=1,JSKIP
            ANGLEP=ANGLE(I)*ACC

c  Modified (Dec-88) to use ANSI S2.20 - 1983 
            PPO=42.48/(RANGES(I,J)*3.2808)**1.1
            SND=20.*ALOG10(PPO)+171.
            AW=20.*ALOG10(WB**0.367)
            ARAY=20.*ALOG10(FACTOR(I,J))
            ATOT(I,J)=DECIBL(I,J)-AW-SND
            ATOT1=DECIBL(I,J)-AW
C            IF(ICONTR .GT. 0 ) WRITE(15,*) RANGES(I,J), DECIBL(I,J)
            WRITE(MT8,610) I,ANGLEP,RANGES(I,J),HIGHS(I,J),
     1      TIMES(I,J),SND,AW,ARAY,ATOT(I,J),ATOT1,DECIBL(I,J),
     2      JRAY(I,J),IFOCAL(I,J)
  610 FORMAT(6X,I3,2X,F7.2,2(5X,F10.2),F10.3,6F10.3,2I5)
  400 CONTINUE
C
C Write out the values for the plot file, in sorted order.(see NOISE) 
c If only raytrace data do not put to a file.
      IF(ICONTR.GT.0) THEN
        Aza(IPSI)   = SIP
        nranges(IPSI) = ISORT
        DO 925 III= 1, ISORT
          WRITE(15,*) RANGES(IDEX(III),JDEX(III)), 
     *                DECIBL(IDEX(III),JDEX(III)),
     *                ATOT(IDEX(III),JDEX(III))
          rangea(III,IPSI) = RANGES(IDEX(III),JDEX(III)) 
          dBa(III,IPSI) = DECIBL(IDEX(III),JDEX(III)) 
925     CONTINUE
      ENDIF
      IF(IRANGE.GT.IRAMAX) IRAMAX=IRANGE
c##################################################
      if(plotray1) then
      write(kt,*) 'in BLAST: SIP,ZMETANG=',SIP,ZMETANG
      write(kt,621) 
  621 format(1h ,'  k    alt      T       WU       WV      SV',
     1  '       V       dU/dz    dV/dz    dc/dz ')
      do k=1,NP
       uz=0.
       vz=0.
       cz=0.
       if((k.gt.1).and.(k.lt.NP)) then
         uz=(WU(k+1)-WU(k-1))/(alt(k+1)-alt(k-1))
         Vz=(WV(k+1)-WV(k-1))/(alt(k+1)-alt(k-1))
         cz=(SV(k+1)-SV(k-1))/(alt(k+1)-alt(k-1))
       endif
       write(kt,622) k,alt(k),T(k),WU(k),WV(k),SV(k),V(k),Uz,Vz,cz
      enddo
  622 format(1h ,i3,2f8.2,2f9.3,2f8.2,3f10.4)
      do iii= 1, ISORT
        WRITE(kt,*) RANGES(IDEX(iii),JDEX(iii)), 
     *                DECIBL(IDEX(iii),JDEX(iii)),
     *                ATOT(IDEX(iii),JDEX(iii))
      enddo
      endif
c####### temporary change #############
      if(plotray1) then
c       print *,'in BLAST: CALLING PLOTRAY: DEGX=',DEGX
c       --- flush the background white
        CALL PLOTRAY(SI)
        call NEWPLT
      endif
c##################################################
 1000 CONTINUE
c##### new code ###################################
c     --- set max range same for all azimuths and extrapolate
c     ---  dB values 
      IF(ICONTR.GT.0) THEN
        rmaxr = 0.
        jmax=0
        do j=1,NDELSI
          rj = rangea(nranges(j),j)
          if(rj.gt.rmaxr) then
            rmaxr=rj
            jmax=j
          endif
        enddo
c       print *,'max range at j,rj=',jmax,rmaxr
        do j=1,NDELSI
          rj = rangea(nranges(j),j)
          if(rj.lt.rmaxr) then
c           print *,'j,old rmax,dB=',j,rj,dBa(nranges(j),j)
            nranges(j)=nranges(j)+1
            i=nranges(j)
            rangea(i,j)=rmaxr
            dBa(i,j)=dBa(i-1,j)
c           print *,'j,new rmax,dB=',j,rmaxr,dBa(i,j)
          endif
        enddo
      ENDIF
c
c Get a hard copy if flag indicates one desired.
c     IF(ICOPY .EQ. 1 ) THEN
c         CALL CJUST(-1.0,0.0)
c         CALL WORLD(0.0,10.0,0.0,10.0)
c         CALL VUPORT(0.0,1.0,0.0,1.0)
c         CALL COLOR(15)
c         WRITE(TEXT,650) METLABEL(1:35)
c         CALL GTX(0.0,6.9,TEXT)
c         CALL ENDPLT
c         IF(IOPT .EQ. 1 ) GO TO 500
c         WRITE(*,*) CHAR(7)
c         READ(KI,1003) ANSWR
c500      CALL NEWPAG
c     END IF
C
  950 FORMAT(///,' ********* WARNING **********'/,
     + ' IRANGE LIMIT (',I3,') HAS BEEN REACHED'/,
     + ' REDIMENSION APPROPRIATE ARRAYS IN COMMON /RAYMAP/')
C
      RETURN
      END
C
      SUBROUTINE COAST(IRANGE,ISKIP,RMAX,THETM,SI,DELANG,RRATIO,
     &                 ISHADE,ICOCNT)
C----------------------------------------------------------------------
C      Large changes in the reflection coefficients have occured, thus
C      signaling a shoreline boundary and a skip ray focusing or, the
C      first downward refracted ray requires an accurate elevation
C      angle for the ray  to reach rmax or the quiet zone boundary.
C
C      VARIABLES: defined in subroutine BLAST
C---------------------------------------------------------------------
      integer ISUR,IMAX,JCOAST,JFOCAL
      real    RANGE,MAGNIF,TIMEOA,HEIGHT,REFLCT,TBRATO,DRA,DRB
      COMMON /SKIP/ISUR,IMAX,RANGE(10),MAGNIF(10),TIMEOA(10),HEIGHT(10),
     &  REFLCT(10),TBRATO(10),JCOAST(10),JFOCAL(10),DRA(10),DRB(10)
      COMMON /TRCINP/ IPLOT, XPLOT(751), YPLOT(751), APLOT(751),
     &                        XTEMP(751), YTEMP(751), ISCALE
      COMMON /RAYMAP/ISK(751), RANGES(751,5), FACTOR(751,5),
     &  TIMES(751,5), HIGHS(751,5), REFLTS(751,5), TBRATS(751,5),
     &  DECIBL(751,5), ANGLE(751), IFOCAL(751,5), JRAY(751,5),
     &  ATOT(751,5)
C
      DFAC = 180./ACOS(-1.)
C  If no interval halving desired, redefine focal point at IRANGE -1.
      IF( ICOCNT .GT. 0) THEN
         IF(ISK(IRANGE) .GT. 0 ) THEN
            DO 26 J = 1,ISK(IRANGE)
              IF(JCOAST(J) .NE. 0 ) IFOCAL(IRANGE,J) = 3
 26         CONTINUE
         END IF
         RETURN
      END IF
c
C  If interval halving is to be done continue below.
      INEG1=IRANGE-1
      IPLUS=IRANGE+1
      ATEMP=ANGLE(IRANGE)
      ITEMP=ISK(IRANGE)
      IF(ISHADE .GT. 0) GO TO 25
C  Set index of valid raytrace from IRANGE to IPLUS
      ISK(IPLUS)=ISK(IRANGE)
      JSUR=ISK(IPLUS)
      ANGLE(IPLUS)=ANGLE(IRANGE)
      DO 20 J=1,JSUR
         RANGES(IPLUS,J)=RANGES(IRANGE,J)
         FACTOR(IPLUS,J)=FACTOR(IRANGE,J)
         TIMES(IPLUS,J)=TIMES(IRANGE,J)
         HIGHS(IPLUS,J)=HIGHS(IRANGE,J)
         REFLTS(IPLUS,J)=REFLTS(IRANGE,J)
         TBRATS(IPLUS,J)=TBRATS(IRANGE,J)
         IFOCAL(IPLUS,J)=IFOCAL(IRANGE,J)
  20  CONTINUE
C   Do interval halving to obtain a ray trace to the shoreline
C   or to RMAX or to quiet zone boundary
      ANGL=ANGLE(IPLUS)
  25  IF(ISHADE .GT. 0) ANGL=ANGLE(IRANGE)
      ANGU=ANGLE(INEG1)
  30  CONTINUE
C
      ANGLE(IRANGE)=(ANGU+ANGL)/2.

      CALL TRACE(RMAX,THETM,ANGLE(IRANGE),SI)
C
      IF (ISUR.GT.0) THEN
        ITEMP=ISUR
        ATEMP=ANGLE(IRANGE)
      ENDIF
      IF( ISKIP .GT. 0) THEN
         RREF=ABS(REFLCT(ISKIP)-REFLTS(INEG1,ISKIP))
      END IF
      IF((ISKIP .GT. 0) .AND. (RREF.GT.1.-RRATIO)) GO TO 35
      IF((ISHADE .GT. 0) .AND. (ISUR .GT. 0).AND. 
     &        (MAGNIF(1) .GE. 0.316228))           GO TO 35
      IF((ANGU-ANGL) .LE. 0.1*DELANG*ANGLE(IRANGE) ) GO TO 40
C
      ANGU=ANGLE(IRANGE)
      GO TO 30
  35  IF((ANGU-ANGL) .LE. 0.1*DELANG*ANGLE(IRANGE) )  GO TO 40

      ANGL=ANGLE(IRANGE)
      GO TO 30
  40  CONTINUE
      IF (ISUR.LE.0) THEN
        IF(IRANGE .NE. 0) THEN
           DEGANG = ANGLE(IRANGE)*DFAC 
C           WRITE(8,*)'COAST,IRANGE= ',IRANGE,'  ANGLE= ', DEGANG,
C     &            '  ISUR= ', ISUR
C        ELSE
C           WRITE(8,*)'COAST,IRANGE=0 NO ANGLE   ISUR = ', ISUR 
        END IF
        ISUR=ITEMP
        ANGLE(IRANGE)=ATEMP
      ENDIF

C  Define refracted ray trace type and save information on the
C  parameters of the ray trace
      DO 46 J=1,ISUR
         IFOCAL(IRANGE,J)=1
         IF(JCOAST(J).NE.0) IFOCAL(IRANGE,J)=3
         IF(J.NE.ISUR) GO TO 48
         ISK(IRANGE)=ISUR
  48     CONTINUE
         RANGES(IRANGE,J)=RANGE(J)
         FACTOR(IRANGE,J)=MAGNIF(J)
         TIMES(IRANGE,J)=TIMEOA(J)
         HIGHS(IRANGE,J)=HEIGHT(J)
         REFLTS(IRANGE,J)=REFLCT(J)
         TBRATS(IRANGE,J)=TBRATO(J)
         IF(ISHADE .EQ. 0) THEN
            DRB(J)=RANGES(IRANGE,J)-RANGES(IPLUS,J)
         END IF
  46  CONTINUE

      IF(ISHADE .EQ. 0) IRANGE = IPLUS
      RETURN
      END
C
      SUBROUTINE FOCAL(IRANGE,ISKIP,RMAX,THETM,SI,DELANG,RRATIO,
     &                 IFOCNT)
C-----------------------------------------------------------------------
C   The range has changed direction as the initial elevation angle
C   decreased; thus indicating a focal region. the focal point is
C   then searched for by interval halving procedure
C
C   VARIABLES:  defined in subroutine BLAST
C---------------------------------------------------------------------
      integer ISUR,IMAX,JCOAST,JFOCAL
      real    RANGE,MAGNIF,TIMEOA,HEIGHT,REFLCT,TBRATO,DRA,DRB
      COMMON /SKIP/ISUR,IMAX,RANGE(10),MAGNIF(10),TIMEOA(10),HEIGHT(10),
     &   REFLCT(10),TBRATO(10),JCOAST(10),JFOCAL(10),DRA(10),DRB(10)
      COMMON /RAYMAP/ISK(751), RANGES(751,5), FACTOR(751,5),
     &  TIMES(751,5), HIGHS(751,5), REFLTS(751,5), TBRATS(751,5),
     &  DECIBL(751,5), ANGLE(751), IFOCAL(751,5), JRAY(751,5),
     &  ATOT(751,5)

      PI = ACOS(-1.)
C
C  If no iterval halving desired, redefine parameters for IRANGE -1.

      IF(IFOCNT .GT. 0 ) THEN
         DO 26 J = 1,ISKIP
            IFOCAL((IRANGE-1),J) = 2
  26     CONTINUE
         RETURN
      END IF
C
C  If interval halving desired then continue below.
      INEG1=IRANGE-1
      INEG2=IRANGE-2

C  Redefine parameters of ray trace for IRANGE+1
      APLUS=ANGLE(IRANGE)
      ATEMP=ANGLE(IRANGE)
      ITEMP=ISK(IRANGE)
      RTEMP=RANGES(IRANGE,ISKIP)

C********
C      JSUR=ISK(IPLUS)
C      DO 20 J=1,JSUR
C          RANGES(IPLUS,J)=RANGES(IRANGE,J)
C          FACTOR(IPLUS,J)=FACTOR(IRANGE,J)
C          TIMES(IPLUS,J)=TIMES(IRANGE,J)
C          HIGHS(IPLUS,J)=HIGHS(IRANGE,J)
C          REFLTS(IPLUS,J)=REFLTS(IRANGE,J)
C          TBRATS(IPLUS,J)=TBRATS(IRANGE,J)
C          IFOCAL(IPLUS,J)=IFOCAL(IRANGE,J)
C  20  CONTINUE
C********

C  Do interval halving on the angle to find the focal point
  30  CONTINUE
      ANGLE(IRANGE)=0.5*(ANGLE(INEG2)+ANGLE(IRANGE))
      CALL TRACE(RMAX,THETM,ANGLE(IRANGE),SI)
C
      IF (ISUR.GT.0) THEN
        ITEMP=ISUR
        ATEMP=ANGLE(IRANGE)
        RTEMP=RANGE(ISKIP)
      ENDIF
      DR=RANGES(INEG2,ISKIP)-RTEMP
      IF(ABS(ANGLE(INEG2)/ANGLE(IRANGE)-1.0) .LE. 0.1*DELANG) GO TO 35
      IF(DRA(ISKIP)*DR.GT.0.0) GO TO 31
      GO TO 30
  31  DIFANG=ABS(ANGLE(INEG2)-ANGLE(IRANGE))/2.0
  32  ANGLE(IRANGE)=ANGLE(IRANGE)+DIFANG
C
      CALL TRACE(RMAX,THETM,ANGLE(IRANGE),SI)
C
      IF (ISUR.GT.0) THEN
        ITEMP=ISUR
        ATEMP=ANGLE(IRANGE)
      ENDIF
      IF(DIFANG .LE. 0.1*DELANG*ANGLE(IRANGE) ) GO TO 35
      IF(ISUR.LE.0) GO TO 33
      RPLUS=RANGE(ISKIP)
      ANGLE(IRANGE)=ANGLE(IRANGE)-2.0*DIFANG
C
      CALL TRACE(RMAX,THETM,ANGLE(IRANGE),SI)
C
      IF (ISUR.GT.0) THEN
        ITEMP=ISUR
        ATEMP=ANGLE(IRANGE)
      ELSE
        GO TO 43
      ENDIF
      RNEG=RANGE(ISKIP)
      IF(DRA(ISKIP)*(RPLUS-RNEG).GT.0.0) GO TO 33
  43  ANGLE(IRANGE)=ANGLE(IRANGE)+2.0*DIFANG
  33  DIFANG=DIFANG/2.0
      GO TO 32
C
C Redefine parameters and identify ray type for each ray trace
  35  CONTINUE
      IF (ISUR.LE.0) THEN
        IF(IRANGE .NE. 0) THEN
           DEGANG = ANGLE(IRANGE) * 180./PI
C        WRITE(8,*)'FOCAL,IRANGE= ',IRANGE,'  ANGLE= ', DEGANG,
C     *            '  ISUR= ', ISUR
C        ELSE
C        WRITE(8,*)' FOCAL, IRANGE = 0, NO ANGLE   ISUR = ', ISUR
        END IF
C
        ISUR=ITEMP
        ANGLE(IRANGE)=ATEMP
      ENDIF
C
      I = INEG1
      DO 36 J = 1,ISUR
         IFOCAL(I,J) = 1
         IF(JFOCAL(J) .NE. 0 ) IFOCAL(I,J) = 2
         RANGES(I,J) = RANGE(J)
         FACTOR(I,J) = MAGNIF(J)
         TIMES(I,J)  = TIMEOA(J)
         HIGHS(I,J)  = HEIGHT(J)
         REFLTS(I,J) = REFLCT(J)
         TBRATS(I,J) = TBRATO(J)
         DRB(J) = RANGES(I,J) - RANGES(IRANGE,J)
36    CONTINUE
      ANGLE(I) = ANGLE(IRANGE)
      ISK(I)   = ISUR
      ANGLE(IRANGE) = APLUS
      RETURN
      END
C
      SUBROUTINE NOISE(NRANGE,SIP,ISORT,IDEX,JDEX)
C-----------------------------------------------------------------------
C
C  FUNCTION: Noise computes sound intensities of each downward
C            refracted ray with acoustic wave interference.
C
C  VARIABLES:  Additional definitions in subroutine BLAST
C             FM     -interpolation of magnification factor from adjacent
C                     raytraces to specified location.
C             RBSK   -Accumulated reflection oceffienents of skip rays
C             TBDAT  -Referenced blast duration for 1 kiloton blast
C             TBRANG -Range values for referenced blast duration
C             TBSK   -Accumulated blast pulse duration of rays.
C             TOA    -Interpolation of time of arrival from adjacent raytraces
C                     for specified location
C---------------------------------------------------------------------
      INTEGER KI,KT,MT1,MT2,MT3,MT4,MT8
      COMMON /LUNT/ KI,KT,MT1,MT2,MT3,MT4,MT8
c-----------------------------------------------------------------------
      INTEGER IGUN
      REAL    XB,YB,ZB,WB,SFPRES,DIRANG,ZBAG
      COMMON /SOURCE/ XB,YB,ZB,WB,SFPRES,DIRANG,IGUN,ZBAG
c-----------------------------------------------------------------------
      COMMON /RAYMAP/ISK(751), RANGES(751,5), FACTOR(751,5),
     &  TIMES(751,5), HIGHS(751,5), REFLTS(751,5), TBRATS(751,5),
     &  DECIBL(751,5), ANGLE(751), IFOCAL(751,5), JRAY(751,5),
     &  ATOT(751,5)
c-----------------------------------------------------------------------
      DIMENSION IDEX(500),JDEX(500)
      DIMENSION TBSK(20),RBSK(20),FM(20),TOA(20)
      DIMENSION TBRANG(7),TBDAT(7)
      DATA TBRANG/ 1.0E+1,1.0E+2,1.0E+3,1.0E+4,1.0E+5,1.0E+6,1.0E+7/
      DATA TBDAT / 0.12,0.097,0.34,0.52,0.93,1.33,1.73 /
      MT8 = 8
      PATM = SFPRES
     
C  Sort and order  by increasing ranges.
C  Save the two index arrays in the common SORT for future use.
      IC = 0
      DO 500 I = 1, NRANGE
        ISKIP = MAX(ISK(I),1)
        DO 550 J = 1, ISKIP
           IC = IC+1
           IF(IC .EQ. 1) THEN
              IDEX(IC) = I
              JDEX(IC) = J
           ELSEIF (RANGES(I,J).GE.RANGES(IDEX(IC-1),JDEX(IC-1)) ) THEN
              IDEX(IC) = I
              JDEX(IC) = J 
           ELSE
              DO 525 IBUB = IC,2,-1
                 IDEX(IBUB) = IDEX(IBUB-1)
                 JDEX(IBUB) = JDEX(IBUB-1)
                 IF(IBUB.EQ. 2) THEN
                    IDEX(1) = I
                    JDEX(1) = J
                 ELSEIF (RANGES(I,J).GE.
     *                    RANGES(IDEX(IBUB-2),JDEX(IBUB-2)) ) THEN
                    IDEX(IBUB-1) = I
                    JDEX(IBUB-1) = J
                    GO TO 550
                 ENDIF 
  525        CONTINUE
           ENDIF
  550    CONTINUE
  500 CONTINUE
      ISORT = IC

      DO 100 I=1,NRANGE
        ISKIP=MAX(ISK(I),1)
        DO 150 IS=1,ISKIP
          JI=0
C  Check to see if wave interference occurs for a given I and IS,
C  and iterate over primary and skip rays.
            DO 200 J=1,NRANGE
               TTBB=1.0
               RRBB=1.0
               JSKIP=MAX(ISK(J),1)
               DO 255 JS=1,JSKIP
                 IF( JI .EQ. 20 ) GO TO 200
                 IF( I .EQ. J .AND. IS .EQ. JS ) THEN
                   JI = JI + 1
                   FM(JI) = FACTOR(I,IS)
                   TOA(JI) = TIMES(I,IS)   
                 ELSE
                   IF(JS.GT.ISK(J+1) .OR. J.EQ.NRANGE .OR.
     &              (I .EQ. J+1 .AND. IS .EQ. JS) )    GO TO 250
                   IF( RANGES(I,IS).LT.RANGES(J,JS).AND.
     &                 RANGES(I,IS).LT.RANGES(J+1,JS)) GO TO 250
                   IF( RANGES(I,IS).GT.RANGES(J,JS).AND.
     &                 RANGES(I,IS).GT.RANGES(J+1,JS)) GO TO 250
                   JI=JI+1 

C  Interpolation for magnification factors and for time of arrivals
C  to RANGES(I,IS)
                   IF(RANGES(J+1,JS) .EQ. RANGES(J,JS) ) THEN
                      FM(JI)  = (FACTOR(J+1,JS)+ FACTOR(J,JS)) /2
                      TOA(JI) = (TIMES(J+1,JS) + TIMES(J,JS) ) /2
                   ELSE  
                      XN = ALOG( FACTOR(J+1,JS)/FACTOR(J,JS) ) /
     &                        ALOG( RANGES(J+1,JS)/RANGES(J,JS) )       
                      FM(JI) =FACTOR(J,JS)*(RANGES(I,IS)/
     &                        RANGES(J,JS))**XN   
                      TOA(JI)=(TIMES(J+1,JS)*(RANGES(I,IS)-
     &                        RANGES(J,JS))+
     &                        TIMES(J,JS)*(RANGES(J+1,JS)-
     &                        RANGES(I,IS)))/
     &                        (RANGES(J+1,JS)-RANGES(J,JS))
                   END IF
                 END IF
C
C  Place an upper limit on a raytrace magnification factor due to the
c  atmospheric turbulance.
                 FM(JI) = MIN(FM(JI),10.0)
C
C Place a lower limit on a raytrace magnification factor due to sound
C diffraction.
                 FMLOWER = 1.0
                 IF(RANGES(I,IS) .GT. 68.06) THEN
                   FMLOWER = 1.0/(0.187* (3.2808*RANGES(I,IS) )**0.31)
                 END IF
                 FM(JI) = MAX(FM(JI),FMLOWER)
                 ITBINT = 1
C
                 DO 275 ITB = 1,7
                   IF(RANGES(I,IS).GT.TBRANG(ITB)) ITBINT = ITB
  275            CONTINUE         
                 IF( ITBINT.EQ.7) ITBINT = 6
                 XN = ALOG(  TBDAT(ITBINT+1) / TBDAT(ITBINT) ) /
     &                ALOG( TBRANG(ITBINT+1)/ TBRANG(ITBINT) )
                 SCALE = (340.294 * TOA(JI) / RANGES(I,IS) ) *
     &                 ( (WB/2.0E+6)* (1013.25/PATM) )**(1./3.)
                 TBSK(JI) = TTBB*SCALE*TBDAT(ITBINT)*
     &                    (RANGES(I,IS)/TBRANG(ITBINT))**XN
C  Save values of accumulated reflection coefficients and blast
C  duration of initial/skip rays.
                 RBSK(JI)=RRBB
 250  CONTINUE

C  Adjust reflection coefficient and relative blast duration after ray skips
                 RRBB=RRBB*REFLTS(J,JS)
                 TTBB=MAX(0.001,TTBB*TBRATS(J,JS) )
 255           CONTINUE
 200        CONTINUE

C  Determine wave interference magnification factor at each TOA(JS)
            RPMAX=0.056234
C
C  JI now is the total number of rays arriving at RANGES(I,IS)
C  at separate times.
            DO 300 JS=1,JI
              RPO=0.0
              DO 350 JT=1,JI
                IF(TOA(JT) .GT. TOA(JS) .OR. TBSK(JT) .EQ. 0.0 )
     &              GO TO 350
                IF( TOA(JT) .EQ. TOA(JS) ) THEN
                  RPO = RPO + RBSK(JT)*FM(JT)
                ELSE
                  IF(TBSK(JT) .GT. 0.0 ) THEN 
                    RPO=RPO+(1.0-(TOA(JS)-TOA(JT))/TBSK(JT))*
     &              EXP((TOA(JT)-TOA(JS)) /TBSK(JT))*RBSK(JT)*FM(JT)
                  END IF
                END IF
 350          CONTINUE
C
C  Select the maximum of all the peaks in the wave interference
C  of pressure pulses.
              IF(ABS(RPO).GT.RPMAX) RPMAX=ABS(RPO)
 300        CONTINUE

C  Compute sound intensity at RANGES(I,IS)
C  Modified (Dec 88)to use ANSI S2.20 - 1983
            PPO=42.48/(RANGES(I,IS)*3.2808)**1.1
            SND=20.*ALOG10(PPO)+171.
            AW=20.*ALOG10(WB**0.367)
            A=20.*ALOG10(RPMAX)
C
C  Correction for blast source directivity.
            CALL DIRECTDB(SIP,DIRDB)
            DECIBL(I,IS)=SND +AW + A + DIRDB
            JRAY(I,IS)=JI
c##################################################
c     if(SIP.eq.0.) then
c       write(kt,*) 'in NOISE: SIP=',SIP
c       WRITE(kt,*) 'I,IS,SND,AW,A,DIRDB,DECIBL(I,IS)=',
c    1               I,IS,SND,AW,A,DIRDB,DECIBL(I,IS)
c     endif
c##################################################
 150     CONTINUE
 100  CONTINUE
      RETURN
      END
C
      SUBROUTINE TRANGE(SI,THETM,THETL,IREFRT)
C---------------------------------------------------------------------
C
C   FUNCTION: TRANGE computes the minimum and maximum initial
C               elevation angles of ray traces along with the maximum
C               altitude for azimuthal direction SI,
C   VARIABLES:  Additional definitions in subroutine BLAST
C             XTEMP - X coordinates for earth surface plots in Raytraces.
C             YTEMP - Y coordinates for earth surface plots in Raytraces.
C             RTSPHT- RATIO OF SPECIFIC HEAT
C---------------------------------------------------------------------
      INTEGER KI,KT,MT1,MT2,MT3,MT4,MT8
      COMMON /LUNT/ KI,KT,MT1,MT2,MT3,MT4,MT8
c-----------------------------------------------------------------------
      REAL    RMAXD, DELRT, DELS, ALTGMX
      INTEGER ATT, ELEV, NPPSPK
      COMMON/WHEELS/ ATT(751,73),ELEV(751,73),RMAXD,NPPSPK,DELRT,DELS,
     &               ALTGMX
c-----------------------------------------------------------------------
      INTEGER IGUN
      REAL    XB,YB,ZB,WB,SFPRES,DIRANG,ZBAG
      COMMON /SOURCE/ XB,YB,ZB,WB,SFPRES,DIRANG,IGUN,ZBAG
c-----------------------------------------------------------------------
      REAL    ALT,T,RH,WN,WE,V,WU,WV,SV,p_mb
      INTEGER NP
      COMMON /WEATHR/ ALT(70),T(70),RH(70),WN(70),WE(70),V(70),WU(70),
     &                WV(70),SV(70),p_mb(70),NP
c-----------------------------------------------------------------------
      COMMON /TRCINP/ IPLOT, XPLOT(751), YPLOT(751), APLOT(751),
     &                        XTEMP(751), YTEMP(751), ISCALE
c-----------------------------------------------------------------------
      real*8     DEGREES_PER_RADIAN
      parameter (DEGREES_PER_RADIAN = 57.29577951308232D0 )
C
C  Obtain temperature, east wind, and west wind versus altitude
C  from radiosonde.  assume sounder released at the same ground
C  altitude as the blast source, or close by.
C
      DATA REARTH /6370999./
      COSANG (SK,C,U,W)= ( (C*C-W*W)/(SK-U) + U) /
     &                   SQRT( (C*C-W*W)*(SK+U)/(SK-U ) + U*U )
      PI=ACOS(-1.)
      ISI=INT(SI*180.0/(PI*DELS) + .000005) + 1
C
      DELSI=PI/2.
      VMAXU=0.0
      VMAXL=0.0
C  Compute effective speed of sound for each altitude increment
      DO 65 I=1,NP
        N=NP-I+1
        WU(N)=0.3048*(WE(N)*SIN(SI-DELSI)-WN(N)*COS(SI-DELSI))
        WV(N)=0.3048*(WE(N)*COS(SI-DELSI)+WN(N)*SIN(SI-DELSI))
c       SV(N)=331.48*SQRT(1.+T(N)/273.15)
        SVdry=331.48*SQRT(1.+T(N)/273.15)

C**  Modification done on speed of sound calculation Aug 1991
C  See Wong and Embleton-- Published 7 Jan 1985
cmm5*** To include humidity use this
c       RTSPHT = 0.04833 + (RH(N)/100. - 0.023) *
c    &            ( 9.2E-5 + 5.5E-6 * T(N) + 4.25E-7 * T(N)**2 ) 
c       SV(N) = SQRT(RTSPHT * 8314.34 * (T(N)+273.15) )
cmm5*** To force to dry use this
        SV(N) = SVdry
C**
        VN=AMAX1(SV(N)**2-WV(N)**2,0.)
        V(N)=SQRT(VN)+WU(N)
c       write(6 ,*)'N,ZB,ALT,SVdry,SV,V=',N,ZB,ALT(N),SVdry,SV(N),V(N)
c       write(kt,*)'N,ZB,ALT,SVdry,SV,V=',N,ZB,ALT(N),SVdry,SV(N),V(N)
        IF( V(N) .GT. VMAXU .AND. ZB .LE. ALT(N))  VMAXU = V(N)
        IF( V(N) .GT. VMAXL .AND. ZB .GE. ALT(N) ) VMAXL = V(N)
  65  CONTINUE
c     --- ensure no trapping in the first layer
c     if(V(2).GE.V(1)) then
c       V(1)=V(2)+0.5
c     endif
c     if((VMAXL.LE.0.) .AND. (ZB.LT.ALT(1))) VMAXL=V(1)
c     write(kt,*) 'VMAXU,VMAXL=',VMAXU,VMAXL

C  Determine maximum and minimum elevation angle of initial ray
      NN=NP-1
      DO 25 I=1,NN
      IF( ALT(I+1).GE.ZB .AND. ALT(I).LE.ZB ) THEN
         VSOUND=(V(I)*(ALT(I+1)-ZB)+V(I+1)*(ZB-ALT(I)))/
     &          (ALT(I+1)-ALT(I))
         SOUNDC=(SV(I)*(ALT(I+1) -ZB) + SV(I+1)*(ZB-ALT(I))) /
     &           (ALT(I+1) - ALT(I) )
         SOUNDU=(WU(I)*(ALT(I+1) -ZB) + WU(I+1)*(ZB-ALT(I))) /
     &           (ALT(I+1) - ALT(I) )
         SOUNDV=(WV(I)*(ALT(I+1) -ZB) + WV(I+1)*(ZB-ALT(I))) /
     &           (ALT(I+1) - ALT(I) )
      ENDIF
  25  CONTINUE
c     write(kt,*) 'VSOUND,SOUNDC,SOUNDU,SOUNDV=',
c    1             VSOUND,SOUNDC,SOUNDU,SOUNDV

      IF( ZB.LT. ALT(1) ) THEN
         VSOUND = V(1)
         SOUNDC = SV(1)
         SOUNDU = WU(1)
         SOUNDV = WV(1)
      ELSE IF (ZB.GT.ALT(NP) ) THEN
         VSOUND = V(NP)
         SOUNDC = SV(NP)
         SOUNDU = WU(NP)
         SOUNDV = WV(NP)
      ENDIF
      IREFRT = 0
c     write(kt,*) 'VSOUND,SOUNDC,SOUNDU,SOUNDV=',
c    1             VSOUND,SOUNDC,SOUNDU,SOUNDV
c
      IF(VMAXU .GT. VSOUND) THEN
         IF(VMAXL .GT. VSOUND ) THEN
            IF(VMAXU .GT. VMAXL ) THEN
               THETM = ACOS(COSANG(VMAXU,SOUNDC,SOUNDU,SOUNDV))
            ELSE
               THETM = -ACOS(COSANG(VMAXL,SOUNDC,SOUNDU,SOUNDV))
            END IF
         ELSE
            THETM = ACOS(COSANG(VMAXU,SOUNDC,SOUNDU,SOUNDV))
         END IF
      ELSE 
         IF( VMAXL .GT. VSOUND) THEN
            THETM = -ACOS(COSANG(VMAXL,SOUNDC,SOUNDU,SOUNDV))
         ELSE
            THETM = 0.0
         END IF
      END IF
c      write(kt,*) 'THTEM=',THETM

      THETCH = ATAN( (ELEV(2,ISI) - ELEV(1,ISI) )/DELRT)
      IF ( ZB.LE.ALT(2) .AND. THETM.LE.THETCH ) THETM = THETCH + 0.05
      IF( ZB.GT.ALT(1) ) THEN
C
C  Limit to 100 meters out.
         THETL = ATAN (( ALT(1) - ZB)/100. )
      ELSE
         THETL = THETCH
         IF( THETM .EQ. THETCH +0.05) THEN
            IREFRT = 1
         END IF
      END IF
cmm5  --- try angles at least up tp 1 deg
c     THETM=AMAX1(THETM,1./DEGREES_PER_RADIAN) 
c     THETM=MAX(THETM,2./DEGREES_PER_RADIAN) 
c     write(kt,*)'THETCH,THETM,THETL,IREFRT=',THETCH,THETM,THETL,IREFRT

C  Compute maximum elevation along angle, SI
      ALTGMX=0.0

C  Define a counter for points.
      NBRPTS = 1
      DO 30 I=1,NPPSPK

C  Calculate the elevation of the earth's surface for the raytrace plot
C  routines.  Record the number of points to be plotted in the first
C  element of the Xtemp and Ytemp arrays.
         NBRPTS = NBRPTS + 1
         XTEMP(NBRPTS) = 1. + (I-1)*DELRT
         YTEMP(NBRPTS) = SQRT( REARTH**2 - XTEMP(NBRPTS)**2) -
     &                         REARTH + ELEV(I,ISI)
         IF (ELEV(I,ISI).GT.ALTGMX) ALTGMX=ELEV(I,ISI)
 30   CONTINUE
C
C  Enter the number of plot points into the first element of the arrays.
      XTEMP(1) = NBRPTS - 1
      YTEMP(1) = NBRPTS - 1
C
      RETURN
      END
C
      SUBROUTINE DIRECTDB( SIP, DIRDB )
C----------------------------------------------------------------------
C  This subroutine calculates the additional Decibels to be added to
C  the total due to the direction of the blast (direction of the weapon).
C  Input variables:
C
C         ALPHA  - azimuth angle (Passed as SIP angle)
C         DIRANG - angle weapon pointed, MET angle converted to(x,y) axis
C         A(N)   - the directional angle values in the table
C         B(N)   - the db values corresponding to the directional angles.
C
C  Output variables:
C         DIRDB  - the additional dbs to be added on due to the direction
C                  of the sound source.
C----------------------------------------------------------------------
      INTEGER IGUN
      REAL    XB,YB,ZB,WB,SFPRES,DIRANG,ZBAG
      COMMON /SOURCE/ XB,YB,ZB,WB,SFPRES,DIRANG,IGUN,ZBAG
c-----------------------------------------------------------------------
      DIMENSION A(7),B(7)
      ALPHA = SIP
C
C  Set dirdb to 0 and return if the blast is considered a uniform blast.
      IF (IGUN .EQ. 0 ) THEN
         DIRDB = 0.
         GO TO 900
      END IF
C  Set the parameters for 105 MM HOWITZER M102
      IF(IGUN .EQ. 1 ) THEN
         B(1) =  6.96
         B(2) =  3.07
         B(3) = -0.82
         B(4) = -4.38
         B(5) = -7.87
         B(6) = -10.31
         B(7) = -10.84
      END IF
C   Set the parameters for 105mm TANK M60
      IF(IGUN .EQ. 2 ) THEN 
         B(1) =  4.57
         B(2) =  3.26
         B(3) =  1.95
         B(4) = -3.17
         B(5) = -5.97
         B(6) = -9.69
         B(7) = -10.78
      END IF
C   Set the parameters for 8" HOWITZER M110
      IF(IGUN .EQ. 3 ) THEN
         B(1) =  6.41
         B(2) =  2.93
         B(3) = -0.54
         B(4) = -3.35
         B(5) = -5.94
         B(6) = -8.00
         B(7) = -7.36
      END IF
C   Set the parameters for 8" SELF PROPELLED M110A1
      IF(IGUN .EQ. 4 ) THEN
         B(1) =  6.43
         B(2) =  3.18
         B(3) = -0.06
         B(4) = -4.49
         B(5) = -6.87
         B(6) = -9.40
         B(7) = -9.90
      END IF
C   Set the parameters for 120 MM TANK (HEAT-TPT) 
      IF(IGUN .EQ. 5 ) THEN
         B(1) =  2.6
         B(2) =  1.9
         B(3) =  1.0
         B(4) = -0.3
         B(5) = -1.8
         B(6) = -2.8
         B(7) = -4.6
      END IF
C   Set the parameters for 120 MM TANK (SABOT)
      IF(IGUN .EQ. 6 ) THEN
         B(1) =  1.8
         B(2) =  1.7
         B(3) =  1.6
         B(4) =  0.1
         B(5) = -2.3
         B(6) = -2.9
         B(7) = -3.6
      END IF

      IF( DIRANG .LE. 180.)THEN
         A(1) = DIRANG
         A(2) = DIRANG +  30
         A(3) = DIRANG +  60
         A(4) = DIRANG +  90
         A(5) = DIRANG + 120
         A(6) = DIRANG + 150
         A(7) = DIRANG + 180
         IF (ALPHA .GT. A(7) )  ALPHA = A(7) - (ALPHA - A(7))
         IF (ALPHA .LT. A(1) )  ALPHA = A(1) + (A(1) - ALPHA)
         I = 1
  10     CONTINUE
         IF (ALPHA .LE. A(I+1) .AND. ALPHA .GE. A(I) ) THEN
              DIRDB = B(I) - ( (ALPHA - A(I) )/(A(I+1) - A(I)) *
     *           (B(I) - B(I+1) )   )
         ELSE
            I = I + 1
            GO TO 10
         END IF

      ELSE IF(DIRANG .GT. 180. )THEN
          A(1) = DIRANG
          A(2) = DIRANG -  30
          A(3) = DIRANG -  60
          A(4) = DIRANG -  90
          A(5) = DIRANG - 120
          A(6) = DIRANG - 150
          A(7) = DIRANG - 180
          IF (ALPHA .GT. A(1) )  ALPHA = A(1) - ( ALPHA - A(1))
          IF (ALPHA .LT. A(7) )  ALPHA = A(7) + ( A(7)  - ALPHA)
C
          I = 1
  20      CONTINUE
          IF (ALPHA .LE. A(I) .AND. ALPHA .GE. A(I+1) ) THEN
             DIRDB = B(I) - ( (ALPHA - A(I) )/
     *          (A(I+1) - A(I) ) * ( B(I) - B(I+1) )   )
          ELSE
             I = I + 1
             GO TO 20
          END IF
      END IF
C
 900  CONTINUE
      RETURN
      END
C
      SUBROUTINE TRACE(RMAX,THETM,ANGO,SI)
C-----------------------------------------------------------------------
C
C  This subroutine calculates the range, magnification factor,
C  and time of arrival of primary and skip refracted rays
C  utilizing the 3-d speed of sound profile and the elevation
C  and land use databases for a given azimuth angle and initial
C  elevation angle.
C  VARIABLES:
C             ISUR =(-1THRU -22) Flags to indicate why raytrace was not
C                                accepted. See definitions in User Manual.
C---------------------------------------------------------------------
      REAL    RMAXD, DELRT, DELS, ALTGMX
      INTEGER ATT, ELEV, NPPSPK
      COMMON/WHEELS/ ATT(751,73),ELEV(751,73),RMAXD,NPPSPK,DELRT,DELS,
     &               ALTGMX
c-----------------------------------------------------------------------
      INTEGER IGUN
      REAL    XB,YB,ZB,WB,SFPRES,DIRANG,ZBAG
      COMMON /SOURCE/ XB,YB,ZB,WB,SFPRES,DIRANG,IGUN,ZBAG
c-----------------------------------------------------------------------
      integer ISUR,IMAX,JCOAST,JFOCAL
      real    RANGE,MAGNIF,TIMEOA,HEIGHT,REFLCT,TBRATO,DRA,DRB
      COMMON /SKIP/ISUR,IMAX,RANGE(10),MAGNIF(10),TIMEOA(10),HEIGHT(10),
     &   REFLCT(10),TBRATO(10),JCOAST(10),JFOCAL(10),DRA(10),DRB(10)
c-----------------------------------------------------------------------
      COMMON /TRCINP/ IPLOT, XPLOT(751), YPLOT(751), APLOT(751),
     &                        XTEMP(751), YTEMP(751), ISCALE
c-----------------------------------------------------------------------
      REAL MF
      DATA REARTH/6370999./,DELRMIN/1.0/, RWATER/1.0/,ANGMIN/0.005/
      SEC(X)=1./COS(X)
      COT(X)=1./TAN(X)

C  Functions for acoustic ray time increment between adjacent layers
      T(H,ANG1,ANG2,VO) = H*COS(ANG1)*(ALOG(SEC(ANG2)+TAN(ANG2))-
     &  ALOG(SEC(ANG1)+TAN(ANG1)))/(VO*(COS(ANG1)-COS(ANG2)))
      TL(H,ANG1,ANG2,VO)=ABS(H / SIN((ANG1+ANG2) *.5) ) / VO

C
      PI     = ACOS(-1.)
      ACC    = 180./PI
      ISI    = INT(SI*ACC/DELS + .000005) + 1
 1    TOA    = 0.
      MF     = 1.0
      ISUR   = 0
      SDIST  = 0.
      ANGI   = ANGO
      RT     = 0.
      RPDR   = 0.0
      ISTEP  = 0
      DNDTO = 0.0
      IPLOT  = 0
      SUMDIS = 0.0
      MT8    = 8
    2 CONTINUE

C  Begin ray trace computations of range, arc length, and time elapsed
      ANG2=ANGI
      COS01=COS(ANGI)
      H=0.0

C  IF RT<1, then subroutine will set ALT1 to blast source altitude
C  and VO1 is speed of sound at blast source.
C  IF RT>1, then ALT1 is ground altitude and VO1 is speed of sound
C  at ground.
      NB = 0
      CALL TERAIN(RT,SI,ANGI,NB,1,0.0,INB1,ALT1,V01,VGRAD,RG,RTBG)
      HD = ALT1
c     print *,'in TRACE after TERRAIN 1: RT,ALT1=',RT,ALT1

C  Record the initial plot data (X,Y,& THETA) for the raytrace graphs.
      IPLOT = IPLOT + 1

C  Check to see if the storage arrays are out-of-bounds.
C  Give the user the option to skip this ray and continue or
c  to abort the program.
      IF( IPLOT .GE. 998) THEN
         IF(ISUR.EQ.0) ISUR = -1
         RETURN
      END IF
      XPLOT(IPLOT) = RT
      YPLOT(IPLOT) = ALT1
      APLOT(IPLOT) = ANG2

      IF(ANGI.GT.0.0) INC=1
      IF(ANGI.LE.0.0) INC=-1
   30 CONTINUE
      ANG1=ACOS(COS01)*INC
      NB=INC
c     print *,'new loop:INC,ANG1=',INC,ANG1

C  Determine altitude and speed of sound at current position
C  If ALT2 is ground altitude, subroutine will set NB=-2
C  If ALT2 is maximum altitude, subroutine will set NB=+2
C
      CALL TERAIN(RT,SI,ANGI,NB,INB1,ALT1,INB2,ALT2,V02,VGRAD,RG,RTBG)
      NBS = 0
c     print *,'in TRACE after TERRAIN 2: NB,NBS=',NB,NBS

  45  CONTINUE
      COS02=(V02/V01)*COS01
c     print *,'COS02,V02,V01,COS01=',COS02,V02,V01,COS01

C  Check for turning over or under
      IF(COS02.GE.1.0 .AND. ABS(ANG1).LT.ANGMIN ) THEN
        IF(ABS(NB).EQ.1.AND.ISUR.EQ.0) ISUR = -2
        RETURN
      ENDIF
      IF(COS02.GE.1.0) GO TO 16

C  Check to see if a circular arc is possible and reasonable.
      ANG2=ACOS(COS02)*INC
c     print *,'ANG2,COS02,INC,ISUR=',ANG2,COS02,INC,ISUR
      IF (COS(0.5*(ANG1+ANG2)).LE.0.0 .OR. ABS(ANG2).LT.ANGMIN) THEN
        IF(NBS.NE.0.AND.INT(ABS(NB)).GT.1) THEN
          IF(ISUR.EQ.0) ISUR = -3
          RETURN
        ELSE
          NB=INC
          IF(NBS.EQ.0) THEN
            CALL TERAIN(RT,SI,ANGI,NB,INB1,ALT1,INB2,ALT2,V02,
     &                  VGRAD,RG,RTBG)
          ELSE
            CALL TERAIN(RT,SI,ANGI,NB,INB2,ALT2,INB2,ALT2,V02,VGRAD,
     &                  RG,RTBG)
          ENDIF
        ENDIF
        NBS=NBS+1
c       print *,'ISUR,NBS=',ISUR,NBS
        IF(NBS.GT.50) THEN
           IF(ISUR.EQ.0) ISUR = -4
           RETURN
        ENDIF
        GO TO 45
      ENDIF
C
C  Predicted raytrace range
      SLINV=COT(.5*(ANG1+ANG2))
      IF ( (ALT2-ALT1)*SLINV.GE.DELRMIN) THEN
        RC=RT+(ALT2-ALT1)*SLINV
      ELSE
        IF( (ALT2-ALT1)*SLINV.LE.0.0 .AND. NB .EQ.2 ) GO TO 32
        IF( (ALT2-ALT1)*SLINV.GE.0.0.AND.NB.LT.(-1) ) THEN
          NB=-2
          GO TO 32
        ENDIF


C  Predicted values of range or altitude are not good enough
c       print *,'ISUR,NB,NBS=',ISUR,NB,NBS
        IF(NBS.NE.0.AND.INT(ABS(NB)).GT.1) THEN
          IF(ISUR.EQ.0) ISUR = -5
          RETURN
        ELSE
          NB=INC
          IF(NBS.EQ.0) THEN
            CALL TERAIN(RT,SI,ANGI,NB,INB1,ALT1,INB2,ALT2,V02,VGRAD,
     &                  RG,RTBG)
          ELSE
            CALL TERAIN(RT,SI,ANGI,NB,INB2,ALT2,INB2,ALT2,V02,VGRAD,
     &                  RG,RTBG)
          ENDIF
        ENDIF
        NBS=NBS+1
        IF(NBS.GT.50) THEN
          IF(ISUR.EQ.0) ISUR = -6
          RETURN
        ENDIF
        GO TO 45
      ENDIF

C  Check for ground impact.
      REMAX = REARTH + ALTGMX
C
         IREF=INT(RT/DELRT) + 1
         IF (ALT2.EQ.ALT1.AND.SLINV.LT.0.0) THEN
           ILIMIT = NPPSPK-IREF
         ELSE
           ILIMIT = MAX( MIN( INT(RC/DELRT)+1, NPPSPK-1 ) -IREF +1, 1)
         ENDIF
c  	 print *,'ILIMIT,ISUR=',ILIMIT,ISUR
C  Increment through the elevation database to find the range at which
C  the raytrace crosses over the terrain.
      DO 36 ICOUNT = 1, ILIMIT
        IP = IREF + ICOUNT
        RP = (IP-1)*DELRT
        ALTP = ELEV(IP,ISI) - REARTH + SQRT(REARTH*REARTH - RP*RP)
        ALTU = (RP-RT)/SLINV + ALT1
c	print *,'in 36 loop: ICOUNT,IREF,IP,ALTP,ALTU=',
c     1                       ICOUNT,IREF,IP,ALTP,ALTU
c	print *,'IP,ISI,ELEV,REARTH,RP=',IP,ISI,ELEV(IP,ISI),REARTH,RP
c        print *,'SQRT(REARTH*REARTH-RP*RP)=',SQRT(REARTH*REARTH-RP*RP)
        IF ((ALTP-ALTU).GE.0.0) THEN
C  Calculate the raytrace termination point on the ground.
          RPS = RP - DELRT
          ALTS = ELEV(IP-1,ISI)-REARTH+SQRT(REARTH*REARTH - RPS*RPS)
          RCINT=((RP-RPS)*(SLINV*(ALT1-ALTS)-RT)+RPS*SLINV*(ALTP-ALTS))
     &          /(SLINV*(ALTP-ALTS)-(RP-RPS))
c  if the predicted intercepted range is beyond the calculated range,
c  then increment to the next circular arc, unless already at the ground.
c	print *,'in 36 loop: RC,RCINT,INB2=',RC,RCINT,INB2
          IF(RCINT.GT.RC.AND.INB2.GT.1) THEN
            GO TO 36
          ELSE
            RC = RCINT
          ENDIF
          IF(RC.LT.RT) THEN
            IF(ISUR.EQ.0) ISUR = -7
            RETURN
          END IF

C  Define new values for RG,RTBG, and ALT2 at ground inpact.
          CALL TERAIN(RC,SI,ANGI,NB,INB1,ALT1,INB2,ALT2,VP,VGRAD2,
     &                RG,RTBG)
c	  print *,'after TERAIN: ISUR=',ISUR
          ALT2=(RC-RT)/SLINV + ALT1
          IF(SLINV.LT.0.) NB=-2
          IF(SLINV.GT.0.) NB=3
          GOTO 35
        ENDIF
C        
        IF ( ICOUNT.EQ.ILIMIT.AND.ILIMIT.EQ.NPPSPK-IREF )THEN
C Raytrace went beyond max range without ground impact.
           NB = 2
           GO TO 35
        END IF
 36     CONTINUE
 35     CONTINUE
c        print *,'ICOUNT,ILIMIT,NPPSPK,IREF=',
c     1           ICOUNT,ILIMIT,NPPSPK,IREF
C      ENDIF
C
C  Return if ray goes straight up or down
      IF((ANG2 .LE. -PI/2.) .OR. (ANG2 .GE. PI/2.)) THEN
         IF(ISUR.EQ.0) ISUR = -8
         RETURN
      ENDIF

C  Return if current ray trace angles are too small for focusing
      IF(ABS(ANG1).LT.ANGMIN .OR. ABS(ANG2).LT.ANGMIN) THEN
        IF(ISUR.EQ.0) ISUR = -9
        RETURN
      ENDIF
C
C  Calculate parameters needed for Krol's method for focusing magnitude.
      COSDIF = ABS(COS01 - COS02)
      SUMDS0 = SUMDIS
C
      SUMDIS = SUMDIS + (RC-RT)/(SIN(ANG1)*SIN(ANG2))
      ISUMD = 1
C
      DNDTH = ABS(TAN(ANGI)*SIN(ANG2)*SUMDIS) + DNDTO
      RT=RC
      H=ALT2-ALT1
c      print *,'COSDIF,SUMDIS=',COSDIF,SUMDIS
c
C  Arc length and time increment computation
      IF(COSDIF.LT.1.E-5) SDIST=SDIST+ ABS(H/SIN(0.5*(ANG1+ANG2)) )
      IF(COSDIF.GE.1.E-5)SDIST=SDIST+ H*(ANG1-ANG2)/(COS02 - COS01)
      IF(SDIST .LT. 0.0) THEN
         ISUR = -11
         RETURN
      END IF
      IF(COSDIF .LT. 1.E-5) TOA = TOA + TL(H,ANG1,ANG2,V01)
      IF(COSDIF .GE. 1.E-5) TOA = TOA + T(H,ANG1,ANG2,V01)
      IF(DNDTH.NE.0.0 .AND. SDIST.NE.0.0) THEN
        MF = SQRT(SDIST/DNDTH)
      ELSE
        MF = 1.0
      ENDIF
c      print *,'TOA,MF=',TOA,MF
C
C  Record the plot data( X,Y, & THETA) for the raytrace plots,
C  If RT is larger than the last point read into the data base.
C  This check is necessary to avoid division by zero in RAYTRACE.
      XCOMPA = XPLOT(IPLOT)
      IF( RT .GT. (XPLOT(IPLOT) + 0.0001)) THEN
         IPLOT = IPLOT + 1

C  Check to see if the storage arrays are out-of-bounds.
C  Give the user the option to skip this ray and continue or
c  to abort the program.
         IF( IPLOT .GE. 751) THEN
           IF(ISUR.EQ.0) ISUR = -12
           RETURN
         END IF
C
         XPLOT(IPLOT) = RT
         YPLOT(IPLOT) = ALT2
         APLOT(IPLOT) = ANG2
      END IF
C
 32   CONTINUE
c
C  Update some raytrace parameters for the next integration step.
      COS01=COS02
      V01=V02
      ALT1=ALT2
      INB1 = INB2

C  Return if range limits have been exceeded
      IR=INT(RT/DELRT+.000005) + 1
      IF ((RT.GT.RMAX).OR.(RT .LE. 0.000001).OR.(ELEV(IR,ISI).LT.0.))
     &   NB = 2

C  Stop ray trace computation if the ground has been reached
      IF(NB.EQ.-2.OR.NB.EQ.3) THEN
        IF(( SUMDS0*SUMDIS) .LT. 0.0 .AND. ISTEP .GT. 1 ) THEN
           IF (ISUR .EQ. 0) ISUR = -10
           RETURN
        END IF
        GO TO 10
      END IF

C Return if altitude limits have been exceeded
      IF(NB.EQ.2) RETURN
C
C  Return if too many steps are involved.(avoids getting stuck in the loop.)
      ISTEP = ISTEP + 1
      IF((ISTEP-IPLOT) .GT. 50 .AND. (RT-XCOMPA) .LE. 0.0001 ) THEN
        IF(ISUR .EQ. 0 )  ISUR = -30
        RETURN
      END IF
C
C  Continue ray trace increments
      GO TO 30
C
C  Compute range, arc length and time increments of the turning acoustic ray
  16  CONTINUE
      IF (ABS(ALT2-ALT1).LT.1.E-6.OR.VGRAD*ANG1.LE.0.0) THEN
        IF(ISUR .EQ.0) ISUR = -14
        RETURN           
      ENDIF
      H=(V01/COS01-V01)/VGRAD
      RCONT=V01/(ABS(VGRAD)*COS01)
      XCONT=RT+(V01/VGRAD)*TAN(ANG1)
      YCONT=ALT1-(V01/VGRAD)
      RC=XCONT+(XCONT-RT)

      IF(INC.EQ.1) HD=ALT1+H

C  Check for ground impact.
      REMAX = REARTH + ALTGMX
      IF(ABS(RC). GE. REMAX .OR. ABS(RT) .GE. REMAX )  THEN
        IF(ISUR .EQ. 0) ISUR = -15
        RETURN
      END IF
C
      IREF=INT(RT/DELRT) + 1
      ILIMIT = MAX( MIN( INT(RC/DELRT)+1, NPPSPK-1 ) -IREF +1, 1)
c     print *,'IREF,ILIMIT=',IREF,ILIMIT
      DO 38 ICOUNT = 1, ILIMIT
C  Increment through elevation database to find the raytrace crossover.
        IP = IREF + ICOUNT
        RP = (IP-1)*DELRT
C  Calculate the altitude of the turnover arc to a certain range.
        ALTP = ELEV(IP,ISI) - REARTH + SQRT(REARTH*REARTH - RP*RP)

C  Check to see if RP was incremented such that it is now outside the
C  arc of the circle radius RCONT.
        IF(RP .GT. (XCONT+RCONT) .OR. RP .LT. (XCONT-RCONT) )
     &       GO TO 381 
        ALTU=SIGN(1.,ANG1)*SQRT(RCONT**2-(RP-XCONT)**2)+YCONT

        RPS = RP - DELRT
        ALTS = ELEV(IP-1,ISI)-REARTH+SQRT(REARTH*REARTH - RPS*RPS)
        SLOPE=(ALTP-ALTS)/(RP-RPS)
        YINTC=ALTS-RPS*SLOPE
        ATERM=1.0+SLOPE**2
        BTERM=2.0*(SLOPE*(YINTC-YCONT)-XCONT)
        CTERM=XCONT**2-RCONT**2+(YINTC-YCONT)**2
        DET=BTERM**2-4.0*ATERM*CTERM
        IF(DET.GE.0.0) THEN
          RC1 = ( BTERM-SQRT(DET) )/(-2.*ATERM)
          RC2 = ( BTERM+SQRT(DET) )/(-2.*ATERM)
          RCM = MIN(RC1,RC2)
          RCP = MAX(RC1,RC2)
        END IF

        IF((( ( RCM.GE.RPS .AND. RCM.LE.RP .AND.RCM.GT.RT .AND.
     &            RCM.LE.RC .AND. RCM.GE.DELRMIN ) .OR. 
     &          (RCP.GE.RPS .AND. RCP.LE.RP .AND. RCP.GT.RT .AND.
     &            RCP.LE.RC )  ) .AND. 
     &          DET.GE.0.0 ) )  THEN

C  The turnover arc has intersected the ground, calculate the ground 
C  impact point.
          IF(      DET.GE.0.0 .AND. RCM.GE.RPS .AND. RCM.LE.RP .AND.
     &        RCM.GT.RT .AND.  RCM.LE.RC .AND. RCM.GE.DELRMIN) THEN
            RC = RCM
          ELSEIF (DET.GE.0.0 .AND. RCP.GE.RPS .AND. RCP.LE.RP .AND.
     &              RCP.GT.RT  .AND. RCP.LE.RC ) THEN
            RC = RCP
          ELSEIF (ABS(ALTP-ALTS) .LE. 1.E-7) THEN
            RC = (RP+RPS)/2.
          ELSE
            RC=RPS+(ALT1-ALTS)*(RP-RPS)/(ALTP-ALTS)
            RC = MAX(RPS,RC)
            RC = MIN(RP,RC)
          END IF
C  The ground impact point has been found
          ALT2=SLOPE*RC+YINTC
C  Calculate the ground slope angle.
          NB = -1
          CALL TERAIN(RC,SI,ANGI,NB,1,ALT2,INB2,ALT2,V02,VGRAD2,
     &                RG,RTBG)
          ANG2=ATAN((XCONT-RC)/(ALT2-YCONT))
          COS02= COS(ANG2)
C  Reset raytrace parameters to indicate the status of the raytrace.
C  -2 = terminated coming down, 3=terminated going up.
          IF(ANG2.LE.0.0) NB = -2
          IF(ANG2.GT.0.0) NB = 3

C  Determine arc length and time of arrival of turnover ray trace
C  Return if current ray trace angles are too small for focusing
          IF(ABS(ANG1).LT.ANGMIN .OR. ABS(ANG2).LT.ANGMIN) THEN
C  Raytrace angle is too small, causing large round off error in certain
C  parameters.
             IF(ISUR.EQ.0) ISUR = -16
               RETURN
          ENDIF
             IF(H.NE.0.0) THEN
                IF(RC.GT.XCONT) THEN
C  Calculate arc length,Krol's effective length, and time of arrival on the
C  ground prior to turnover point.
                   COSDF1 = 1.0 - COS(ANG1)
                   COSDF2 = 1.0 - COS(ANG2)

                   SDIST=SDIST+RCONT*(ABS(ANG1)+ABS(ANG2))
                   SUMDS0 = SUMDIS
                   SUMDIS=SUMDIS+((V02/TAN(ANG2)-V01/TAN(ANG1)) 
     &                              /VGRAD)-RC+RT
                   DNDTH=ABS(TAN(ANGI)*SIN(ANG2)*SUMDIS) + DNDTO
                   isumd = 3

                   IF(COSDF1.LT.1.E-5) TOA=TOA+TL(H,ANG1,0.,V01)
                   IF(COSDF1.GE.1.E-5) TOA=TOA+T(H,ANG1,0.0,V01)
                   IF(COSDF2.LT.1.E-5) TOA=TOA+TL(ALT1+H-ALT2,
     &                                       -ANG2,0.,V02)
                   IF(COSDF2.GE.1.E-5) TOA=TOA+T(ALT1+H-ALT2,
     &                                       -ANG2,0.,V02)
                ELSE
C  Calculate accumulated arc length, Krol's effective length, and time of
C  arrival on the ground after the turnover point.
                   SDIST=SDIST+RCONT*ABS(ANG1-ANG2)
                   COSDIF = ABS( COS(ANG1) - COS(ANG2) )
                   SUMDS0 = SUMDIS
                   SUMDIS = SUMDIS +(RC-RT)/(SIN(ANG1)*SIN(ANG2))
                       isumd = 5
                   DNDTH = ABS(TAN(ANGI)*SIN(ANG2)*SUMDIS) + DNDTO
                   IF(COSDIF.LT.1.E-5)
     &                TOA=TOA+TL(ALT2-ALT1,ANG1,ANG2,V01)
                   IF(COSDIF.GE.1.E-5) 
     &                TOA=TOA+T(ALT2-ALT1,ANG1,ANG2,V01)
                ENDIF
             ELSE
                SDIST=SDIST+DELRMIN
                TOA=TOA+DELRMIN/V01
                DNDTH=SDIST
             ENDIF
C  Bypass the algorithm for the non-impacting turnover raytrace.
             GOTO 37
          ENDIF
 381      CONTINUE
C Raytrace went beyond max range without hitting ground.
          IF ( ICOUNT.EQ.ILIMIT.AND.ILIMIT.EQ.NPPSPK-IREF ) NB=2
 38    CONTINUE
c
C  Arriving at this point indicates no ground impact of raytrace was determined.
C  The trunover raytrace segment is a symmetric circular arc, so reset the
C  parameters for the raytrace for calculation of the next integration step.
      ANG2 = -ANG1
      COS02 = COS01
      V02 = V01
      ALT2 = ALT1
      INB2 = INB1

C  Return if current ray trace angles are too small for focusing
      IF (ABS(ANG1).LT.ANGMIN .OR. ABS(ANG2).LT.ANGMIN) THEN
         IF(ISUR.EQ.0) ISUR = -18
         RETURN
      ENDIF
C  Calculate accumulated arc length, Krol's effective length, and time of 
C  arrival at the end of the circular arc.

      SUMDS0 = SUMDIS
      SUMDIS=SUMDIS+((V02/TAN(ANG2)-V01/TAN(ANG1))/VGRAD)-RC+RT
      DNDTH=ABS(TAN(ANGI)*SIN(ANG2)*SUMDIS) + DNDTO
      isumd = 7
      COSDIF = 1.0 - COS(ANG1)
      IF(H.NE.0.0) THEN
        IF(COSDIF.LT.1.E-5) SDIST=SDIST+2.*ABS(H/SIN(0.5*ANG1) )
        IF(COSDIF.GE.1.E-5) SDIST=SDIST+2.*H*ANG1/COSDIF
        IF (SDIST .LT. 0.0 ) THEN
          ISUR = -19
          RETURN
        END IF
        IF(COSDIF .LT. 1.E-5) TOA = TOA + 2.*TL(H,ANG1,0.0,V01)
        IF(COSDIF .GE. 1.E-5) TOA = TOA + 2.*T(H,ANG1,0.0,V01)
      ELSE
        SDIST=SDIST+DELRMIN
        TOA=TOA+DELRMIN/V01
        DNDTH=SDIST
        isumd = 9
      ENDIF
 37   CONTINUE
      IF(DNDTH.NE.0.0 .AND. SDIST.NE.0.0) THEN
        MF = SQRT(SDIST/DNDTH)
      ELSE
        MF = 1.0
      ENDIF
      RT=RC

C  Record the plot data(x,y,theta) for the raytrace plots,
C  if RT is larger than the last point read into the data base.
      XCOMPA = XPLOT(IPLOT)
      IF( RT .GT. (XPLOT(IPLOT) + 0.0001)) THEN
         IPLOT = IPLOT + 1

C  Check to see if the storage arrays are out-of-bounds.
C  Give the user the option to skip this ray and continue or
c  to abort the program.
         IF( IPLOT .GE. 751) THEN
            IF(ISUR.EQ.0) ISUR = -20
            RETURN
         END IF

         XPLOT(IPLOT) = RT
         YPLOT(IPLOT) = ALT2
         APLOT(IPLOT) = ANG2
      END IF

C  Continue ray trace computation after turning over or under
      INC=-1*INC
      GO TO 32
C
C  End of ray trace turnover
C
  10  CONTINUE
      IF(RT .LE. 1.0 ) THEN
        IF(ISUR .EQ. 0)  ISUR = -21
        RETURN
      END IF
C
C  Skip range too small, start over.
      IF( ISUR .GE. 1 .AND. RT-RANGE(ISUR) .LT. DELRMIN) THEN
        IF(ISUR .EQ. 0)  ISUR = -22
        RETURN
      END IF
C
C  Calculate reflecting angle of the shock wave.
      IREF=RT/DELRT + 1
      ALTP=ELEV(IREF+1,ISI)-REARTH
     &     + SQRT(REARTH*REARTH-IREF*DELRT*IREF*DELRT)
      IF(RG.LT.RWATER) THEN
         ALTN=ELEV(IREF,ISI)-REARTH
     &     + SQRT(REARTH*REARTH-(IREF-1)*DELRT*(IREF-1)*DELRT)
      ELSE
         ALTN = ALTP
      END IF
      BETA=ATAN((ALTP-ALTN)/DELRT)-RT/SQRT(REARTH*REARTH-RT*RT)

C  Store results for primary and skip rays
      ISUR=ISUR+1
      RANGE(ISUR)=RT
C  The upper limit of magnification is set to 10. due to turbulence or 
C  diffraction.
      MAGNIF(ISUR)=MIN(MF,10.0)
      TIMEOA(ISUR)=TOA
C
C  Correct for earth's curvature.
      HEIGHT(ISUR)=HD+REARTH-SQRT(REARTH*REARTH-RT*RT)
      REFLCT(ISUR)=RG
      TBRATO(ISUR)=RTBG
C
C  Return if no more skip rays are allowed
      IF(RG.LE.0.0.OR.ISUR.EQ.IMAX) RETURN
C Computation of skip rays
      MF=1.0
C
C Compute reflected ray elevation angle due to terrain
      ANGI   = 2.*BETA-ANG2
      SUMDIS = 0.0
      DNDTO = DNDTH
      IF(ANGI.GT.THETM.OR.ANGI.LT.BETA) RETURN
      GO TO 2
      END
C
      SUBROUTINE TERAIN(RT,SI,ANGI,NB,INB1,ALT1,INB2,ALT2,VSOUND,
     &                  VGRAD,RG,RTBG)
C-----------------------------------------------------------------------
C
C  FUNCTION: Terain computes the speed of sound, reflection
C            coefficient, and relative blast duration for range rt,
C            azimuth angle si, and new altitude ALT2.
C            ALT2 is computed from ALT1 and NB.
C  	VARIABLES:
C             
c-----------------------------------------------------------------------
      REAL    RMAXD, DELRT, DELS, ALTGMX
      INTEGER ATT, ELEV, NPPSPK
      COMMON/WHEELS/ ATT(751,73),ELEV(751,73),RMAXD,NPPSPK,DELRT,DELS,
     &               ALTGMX
c-----------------------------------------------------------------------
      INTEGER IGUN
      REAL    XB,YB,ZB,WB,SFPRES,DIRANG,ZBAG
      COMMON /SOURCE/ XB,YB,ZB,WB,SFPRES,DIRANG,IGUN,ZBAG
c-----------------------------------------------------------------------
      REAL    ALT,T,RH,WN,WE,V,WU,WV,SV,p_mb
      INTEGER NP
      COMMON /WEATHR/ALT(70),T(70),RH(70),WN(70),WE(70),V(70),WU(70),
     *               WV(70),SV(70),p_mb(70),NP
c-----------------------------------------------------------------------
cmm5  --- save off computation of snellk for next call (changed 7/24/00)
      SAVE SNELLK
c-----------------------------------------------------------------------
      DATA REARTH /6370999./
      COSANG (SK,C,U,W)= ( (C*C-W*W)/(SK-U) + U) /
     &                   SQRT( (C*C-W*W)*(SK+U)/(SK-U ) + U*U )
      SNELL(A,C,U,W)= U + (C*C - W*W) / (COS(A) * 
     &               SQRT( C*C - W*W - U*U*SIN(A)**2) - U*SIN(A)**2 )
c-----------------------------------------------------------------------
C
      PI = ACOS(-1.)
      ACC=180./PI

      CW= 1.0
C  Compute earth curvature correction for vertical distance
      JMAX=MIN(751,INT(RMAXD/DELRT))
      ZCURV=REARTH-SQRT(REARTH*REARTH - RT*RT)
c      print *,'on entry: INB,ALTP,ALTC,SNELLK,VSOUND=',
c    1                    INB,ALTP,ALTC,SNELLK,VSOUND

C  Translate to earth curvature coordinate system
      Z1=ALT1+ZCURV

C  Interpolate elevation and land use parameters to current location
      I=INT(SI*ACC/DELS + .000005) + 1
      J=INT((RT/DELRT) +1)
      IF( J .LE. JMAX) THEN
        ZGRND=(ELEV(J,I)*(J*DELRT-RT) + ELEV(J+1,I)*(RT -(J-1)*DELRT))
     &        /DELRT
      ELSE
        ZGRND = ELEV(JMAX+1,I)
      END IF
      RTBG=1.0
      IF(J .LE. JMAX) THEN
         IF (RT.GT.((J-1)*DELRT + DELRT/2.)) THEN
           RG=ATT(J+1,I)
         ELSE
           RG=ATT(J,I)
         ENDIF
      ELSE
         RG = ATT(JMAX+1,I)
      END IF
c     Print *,'J,JMAX,RG,NB=',J,JMAX,RG,NB
c
      ICOUNT = 0
      INBINT = INB1
      INB = INB1
      IF(NB) 10,20,30

C  Compute speed of sound and other conditions at the blast source,
C  and at the skip ray source.
  20  CONTINUE

C  Blast source MSL
      Z2=ZB
C
C  ZGRND = Skip ray's ground altitude in earth curvature coordinate system.
      IF(RT.GT.1.0) Z2=ZGRND
C  Correct met altitude for ground elevation height with reference to the 
C  source ground location.
      NN=NP-1
      DO 25 I=1,NN
        IF (( ALT(I)- ALT(1)) .GE. 3.*ABS(ZGRND-ALT(1)) - 0.1 ) THEN
          ALTC=ALT(I)
        ELSE
          ALTC=ALT(I) + (ZGRND - ALT(1))*
     &         EXP(CW*(ALT(I)-ALT(1))/(ALT(I)- ALT(1)
     &        -3.*ABS(ZGRND-ALT(1))) )
        ENDIF
        IF ( (ALT(I+1)-ALT(1) ) .GE. 3.*ABS(ZGRND-ALT(1)) - 0.1 ) THEN
          ALTP=ALT(I+1)
        ELSE
          ALTP=ALT(I+1) + (ZGRND -  ALT(1))*
     &       EXP(CW*(ALT(I+1)-ALT(1))/(ALT(I+1) - ALT(1)
     &       -3.*ABS(ZGRND-ALT(1))))

        ENDIF

C  Calculate generalized Snell constant for each change in the initial or
C  reflected angles of each ray trace.
        IF (ALTP.GE.Z2.AND.ALTC.LE.Z2) THEN
          UWIND=(WU(I)*(ALTP-Z2)+WU(I+1)*(Z2-ALTC))/(ALTP-ALTC)
          VWIND=(WV(I)*(ALTP-Z2)+WV(I+1)*(Z2-ALTC))/(ALTP-ALTC)
          CTEMP=(SV(I)*(ALTP-Z2)+SV(I+1)*(Z2-ALTC))/(ALTP-ALTC)
          SNELLK=SNELL(ANGI,CTEMP,UWIND,VWIND)
          VSOUND=SNELLK*COS(ANGI)
          INB=I
        ELSEIF (ALTP .LT. Z2 .AND. I .EQ. NN) THEN
            SNELLK = SNELL(ANGI,SV(NP),WU(NP),WV(NP) )
            VSOUND = SNELLK*COS(ANGI)
            INB = NP
        ELSEIF (ALTC .GT. Z2 .AND. I .EQ. 1) THEN
            SNELLK = SNELL(ANGI,SV(1),WU(1),WV(1) )
            VSOUND = SNELLK*COS(ANGI)
            INB = 1
        ENDIF
  25  CONTINUE
c      print *,'after 25: INB,ALTP,ALTC,SNELLK,VSOUND=',
c    1                    INB,ALTP,ALTC,SNELLK,VSOUND
C
C  Translate back to ray trace coordinate system
      ALT2=Z2 - ZCURV
      INB2 = INB
      RETURN
C
C  Compute speed of sound and  other conditions of ascending ray
  30  CONTINUE
      ICOUNT = ICOUNT + 1
C
C  Correct vertical profile due to current ground elevation.
      IF ((ALT(INB)-ALT(1) ) .GE. 3.*ABS(ZGRND-ALT(1)) - 0.1 ) THEN
        ALTC=ALT(INB)
      ELSE
        ALTC=ALT(INB) + (ZGRND - ALT(1))*
     &     EXP(CW*(ALT(INB)-ALT(1))/(ALT(INB) - ALT(1)
     &     -3.*ABS(ZGRND-ALT(1))) )
      ENDIF

      IF( INB .GT. 1) THEN
         IF ((ALT(INB-1)-ALT(1)) .GE. 3.*ABS(ZGRND-ALT(1)) -0.1)  THEN
           ALTN=ALT(INB-1)
         ELSE
           ALTN=ALT(INB-1) + (ZGRND - ALT(1))*
     &       EXP(CW*(ALT(INB-1)-ALT(1))/(ALT(INB-1)-ALT(1)-
     &       3.* ABS(ZGRND-ALT(1))))
         ENDIF
      END IF
c     print *,'after 30: ICOUNT,ALC,ALTN=',ICOUNT,ALTC,ALTN
c
c  Determine the next altitude in ascending direction and its index,
c  INB, for speed of sound value.
      IF (ALTC.LE.(Z1+0.1) .OR. INB .EQ. INBINT) GO TO 32
      Z2=ALTC
c     print *,'Z1,Z2,INB,INBINT=',Z1,Z2,INB,INBINT
c
      IF (INB.EQ.1) GO TO 35
C
      IF (ALTN.LE.(Z1+0.1) .OR. (INB - 1) .EQ. INBINT ) GO TO 35
      IF( ICOUNT. EQ. NP ) GO TO 35
      INB=INB-1
      GO TO 30
 32   IF(INB.EQ.NP) THEN
         Z2 = ALTC
      ELSE
         IF( ICOUNT. EQ. NP ) GO TO 35
         INB=INB+1
         GO TO 30
      END IF
  35  VSOUND=SNELLK*COSANG( SNELLK,SV(INB),WU(INB),WV(INB) )
c     print *,'INB,VSOUND,SNELLK,SV(INB),WU(INB),WV(INB)=',
c    1         INB,VSOUND,SNELLK,SV(INB),WU(INB),WV(INB)
c
c  Calculate the correct speed of sound SLOPE.
      IF(INB .EQ. INBINT) THEN
         IF(INB.GT. 1) THEN
           INBNG = INB-1
         ELSE 
           INBNG = INB+1
         END IF
      ELSE
         INBNG = INBINT
      END IF

      IF ((ALT(INBNG)-ALT(1)) .GE. 3.*ABS(ZGRND-ALT(1)) -0.1)  THEN
         ALTNG = ALT(INBNG)
      ELSE
         ALTNG = ALT(INBNG) + (ZGRND - ALT(1))*
     &    EXP(CW*(ALT(INBNG)-ALT(1))/(ALT(INBNG)-ALT(1)-
     &    3.* ABS(ZGRND-ALT(1))))
      END IF 
      VN = SNELLK*COSANG( SNELLK,SV(INBNG),WU(INBNG),WV(INBNG) )
      VGRAD = (VSOUND-VN) / (ALTC - ALTNG)   

C  Ray trace may have reached maximum altitude.
      IF(INB.EQ.NP .AND. INBINT .EQ. NP) NB=2
C
C  Translate back to ray trace coordinate system
      ALT2=Z2 - ZCURV
      INB2 = INB
      RETURN
C
C  Compute speed of sound and other conditions of descending ray.
C  Correct vertical profile due to current ground elevation.
  10  CONTINUE
      ICOUNT = ICOUNT + 1
      IBEGIN = INB
      IF((ALT(INB)-ALT(1) ) .GE. 3.*ABS(ZGRND-ALT(1)) - 0.1) THEN
        ALTC=ALT(INB)
      ELSE
	      ALTC=ALT(INB) + (ZGRND -  ALT(1))*
     &      EXP(CW*(ALT(INB)-ALT(1))/(ALT(INB) - ALT(1)
     &      -3.*ABS(ZGRND-ALT(1)) ))
      ENDIF

      IF((INB+1) .LE. NP ) THEN
         IF((ALT(INB+1)-ALT(1) ) .GE. 3.*ABS(ZGRND-ALT(1)) - 0.1) THEN
           ALTP=ALT(INB+1)
         ELSE
	         ALTP=ALT(INB+1) + (ZGRND -  ALT(1))*
     &       EXP(CW*(ALT(INB+1)-ALT(1))/(ALT(INB+1)- ALT(1)
     &       -3.*ABS(ZGRND-ALT(1))))
         ENDIF
      END IF
C
C  Determine the next altitude in descending direction and its index,
C  INB, for the speed of sound value.
      IF(ALTC .GE. (Z1-0.1) .OR. INB .EQ. INBINT) GO TO 12
      Z2=ALTC
C
      IF (INB.EQ.NP) GO TO 15
C
      IF(ALTP .GE. (Z1-0.1) .OR. (INB+1).EQ.INBINT) GO TO 15
      IF( ICOUNT. EQ. NP ) GO TO 15
      INB=INB+1
      GO TO 10
  12  IF(INB.EQ.1) THEN
         Z2 = ALTC
      ELSE
         IF( ICOUNT. EQ. NP ) GO TO 15
         INB=INB-1
         GO TO 10
      END IF
  15  VSOUND=SNELLK*COSANG( SNELLK,SV(INB),WU(INB),WV(INB) )
C
C  Calculate the correct speed of sound slope
      IF(INB .EQ. INBINT) THEN
        IF(INB.LT.NP) THEN
          INBPS = INB+1
        ELSE
          INBPS = INB-1
        END IF
      ELSE
        INBPS = INBINT
      END IF
      IF ((ALT(INBPS)-ALT(1)) .GE. 3.*ABS(ZGRND-ALT(1)) -0.1)  THEN
         ALTPS = ALT(INBPS)
      ELSE
         ALTPS=ALT(INBPS) + (ZGRND -  ALT(1))*
     &     EXP(CW*(ALT(INBPS)-ALT(1))/(ALT(INBPS)- ALT(1)
     &     -3.*ABS(ZGRND-ALT(1))))
      END IF
      VP =SNELLK*COSANG( SNELLK,SV(INBPS),WU(INBPS),WV(INBPS) )
      VGRAD = (VSOUND-VP)  / (ALTC-ALTPS)
C
C  Ray trace may have reached ground level.
      IF( IBEGIN .EQ. INB .AND. INB .EQ. 1 ) NB = -3
C
C  Translate back to ray trace coordinate system.
      ALT2 = Z2 - ZCURV
      INB2 = INB
      RETURN
      END
C
      SUBROUTINE RAYTRACE (SI,iray)
C----------------------------------------------------------------------
C
C  This subroutine accepts input date generated by the subroutine BLAST
C  the data is in the common block TRCINP and represents the X,Y, and  
C  the angle THETA for each given point in BLAST. The X and Y coordinates
C  are computed and passed on to the routine to plot the raytraces.      
C  VARIABLES:
C             IPLOT  - The number of points to be plotted, generated 
C                          in TRACE      
C             XPLOT  - Array of X values used to calculate the X coordinates.
C             YPLOT  - Array of Y values used to calculate the Y coordinates.
C             APLOT  - Array angles used to calculate the X&Y coordinates. 
C             DELRES  - The delta resolution, defines distance 
C                           between X values     
C             ISCALE  - Flag to mark that the graph,title, tic 
C                            marks are drawn.     
C             XCOORD, YCOORD  - the coordinates  used to plot the trace.
C             XCONT,YCONT,RCONT - constants used in calculating the 
C                            coordinates.    
C             IREL,RELDIF  - Flags used to determine relative difference 
C                            and avoid any divisions by zero.  
C             JCUM    - Index for the arrays storing the coordinates.
C
C----------------------------------------------------------------------
      COMMON /TRCINP/ IPLOT, XPLOT(751), YPLOT(751), APLOT(751),
     &                        XTEMP(751), YTEMP(751), ISCALE
c-----------------------------------------------------------------------
      real    XFAC,YFAC,DELRES,RXL,RYL,RXH,RYH,X1,Y1,X2,Y2,
     1  UB,UT,UR,UL,TRXB,TRYB
      COMMON /MAPCOR/ XFAC,YFAC,DELRES,RXL,RYL,RXH,RYH,X1,Y1,X2,Y2,
     1  UB,UT,UR,UL,TRXB,TRYB
c-----------------------------------------------------------------------
      integer ndelsi, nranges, nray
      real    Aza, rangea, dBa, xray, yray
      common /savea/  ndelsi, nranges(361), Aza(361), rangea(751,361),
     1                dBa(751,361), xray(751,101), yray(751,101), nray
      DIMENSION XCOORD(751),YCOORD(751)
C  Dimension X,Y one over the XPLOT limit to allow plot parameters to be
C  recorded.
C  Limit IPLOT to under 500 so the plot arrays do not go out of bounds.
      IF( IPLOT .GE. 499 ) RETURN
C
      XCONT      = 0.0
      YCONT      = 0.0
      RCONT      = 0.0
      IPLTEQ     = 0
      PI         = ACOS(-1.)
      JCUM       = 1
C
      IPL = IPLOT - 1
      DO 500 I = 1,IPL

C  If the X increment is very small, skip the plot.
         IF( ABS(XPLOT(I+1) - XPLOT(I)) .LT. 0.0001)   GO TO 500

C  Relative difference flag.
         IREL = 0

C  Solve for the X,Y, & R constants when APLOT(I) = zero.
         IF(APLOT(I) .EQ. 0.0 ) THEN
             IF( ABS( APLOT(I+1)) .LE. 0.00001) THEN
                IPLTEQ = 2
                GO TO 400

C  Consider the angles equal.
             ELSE
                XCONT = XPLOT(I)
                YCONT = YPLOT(I+1) + (XCONT - XPLOT(I+1))*
     &                  TAN(APLOT(I+1) - PI/2)
                RCONT = ABS( YPLOT(I) - YCONT )
                IPLTEQ = 1
             END IF
             IREL = 1
         END IF
C
C  Solve for the X,Y, & R constants when APLOT(I+1) = zero.
         IF( APLOT(I+1) .EQ. 0.0) THEN
             IF( ABS( APLOT(I)) .LE. 0.00001) THEN
                 IPLTEQ = 2
                 GO TO 400

C  Angles considered equal
             ELSE
                 XCONT  = XPLOT(I+1)
                 YCONT  = YPLOT(I) + (XCONT - XPLOT(I))*
     &                    TAN(APLOT(I) - PI/2)
                 RCONT  = ABS( YPLOT(I+1) - YCONT)
                 IPLTEQ = 1
             END IF
             IREL = 2
          END IF
C
C  If angles are very close in value consider them equal.
C  Check the absolute difference first to avoid division by zero in the
C  following computations.
          IF (ABS ( APLOT(I+1) - APLOT(I) ) .LE. 0.00001) THEN
             IPLTEQ = 2
             GO TO 400
          END IF
C
C  If the angles are not equal, calculate the constant values as follows:
          XCONT = ( YPLOT(I+1) - YPLOT(I) + XPLOT(I) *
     &              TAN(APLOT(I) - PI/2) -
     &              XPLOT(I+1) * TAN(APLOT(I+1) - PI/2) ) /
     &            ( TAN(APLOT(I) -PI/2) - TAN(APLOT(I+1) -PI/2) )

C  Check the absolute value of the angles to determine equation to be used to
C  find the Y and R constants.
          IF( APLOT(I+1) .GT. APLOT(I) ) THEN
              YCONT = YPLOT(I+1) + (XCONT - XPLOT(I+1)) *
     &                TAN(APLOT(I+1) - PI/2)
              RCONT = SQRT( (XPLOT(I) - XCONT)**2 +
     &                      (YPLOT(I) - YCONT)**2    )
          ELSE
              YCONT = YPLOT(I) + (XCONT - XPLOT(I)) *
     &                TAN(APLOT(I) - PI/2)
              RCONT = SQRT( (XPLOT(I+1) - XCONT)**2 +
     &                      (YPLOT(I+1) - YCONT)**2  )
          END IF
          IPLTEQ=   1
  400     CONTINUE
C
C  Using the appropriate arc equation calculate the x & y values
C  for input to the plot routines.   The pre-determined increment
C  (delta-resolution) will determine the number of points to
C  be used to plot the ray trace arc segments.  Use XPLOT(I) as the
C  starting point for each arc segment.
          N = 1 + ( XPLOT(I+1) - XPLOT(I) )/ DELRES
C
C  Limit the value of N so the arrays do not go out of bounds.
          IF( N .GT. (INT(1000/IPL) - 1 ))    N = INT(1000/IPL) - 1
C
C  Determine if the sign should be - or + before the square root function.
          IF(APLOT(I+1) .LT. APLOT(I)) YSIGN = +1.
          IF(APLOT(I+1) .GT. APLOT(I)) YSIGN = -1.
          N = N + 1

C  Leave first element for recording the total number of data points in array.
          DO 450  J = 1,N
             JCUM = JCUM + 1
             XCOORD(JCUM) = (J-1)* (XPLOT(I+1) - XPLOT(I))/(N-1) +
     &                      XPLOT(I)
C
             IF( IPLTEQ .EQ. 1) THEN
                 YCOORD(JCUM) =YSIGN*( SQRT( RCONT**2 - (XCOORD(JCUM)-
     &                                   XCONT)**2)) + YCONT
             ELSE
                 YCOORD(JCUM) = YPLOT(I) + (XCOORD(JCUM) - XPLOT(I)) *
     &           ( (YPLOT(I+1) - YPLOT(I)) /(XPLOT(I+1) - XPLOT(I)) )
             END IF
  450     CONTINUE
C
  500 CONTINUE

C  Determine the plot file parameter, # of pts
      XCOORD(1) = JCUM - 1
      YCOORD(1) = JCUM - 1
      NBRPTS    = JCUM - 1
C
C  Call routine to plot the raytrace.
      nray=iray
      do i=1,nbrpts
        xray(i,nray)=XCOORD(i)
        yray(i,nray)=YCOORD(i)
      enddo
c     CALL PLOTRAY(XCOORD, YCOORD, SI)
c
      RETURN
      END
c
c     SUBROUTINE PLOTRAY(XCOORD,YCOORD,SI)
      SUBROUTINE PLOTRAY(SI)
C-----------------------------------------------------------------------
C  This routine is called each time a ray is to be plotted. The parameters
C  for plotting are passed in common.
C  VARIABLE:  PLOTIT  - Flag to request the axes plotting be done.
C-----------------------------------------------------------------------
c-----------------------------------------------------------------------
      INTEGER isnd, ICHOICE, ISITE, IOPT, ICONTR, nsnds, nlevels,
     1        DBPLOTOPT
      REAL    PSIMIN, PSIMAX, LATS, LONS, LATB, LONB, input_delpsi,
     1        xminp,xmaxp,yminp,ymaxp
      LOGICAL DOELV, DOATT, white_backgrnd
      CHARACTER*60 snd_label
      COMMON  /INPUTS/ snd_label, ICHOICE, ISITE, IOPT, ICONTR, PSIMIN,
     1        PSIMAX, LATS, LONS, LATB, LONB, input_delpsi, isnd, DOELV,
     2        DOATT, white_backgrnd, xminp,xmaxp,yminp,ymaxp, nsnds,
     3        nlevels, DBPLOTOPT
c-----------------------------------------------------------------------
      integer ncdir,nodir,nmdir 
      CHARACTER*60 DATADIR,OUTDIR,METDIR,FILELV,FILEAT,FILMET,FILSUM,
     1  FILOUT,met_dsn
      COMMON /filenm/ DATADIR,OUTDIR,METDIR,FILELV,FILEAT,FILMET,FILSUM,
     1  FILOUT,met_dsn,ncdir,nodir,nmdir
c-----------------------------------------------------------------------
      integer KOUNT
      COMMON /DATA/ YMETRS(70),TEMPER(70),RELHUM(70),WNDSPD(70),
     &              WNDDRT(70),KOUNT
c-----------------------------------------------------------------------
      REAL    ALTN,TN,RH,WN,WE,V,WU,WV,SV,p_mb
      INTEGER NWP
      COMMON /WEATHR/ALTN(70),TN(70),RH(70),WN(70),WE(70),V(70),WU(70),
     &                WV(70),SV(70),p_mb(70),NWP
c-----------------------------------------------------------------------
      COMMON /TRCINP/ IPLOT, XPLOT(751), YPLOT(751), APLOT(751),
     &                        XTEMP(751), YTEMP(751), ISCALE
c-----------------------------------------------------------------------
      REAL    RMAXD, DELRT, DELS, ALTGMX
      INTEGER ATT, ELEV, NPPSPK
      COMMON/WHEELS/ ATT(751,73),ELEV(751,73),RMAXD,NPPSPK,DELRT,DELS,
     &               ALTGMX
c-----------------------------------------------------------------------
      LOGICAL*1 PLOTIT
      COMMON /PLTRT/ PLOTIT
c-----------------------------------------------------------------------
      integer ndelsi, nranges, nray
      real    Aza, rangea, dBa, xray, yray
      common /savea/  ndelsi, nranges(361), Aza(361), rangea(751,361),
     1                dBa(751,361), xray(751,101), yray(751,101), nray
c-----------------------------------------------------------------------
      DIMENSION XCOORD(751),YCOORD(751)
      CHARACTER*40 TEXT
      real       FTPERM, DEGPRAD, FPSTOKN
      parameter (FTPERM=3.280833, DEGPRAD=57.29578, FPSTOKN=0.5925)
C
C Check to see if the axes need to be drawn. PLOTIT true or false.
c     IF(PLOTIT) THEN
c        XMIN = 0.0
c        XMAX = 30.
c        YMIN = ELEV(1,1) - 100.
c        YMAX = ELEV(1,1) + 1500.
c        IF( ISCALE .EQ. 0 ) THEN
C
C  Add the speed of sound curve on the basic axis
c        CALL SSPLOT(SI)
c  Plot earth's curvature
c        NPTS = XTEMP(1)
c        CALL GCLIP(0)
c        CALL COLOR(15)
c        CALL XAXIS(XMIN,XMAX,0.0,1,'RANGE (kilometers) ' )
c        CALL YAXIS(YMIN,YMAX,0.0,1,'ALTITUDE (meters) ' )
c        CALL CHART(0.15,0.95,0.1,0.65)
c        CALL GCLIP(1)
c        IF(NPTS .LE. 0 ) RETURN
c          DO 10 I=1,NPTS
c10	       XTEMP(I+1)=XTEMP(I+1)/1000.
c          CALL COLOR(10)
c          CALL GPL(NPTS,XTEMP(2),YTEMP(2) )
c          ISCALE = 1
c          CALL COLOR(15)
c        END IF
c
c     --- NCAR graphics calls
      XMIN = 0.0
      XMAX = 40.
      YMIN = 0.
      YMAX = +1500.
c     YMAX = +5000.
      FL=0.15
      FR=0.95
      FB=0.15
      FT=0.70
      LL=1      ! linear x,y plot
      UL = XMIN
      UR = XMAX
      UB = YMIN
      UT = YMAX
      mjrx=8
      mnrx=2
      mjrz=3
c     mjrz=5
      mnrz=5
c
c     --- initialize plot
      if(white_backgrnd) then
        lc=0
        call wflush
      else
        lc=1
      endif
      CALL COLOR(lc)
      CALL GSPLCI(lc)
      call gaseti('LTY',1)  ! use PLCHHQ to draw labels
      call pcseti('FN',21)  ! font 21
      call gsfaci(0)   ! black text
      call gspmci(0)   ! black text
c     print *,'in plotray: calling set: UL,UR,UB,UT=',UL,UR,UB,UT
      call set(FL,FR,FB,FT,UL,UR,UB,UT,LL)
      dU = UT-UB
      call labmod('(I2)','(I5)',0,0,2,2,20,20,0)
      call periml(mjrx,mnrx,mjrz,mnrz)
      call plchhq(UL,UT+0.2*UT,met_dsn,0.025,0.,-1.)
      DEGX = SI * 57.2957
      IF(DEGX .LE. 90) ZMETANG = 90. - DEGX
      IF(DEGX .GT. 90) ZMETANG = 450.- DEGX
c     print *,'in plotray: psi,az=',DEGX,ZMETANG
c     write(text,101) ZMETANG
      write(text,101) DEGX
  101 format(' Az=',F6.0)
      call plchhq(0.5*(UR-UL),UT+0.2*UT,text,0.025,0.,-1.)
c
c     --- label ordinate
      uy = UT/2.
      ux = UL-6.
      call plchhq(ux,uy,'z(m)',0.02,90.,0.)
c
c     --- label abscissa
      ux = (UR-1.0)/2.
      uy = UB-0.15*dU
      call plchhq(ux,uy,'range(km)',0.02,0.,0.)
c
c     --- plot speed of sound vs ht in m/s
      kp=0
      cmax=-1.0E30
      cmin= 1.0E30
      do k=1,NWP
        if(ALTN(k).gt.UT) go to 15
        kp=kp+1
        cmax=AMAX1(V(kp),cmax)	  ! m/s
        cmin=AMIN1(V(kp),cmin)
      enddo
   15 continue
      dc = cmax-cmin
c     print *, 'psi,cmax,cmin,kp=',DEGX,cmax,cmin,kp
      do k=1,kp
        xp=(V(k)-cmin)*10./dc
        xcoord(k)=xp
        yp=ALTN(k)
        yp=AMIN1(UT,yp)
        ycoord(k)=AMAX1(UB,yp)
c       print *, 'psi,k,c,xp,alt,yp=',
c    1    DEGX,k,V(k),xp,ALTN(k),ycoord(k)
      enddo
      call setusv('LW',2000)
c     call GSLN(3) ! dotted
      call GSLN(1) ! solid
      CALL COLOR(2)    ! red
      CALL GSPLCI(2)   ! red
      call curved(xcoord,ycoord,kp)
      call setusv('LW',1000)
      call plchhq(xcoord(kp)+1.,ycoord(kp)+1.,'ceff',0.02,0.,0.)
      CALL COLOR(lc)
      CALL GSPLCI(lc)
      write(text,102) cmax,cmin
  102 format('cmax,cmin(m/s)=',2F8.1)
      call plchhq(UL,UT+0.1*UT,text,0.02,0.,-1.)
C
C  Now draw each raytrace for a given azimuth angle on the original axis.
C  For convience of scaling the X coordinate divided by 1000 
      do iray=1,nray
        NPTS = xray(1,iray)-10
        IF(NPTS .LE.0) go to 25
        np=0
        do i=1,NPTS
          xp=xray(i+1,iray)/1000.
          xcoord(i)=AMIN1(UR,xp)
          yp=yray(i+1,iray)
          yp=AMIN1(UT,yp)
          ycoord(i)=AMAX1(UB,yp)
        enddo
c
c       --- plot ray coordinates
c       do i=1,NPTS
c         print *,'iray,i,x,y=',iray,(i+1),xcoord(i+1),ycoord(i+1)
c       enddo
        call setusv('LW',1000)
c       call setusv('LW',2000)
        call GSLN(1) ! solid
        call curved(xcoord,ycoord,NPTS)
      enddo ! iray
   25 continue
c 
c     --- plot wind speed along this azimuth
      kp=0
      wsmax=-1.0E30
      wsmin= 1.0E30
      Tmax=-1.0E30
      Tmin= 1.0E30
      do k=1,NWP
        if(ALTN(k).gt.UT) go to 30
        kp=kp+1
        wsmax=AMAX1(wu(kp),wsmax)	  ! m/s
        wsmin=AMIN1(wu(kp),wsmin)
        Tmax =AMAX1(TN(kp),Tmax)
        Tmin =AMIN1(TN(kp),Tmin)
c        print *,'si,kp,uk,vk,ws,wu(kp),wv(kp)=',
c     1           DEGX,kp,uk,vk,ws,wu(kp),wv(kp)
      enddo
   30 continue
c
      call NEWPLT
      if(white_backgrnd) then
        lc=0
        call wflush
      endif
      call setusv('LW',1000)
      CALL COLOR(lc)
      CALL GSPLCI(lc)
      call gaseti('LTY',1)  ! use PLCHHQ to draw labels
      call pcseti('FN',21)  ! font 21
      call gsfaci(0)   ! black text
      call gspmci(0)   ! black text
      ns=5
      if(ABS(wsmin-wsmax).LT.0.1) then
        wminp=wsmin-2.
        wmaxp=wsmax+2.
      else
        call scale1(wsmin, wsmax, ns, wminp, wmaxp, dw, ierr)
      endif
      XMIN = wminp
      XMAX = wmaxp
      FL=0.15
      FR=0.45
      FB=0.15
      FT=0.70
      LL=1      ! linear x,y plot
      UL = XMIN
      UR = XMAX
      UB = YMIN
      UT = YMAX
      mjrx=ns
      mnrx=1
c     print *,'in plotray: calling set: UL,UR,UB,UT=',UL,UR,UB,UT
      call set(FL,FR,FB,FT,UL,UR,UB,UT,LL)
      dU = UT-UB
      call labmod('(I2)','(I5)',0,0,2,2,20,20,0)
      call periml(mjrx,mnrx,mjrz,mnrz)
      call plchhq(UL,UT+0.1*UT,met_dsn,0.025,0.,-1.)
c     write(text,101) ZMETANG
      write(text,101) DEGX
      call plchhq(UR,UT+0.1*UT,text,0.025,0.,-1.)
c     --- label ordinate
      uy = (UT-UB)/2.
      ux = CPUX(50)
      call plchhq(ux,uy,'z(m)',0.02,90.,0.)
c     --- label abscissa
      ux = UL+(UR-UL)/2.
      uy = UB-0.15*dU
      call plchhq(ux,uy,'Uaz(m/s)',0.02,0.,0.)
c     --- plot along azimuth wind 
      call setusv('LW',2000)
      call GSLN(1) ! solid
      call curved(WU,ALTN,kp)
c
      call scale1(Tmin, Tmax, ns, Tminp, Tmaxp, Tw, ierr)
      XMIN = Tminp
      XMAX = Tmaxp
      FL=0.55
      FR=0.85
      FB=0.15
      FT=0.70
      LL=1      ! linear x,y plot
      UL = XMIN
      UR = XMAX
      UB = YMIN
      UT = YMAX
      mjrx=ns
      mnrx=1
c     print *,'in plotray: calling set: UL,UR,UB,UT=',UL,UR,UB,UT
      call set(FL,FR,FB,FT,UL,UR,UB,UT,LL)
      call setusv('LW',1000)
      dU = UT-UB
      call labmod('(I2)','(I5)',0,0,2,2,20,20,0)
      call periml(mjrx,mnrx,mjrz,mnrz)
c     --- label abscissa
      ux = UL+(UR-UL)/2.
      uy = UB-0.15*dU
      call plchhq(ux,uy,'T(C)',0.02,0.,0.)
c     --- plot T(z)
      call GSLN(1) ! solid
      call setusv('LW',2000)
      call curved(TN,ALTN,kp)
      call setusv('LW',1000)
c
      return
      end
C
       SUBROUTINE PLOTOPTS(JCHOICE,JSITE)
C-----------------------------------------------------------------------
C  The NAPS  module has run,  now get the user options for plotting
C  data generated by the model. At present time only NOT ALL THE FEATURES
C  in the code are operable.
c  VARIABLES:
C             CONTRS  - The contour level values
C             NCONTS  - The number of contour levels
C             DGTORD  - The degree to radians conversion factor 0.0174532
C      COMMON/MAPCOR/ - See subroutine BLASTMAP for definitions       
C-----------------------------------------------------------------------
      INTEGER JCHOICE, JSITE
      integer ncdir,nodir,nmdir 
      CHARACTER*60 DATADIR,OUTDIR,METDIR,FILELV,FILEAT,FILMET,FILSUM,
     1  FILOUT,met_dsn
      COMMON /filenm/ DATADIR,OUTDIR,METDIR,FILELV,FILEAT,FILMET,FILSUM,
     1  FILOUT,met_dsn,ncdir,nodir,nmdir
c-----------------------------------------------------------------------
      INTEGER isnd, ICHOICE, ISITE, IOPT, ICONTR, nsnds, nlevels,
     1        DBPLOTOPT
      REAL    PSIMIN, PSIMAX, LATS, LONS, LATB, LONB, input_delpsi,
     1        xminp,xmaxp,yminp,ymaxp
      LOGICAL DOELV, DOATT, white_backgrnd
      CHARACTER*60 snd_label
      COMMON  /INPUTS/ snd_label, ICHOICE, ISITE, IOPT, ICONTR, PSIMIN,
     1        PSIMAX, LATS, LONS, LATB, LONB, input_delpsi, isnd, DOELV,
     2        DOATT, white_backgrnd, xminp,xmaxp,yminp,ymaxp, nsnds,
     3        nlevels, DBPLOTOPT
c-----------------------------------------------------------------------
      REAL    RMAXD, DELRT, DELS, ALTGMX
      INTEGER ATT, ELEV, NPPSPK
      COMMON/WHEELS/ ATT(751,73),ELEV(751,73),RMAXD,NPPSPK,DELRT,DELS,
     &               ALTGMX
c-----------------------------------------------------------------------
      real    A,CONTRS,RMAX,DELPSI,RRATIO
      integer M,N,IRAMAX,NRO,NCONTS
      COMMON /SOUND/ A(751,361),M,N,CONTRS(51),RMAX,DELPSI,IRAMAX,
     &               NRO,NCONTS,RRATIO
c-----------------------------------------------------------------------
      REAL    ALT,T,RH,WN,WE,V,WU,WV,SV,p_mb
      INTEGER NP
      COMMON /WEATHR/ ALT(70),T(70),RH(70),WN(70),WE(70),V(70),WU(70),
     &                WV(70),SV(70),p_mb(70),NP
c-----------------------------------------------------------------------
      INTEGER IGUN
      REAL    XB,YB,ZB,WB,SFPRES,DIRANG,ZBAG
      COMMON /SOURCE/ XB,YB,ZB,WB,SFPRES,DIRANG,IGUN,ZBAG
c-----------------------------------------------------------------------
      real    XFAC,YFAC,DELRES,RXL,RYL,RXH,RYH,X1,Y1,X2,Y2,
     1  UB,UT,UR,UL,TRXB,TRYB
      COMMON /MAPCOR/ XFAC,YFAC,DELRES,RXL,RYL,RXH,RYH,X1,Y1,X2,Y2,
     1  UB,UT,UR,UL,TRXB,TRYB
c-----------------------------------------------------------------------
      CHARACTER METLABEL*80, FILLABEL*80, SITE*10
      COMMON /IDTAG/ METLABEL,FILLABEL,SITE
c-----------------------------------------------------------------------
      integer KI,KT,MT1,MT2,MT3,MT4,MT8
      COMMON /LUNT/ KI,KT,MT1,MT2,MT3,MT4,MT8
c-----------------------------------------------------------------------
      LOGICAL DEFLT
      COMMON /DEFAULT/ DEFLT
c-----------------------------------------------------------------------
      integer ICLORS
      COMMON /CLRMAP/ ICLORS(20)
c-----------------------------------------------------------------------
      REAL CONT(1)
      logical cntfill,chfill
      logical NOGUN
      CHARACTER*20 TEXT
      CHARACTER ANSR*1
C
      DATA DGTORD /0.0174532/
C
C  Define a function to convert degrees to radians for the trig functions.
      COSD(DEG) = COS( DEG * DGTORD )
      SIND(DEG) = SIN( DEG * DGTORD )
c
 1000 FORMAT(A)
 1100 FORMAT(A,$)
c     print *,'in PLOTOPTS: JCHOICE,DBPLOTOPT=',JCHOICE,DBPLOTOPT
c     print *,'in PLOTOPTS: LATS,LONS=',LATS,LONS

c     IF(DEFLT) THEN
c        JCHOICE = 3
c        GO TO 175
c     END IF
C Give the user a menu to pick the plot options.  Various options are
C future possibilities and are not at the present time listed. Only
C the options available are visible to the user.
C
C 100 CALL NEWPAG
C
c     WRITE(KT,*)'         PLOT OPTIONS MENU '
c     WRITE(KT,*)'    '
c     WRITE(KT,*)'   1... RETURN TO MAIN MENU'
c     WRITE(KT,*)'   '
c     WRITE(KT,*)'   2... DRAW BACKGROUND MAP'
c     WRITE(KT,*)'   '
c     WRITE(KT,*)'   3... DISPLAY SOUND PROPAGATION CONTOUR DATA'
c     WRITE(KT,*)'   '
c     WRITE(KT,*)'   4... DRAW SOUND ATTENUATION VS. RANGE '
c     WRITE(KT,*)'   '
c     WRITE(KT,*)'   5... DRAW ELEVATION CONTOUR'
c     WRITE(KT,*)'   '
c     WRITE(KT,*)'  99... STOP TERMINATE PROGRAM'
c     WRITE(KT,*)'    '
 150  CONTINUE
c     WRITE(KT,1100)'  ENTER THE NUMBER OF YOUR CHOICE    '
c     READ(KI,*,ERR=150) JCHOICE
C
C     CALL NEWPAG
C
C  Return to main menu
      IF((JCHOICE.LE.0).OR.(JCHOICE.GT.6)) THEN
        IMAIN = 1
        CLOSE(15)
        CLOSE(16)
        RETURN
      END IF
C
C Plot the sonding data
cmm5  --- MM5 change.  Don't plot soundings
cmm5  IF( JCHOICE .EQ. 1) THEN
c       --- open the file
cmm5    CALL GETSITE(JSITE,NOGUN)
cmm5    ALT(1) = ELEV(1,1)
cmm5    IEL =-1
cmm5    call plotmet2
cmm5  END IF
C
C Plot the background data.
cmm5  --- MM5 change.  Don't plot background map
cmm5  IF( JCHOICE .EQ. 2) THEN
C       CALL NEWPAG
cmm5    CALL REDFIN(RMAX)
cmm5    CALL MPCNVT
cmm5    CALL BLASTMAP
cmm5    CALL BOUNDS          ! APG boundary
cmm5    CALL MIKES(.FALSE.)  ! plot mike locations only
C       CALL IDLABEL(SITE)
C       CALL ENDPLT
C       WRITE(*,*) CHAR(7)
C       READ(KI,1000)ANSR
C       GO TO 100
cmm5  END IF
C
C Plot the dB contours on a background map.
 175  CONTINUE
      IF( JCHOICE .EQ. 3) THEN
c       --- plot dB map according to user defined format
c       --- DBPLOTOPT<=0: default - simple xy contour plot (contours filled)
c       --- DBPLOTOPT=1: contour lines on NAPS background (original)
c       --- DBPLOTOPT=2: filled contour lines on NAPS background
c       --- DBPLOTOPT=3: contour lines on WVS background
c       --- DBPLOTOPT=4: filled contour lines on WVS background
c       --- DBPLOTOPT=5: filled contour lines on WVS filled background
        if(DBPLOTOPT.EQ.1) then
          CALL PLOTDB1
        elseif(DBPLOTOPT.EQ.2) then
          CALL PLOTDB2
        elseif(DBPLOTOPT.EQ.3) then
          cntfill=.false.
          chfill=.false.
          CALL PLOTDB3(cntfill,chfill)
        elseif(DBPLOTOPT.EQ.4) then
          cntfill=.true.
          chfill=.false.
          CALL PLOTDB3(cntfill,chfill)
        elseif(DBPLOTOPT.EQ.5) then
          cntfill=.true.
          chfill=.true.
          CALL PLOTDB3(cntfill,chfill)
        else      ! if(DBPLOTOPT.LE.0) then
          CALL PLOTDBXY
        endif
c       CALL NGWSYM('N',0,XCORD,YCORD,0.020,lc,0)
c     --- MM5 change.  Don't plot mikes
cmm5    CALL MIKES(.TRUE.)  ! overlay mike readings
C       CALL NEWPAG
C       CALL LEVELQ(1)
C       CALL REDFIN(RMAX)
C       CALL MPCNVT
C       CALL BLASTMAP
C       CALL CNTOUR
C       CALL CPLOT2(A,M,N,RMAX,1.0,CONTRS,NCONTS)
C       CALL GETLABEL
C       CALL ENDPLT
C       WRITE(*,*) CHAR(7)
C       READ(KI,1000)ANSR
C       CLOSE(18)
C       GO TO 100
      END IF
C
C  Plot the sound attenuation vs. range at a given azimuth
C     IF( JCHOICE .EQ. 4 ) THEN
C       CALL NEWPAG
C       CALL CALADAT(DELPSI)
C       GO TO 100
C       CLOSE(15)
C     END IF
C
C   For a contour map of elevation data.
      IF( JCHOICE .EQ. 5) THEN
C       CALL NEWPAG
c       --- Open the dB contour/color file. Temporarily use the
c       --- dB contour colors for elevation contours.
        CALL LEVELQ(1)
c       --- Now open the elevation contour file to get the contour
c       --- values.
        CALL LEVELQ(2)
C       CALL NEWPAG
c       CALL REDFIN(RMAXD)
c       CALL MPCNVT
C
C  Open and read the elevation file
        CALL GETSITE(JSITE,NOGUN)
        call setusv('LW',1000)
        CALL BLASTMAP
        do 555 ii = 1,73
          do 556 jj = 1,nppspk
            a(jj,ii) =  float(elev(jj,ii))
  556     continue
  555   continue
        call setusv('LW',2000)
        do IC=1,NCONTS
          CALL GSPLCI(ICLORS(IC))
          CONT(1)=CONTRS(IC)
          NC=1
C         CALL CPLOT2(a,73,NPPSPK,RMAXD,1.0,CONTRS,NCONTS)
          CALL CPLOT2(a,73,NPPSPK,RMAXD,1.0,CONT,NC)
        enddo
        call setusv('LW',1000)
        CALL ELELABEL(SITE,NCONTS,CONTRS)
C#      CALL ENDPLT
C#      WRITE(*,*) CHAR(7)
C#      READ(KI,1000)ANSR
C#      CLOSE(18)
C#      GO TO 100
      END IF
C
C---An options for plotting the land use data file, water points only.
C----Alternately, plot points along spokes.
      IF( JCHOICE .EQ. 6) THEN
        CALL GETSITE(JSITE,NOGUN)
C#      CALL NEWPAG
        CALL REDFIN(RMAXD)
        CALL MPCNVT
        CALL BLASTMAP
        X = RXL
        Y = RYH+.05*(RYH-RYL)
        CALL WTSTR(X,Y,'LAND USE MAP',3,0,-1)
        WRITE(TEXT,188) SITE
 188    FORMAT('SITE: ', A10)
        X = RXL+.45*(RXH-RXL)
        Y = RYH+.05*(RYH-RYL)
        CALL WTSTR(X,Y,TEXT,3,0,-1)
        TRXB = (XB-XFAC)/10.
        TRYB = (YB-YFAC)/10.
        DO 250 L = 1,72
          COSTH = COSD(5.*(L-1))
          SINTH = SIND(5.*(L-1))
          DO 200 M = 2, NPPSPK
            IF( ATT(M,L) .EQ. 1 ) THEN
c           IF(ELEV(M,L) .NE. 0 ) THEN
              XPT = TRXB + M*DELRT/10*COSTH
              YPT = TRYB + M*DELRT/10*SINTH
              if((YPT.ge.RYL).and.(YPT.le.RYH)) then
                CALL COLOR(6) ! light blue
                CALL WTSTR(XPT,YPT,'*',2,0,0)
C#              CALL COLOR(14) 
C#              CALL GSPM(0,2.0,0.0,0)
C#              CALL GPM(1, XPT,YPT)
              endif
c#          END IF
            END IF    
  200     CONTINUE
  250   CONTINUE
C#Require reader to press a key to continue
C#      READ(KI,1000)ANSR
C#      CALL ENDPLT
C#      GO TO 100
      END IF
c
C# Opted to terminate program.
C#    IF( JCHOICE .EQ. 99 ) THEN
C#If DB data was generated, then give option to save the file for future
C#plotting.    Used in debugging not active at present time. Need the PLOT.FOR
C#program.
C#      IF(DEFLT) GO TO 300
C#       IF (IDBFLG .EQ. 1) THEN
C#          CALL NAMFILE
C#          CLOSE (16)
C#       END IF
C300    CLOSE (15)
C#      STOP 'PROGRAM TERMINATED'      
C#    END IF
C#
c#User's choice was an invalid number.
C#
C#    WRITE(KT,*)' INVALID CHOICE, TRY AGAIN. '
C#    WRITE(KT,*)' '
C#    GO TO 100
C       
      RETURN
      END
c
      SUBROUTINE PLOTDB1
C----------------------------------------------------------------------
C  This subroutine calls the necessary routines to plot a file of DB values
C  that were collected on a previous run ( or the average/maximum  of several
C  runs)  of NAPS.  This archived file was named "DECIBEL.DAT"  by default 
C  or it was renamed by the user to a more meaningful name and stored for 
C  future  plotting or averaging.
c  Plotting is accomplished by by contouring radials directly.  Background
c  is in UTMs supplied with NAPS 5.1.
C----------------------------------------------------------------------
c-----------------------------------------------------------------------
      INTEGER isnd, ICHOICE, ISITE, IOPT, ICONTR, nsnds, nlevels,
     1        DBPLOTOPT
      REAL    PSIMIN, PSIMAX, LATS, LONS, LATB, LONB, input_delpsi,
     1        xminp,xmaxp,yminp,ymaxp
      LOGICAL DOELV, DOATT, white_backgrnd
      CHARACTER*60 snd_label
      COMMON  /INPUTS/ snd_label, ICHOICE, ISITE, IOPT, ICONTR, PSIMIN,
     1        PSIMAX, LATS, LONS, LATB, LONB, input_delpsi, isnd, DOELV,
     2        DOATT, white_backgrnd, xminp,xmaxp,yminp,ymaxp, nsnds,
     3        nlevels, DBPLOTOPT
c-----------------------------------------------------------------------
      CHARACTER*60 DBFLNAM
      COMMON /DBFILE/ DBFLNAM
      integer KI,KT,MT1,MT2,MT3,MT4,MT8
      COMMON /LUNT/ KI,KT,MT1,MT2,MT3,MT4,MT8
      CHARACTER METLABEL*80, FILLABEL*80, SITE*10
      COMMON /IDTAG/ METLABEL, FILLABEL, SITE
c-----------------------------------------------------------------------
      real    A,CONTRS,RMAX,DELPSI,RRATIO
      integer M,N,IRAMAX,NRO,NCONTS
      COMMON /SOUND/ A(751,361),M,N,CONTRS(51),RMAX,DELPSI,IRAMAX,
     &               NRO,NCONTS,RRATIO
c-----------------------------------------------------------------------
      INTEGER IGUN
      REAL    XB,YB,ZB,WB,SFPRES,DIRANG,ZBAG
      COMMON /SOURCE/ XB,YB,ZB,WB,SFPRES,DIRANG,IGUN,ZBAG
c-----------------------------------------------------------------------
      real    XFAC,YFAC,DELRES,RXL,RYL,RXH,RYH,X1,Y1,X2,Y2,
     1  UB,UT,UR,UL,TRXB,TRYB
      COMMON /MAPCOR/ XFAC,YFAC,DELRES,RXL,RYL,RXH,RYH,X1,Y1,X2,Y2,
     1  UB,UT,UR,UL,TRXB,TRYB
c-----------------------------------------------------------------------
      integer ndelsi, nranges, nray
      real    Aza, rangea, dBa, xray, yray
      common /savea/  ndelsi, nranges(361), Aza(361), rangea(751,361),
     1                dBa(751,361), xray(751,101), yray(751,101), nray
      COMMON /FLAGS/ DBFLG
c-----------------------------------------------------------------------
      integer ICLORS
      COMMON /CLRMAP/ ICLORS(20)
c-----------------------------------------------------------------------
      integer ncdir,nodir,nmdir 
      CHARACTER*60 DATADIR,OUTDIR,METDIR,FILELV,FILEAT,FILMET,FILSUM,
     1  FILOUT,met_dsn
      COMMON /filenm/ DATADIR,OUTDIR,METDIR,FILELV,FILEAT,FILMET,FILSUM,
     1  FILOUT,met_dsn,ncdir,nodir,nmdir
      LOGICAL DBFLG
      real CONT(1)
c
      XFAC =  200000.     ! UTM adjustments for the plotting routines.
      YFAC = 4300000.
c     print *,'in PLOTDB1'
c     print*,'XFAC,YFAC,DELRES,RXL,RYL,RXH,RYH,UB,UT,UR,UL,TRXB,TRYB=',
c    1        XFAC,YFAC,DELRES,RXL,RYL,RXH,RYH,UB,UT,UR,UL,TRXB,TRYB
 1000 FORMAT (A)
 1100 FORMAT (A,$)
 200  CONTINUE
C     Get the data not already available when an archived file is requested.
      OPEN(UNIT=15,FILE=DBFLNAM,FORM='FORMATTED',STATUS='OLD',
     1 iostat=ios)
      if(ios.ne.0) then
        write(kt,*) 'error opening input decibel file:',DBFLNAM
        stop
      endif
      READ(15,1000) FILLABEL
      READ(15,1000) METLABEL
      SITE= METLABEL(1:10)
      READ(15,*)XB,YB,ZBAG,WB,RMAX,DELPSI,DELRES
      REWIND(15)
c     print *,'XB,YB,ZBAG,WB,RMAX,DELPSI,DELRES=',
c    1         XB,YB,ZBAG,WB,RMAX,DELPSI,DELRES
c
      CALL LEVELQ(1)
      CALL REDFIN(RMAX)
      CALL MPCNVT
      call setusv('LW',1000)
      if(white_backgrnd) then
c       --- flush the background white
        call wflush
      endif
      CALL BLASTMAP
      CALL CNTOUR
      do IC=1,NCONTS
        CALL GSPLCI(ICLORS(IC))
        call setusv('LW',2000)
        CONT(1)=CONTRS(IC)
        NC=1
        CALL CPLOT2(A,M,N,RMAX,1.0,CONT,NC)
      enddo
      call setusv('LW',1000)
      CALL GETLABEL
      REWIND(15)
      DBFLG = .FALSE.
cmm5
c     --- Place a mark at the blast site.
      if(white_backgrnd) then
        lc=0
      else
        lc=1
      endif
      CALL COLOR(lc)
      call setusv('LW',2000)
      TRXB = (XB-XFAC)/10.
      TRYB = (YB-YFAC)/10.
      call wtstr(TRXB,TRYB,'+',2,0,0)
c     --- Place a mark at the sonding location
      call latlon_to_utm(LATS,LONS,XS,YS,izone,0,0)  ! 0,0=sphere,forward
      XSS = (XS-XFAC)/10.
      YSS = (YS-YFAC)/10.
c     --- plot as an open circle
c     icolor=4  ! blue-green
c     icolor=2  ! red
c     icolor=lc ! black
c     CALL NGWSYM('N',9,XSS,YSS,0.015,icolor,0)
      CALL COLOR(lc)
      call wtstr(XSS,YSS,'*',3,0,0)
c
      RETURN
      END
C
      SUBROUTINE GETLABEL
C-----------------------------------------------------------------------
C  FUNCTION: This routine will put the color legend and the ID title on
C            the finished decibel contour graph.
C
C-----------------------------------------------------------------------
      INTEGER IGUN
      REAL    XB,YB,ZB,WB,SFPRES,DIRANG,ZBAG
      COMMON /SOURCE/ XB,YB,ZB,WB,SFPRES,DIRANG,IGUN,ZBAG
c-----------------------------------------------------------------------
      CHARACTER METLABEL*80, FILLABEL*80, SITE*10
      COMMON /IDTAG/ METLABEL,FILLABEL,SITE
c-----------------------------------------------------------------------
      integer ncdir,nodir,nmdir 
      CHARACTER*60 DATADIR,OUTDIR,METDIR,FILELV,FILEAT,FILMET,FILSUM,
     1  FILOUT,met_dsn
c-----------------------------------------------------------------------
      COMMON /filenm/ DATADIR,OUTDIR,METDIR,FILELV,FILEAT,FILMET,FILSUM,
     1  FILOUT,met_dsn,ncdir,nodir,nmdir
c-----------------------------------------------------------------------
      real    A,CONTRS,RMAX,DELPSI,RRATIO
      integer M,N,IRAMAX,NRO,NCONTS
      COMMON /SOUND/ A(751,361),M,N,CONTRS(51),RMAX,DELPSI,IRAMAX,
     &               NRO,NCONTS,RRATIO
c-----------------------------------------------------------------------
      real    XFAC,YFAC,DELRES,RXL,RYL,RXH,RYH,X1,Y1,X2,Y2,
     1  UB,UT,UR,UL,TRXB,TRYB
      COMMON /MAPCOR/ XFAC,YFAC,DELRES,RXL,RYL,RXH,RYH,X1,Y1,X2,Y2,
     1  UB,UT,UR,UL,TRXB,TRYB
c-----------------------------------------------------------------------
      integer ICLORS
      COMMON /CLRMAP/ ICLORS(20)
c-----------------------------------------------------------------------
      INTEGER isnd, ICHOICE, ISITE, IOPT, ICONTR, nsnds, nlevels,
     1        DBPLOTOPT
      REAL    PSIMIN, PSIMAX, LATS, LONS, LATB, LONB, input_delpsi,
     1        xminp,xmaxp,yminp,ymaxp
      LOGICAL DOELV, DOATT, white_backgrnd
      CHARACTER*60 snd_label
      COMMON  /INPUTS/ snd_label, ICHOICE, ISITE, IOPT, ICONTR, PSIMIN,
     1        PSIMAX, LATS, LONS, LATB, LONB, input_delpsi, isnd, DOELV,
     2        DOATT, white_backgrnd, xminp,xmaxp,yminp,ymaxp, nsnds,
     3        nlevels, DBPLOTOPT
c-----------------------------------------------------------------------
c
      CHARACTER*25 TEXT
c
      if(white_backgrnd) then
        CALL COLOR(0)    ! black text
        call gsfaci(0)   ! black text (PLCHHQ calls)
      else
        CALL COLOR(1)    ! white text
        call gsfaci(1)   ! white text
      endif
c
      call pcseti('FN',21)  ! font 21
      idy = 40
C Place a legend on the graph.
      x = cpux(50)
      iy1=980
      iy= iy1
      y = cpuy(iy)
      call plchhq(x,y,FILLABEL,0.018,0.,-1.)
      write(text,230) ZBAG
  230 FORMAT('BLAST HT =',F5.0)
      iy=iy-idy
      y = cpuy(iy)
      call plchhq(x,y,TEXT,0.017,0.,-1.)
      write(text,240) WB
  240 FORMAT('BLAST WT =',F5.0)
      iy=iy-idy
      y = cpuy(iy)
      call plchhq(x,y,TEXT,0.017,0.,-1.)
c
      x = cpux(600)
      iy= iy1-idy
      y = cpuy(iy)
      write(text,250) SITE
 250  FORMAT('SITE = ',A10)
      call plchhq(x,y,TEXT,0.017,0.,-1.)
      if((IGUN.ge.1).and.(IGUN.le.6)) then
        write(text,261) IGUN
  261   format('IGUN =',I2)
      else
        write(text,260) IGUN
  260   format('IGUN =',I2,' (uniform blast)')
      endif
      iy=iy-idy
      y = cpuy(iy)
      call plchhq(x,y,TEXT,0.017,0.,-1.)
cmm5  --- Place sounding grid point location in lower rt corner
      x = cpux(550)
      y = RYL-.04*(RYH-RYL)
      call plchhq(x,y,snd_label,0.013,0.,-1.)
c
c Cycle over the contour using different colors for each curve.
      do i=1,NCONTS
         CALL COLOR(ICLORS(i))
         write(text,203) NINT(contrs(i))
         x = RXL+0.73*(RXH-RXL)
         y = RYL+((0.02+0.3*i)/10.)*(RYH-RYL)
         call wtstr(x,y,text,2,0,-1)
      enddo
 203  FORMAT('      ',I4,' dB' )
c
      RETURN
      END
c
      SUBROUTINE ELELABEL(SITE,NCONTS,CONTRS)
C*********************************************************************
C  This routine titles and puts a legend on the elevation contour plot.
c*********************************************************************
c
c-----------------------------------------------------------------------
      INTEGER isnd, ICHOICE, ISITE, IOPT, ICONTR, nsnds, nlevels,
     1        DBPLOTOPT
      REAL    PSIMIN, PSIMAX, LATS, LONS, LATB, LONB, input_delpsi,
     1        xminp,xmaxp,yminp,ymaxp
      LOGICAL DOELV, DOATT, white_backgrnd
      CHARACTER*60 snd_label
      COMMON  /INPUTS/ snd_label, ICHOICE, ISITE, IOPT, ICONTR, PSIMIN,
     1        PSIMAX, LATS, LONS, LATB, LONB, input_delpsi, isnd, DOELV,
     2        DOATT, white_backgrnd, xminp,xmaxp,yminp,ymaxp, nsnds,
     3        nlevels, DBPLOTOPT
c-----------------------------------------------------------------------
      COMMON /MAPCOR/ XFAC,YFAC,DELRES,RXL,RYL,RXH,RYH,X1,Y1,X2,Y2,
     1  UB,UT,UR,UL,TRXB,TRYB
c-----------------------------------------------------------------------
      integer ICLORS
      COMMON /CLRMAP/ ICLORS(20)
c-----------------------------------------------------------------------
      CHARACTER*20 TEXT,TXT1
      CHARACTER*10 SITE
      DIMENSION CONTRS(51)
      I   = NCONTS

C Place a legend on the graph.
C#    CALL CJUST(-1.0,0.0)
C#    CALL WORLD(0.0,10.0,0.0,10.0)
C#    CALL VUPORT(0.0,1.0,0.0,1.0)
C#    CALL CSIZE(.75,0.0)
c Cycle over the contour using different colors for each curve.
      DO 1234 I=1,NCONTS
         CALL COLOR(ICLORS(I))
         WRITE(TEXT,203) CONTRS(I)
 203  FORMAT('      ',F5.1,' M' )
c        CALL GTX(7.9,0.2+0.2*i,TEXT)
         X = RXL+(8.5/10.)*(RXH-RXL)
         Y = RYL+((0.2+0.3*i)/10.)*(RYH-RYL)
         CALL WTSTR(X,Y,TEXT,2,0,0)
 1234 CONTINUE
      if(white_backgrnd) then
        CALL COLOR(0)    ! black text
      else
        CALL COLOR(1)    ! white text
      endif
C
      TXT1= 'ELEVATION PLOT    '
      WRITE(TEXT,200)TXT1
 200  FORMAT(A20)
      X = RXL
      Y = RYH+.05*(RYH-RYL)
      CALL WTSTR(X,Y,TEXT,3,0,-1)
C     CALL GTX(8.1,7.4,TEXT )
C
      WRITE(TEXT,250) SITE
 250  FORMAT('SITE: ', A10)
      X = RXL+.45*(RXH-RXL)
      Y = RYH+.05*(RYH-RYL)
      CALL WTSTR(X,Y,TEXT,3,0,-1)
C     CALL GTX(8.1,7.2,TEXT)

      RETURN
      END
c
      SUBROUTINE REDFIN(RMAX)
C-----------------------------------------------------------------------
C
C  FUNCTION: This routine will re-initialize the window boundaries in UTM's.
C
C-----------------------------------------------------------------------
      INTEGER IGUN
      REAL    XB,YB,ZB,WB,SFPRES,DIRANG,ZBAG
      COMMON /SOURCE/ XB,YB,ZB,WB,SFPRES,DIRANG,IGUN,ZBAG
c-----------------------------------------------------------------------
      real    XFAC,YFAC,DELRES,RXL,RYL,RXH,RYH,X1,Y1,X2,Y2,
     1  UB,UT,UR,UL,TRXB,TRYB
      COMMON /MAPCOR/ XFAC,YFAC,DELRES,RXL,RYL,RXH,RYH,X1,Y1,X2,Y2,
     1  UB,UT,UR,UL,TRXB,TRYB
c-----------------------------------------------------------------------
      RXL = XB - RMAX
      RYH = YB + RMAX
      RXH = XB + RMAX
      RYL = YB - RMAX
      RETURN
      END
C
      SUBROUTINE LEVELQ(itype)
C-----------------------------------------------------------------------
C   Function: This subroutine gives the user the option of changing
C             the contour levels.  Enter the desired levels or accept
C             the present default values stored in the file "DBcolor.DAT"
C   Version 5.0 took this option out of the users domain and the levels
C             and/or colors can be change by editing the file.
c
C-----------------------------------------------------------------------
      integer itype
c-----------------------------------------------------------------------
      real    A,CONTRS,RMAX,DELPSI,RRATIO
      integer M,N,IRAMAX,NRO,NCONTS
      COMMON /SOUND/ A(751,361),M,N,CONTRS(51),RMAX,DELPSI,IRAMAX,
     &               NRO,NCONTS,RRATIO
c-----------------------------------------------------------------------
      integer ncdir,nodir,nmdir 
      CHARACTER*60 DATADIR,OUTDIR,METDIR,FILELV,FILEAT,FILMET,FILSUM,
     1  FILOUT,met_dsn
      COMMON /filenm/ DATADIR,OUTDIR,METDIR,FILELV,FILEAT,FILMET,FILSUM,
     1  FILOUT,met_dsn,ncdir,nodir,nmdir
      integer KI,KT,MT1,MT2,MT3,MT4,MT8
      COMMON /LUNT/ KI,KT,MT1,MT2,MT3,MT4,MT8
c-----------------------------------------------------------------------
      integer ICLORS
      COMMON /CLRMAP/ ICLORS(20)
c-----------------------------------------------------------------------
      CHARACTER*60 DUMMY
      data iclors/1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1/ ! default=white
 1100 FORMAT (A)
 2000 FORMAT (A,$)
C
  10  CONTINUE
C Check to see which contour the user requested.
C If itype = 1  open a DB contour file, If itype =2 open elevation contour.
      IF( ITYPE .EQ. 1) THEN
        IUNIT = 18
C  Open the file containing the default DB levels. 
C
        open (unit=18, file=datadir(1:ncdir)//'dbcolor.dat',
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
         GO TO 150
      ELSE
         IUNIT = 19
         WRITE(KT,*)'   '
         WRITE(KT,*)'  '
         WRITE(KT,*)' PROGRAM PLOTS AN ELEVATION CONTOUR MAP CENTERED'
         WRITE(KT,*)' AROUND THE BLAST SITE. YOU MAY USE DEFAULT LEVELS'
         WRITE(KT,*)' OR ASSIGN NEW LEVELS. '

C  Open the file containing the default DB levels.  This file is constantly
C  updated to always store the last DB levels used.
C
         OPEN (UNIT=19, FILE=datadir(1:ncdir)//'elevctr.dat',
     &                FORM = 'FORMATTED',STATUS = 'UNKNOWN',ERR= 900)
      END IF
C Read in the default levels.
      READ(IUNIT,*,ERR=900) NCONTS, ( CONTRS(J), J=1,NCONTS)
      GO TO 20
 900  WRITE(KT,*)' SORRY, NO DEFAULT LEVELS AVAILABLE'
C     CALL LVDEFINE
      GO TO 100
C
 20   CONTINUE
c     WRITE(KT,1000) (CONTRS(J),J = 1,NCONTS )
c     WRITE(KT,*)'  '
c     WRITE(KT,*)' 1... USE DEFAULT  LEVELS'
c     WRITE(KT,*)' 2... ASSIGN NEW  LEVELS FOR THE CONTOUR PLOT. '
c     WRITE(KT,2000)'    PLEASE ENTER YOUR CHOICE. (1 OR 2)   '
c     READ(KI,*,ERR=20)IANS
c     IF ( IANS. EQ. 2)  CALL LVDEFINE
c
c1000 FORMAT( 2X,'THE PRESENT DEFAULT LEVELS ARE: ',(/,20X,6(2X,F5.1)))
  100 REWIND (IUNIT)
C
C  Re-store the current DB levels.
c     WRITE(IUNIT,*) NCONTS, (CONTRS(I),I= 1,NCONTS)
 150  CONTINUE
      CLOSE (IUNIT)
c
      RETURN
      END
C
      SUBROUTINE MPCNVT
C-----------------------------------------------------------------------
C
C  FUNCTION: The data points for the maps are in a pseudo UTM coordinate
C             system.  This routine makes the transformation to UTMS as
C             they are commonly defined and then scales by factor of 10.
C  VARIABLES:
C             RXL,RXH - THE MIN AND MAX X VALUES OF THE MAP WINDOW
C             RYL,RYH - THE MIN AND MAX Y VALUES OF THE MAP WINDOW
C
C     These values are defined by the sum and differnce of the blast
C     location (XB,YB) and the maximum range of the blast (RMAX).
C
C-------------------------------------------------------------------------
c-----------------------------------------------------------------------
      real    XFAC,YFAC,DELRES,RXL,RYL,RXH,RYH,X1,Y1,X2,Y2,
     1  UB,UT,UR,UL,TRXB,TRYB
      COMMON /MAPCOR/ XFAC,YFAC,DELRES,RXL,RYL,RXH,RYH,X1,Y1,X2,Y2,
     1  UB,UT,UR,UL,TRXB,TRYB
c-----------------------------------------------------------------------
C
      RXL = (RXL - XFAC) / 10.
      RYH = (RYH - YFAC) / 10.
      RXH = (RXH - XFAC) / 10.
      RYL = (RYL - YFAC) / 10.
      RETURN
      END
c
      SUBROUTINE BLASTMAP
C------------------------------------------------------------------------
C
C  FUNCTION: BLASTMAP originated from a program TEKGRS written
C            by RCLAIR to display the Baltimore and Wilmington area.
C    *****   The above statement no longer holds for the PC version.
C            The original routines have been modified drastically and
C            the original data base was changed to accomodate the new
C            graphics used to plot the maps.
C            FILES : STORED IN ARRAY "IN"
c              UNIT  9 - Aberdeen outline
C              UNIT 10 - City locations and names.
C              UNIT 11 - Water area perimeter data
C              UNIT 12 - Political markings data
C              UNIT 13 - Mike sites data
C-------------------------------------------------------------------------
c-----------------------------------------------------------------------
      INTEGER isnd, ICHOICE, ISITE, IOPT, ICONTR, nsnds, nlevels,
     1        DBPLOTOPT
      REAL    PSIMIN, PSIMAX, LATS, LONS, LATB, LONB, input_delpsi,
     1        xminp,xmaxp,yminp,ymaxp
      LOGICAL DOELV, DOATT, white_backgrnd
      CHARACTER*60 snd_label
      COMMON  /INPUTS/ snd_label, ICHOICE, ISITE, IOPT, ICONTR, PSIMIN,
     1        PSIMAX, LATS, LONS, LATB, LONB, input_delpsi, isnd, DOELV,
     2        DOATT, white_backgrnd, xminp,xmaxp,yminp,ymaxp, nsnds,
     3        nlevels, DBPLOTOPT
c-----------------------------------------------------------------------
      INTEGER IGUN
      REAL    XB,YB,ZB,WB,SFPRES,DIRANG,ZBAG
      COMMON /SOURCE/XB,YB,ZB,WB,SFPRES,DIRANG,IGUN,ZBAG
c-----------------------------------------------------------------------
      CHARACTER METLABEL*80, FILLABEL*80, SITE*10
      COMMON /IDTAG/METLABEL,FILLABEL,SITE
c-----------------------------------------------------------------------
      real    XFAC,YFAC,DELRES,RXL,RYL,RXH,RYH,X1,Y1,X2,Y2,
     1  UB,UT,UR,UL,TRXB,TRYB
      COMMON /MAPCOR/ XFAC,YFAC,DELRES,RXL,RYL,RXH,RYH,X1,Y1,X2,Y2,
     1  UB,UT,UR,UL,TRXB,TRYB
c-----------------------------------------------------------------------
      integer ncdir,nodir,nmdir 
      CHARACTER*60 DATADIR,OUTDIR,METDIR,FILELV,FILEAT,FILMET,FILSUM,
     1  FILOUT,met_dsn
      COMMON /filenm/ DATADIR,OUTDIR,METDIR,FILELV,FILEAT,FILMET,FILSUM,
     1  FILOUT,met_dsn,ncdir,nodir,nmdir
C     
C Set a new hi for the XH value to extend the map to the right.
C Set a new low for the YL value to match the lowest Y data points available.
cold  RXL  =  15200.
      RXL  =  15000.
      RXH  =  25000.
cold  RYL  =   1706.
      RYL  =   1700.
cold  RYH  =   7500.
      RYH  =   9700.
      XFAC = 200000.
      YFAC =4300000.
C
C Adjust the blast coordinates to the commonly define UTM's for plotting.
      TRXB = (XB-XFAC)/10.
      TRYB = (YB-YFAC)/10.
C
C Initialize the plot routines
c     CALL INITPLT
c     CALL NEWPLT

C Initalize the screen for the data plots
c     CALL WORLD(RXL,RXH,RYL,RYH)
      DX = RXH-RXL
      DY = RYH-RYL
      SIZE = AMAX1(DX,DY/0.75)
      DX = DX/SIZE
      DY = DY/SIZE
c###############################
c#    CALL VUPORT(0.0,DX,0.0,DY)
c#    CALL GFRAME(0)
c#    CALL GSFA(-1,1.0,0.0,1)
c#    --- NCAR graphics calls
      X1=0.075
      Y1=0.075
      X2=(1.-X1)
      DX=X2-X1
      DY=0.8*DX
      Y2=Y1+DY
c     Y2=(1.-Y1)
      UL=RXL
      UR=RXH
      UB=RYL
      UT=RYH
      call set(X1,X2,Y1,Y2,UL,UR,UB,UT,1)
      call GSLN(1) ! solid lines
      if(white_backgrnd) then
        CALL COLOR(0)    ! black text
        CALL GSPLCI(0)   ! black lines
      else
        CALL COLOR(1)    ! white text
        CALL GSPLCI(1)   ! white lines
      endif
c     call perim(1,0,1,0)  ! no ticks
      call perim(20,0,16,0) ! ticks every 5 km
c###############################
C
C  Open all the data files necessary for plotting the boundary map and
C  all cities used as reference points.

      OPEN( UNIT = 11, FILE=DATADIR(1:ncdir)//'water.dat',
     *      STATUS = 'OLD', FORM = 'FORMATTED', iostat=ios)
      if(ios.ne.0) then
        write(kt,*) 'error opening input water file'
        stop
      endif
      OPEN( UNIT = 12, FILE =DATADIR(1:ncdir)//'politic.dat',
     *      STATUS = 'OLD', FORM = 'FORMATTED', iostat=ios)
      if(ios.ne.0) then
        write(kt,*) 'error opening input politic file'
        stop
      endif

C Call a routine to read in map files and draw the maps
      CALL MERGE
c
      RETURN
      END
c
      SUBROUTINE MERGE
C---------------------------------------------------------------------------
C
C  FUNCTION: To read in, adjust if need be, and then display all
C            background material of the APG area.
C
C---------------------------------------------------------------------------
c-----------------------------------------------------------------------
      INTEGER isnd, ICHOICE, ISITE, IOPT, ICONTR, nsnds, nlevels,
     1        DBPLOTOPT
      REAL    PSIMIN, PSIMAX, LATS, LONS, LATB, LONB, input_delpsi,
     1        xminp,xmaxp,yminp,ymaxp
      LOGICAL DOELV, DOATT, white_backgrnd
      CHARACTER*60 snd_label
      COMMON  /INPUTS/ snd_label, ICHOICE, ISITE, IOPT, ICONTR, PSIMIN,
     1        PSIMAX, LATS, LONS, LATB, LONB, input_delpsi, isnd, DOELV,
     2        DOATT, white_backgrnd, xminp,xmaxp,yminp,ymaxp, nsnds,
     3        nlevels, DBPLOTOPT
c-----------------------------------------------------------------------
      DIMENSION IN(2)
      IN(1) = 11                 
      IN(2) = 12                 
      REWIND 11
      REWIND 12
      if(white_backgrnd) then
        CALL COLOR(0)    ! black text
        CALL GSPLCI(0)   ! black lines
      else
        CALL COLOR(1)    ! white text
        CALL GSPLCI(1)   ! white lines
      endif
C Draw the background map.
      DO 10 I=1,2
         CALL HEADIN(IN(I))
         CALL DATAIN(IN(I))
         CALL DRMAP
C#       CALL COLOR(15)
  10  CONTINUE
c
C Draw in the cities and APG boundary.
      CALL CITIES
C#    CALL COLOR(8)
      CALL BOUNDS
      call refscale
C#    CALL COLOR(15)
      CALL MIKES(.false.)
      RETURN
      END
C
C  The following subroutines were taken from the GIRAS program supplies to
C  read the USGS Land Use and Land Cover digital files: HEADIN,DATAIN, DRMAP
C  DRARC,IMARC,SWAP, FLIPXY.
C
      SUBROUTINE DRMAP
C-----------------------------------------------------------------------
C  FUNCTION:   Draw all arcs on map
C
C-----------------------------------------------------------------------
      IMPLICIT INTEGER(A-Z)
      INTEGER NAS,NCS, AID,PLC
      COMMON /SHED/ NAS,NCS
      COMMON /ARCREC/ AID(150),PLC(150)
c
      DO 200 I=1,NAS
        K=AID(I)
        CALL DRARC(K)
  200 CONTINUE
      RETURN
      END
c
      SUBROUTINE DRARC(K)
C-----------------------------------------------------------------------
C  FUNCTION:  DRAW ARC K
C
C-----------------------------------------------------------------------
      DIMENSION X(3000),Y(3000)
C
      L=IABS(K)
      CALL IMARC(L,NC,X,Y)
      IF (K.GT.0) GOTO 300
      CALL FLIPXY(NC,X,Y)
  300 CONTINUE
      CALL GPL(NC,X,Y)
      RETURN
      END
C
      SUBROUTINE IMARC(L,NC,X,Y)
C-----------------------------------------------------------------------
C  FUNCTION:  GET IMAGE DATA FOR ARC L
C
C-----------------------------------------------------------------------
      INTEGER START,END, AID,PLC, COORD
      COMMON /ARCREC/ AID(150),PLC(150)
      COMMON/CORD/COORD(6000)
      DIMENSION X(3000),Y(3000)
C
      K=0
      START=2
      IF (L.GT.1) START=PLC(L-1)+2
      END=PLC(L)
      DO 200 I=START,END,2
        J=I-1
        K=K+1
        X(K)=COORD(J)
        Y(K)=COORD(I)
  200 CONTINUE
      NC=K
      RETURN
      END
C
      SUBROUTINE HEADIN (IN)
C----------------------------------------------------------------------
C  FUNCTION: This subroutine reads header material which is not needed.
C            Data retained in file as a ID element if needed.
C----------------------------------------------------------------------
      CHARACTER*60 TITLE
C
C  Read statements: lines 1 thru 5, the header information---not used.
C
      DO 100 I = 1,4
        READ  (IN,*) IDUMMY
 100  CONTINUE
      READ  (IN,'(A60)') TITLE
      RETURN
      END
C
      SUBROUTINE DATAIN (IN)
C-----------------------------------------------------------------------
C  FUNCTION:  Read GIRAS  data from each section placing
C             in common.
C-----------------------------------------------------------------------
      INTEGER COORD,SEC,NAS,NCS,AID,PLC
      COMMON /SHED/  NAS,NCS
      COMMON /ARCREC/ AID(150),PLC(150)
      COMMON /CORD/ COORD(6000)
C
C Read in line six of the data base.

      READ  (IN,1000) SEC,NAS,NCS
 1000 FORMAT(3I5)

C  Reads in all arc records and places them in labeled common /ARCREC/
C  Read in lines 7 thru NAS.
C
      DO 10 I = 1, NAS
       READ (IN,2000) AID(I),PLC(I)
 2000 FORMAT(2I5)
   10 CONTINUE
C
C  Reads in all coordinates of a section and places them in labeled
C  COMMON /CORD/
C
C  Read in the array of coordinates.
      READ (IN,3000) (COORD(J),J=1,NCS)
 3000 FORMAT(16I5)
      RETURN
      END
c
      SUBROUTINE CITIES
C-------------------------------------------------------------------
C
C  FUNCTION:  This subroutine reads in the city name  and the
C             city location and prints the name above the mark..
C  VARIABLES:
C             CITYX,CITYY -  the "adjusted" UTMs - that mark the location
C                            of the cities.
C             CITNAM      - string name of the city
C-------------------------------------------------------------------
      INTEGER KI,KT,MT1,MT2,MT3,MT4,MT8
      COMMON /LUNT/ KI,KT,MT1,MT2,MT3,MT4,MT8
c-----------------------------------------------------------------------
      INTEGER isnd, ICHOICE, ISITE, IOPT, ICONTR, nsnds, nlevels,
     1        DBPLOTOPT
      REAL    PSIMIN, PSIMAX, LATS, LONS, LATB, LONB, input_delpsi,
     1        xminp,xmaxp,yminp,ymaxp
      LOGICAL DOELV, DOATT, white_backgrnd
      CHARACTER*60 snd_label
      COMMON  /INPUTS/ snd_label, ICHOICE, ISITE, IOPT, ICONTR, PSIMIN,
     1        PSIMAX, LATS, LONS, LATB, LONB, input_delpsi, isnd, DOELV,
     2        DOATT, white_backgrnd, xminp,xmaxp,yminp,ymaxp, nsnds,
     3        nlevels, DBPLOTOPT
c-----------------------------------------------------------------------
      integer ncdir,nodir,nmdir 
      CHARACTER*60 DATADIR,OUTDIR,METDIR,FILELV,FILEAT,FILMET,FILSUM,
     1  FILOUT,met_dsn
      COMMON /filenm/ DATADIR,OUTDIR,METDIR,FILELV,FILEAT,FILMET,FILSUM,
     1  FILOUT,met_dsn,ncdir,nodir,nmdir
c-----------------------------------------------------------------------
      real    XFAC,YFAC,DELRES,RXL,RYL,RXH,RYH,X1,Y1,X2,Y2,
     1  UB,UT,UR,UL,TRXB,TRYB
      COMMON /MAPCOR/ XFAC,YFAC,DELRES,RXL,RYL,RXH,RYH,X1,Y1,X2,Y2,
     1  UB,UT,UR,UL,TRXB,TRYB
c-----------------------------------------------------------------------
      CHARACTER*9 CITNAM
      CHARACTER*5 TXT
C
c     --- open the city coordinate file
      OPEN( UNIT = 10, FILE =DATADIR(1:ncdir)//'city.dat',
     *      STATUS = 'OLD', FORM = 'FORMATTED', iostat=ios)
      if(ios.ne.0) then
        write(kt,*) 'error opening input city file'
        stop
      endif
      REWIND 10
C   (BCITYX,BCITYY) is the location of the name Baltimore.
      BCITYX=15800.00
      BCITYY= 5041.13
   50 READ(10,1000,END=150) NCHAR,CITNAM,CITYX,CITYY
 1000 FORMAT(I1,2X,A9,2F10.2)
c     print *,'in CITIES: NCHAR,CITNAM,CITYX,CITYY=',
c    1                    NCHAR,CITNAM,CITYX,CITYY
C
C   (TLOCX,TLOCY) is the relative location of the title of
C                     each of the cities.
      TLOCX=  0.0
      TLOCY=  250.00
C
C   We are  determining if the city location
C  lies within the window defined by (RXL,RYL) and (RXH,RYH).
C  If it does not then neither the box nor the tile is written
C  on the screen.  If, however, the city lies within the window
C  but the title begins outside the window we draw the box and
C  adjust the location of the title (TLOCX,TLOCY) so that it
C  begins inside the window.  And if everything is inside the
C  window already, we do nothing.
C
      IF (  (CITYX.LT.RXL).OR.(CITYY.LT.RYL)
     &  .OR.(CITYX.GT.RXH).OR.(CITYY.GT.RYH) ) THEN
           GO TO 50
      ELSE IF ((CITYX.GT.RXL).AND.(CITYX+TLOCX.LT.RXL)
     &    .AND.(CITYY.LT.RYH).AND.(CITYY+TLOCY.GT.RYH)) THEN
           TLOCX= 0.0
           TLOCY=-TLOCY
      ELSE IF ((CITYX.GT.RXL).AND.(CITYX+TLOCX.LT.RXL)) THEN
           TLOCX= 0.0
      ELSE IF ((CITYY.LT.RYH).AND.(CITYY+TLOCY.GT.RYH)) THEN
           TLOCY=-TLOCY
      ENDIF
c     CALL CSIZE(.75,0.0)
C      CALL GSPM(5,.8,0.,0)  !Drop x at the city sites.
C      IF (CITYX.NE.BCITYX.AND.CITYY.NE.BCITYY)
C    1   CALL GPM(1,CITYX,CITYY)
c    1   call wtstr(CITYX,CITYY,'X',2,0,0)

C Change the pattern, write Chestertown name below the mark and Abrdn
C above the mark.
      IF(CITNAM .EQ. 'CHSTOWN') THEN
c        CALL GTX(CITYX,CITYY-250.,CITNAM(1:NCHAR) )
         call wtstr(CITYX,CITYY-250.,CITNAM(1:NCHAR),2,0,0)
      ELSE IF (CITNAM .EQ. 'ABRDN') THEN
c        CALL GTX(CITYX,CITYY+250.,CITNAM(1:NCHAR) )
         call wtstr(CITYX,CITYY+250.,CITNAM(1:NCHAR),2,0,0)
      ELSE
c        CALL GTX(CITYX,CITYY+TLOCY,CITNAM(1:NCHAR) )
         call wtstr(CITYX,CITYY+TLOCY,CITNAM(1:NCHAR),2,0,0)
      END IF
      GO TO 50
C
 150  CONTINUE
C Put the state line label on graph.
      WRITE(TXT,2000)
 2000 FORMAT('MD')
c     CALL GTX(23425.,5000.,TXT)
      call wtstr(23400.,5000.,TXT,3,0,0)
      WRITE(TXT,3000)
 3000 FORMAT('DEL')
c     CALL GTX(24000.,5000.,TXT)
      call wtstr(24500.,5000.,TXT,3,0,0)
      CLOSE(unit=10)
      RETURN
      END
c
      SUBROUTINE BOUNDS
C----------------------------------------------------------------
C
C  FUNCTION: This subroutine draws the boundary of the Aberdeen
C            Proving Grounds and then labels them with APG.
C
C               unit 13 - BOUNDARY.DAT
C  Variables:   ILETER - Number of characters in the title.
C               NCHAR  - Cardinal number of the title character.
C               STRING - The title. (APG)
C               CHARX,CHARY - location of each letter of the title.
C               X(I),Y(I) - points to plot the border of APG
C               NPTS  - Number of points in the border data.
C----------------------------------------------------------------
      INTEGER KI,KT,MT1,MT2,MT3,MT4,MT8
      COMMON /LUNT/ KI,KT,MT1,MT2,MT3,MT4,MT8
c-----------------------------------------------------------------------
      INTEGER isnd, ICHOICE, ISITE, IOPT, ICONTR, nsnds, nlevels,
     1        DBPLOTOPT
      REAL    PSIMIN, PSIMAX, LATS, LONS, LATB, LONB, input_delpsi,
     1        xminp,xmaxp,yminp,ymaxp
      LOGICAL DOELV, DOATT, white_backgrnd
      CHARACTER*60 snd_label
      COMMON  /INPUTS/ snd_label, ICHOICE, ISITE, IOPT, ICONTR, PSIMIN,
     1        PSIMAX, LATS, LONS, LATB, LONB, input_delpsi, isnd, DOELV,
     2        DOATT, white_backgrnd, xminp,xmaxp,yminp,ymaxp, nsnds,
     3        nlevels, DBPLOTOPT
c-----------------------------------------------------------------------
      integer ncdir,nodir,nmdir 
      CHARACTER*60 DATADIR,OUTDIR,METDIR,FILELV,FILEAT,FILMET,FILSUM,
     1  FILOUT,met_dsn
      COMMON /filenm/ DATADIR,OUTDIR,METDIR,FILELV,FILEAT,FILMET,FILSUM,
     1  FILOUT,met_dsn,ncdir,nodir,nmdir
c-----------------------------------------------------------------------
      real    XFAC,YFAC,DELRES,RXL,RYL,RXH,RYH,X1,Y1,X2,Y2,
     1  UB,UT,UR,UL,TRXB,TRYB
      COMMON /MAPCOR/ XFAC,YFAC,DELRES,RXL,RYL,RXH,RYH,X1,Y1,X2,Y2,
     1  UB,UT,UR,UL,TRXB,TRYB
c-----------------------------------------------------------------------
      CHARACTER*5  STRING
      DIMENSION X(20),Y(20)
C
c     --- open the APG boundary file
      OPEN( UNIT = 13,  FILE =DATADIR(1:ncdir)//'boundary.dat',
     *      STATUS = 'OLD', FORM = 'FORMATTED', iostat=ios)
      if(ios.ne.0) then
        write(kt,*) 'error opening input boundary file'
        stop
      endif
      REWIND 13
c
C Read in the title to be placed on the map.
      READ(13,*) ILETER
      DO 50 M=1,ILETER
         READ(13,1000) NCHAR,STRING,CHARX,CHARY
c        CALL GTX(CHARX,CHARY,STRING(1:NCHAR))
         call wtstr(CHARX,CHARY,STRING(1:NCHAR),3,0,0)
   50 CONTINUE
 1000 FORMAT(I1,2X,A5,2F10.2)

C Read in the data points to outline the APG.
   75 DO 80 I=1,20
         READ(13,2000,END=250) CONERX,CONERY
         NPTS=I
         X(I)=CONERX
         Y(I)=CONERY
 80   CONTINUE
 250  CONTINUE
 2000 FORMAT(F8.2,2X,F8.2)
      CLOSE(unit=13)
C
C  Plot the border.
      if(white_backgrnd) then
        lc=0
      else
        lc=1
      endif
      CALL COLOR(lc)    ! white text
      CALL GSPLCI(lc)   ! white lines
      CALL GPL(NPTS,X,Y)
c
      RETURN
      END
c
      SUBROUTINE BOUNDS3
c-----------------------------------------------------------------------
C  FUNCTION: This subroutine draws the boundary of the Aberdeen
C            Proving Grounds and then labels them with APG.
C            xb,yb(npts) - lat,lon points to plot the border of APG
C            npts  - Number of points in the border data.
c-----------------------------------------------------------------------
      implicit none
      integer npts
      parameter (npts=13)
      real    xb(npts),yb(npts)
      real    thick
      integer ltype, lcolor
c     --- APG boundary data per Charles Clough email 13Jul00
c     --- Note last point wraps to first point
      data xb/-76.271,-76.346,-76.365,-76.321,-76.250,
     1        -76.208,-76.160,-76.113,-76.078,-76.061,
     2        -76.050,-76.163,-76.271/                   
      data yb/ 39.275, 39.299, 39.321, 39.401, 39.433,
     1         39.442, 39.503, 39.486, 39.477, 39.470,
     2         39.448, 39.376, 39.275/
c-----------------------------------------------------------------------
c
c     --- Save off current line parameters
c     call GQLN(ltype)     ! line type
c     call GQLWSC(thick)   ! line thickness
c     call GQPLCI(lcolor)  ! line color
c     print *,'in BOUNDS3: ltype,thick,lcolor=',ltype,thick,lcolor
c     --- Set line parameters for the border
      CALL GSLN(2)      ! 2=dashed
      CALL GSLWSC(3.)   ! 3X thickness
      CALL GSPLCI(13)   ! dark blue lines
c     --- Plot the border
      CALL GPL(npts,xb,yb)
c     --- Restore line characteristics
c     CALL GSLN(ltype)
c     CALL GSLWSC(thick)
c     CALL GSPLCI(lcolor)
c
      return
      end
c
      subroutine refscale   
c     --- Plot the scale line at the lower left corner.
c-----------------------------------------------------------------------
      INTEGER isnd, ICHOICE, ISITE, IOPT, ICONTR, nsnds, nlevels,
     1        DBPLOTOPT
      REAL    PSIMIN, PSIMAX, LATS, LONS, LATB, LONB, input_delpsi,
     1        xminp,xmaxp,yminp,ymaxp
      LOGICAL DOELV, DOATT, white_backgrnd
      CHARACTER*60 snd_label
      COMMON  /INPUTS/ snd_label, ICHOICE, ISITE, IOPT, ICONTR, PSIMIN,
     1        PSIMAX, LATS, LONS, LATB, LONB, input_delpsi, isnd, DOELV,
     2        DOATT, white_backgrnd, xminp,xmaxp,yminp,ymaxp, nsnds,
     3        nlevels, DBPLOTOPT
c-----------------------------------------------------------------------
      real    XFAC,YFAC,DELRES,RXL,RYL,RXH,RYH,X1,Y1,X2,Y2,
     1  UB,UT,UR,UL,TRXB,TRYB
      COMMON /MAPCOR/ XFAC,YFAC,DELRES,RXL,RYL,RXH,RYH,X1,Y1,X2,Y2,
     1  UB,UT,UR,UL,TRXB,TRYB
c-----------------------------------------------------------------------
      REAL    Xx(3),Yy(3)
c
      if(white_backgrnd) then
        CALL COLOR(0)    ! black text
        CALL GSPLCI(0)   ! black lines
      else
        CALL COLOR(1)    ! white text
        CALL GSPLCI(1)   ! white lines
      endif
c
C  Define the coordinates for the scale line to represent 5 km (5000/10=500)
      dtic=500.
      Xx(1) = RXL + dtic
      Xx(2) = Xx(1)
      Xx(3) = Xx(1) + dtic
      Yy(2) = RYL + dtic
      Yy(1) = Yy(2) + dtic
      Yy(3) = Yy(2)
      XLAB  = Xx(2) + dtic/4.
      YLAB  = Yy(2) + dtic/2.
C
c   Plot the scale line at the lower left corner.
      CALL GPL(3,Xx,Yy)
C Place tics at end of reference scale,last line read in above.
      do j=1,3
        call wtstr(Xx(j),Yy(j),'+',2,0,0)
      enddo
c Place title at end of the reference lines.
      call wtstr(XLAB,YLAB,'5 km',2,0,-1)
      RETURN
      END
c
      SUBROUTINE SWAP(A,B)
C-----------------------------------------------------------------------
C  FUNCTION: Utility to swap two variables.
C-----------------------------------------------------------------------
      HOLD=A
      A=B
      B=HOLD
      RETURN
      END
c
      SUBROUTINE FLIPXY(NC,X,Y)
C-----------------------------------------------------------------------
C  FUNCTION: Flip order of coordinates in string.
C-----------------------------------------------------------------------
      DIMENSION X(3000),Y(3000)
C
      M=NC/2
      DO 200 I=1,M
        J=NC-I+1
        CALL SWAP(X(I),X(J))
        CALL SWAP(Y(I),Y(J))
  200 CONTINUE
      RETURN
      END
c
      SUBROUTINE MIKES(colorfill)
c----------------------------------------------------------------------
c  This routine will place a small + and a number at the location of 
c  various microphones surrounding APG.
c  Logical input parameter colorfill is used to plot microphone locations
c  only if .FALSE., otherwise will plot only those microphones having
c  data, color coded according to the ICOLRS scheme.
c----------------------------------------------------------------------
      LOGICAL colorfill
      INTEGER KI,KT,MT1,MT2,MT3,MT4,MT8
      COMMON /LUNT/ KI,KT,MT1,MT2,MT3,MT4,MT8
      integer ncdir,nodir,nmdir 
      CHARACTER*60 DATADIR,OUTDIR,METDIR,FILELV,FILEAT,FILMET,FILSUM,
     1  FILOUT,met_dsn
      COMMON /filenm/ DATADIR,OUTDIR,METDIR,FILELV,FILEAT,FILMET,FILSUM,
     1  FILOUT,met_dsn,ncdir,nodir,nmdir
c-----------------------------------------------------------------------
      real    XFAC,YFAC,DELRES,RXL,RYL,RXH,RYH,X1,Y1,X2,Y2,
     1  UB,UT,UR,UL,TRXB,TRYB
      COMMON /MAPCOR/ XFAC,YFAC,DELRES,RXL,RYL,RXH,RYH,X1,Y1,X2,Y2,
     1  UB,UT,UR,UL,TRXB,TRYB
c-----------------------------------------------------------------------
      INTEGER isnd, ICHOICE, ISITE, IOPT, ICONTR, nsnds, nlevels,
     1        DBPLOTOPT
      REAL    PSIMIN, PSIMAX, LATS, LONS, LATB, LONB, input_delpsi,
     1        xminp,xmaxp,yminp,ymaxp
      LOGICAL DOELV, DOATT, white_backgrnd
      CHARACTER*60 snd_label
      COMMON  /INPUTS/ snd_label, ICHOICE, ISITE, IOPT, ICONTR, PSIMIN,
     1        PSIMAX, LATS, LONS, LATB, LONB, input_delpsi, isnd, DOELV,
     2        DOATT, white_backgrnd, xminp,xmaxp,yminp,ymaxp, nsnds,
     3        nlevels, DBPLOTOPT
c-----------------------------------------------------------------------
      real    A,CONTRS,RMAX,DELPSI,RRATIO
      integer M,N,IRAMAX,NRO,NCONTS
      COMMON /SOUND/ A(751,361),M,N,CONTRS(51),RMAX,DELPSI,IRAMAX,
     &               NRO,NCONTS,RRATIO
c-----------------------------------------------------------------------
      integer ICLORS
      COMMON /CLRMAP/ ICLORS(20)
c-----------------------------------------------------------------------
      integer idummy
      real reading  ! dB
      CHARACTER*23 STRDB
      CHARACTER*80 record
c
      OPEN( UNIT = 14, FILE =DATADIR(1:ncdir)//'micro.dat',
c     OPEN( UNIT = 14, FILE =DATADIR(1:ncdir)//'micro_804108.dat',
     *      STATUS = 'OLD', FORM = 'FORMATTED', iostat=ios)
      if(ios.ne.0) then
        write(kt,*) 'error opening input microphone file'
        stop
      endif
 1111 FORMAT (A)
      if(white_backgrnd) then
        lc=0
      else
        lc=1
      endif
      DO 100 I = 1,25
         READ(14,1111,END=150) record
c        print *,record
         CALL shftlft(record)
         N = MAX(INDEX(record,' ')-1,1)
         XCORD=0.
         read(record(1:N), '(E30.0)', IOSTAT = IOS) XCORD
         record = record(N+1: )
         CALL shftlft(record)
         N = MAX(INDEX(record,' ')-1,1)
         YCORD=0.
         read(record(1:N), '(E30.0)', IOSTAT = IOS) YCORD
         record = record(N+1: )
         CALL shftlft(record)
         N = MAX(INDEX(record,' ')-1,1)
         read(record(1:N), '(I30)', IOSTAT = IOS) idummy
         record = record(N+1: )
         CALL shftlft(record)
         N = MAX(INDEX(record,' ')-1,1)
         if(record(N+2:N+2).ne.' ') N=N+INDEX(record(N+2: ),' ')
         if(record(N+2:N+2).ne.' ') N=N+INDEX(record(N+2: ),' ')
         N = MAX(N,1)
         STRDB = record(1:N)
         record = record(N+1: )
         CALL shftlft(record)
         N = MAX(INDEX(record,' ')-1,1)
         reading=0.
         read(record(1:N), '(E30.0)', IOSTAT = IOS) reading
         IF ((XCORD.GE.RXL).AND.(XCORD.LE.RXH)
     &  .AND.(YCORD.GE.RYL).AND.(YCORD.LE.RYH) ) THEN
           if(colorfill) then
c            --- find the appropriate color
             do j=1,NCONTS-1
               if((reading.ge.contrs(j)).and.(reading.lt.contrs(j+1)))
     &         then
                 icolor=j
c                --- fill a circle with the appropriate color
                 CALL NGWSYM('N',8,XCORD,YCORD,0.014,ICLORS(icolor),0)
c                --- overlay an open circle with the background color
                 CALL NGWSYM('N',0,XCORD,YCORD,0.014,lc,0)
                 go to 100
               endif
             enddo
           else
c            --- fill a circle with the background color
             CALL NGWSYM('N',8,XCORD,YCORD,0.012,lc,0)
           endif
         ENDIF
  100 CONTINUE
  150 CONTINUE
      CLOSE(UNIT=14)
      RETURN
      END
c
      SUBROUTINE MIKES3(colorfill)
c----------------------------------------------------------------------
c  This routine will place a box a number at the location of 
c  various microphones surrounding APG.
c  Logical input parameter colorfill is used to plot microphone locations
c  only if .FALSE., otherwise will plot only those microphones having
c  data, color coded according to the ICOLRS scheme.
c----------------------------------------------------------------------
      implicit none
      logical colorfill
c----------------------------------------------------------------------
      INTEGER KI,KT,MT1,MT2,MT3,MT4,MT8
      COMMON /LUNT/ KI,KT,MT1,MT2,MT3,MT4,MT8
c----------------------------------------------------------------------
      integer ncdir,nodir,nmdir 
      CHARACTER*60 DATADIR,OUTDIR,METDIR,FILELV,FILEAT,FILMET,FILSUM,
     1  FILOUT,met_dsn
      COMMON /filenm/ DATADIR,OUTDIR,METDIR,FILELV,FILEAT,FILMET,FILSUM,
     1  FILOUT,met_dsn,ncdir,nodir,nmdir
c-----------------------------------------------------------------------
      INTEGER isnd, ICHOICE, ISITE, IOPT, ICONTR, nsnds, nlevels,
     1        DBPLOTOPT
      REAL    PSIMIN, PSIMAX, LATS, LONS, LATB, LONB, input_delpsi,
     1        xminp,xmaxp,yminp,ymaxp
      LOGICAL DOELV, DOATT, white_backgrnd
      CHARACTER*60 snd_label
      COMMON  /INPUTS/ snd_label, ICHOICE, ISITE, IOPT, ICONTR, PSIMIN,
     1        PSIMAX, LATS, LONS, LATB, LONB, input_delpsi, isnd, DOELV,
     2        DOATT, white_backgrnd, xminp,xmaxp,yminp,ymaxp, nsnds,
     3        nlevels, DBPLOTOPT
c-----------------------------------------------------------------------
      real    XFAC,YFAC,DELRES,RXL,RYL,RXH,RYH,X1,Y1,X2,Y2,
     1  UB,UT,UR,UL,TRXB,TRYB
      COMMON /MAPCOR/ XFAC,YFAC,DELRES,RXL,RYL,RXH,RYH,X1,Y1,X2,Y2,
     1  UB,UT,UR,UL,TRXB,TRYB
c-----------------------------------------------------------------------
      real    A,CONTRS,RMAX,DELPSI,RRATIO
      integer M,N,IRAMAX,NRO,NCONTS
      COMMON /SOUND/ A(751,361),M,N,CONTRS(51),RMAX,DELPSI,IRAMAX,
     &               NRO,NCONTS,RRATIO
c-----------------------------------------------------------------------
      integer ICLORS
      COMMON /CLRMAP/ ICLORS(20)
c-----------------------------------------------------------------------
      integer maxm
      parameter (maxm=25)
      real    latm(maxm), lonm(maxm)
      integer nmike(maxm)
      character*10 namem(maxm)
      real    degs,mins,secs,hchar
      integer i,numm,icolor,ios,j,jrec,lc,nmikes,nrec
      real    reading  ! dB
      real    xcord, ycord, xl, yl
      CHARACTER*80 record
c
c     print *,'entering mikes3'
      OPEN( UNIT = 14, FILE =DATADIR(1:ncdir)//'mikes.dat',
c     OPEN( UNIT = 14, FILE =DATADIR(1:ncdir)//'micro_804108.dat',
     *      STATUS = 'OLD', FORM = 'FORMATTED', iostat=ios)
      if(ios.ne.0) then
        write(kt,*) 'error opening input microphone file'
        stop
      endif
 1111 FORMAT (A)
c
c     --- Read in the data.  Note there are 3 header records
      read(14,1111,END=150) record
      read(14,1111,END=150) record
      read(14,1111,END=150) record
      i=0
      do jrec = 1,maxm
        READ(14,1111,END=100) record
        CALL shftlft(record)
        nrec = MAX(INDEX(record,' ')-1,1)
        numm=0
        read(record(1:nrec), '(I30)', IOSTAT = IOS) numm
        i = i+1
        nmike(i)=numm
        record = record(nrec+1: )
        CALL shftlft(record)
        nrec = MAX(INDEX(record,':')-1,1)
        degs=0.
        read(record(1:nrec), '(E30.0)', IOSTAT = IOS) degs
        record = record(nrec+2: )
        CALL shftlft(record)
        nrec = MAX(INDEX(record,':')-1,1)
        mins=0.
        read(record(1:nrec), '(E30.0)', IOSTAT = IOS) mins
        record = record(nrec+2: )
        CALL shftlft(record)
        nrec = MAX(INDEX(record,' ')-1,1)
        secs=0.
        read(record(1:nrec), '(E30.0)', IOSTAT = IOS) secs
        latm(i)=degs+(mins+(secs/60.))/60.
c
        record = record(nrec+1: )
        CALL shftlft(record)
        nrec = MAX(INDEX(record,':')-1,1)
        degs=0.
        read(record(1:nrec), '(E30.0)', IOSTAT = IOS) degs
        record = record(nrec+2: )
        CALL shftlft(record)
        nrec = MAX(INDEX(record,':')-1,1)
        mins=0.
        read(record(1:nrec), '(E30.0)', IOSTAT = IOS) mins
        record = record(nrec+2: )
        CALL shftlft(record)
        nrec = MAX(INDEX(record,' ')-1,1)
        secs=0.
        read(record(1:nrec), '(E30.0)', IOSTAT = IOS) secs
        lonm(i)=degs+SIGN((mins+(secs/60.))/60.,degs)
c
        record = record(nrec+1: )
        CALL shftlft(record)
        nrec = MAX(INDEX(record,' ')-1,1)
        namem(i)=record(1:nrec)
      enddo
  100 continue
      nmikes=i
      close(unit=14)
c     print *,'nmikes=',nmikes
c     do i=1,nmikes
c       print *,'i,nmike,lat,lon=',i,nmike(i),latm(i),lonm(i)
c     enddo
c
c     --- Plot the microphone locations
      if(white_backgrnd) then
        lc=0
      else
        lc=1
      endif
      do i=1,nmikes
        xcord=lonm(i)
        ycord=latm(i)
        reading=0.
        if(colorfill) then
c         --- find the appropriate color
          do j=1,NCONTS-1
            if((reading.ge.contrs(j)).and.(reading.lt.contrs(j+1)))
     &      then
              icolor=j
c             --- fill a circle with the appropriate color
              CALL NGWSYM('N',8,XCORD,YCORD,0.014,ICLORS(icolor),0)
c             --- overlay an open circle with the background color
              CALL NGWSYM('N',0,XCORD,YCORD,0.014,lc,0)
              go to 150
            endif
          enddo
        else
c         --- fill a circle with the background color
          CALL NGWSYM('N',8,XCORD,YCORD,0.008,lc,0)
c         --- number the location
          record(1:2)='  '
          write(record,230) nmike(i)
  230     FORMAT(I2)
          CALL shftlft(record)
          nrec = 1
          if(nmike(i).ge.10) nrec=2
          hchar=0.015  ! as a fraction of the frame
          yl=ycord+0.5*hchar*(UT-UB) ! 1/2 Char ht up
          if(xcord.LT.-76.3) then
            xl=xcord-hchar*(UR-UL) ! 1 Char ht to right of circle
            call plchhq(xl,yl,record(1:nrec),hchar,0.,+1.)
          else
            xl=xcord+hchar*(UR-UL) ! 1 Char ht to left of circle
            call plchhq(xl,yl,record(1:nrec),hchar,0.,-1.)
          endif
c         print *,'i,nmike(i),x,y=',i,record(1:nrec),xcord,ycord
        endif
      enddo
  150 continue
      return
      end
c
      SUBROUTINE CNTOUR
C---------------------------------------------------------------------
C
C  FUNCTION: CNTOUR reads in the  sound intensity in decibels.
C            Finds the max and min values and then sorts and stores
C            the data for plotting the sound intensity contour map.
C            A(N,M) Array of db values on each spoke (evenly spaced)
C            N number of points on each spoke
c            M number of spokes for this blast.
C---------------------------------------------------------------------
      implicit none
c-----------------------------------------------------------------------
      INTEGER isnd, ICHOICE, ISITE, IOPT, ICONTR, nsnds, nlevels,
     1        DBPLOTOPT
      REAL    PSIMIN, PSIMAX, LATS, LONS, LATB, LONB, input_delpsi,
     1        xminp,xmaxp,yminp,ymaxp
      LOGICAL DOELV, DOATT, white_backgrnd
      CHARACTER*60 snd_label
      COMMON  /INPUTS/ snd_label, ICHOICE, ISITE, IOPT, ICONTR, PSIMIN,
     1        PSIMAX, LATS, LONS, LATB, LONB, input_delpsi, isnd, DOELV,
     2        DOATT, white_backgrnd, xminp,xmaxp,yminp,ymaxp, nsnds,
     3        nlevels, DBPLOTOPT
c-----------------------------------------------------------------------
      integer KI,KT,MT1,MT2,MT3,MT4,MT8
      COMMON /LUNT/ KI,KT,MT1,MT2,MT3,MT4,MT8
c-----------------------------------------------------------------------
      real    A,CONTRS,RMAX,DELPSI,RRATIO
      integer M,N,IRAMAX,NRO,NCONTS
      COMMON /SOUND/ A(751,361),M,N,CONTRS(51),RMAX,DELPSI,IRAMAX,
     &               NRO,NCONTS,RRATIO
c-----------------------------------------------------------------------
      INTEGER IGUN
      REAL    XB,YB,ZB,WB,SFPRES,DIRANG,ZBAG
      COMMON  /SOURCE/ XB,YB,ZB,WB,SFPRES,DIRANG,IGUN,ZBAG
c-----------------------------------------------------------------------
      real    X,Y,RGARRY,DBARRY,ATOTRY
      COMMON /DEDAT/ X(751),Y(361),RGARRY(751),DBARRY(751),ATOTRY(751)
c-----------------------------------------------------------------------
      CHARACTER METLABEL*80, FILLABEL*80, SITE*10
      COMMON /IDTAG/  METLABEL, FILLABEL, SITE
      CHARACTER*60 DBFLNAM
      COMMON /DBFILE/ DBFLNAM
      integer ncdir,nodir,nmdir 
      CHARACTER*60 DATADIR,OUTDIR,METDIR,FILELV,FILEAT,FILMET,FILSUM,
     1  FILOUT,met_dsn
      COMMON /filenm/ DATADIR,OUTDIR,METDIR,FILELV,FILEAT,FILMET,FILSUM,
     1  FILOUT,met_dsn,ncdir,nodir,nmdir
      COMMON /FLAGS/ DBFLG
      LOGICAL DBFLG
      integer ndelsi, nranges, nray
      real    Aza, rangea, dBa, xray, yray
      common /savea/  ndelsi, nranges(361), Aza(361), rangea(751,361),
     1                dBa(751,361), xray(751,101), yray(751,101), nray
      integer i,MM1,ic,ios,ipsi,isort,isortn,j,k
      REAL    DELRNG, SIP, XMIN, XMAX, XN
      REAL*8 PI
      PARAMETER (PI=3.14159265358979323846264338327950D0)
C
C
      CLOSE(15)
C  Open the decibel file to get values to plot the contour levels.
C  If not a default file then get new name.
      if (.NOT. DBFLG) dbflnam = 'decibel.dat'
      open(unit=15, file=dbflnam, form='formatted', status='old',
     1   access='sequential',iostat=ios)
      if(ios.ne.0) then
        print *,'error opening DBFLNAM file'
        stop
      endif
C
C  Read in the first three lines of the file.
      READ(15,1000) FILLABEL
      READ(15,1000) METLABEL
c     write(*,1000) FILLABEL
c     write(*,1000) METLABEL
      read(15,1001) XB, YB, ZBAG, WB, RMAX, DELPSI, DELRNG
      READ(15,*) DELPSI,NDELSI
      M=NDELSI+1
      DO I=1,M
        Y(I)=(I-1)*DELPSI*PI/180.
      ENDDO
 1000 FORMAT(A)
 1001 format(7F12.2)
C
C  Read in the range and corresponding DB value, find the Max and Min
C  DB points, then rewind the file.
      MM1=M-1
c     DO 100 IPSI=1,MM
      DO 100 IPSI=1,NDELSI
          READ(15,*,end=101) SIP,ISORT 
c#        IF(INT(SIP/DELPSI + 1.) .NE. IPSI ) THEN
c#           WRITE(KT,*)' ERROR IN READING THE DECIBL DATA FILE.'
c#        END IF
c         print *,'ipsi,sip,Y,isort=',ipsi,sip,Y(ipsi),isort
          Aza(ipsi)=SIP
          nranges(ipsi)=ISORT
          DO 105 I = 1,ISORT
             READ(15,*) RGARRY(I),DBARRY(I)
c            if((ipsi/5)*5.eq.ipsi) print *,'i,rgarry,dbarray=',
c    1         i,rgarry(i),dbarry(i)
             rangea(i,ipsi)=RGARRY(i)
             dBa(i,ipsi)=DBARRY(i)
 105      CONTINUE
          IF (IPSI .EQ.1) THEN
            XMIN = RGARRY(1)
            XMAX = RGARRY(ISORT)
          ELSE
            XMIN = MIN(XMIN,RGARRY(1))
            XMAX = MAX(XMAX,RGARRY(ISORT))
          END IF
 100  CONTINUE
 101  continue

      REWIND 15
C
      XMIN=MAX(1.0,XMIN)
      N = (XMAX - XMIN) / DELRNG + 1.000001
C
C  Interpolation
      DO 10 J=1,N
 10     X(J)=XMIN+(J-1)*(XMAX-XMIN)/(N-1)
c     write(kt,*) 'XMAX,XMIN,DELRNG,N=',XMAX,XMIN,DELRNG,N
c
      DO 30 I=1,MM1
         SIP=Aza(i)
         ISORT=nranges(i)
         DO 33 K = 1,ISORT
           RGARRY(k)=rangea(k,i)
           DBARRY(k)=dBa(k,i)
 33      CONTINUE
         J = 1
         ISORTN = ISORT - 1
C  Calculate the db values for the evenly spaced range values
         DO 31 IC = 1,ISORTN
 35         CONTINUE 
            IF(X(J) .LT. RGARRY(IC+1) .AND. X(J).GE.RGARRY(IC)) THEN   
               IF(ABS(RGARRY(IC+1) - RGARRY(IC) ).LE. 0.00001) THEN
                  A(J,I) = (DBARRY(IC+1) + DBARRY(IC))/2
               ELSE
                  XN = (DBARRY(IC) - DBARRY(IC+1)) /
     &              LOG(RGARRY(IC)/RGARRY(IC+1))
                  A(J,I) = DBARRY(IC+1) + XN*LOG(X(J)/RGARRY(IC+1))
               END IF
               IF (J .GE.N) GO TO 30   
               J = J + 1
               GO TO 35
            ELSEIF (IC.EQ.1 .AND. X(J).LT.RGARRY(IC) ) THEN
               A(J,I) = DBARRY(IC)
               J = J+ 1
               GO TO 35
            ELSEIF (IC.EQ.ISORTN .AND. X(J).GE.RGARRY(IC+1) ) THEN
               A(J,I) = DBARRY(IC+1)
               IF(J .GE. N) GO TO 30
               J = J + 1
               GO TO 35
            END IF
 31      CONTINUE
 30   CONTINUE
C
C  Store the sorted data to be passed to the plotting routine.
      DO 70 J = 1,N
         A(J,M) = A(J,1)
 70   CONTINUE
c     write(kt,*) 'exiting CNTOUR: N,M=',N,M
c     write(kt,*) 'x=',(x(i),i=1,N)
c     write(kt,*) 'y=',(y(j),j=1,M)
c     write(kt,*) 'A='
c     do i=1,M
c       write(kt,*) 'i,psi=',i,Aza(i)
c       write(kt,*) (A(j,i),j=1,N)
c     enddo
      RETURN
      END
C
C-----------------------------------------------------------------------
C
C  CONTOUR DRAWING SUBROUTINE
C   VERSION: 20 NOV 84   J. L. KELLER  UDRI

       SUBROUTINE CPLOT2(AGEN,MGEN,NGEN,RMAX,RMIN,CONTRS,NCONTS)
C
C  This program draws a contour plot of function F. It is assumed that
C  F is defined over (0,1) in both the X and Y directions. NCONTS
C  is the number of contours to be drawn (the number of entries
C  in CONTRS.  CONTRS is the array containing the values which will
C  correspond to the drawn contour lines.  IRES is the number of divisions
C  to be made in the X and in the Y directions. This IRES determines the
C  resolution of the plot.  The program divides the (0,1) (0,1) space into
C  IRES*IRES cells.  Each of the four walls (edges of the cell) is checked
C  to see if it intersects any of the NCONTS contour lines.
C
C-----------------------------------------------------------------------
      DIMENSION  ALT(4),AGEN(751,361), CONTRS(51)
c-----------------------------------------------------------------------
      integer ICLORS
      COMMON /CLRMAP/ ICLORS(20)
c-----------------------------------------------------------------------
C
      IRESX = NGEN-1
      IRESY = MGEN-1
C
C  Divide the space into deltax and deltay squares
      DELX = 1./FLOAT(IRESX)
      DELY = 1./FLOAT(IRESY)
      IYDIM = MGEN
      IXDIM = NGEN
      DO 100 IY = 1,IRESY
          YLL = (IY - 1) * DELY
          DO 100 IX = 1,IRESX
              XLL = (IX - 1) * DELX
              ALT(1) = AGEN(IX+1,IY+1)
              ALT(2) = AGEN(IX,IY+1)
              ALT(3) = AGEN(IX,IY)
              ALT(4) = AGEN(IX+1,IY)

           CALL CCELL(ALT,NCONTS,CONTRS,XLL,YLL,DELX,DELY,
     &                RMAX,RMIN,IX,IY,IYDIM)

100   CONTINUE
300   RETURN
      END
C
C
      SUBROUTINE CCELL(ALT,NCONTS,CONTRS,XLL,YLL,DELX,DELY,
     &               RMAX,RMIN,IX,IY,IYDIM)
C-----------------------------------------------------------------------
C
C  This subroutine draws the contour line as a straight line
C  connecting the points on the cells perimeter where the
C  contour line intersects.  Thus the intersection of the contour
C  line with each cell wall is tested for.
C
C-----------------------------------------------------------------------
      DIMENSION ALT(4), CONTRS(51)
      COMMON /SIDES/ IOUT(3),IBOT(203),ITOP(203),IRIGHT(203),
     &               ILEFT(203),ICONT(51)
c-----------------------------------------------------------------------
      integer ICLORS
      COMMON /CLRMAP/ ICLORS(20)
c-----------------------------------------------------------------------
C  For each contour line level do the following:
      ITOP(IX) = 0
      IRIGHT(IX) = 0
      IF((IY .EQ. 1 ) .OR. (IY .EQ. IYDIM/4.) .OR. (IY .EQ. IYDIM/2.)
     &   .OR. (IY .EQ. 3. * IYDIM/4.)) THEN
             IBOT(IX) = 0
             IRIGHT(IX) = 0
       END IF
C
C  Define the color for each contour level and the line type.
C
      DO 100 IC = 1,NCONTS
c#       CALL GSPL(1)
c#       CALL COLOR(ICLORS(IC) )
         CONT = CONTRS(IC)
c        print *,'ccel: ic,icolr,cont=',ic,iclors(ic),cont
C  To avoid possible gaps in the contour plots, check to see if the contour
C  level equals one of the four corners of a given cell.
         if(nconts.gt.1) then
           IF( CONTRS(IC) .EQ. ALT(1) .OR. CONTRS(IC) .EQ. ALT(2) .OR.
     &         CONTRS(IC) .EQ. ALT(3) .OR. CONTRS(IC) .EQ. ALT(4) ) THEN
               IF(IC .LT. NCONTS) THEN
                  CONT = CONT + 0.0001*(CONTRS(IC+1) - CONTRS(IC) )
               ELSE
                  CONT = CONT + 0.0001*(CONTRS(IC) - CONTRS(IC-1) )
               END IF
           END IF
         endif
C
         NPEN = 3
         ICONT(IC) = 0

C  Check the top wall
          IF((ALT(1)-CONT)*(ALT(2)-CONT).LT.0.) THEN
              Y = YLL + DELY
              X = (CONT-ALT(2))/(ALT(1)-ALT(2))*DELX+XLL
              CALL TRNFRM(X,Y,XX,YY,RMAX,RMIN)
              CALL MOVIT(XX,YY,NPEN)
              NPEN= 2
              ITOP(IX) = 1
              ICONT(IC) = 1
          ENDIF
C
C  Check the left wall
          IF((ALT(2)-CONT)*(ALT(3)-CONT).LT.0.) THEN
              X = XLL
              Y = (CONT-ALT(3))/(ALT(2)-ALT(3))*DELY+YLL
              CALL TRNFRM(X,Y,XX,YY,RMAX,RMIN)
              CALL MOVIT(XX,YY,NPEN)
              NPEN = 2
              ICONT(IC) = 1
          ENDIF
C
C  Check the bottom wall
          IF((ALT(3)-CONT)*(ALT(4)-CONT).LT.0.) THEN
              Y = YLL
              X = (CONT-ALT(3))/(ALT(4)-ALT(3))*DELX +XLL
              CALL TRNFRM(X,Y,XX,YY,RMAX,RMIN)
              CALL MOVIT(XX,YY,NPEN)
              NPEN = 2
              ICONT(IC) = 1
          ENDIF
C
C  Check the right wall
          IF((ALT(1)-CONT)*(ALT(4)-CONT).LT.0.) THEN
              X = XLL + DELX
              Y = (CONT-ALT(4))/(ALT(1)-ALT(4))*DELY + YLL
              CALL TRNFRM(X,Y,XX,YY,RMAX,RMIN)
              IRIGHT(IX) = 1
              ICONT(IC) = 1
              CALL MOVIT(XX,YY,NPEN)
          ENDIF
100   CONTINUE
C
      ICOUNT = 0
      DO 200 IC = 1,NCONTS
        IF(ICONT(IC) .NE. 0) THEN
          ICOUNT = ICOUNT + 1
          ICC = IC
        END IF
 200  CONTINUE
C
      IF( ICOUNT .NE. 1) RETURN
      IF ((IBOT(IX) .EQ. 1) .OR. (ILEFT(IX) .EQ. 1)) THEN
         ILEFT(IX+1) = IRIGHT(IX)
         IBOT(IX) = ITOP(IX)
         RETURN
      ELSE
         ILEFT(IX + 1) = IRIGHT(IX)
         IBOT(IX) = ITOP(IX)
      END IF
      RETURN
      END
C
      SUBROUTINE MOVIT(X,Y,NPEN)
C---------------------------------------------------------------------------
C
C  FUNCTION: USED BY CCELL TO MOVE THE PEN ACCORDING.
C
C    NPEN - 1....Move with pen up
C    NPEM - 2....Draw to next point
C
C----------------------------------------------------------------------------
c-----------------------------------------------------------------------
      real    XFAC,YFAC,DELRES,RXL,RYL,RXH,RYH,X1,Y1,X2,Y2,
     1  UB,UT,UR,UL,TRXB,TRYB
      COMMON /MAPCOR/ XFAC,YFAC,DELRES,RXL,RYL,RXH,RYH,X1,Y1,X2,Y2,
     1  UB,UT,UR,UL,TRXB,TRYB
c-----------------------------------------------------------------------
      INTEGER NPEN
      REAL X,Y
C
C Change the values of X and Y to match the graph coordinates used to plot
C the background map.  See subroutine MPCNVT (map coordinate convertion).
      A = ( X - XFAC)/10.
      B = ( Y - YFAC)/10.
C
      if((NPEN.EQ.2).and.(A.ge.RXL).and.(A.le.RXH).and.
     1  (B.ge.RYL).and.(B.le.RYH)) then
        CALL VECTD(A,B)  ! pen down
      ELSE
        CALL FRSTD(A,B)  ! pen up
      END IF
      RETURN
      END
C
      SUBROUTINE TRNFRM(X,Y,XX,YY,RMAX,RMIN)
C-----------------------------------------------------------------------
C
C  FUNCTION: To transform X,Y from polar to rectangular coordinate system
C
C-----------------------------------------------------------------------
      INTEGER IGUN
      REAL    XB,YB,ZB,WB,SFPRES,DIRANG,ZBAG
      COMMON /SOURCE/XB,YB,ZB,WB,SFPRES,DIRANG,IGUN,ZBAG
c-----------------------------------------------------------------------
      REAL PI,XX,X,YY,Y,RMAX
c
      PI=ACOS(-1.)
      YY=2.0 * PI * Y
      XX=(RMAX-RMIN)*X + RMIN
      XSAVE=XX
      XX=XSAVE*COS(YY)
      YY=XSAVE*SIN(YY)
C
      XX = XB + XX
      YY = YB + YY
C
      RETURN
      END
c
      SUBROUTINE COLOR(i)
      integer i
      call gstxci(i)
      return
      end             
c
c--- RDS added routines
c
c
      subroutine plotmet2
      IMPLICIT NONE
c-----------------------------------------------------------------------
      INTEGER isnd, ICHOICE, ISITE, IOPT, ICONTR, nsnds, nlevels,
     1        DBPLOTOPT
      REAL    PSIMIN, PSIMAX, LATS, LONS, LATB, LONB, input_delpsi,
     1        xminp,xmaxp,yminp,ymaxp
      LOGICAL DOELV, DOATT, white_backgrnd
      CHARACTER*60 snd_label
      COMMON  /INPUTS/ snd_label, ICHOICE, ISITE, IOPT, ICONTR, PSIMIN,
     1        PSIMAX, LATS, LONS, LATB, LONB, input_delpsi, isnd, DOELV,
     2        DOATT, white_backgrnd, xminp,xmaxp,yminp,ymaxp, nsnds,
     3        nlevels, DBPLOTOPT
c-----------------------------------------------------------------------
      INTEGER KI,KT,MT1,MT2,MT3,MT4,MT8
      COMMON /LUNT/ KI,KT,MT1,MT2,MT3,MT4,MT8
c-----------------------------------------------------------------------
      integer ncdir,nodir,nmdir 
      CHARACTER*60 DATADIR,OUTDIR,METDIR,FILELV,FILEAT,FILMET,FILSUM,
     1  FILOUT,met_dsn
c-----------------------------------------------------------------------
      COMMON /filenm/ DATADIR,OUTDIR,METDIR,FILELV,FILEAT,FILMET,FILSUM,
     1  FILOUT,met_dsn,ncdir,nodir,nmdir
      CHARACTER*60 savefile,filmetref
c-----------------------------------------------------------------------
      INTEGER idash,ifile,IEL
c
      filmetref='804108_1100.dat'
      savefile=FILMET
      do ifile=1,2
        if(ifile.eq.1) then
          FILMET=savefile
          idash=0
        elseif(ifile.eq.2) then
          FILMET=filmetref
          idash=1
        endif
c       print *,'ifile,FILMET=',ifile,FILMET
        IEL =-1
        CALL MET(IEL)
        if(ifile.eq.1) then
c         --- initialize plot
          if(white_backgrnd) then
            CALL COLOR(0)    ! black text
            CALL GSPLCI(0)   ! black lines
c           --- flush the background white
            call wflush
          else
            CALL COLOR(1)    ! white text
            CALL GSPLCI(1)   ! white lines
          endif
        endif
        CALL PLOTMET(idash)
c       CALL NEWPLT  ! advance frame for seperate plots
      enddo
      return
      end
c
      SUBROUTINE PLOTMET(idash)
c     --- plot meterology profiles as dashed(idash=1) or solid colored lines 
      IMPLICIT NONE
      INTEGER idash
c-----------------------------------------------------------------------
      INTEGER isnd, ICHOICE, ISITE, IOPT, ICONTR, nsnds, nlevels,
     1        DBPLOTOPT
      REAL    PSIMIN, PSIMAX, LATS, LONS, LATB, LONB, input_delpsi,
     1        xminp,xmaxp,yminp,ymaxp
      LOGICAL DOELV, DOATT, white_backgrnd
      CHARACTER*60 snd_label
      COMMON  /INPUTS/ snd_label, ICHOICE, ISITE, IOPT, ICONTR, PSIMIN,
     1        PSIMAX, LATS, LONS, LATB, LONB, input_delpsi, isnd, DOELV,
     2        DOATT, white_backgrnd, xminp,xmaxp,yminp,ymaxp, nsnds,
     3        nlevels, DBPLOTOPT
c-----------------------------------------------------------------------
      INTEGER KI,KT,MT1,MT2,MT3,MT4,MT8
      COMMON /LUNT/ KI,KT,MT1,MT2,MT3,MT4,MT8
      CHARACTER METLABEL*80, FILLABEL*80, SITE*10
      COMMON /IDTAG/  METLABEL, FILLABEL, SITE
c-----------------------------------------------------------------------
      REAL    ALTN,TN,RH,WN,WE,V,WU,WV,SV,p_mb
      INTEGER II
      COMMON /WEATHR/ALTN(70),TN(70),RH(70),WN(70),WE(70),V(70),WU(70),
     &                WV(70),SV(70),p_mb(70),II
c-----------------------------------------------------------------------
      integer ncdir,nodir,nmdir 
      CHARACTER*60 DATADIR,OUTDIR,METDIR,FILELV,FILEAT,FILMET,FILSUM,
     1  FILOUT,met_dsn
c-----------------------------------------------------------------------
      COMMON /filenm/ DATADIR,OUTDIR,METDIR,FILELV,FILEAT,FILMET,FILSUM,
     1  FILOUT,met_dsn,ncdir,nodir,nmdir
      real    YMETRS,TEMPER,RELHUM,WNDSPD,WNDDRT
c-----------------------------------------------------------------------
      integer KOUNT
      COMMON /DATA/ YMETRS(70),TEMPER(70),RELHUM(70),WNDSPD(70),
     &              WNDDRT(70),KOUNT
c-----------------------------------------------------------------------
      REAL    xp(70),yp(70),xx(2),yy(2)
      REAL    FL,FR,FB,FT,ztop,UL,UR,UB,UT,ux,uy,uxl,uyl
      REAL    Tmin,Tmax,RHmax,RHmin,umax,umin,ubar,vbar,y,
     &        uk,uk1,vk,vk1
      INTEGER k,kk,kmax,LL,mjrx,mnrx,mjrz,mnrz
      real FTPERM, DEGPRAD, FPSTOKN
      parameter (FTPERM=3.280833, DEGPRAD=57.29578, FPSTOKN=0.5925)
      logical colorplot,plotbarb
      data colorplot/.true./ , plotbarb/.false./
c
      LL=1      ! linear x,y plot
c
c     --- initialize plot
c     if(white_backgrnd) then
c       CALL COLOR(0)    ! black text
c       CALL GSPLCI(0)   ! black lines
c       --- flush the background white
c       call wflush
c     else
c       CALL COLOR(1)    ! white text
c       CALL GSPLCI(1)   ! white lines
c     endif
      FL=0.15
      FR=0.85
      FB=0.15
      FT=0.85
      ztop=1.   ! km
c#    UL = 0.
c#    UR = 50.
      UL =-2.0
      UR = 8.0
      UB=0.
      UT=ztop
      call set(FL,FR,FB,FT,UL,UR,UB,UT,LL)
      call labmod('(f6.1)','(f6.1)',6,6,2,2,20,20,0)
      mjrx=5
      mnrx=2
c#    mjrz=6
      mjrz=5
      mnrz=2
      call periml(mjrx,mnrx,mjrz,mnrz)
      if(idash.eq.0) then
        call wtstr(UL,UT+0.15,FILMET,2,0,-1)
      else
        call wtstr(UL,UT+0.1,FILMET,2,0,-1)
      endif
c     call wtstr(UL,UT+1.0,FILMET,2,0,-1)
c
c     --- label ordinate
      uy = UT/2.
      ux = UL-2.
      call wtstr(ux,uy,'z(km)',3,90,0)
c
c     --- label abscissa
c#    ux = (UR-1.0)/2.
c#    uy = UB-0.7
c#    call wtstr(ux,uy,'T+25(C), RH/2, u,v+25(m/s)',3,0,0)
c
c     --- convert msl alt to km
      kmax=II
      kk=1
      do k=1,kmax
        yp(k)=1.0E-3*ALTN(k)
        if(yp(k).le.ztop) then
          kk=k
        endif
      enddo
      kmax=kk
c
c     --- set dashed pattern according to idash
      if(idash.eq.1) then
        call GSLN(2) ! dashed
      else
        call GSLN(1) ! solid
      endif
c
c     --- set ranges
      Tmin = 0.    ! deg C
c#    Tmin = -25.  ! deg C
      Tmax = +25.  ! deg C
      RHmax = 100. ! %
      RHmin = 0.   ! %
      umax = 50.   ! m/s
      umin = 0.    ! m/s
c
c     --- plot T
      if(colorplot) CALL GSPLCI(2)   ! red
      call setusv('LW',2000)
      do k=1,kmax
        xp(k) = TN(k) - Tmin
      enddo
      call curved(xp,yp,kmax)
      if(idash.eq.0) then
        if(colorplot) CALL COLOR(2)   ! red
        call wtstr(xp(kmax)+.4,yp(kmax)-0.03,'T',3,0,0)
      endif
c     ux = UL+35.
c     uy = UB+.4
c     xx(1)=ux
c     xx(2)=ux+4.
c     yy(1)=uy
c     yy(2)=uy
c     call curved(xx,yy,2)
c     uxl=xx(2)+1.
c     uyl=uy
c     call wtstr(uxl,uyl,'T+25(C)',2,0,-1)
c
c     --- plot RH
c#    if(colorplot) CALL GSPLCI(3)   ! green
c     call GSLN(2) ! dashed
c#    do k=1,kmax
c#      xp(k) = RH(k)/2.
c$    enddo
c#    call curved(xp,yp,kmax)
c#    if(colorplot) CALL COLOR(3)   ! green
c#    call wtstr(xp(kmax)+1.,yp(kmax)+1.,'RH',3,0,0)
c     uy = uy+.25
c     yy(1)=uy
c     yy(2)=uy
c     call curved(xx,yy,2)
c     uyl=uy
c     call wtstr(uxl,uyl,'RH/2(%)',2,0,-1)
c
c     --- plot u (east-west)
      if(colorplot) CALL GSPLCI(5)   ! blue-green
c     call GSLN(3) ! dotted
c     call GSLN(1) ! solid
      do k=1,kmax
        uk = -wndspd(k)*sin(wnddrt(k)/degprad)  ! m/s
c#      xp(k) = uk + 25.
        xp(k) = uk      
      enddo
      call curved(xp,yp,kmax)
      if(idash.eq.0) then
        if(colorplot) CALL COLOR(5)   ! blue-green
        call wtstr(xp(kmax)-.5,yp(kmax)-0.03,'u',3,0,0)
      endif
c     uy = uy+.25
c     yy(1)=uy
c     yy(2)=uy
c     call curved(xx,yy,2)
c     uyl=uy
c     call wtstr(uxl,uyl,'u+25(m/s)',2,0,-1)
c
c     --- plot v (north-south)
      if(colorplot) CALL GSPLCI(3)   ! green
c     if(colorplot) CALL GSPLCI(7)   ! magenta
c     call GSLN(4) ! dash-dotted
      do k=1,kmax
        vk = -WNDSPD(k)*cos(WNDDRT(k)/DEGPRAD)  ! m/s
c#      xp(k) = vk + 25.
        xp(k) = vk
      enddo
      call curved(xp,yp,kmax)
      if(idash.eq.0) then
c       if(colorplot) CALL COLOR(7)   ! magenta
        if(colorplot) CALL COLOR(3)   ! green
        call wtstr(xp(kmax)-.2,yp(kmax)-0.03,'v',3,0,0)
      endif
c     uy = uy+.25
c     yy(1)=uy
c     yy(2)=uy
c     call curved(xx,yy,2)
c     uyl=uy
c     call wtstr(uxl,uyl,'v+25(m/s)',2,0,-1)
c
      if(white_backgrnd) then
        CALL COLOR(0)    ! black text
        CALL GSPLCI(0)   ! black lines
      else
        CALL COLOR(1)    ! white text
        CALL GSPLCI(1)   ! white lines
      endif
      call GSLN(1)   ! solid
c     call setusv('LW',1000)
c
c     --- now the wind barbs
      if(plotbarb) then
        ux = UR+4.
        CALL FRSTD(ux,UT)  ! pen up
        CALL VECTD(ux,UB)  ! pen down
        do k=1,kmax-1
          y = yp(k)  ! km
          uk = -WNDSPD(k)*sin(WNDDRT(k)/DEGPRAD)
          vk = -WNDSPD(k)*cos(WNDDRT(k)/DEGPRAD)
c         --- uk,vk in m/s
          call wndbarb(ux,y,uk,vk)
        enddo
      endif
c
      return
      end
c
      subroutine wndbarb (xbase,ybase,u,v)
c***********************************************************************
c wndbarb - for the graph portion of grin
C  THIS ROUTINE DRAWS A SINGLE WIND BARB PER CALL.
C
C ON INPUT - FOUR VARIABLES COME IN.  XBASE CONTAINS THE HORIZONTAL COOR
C            DINATE OF THE BASE OF THE WIND BARB.  YBASE CONTAINS THE
C            VERTICAL COORDINATE OF THE BASE OF THE WIND BARB.  U CONTAI
C            THE EAST-WEST WIND COMPONENT IN METERS PER SECOND.  V CONTA
C            THE NORTH-SOUTH WIND COMPONENT IN METERS PER SECOND.
C
C ON OUTPUT - ONE BARB HAS BEEN DRAWN TO UNIT NUMBER 2, WHICH CORRESPOND
C            TO GMETA.CGM, THE GKS OUTPUT META CODE FILE.
C
C ASSUMPTIONS - THIS ROUTINE ASSUMES THAT GKS HAS BEEN OPENED AND A WORK
C            STATION HAS BEEN SET.
C
C REVISED BY - JEREMY ASBILL ON MAY 3, 1990.
C*********************************************************************** 
C
C  INPUT VARIABLE DECLARATIONS ...
      REAL      XBASE,YBASE,
     *          U,V

C  LOCAL PARAMETER ...
C    SC SPECIFIES IN THE NORMALIZED (FRACTIONAL) GRAPHICS COORDINATE
C       SYSTEM HOW LONG THE BARB SHAFT IS TO BE
C    COORDINATE SYSTEMS ARE EXPLAINED IN NCAR GRAPHICS USER'S GUIDE VERS
C    2.00 ON PAGE 46.
      PARAMETER (SC = 0.05493)                                         
C
C  LOCAL VARIABLE DECLARATIONS ...                                    
                                                                      
      INTEGER   LLSV                 ! SAVE VARIABLE, SCALING FOR SET 
                                                                      
      LOGICAL DONE                   ! T => SUBROUTINE ENDS, F => LOOP A
                                                                       
      REAL      WINDVCT,             ! WIND VECTOR MAGNITUDE           
     *          FLSV,FRSV,FBSV,FTSV, ! SAVE VARIABLES, FRACTIONAL COORDI
     *          ULSV,URSV,UBSV,UTSV, ! SAVE VARIABLES, INCOMING USER COO
     *          NEWXBASE,            ! FRACTIONAL X COORD. FOR BARB BASE
     *          NEWYBASE,            ! FRACTIONAL Y COORD. FOR BARB BASE
     *          XCOMP,               ! X COMPONENT OF GRAPHICAL VECTOR (
     *          YCOMP,               ! Y COMPONENT OF GRAPHICAL VECTOR (
     *          PK,                  ! PLACE KEEPER                     
     *          FETHLENX,            ! X COMPONENT OF GRAPHICAL VECT. (F
     *          FETHLENY             ! Y COMPONENT OF GRAPHICAL VECT. (F
                                                                        
C  LOCAL ARRAY DECLARATIONS ...                                         
      INTEGER   IJUNK(5)             ! CALCULATION ARRAY FOR SFSGFA     
      REAL      POINTX(3),           ! USED TO SPECIFY POINTS TO DRAW BE
     *          POINTY(3),           ! USED TO SPECIFY POINTS TO DRAW BE
     *          JUNK(7)              ! CALCULATION ARRAY FOR SFSGFA     

C***************************** SUBROUTINE BEGIN ************************
                                                                        
C  INITIALIZE LOOP, BOOLEAN INDICATOR
      DONE = .FALSE.                                                    

C  CALCULATE THE WIND VECTOR MAGNITUDE IN KNOTS
      IF ((U .EQ. 0) .AND. (V .EQ. 0)) THEN
        WINDVCT = 1.0
      ELSE
C       WINDVCT = SQRT(U**2 + V**2) *1.94
        WINDVCT = SQRT(U**2 + V**2) / 1.94
      END IF

C  SAVE INCOMING USER COORDINATES AND CHANGE BACK TO NORMALIZED COORDINA
C  DOCUMENTATION FOR SET AND GETSET CAN BE FOUND IN NCAR GRAPHICS USER'S
C  GUIDE VERSION 2.00 ON PAGES 49 (GETSET) AND 53 (SET).
      CALL GETSET (FLSV,FRSV,FBSV,FTSV,ULSV,URSV,UBSV,UTSV,LLSV)
      CALL SET    (FLSV,FRSV,FBSV,FTSV, 0.0, 1.0, 0.0, 1.0, 1)

C  DETERMINE WHERE THE BASE OF THE BARB IS IN THE NORMALIZED COORDINATES
      NEWXBASE = (XBASE - ULSV)/(URSV - ULSV)
      NEWYBASE = (YBASE - UBSV)/(UTSV - UBSV)
C
C  IF WIND MAG < 1.0 KTS DRAW A CIRCLE
      if(WINDVCT.LE.1.0) THEN
        CALL WTSTR(NEWXBASE,NEWYBASE,'o',2,0,0)
C  RESET USER COORDINATES TO THE INCOMING VALUES
        CALL SET (FLSV,FRSV,FBSV,FTSV,ULSV,URSV,UBSV,UTSV,LLSV)
        RETURN
      endif

C  CALCULATE THE X DISTANCE AND Y DISTANCE FROM THE BASE OF THE BARB THAT
C  DEFINES THE BARBS TIP (NORMALIZED COORD'S)
C      XCOMP = -SC * U * 1.94/WINDVCT
C      YCOMP = -SC * V * 1.94/WINDVCT
      XCOMP = -SC * U * .5/WINDVCT
      YCOMP = -SC * V * .5/WINDVCT

C  DETERMINE THE ACTUAL LOCATION IN NORMALIZED COORDINATES OF THE BARB'S
      POINTX(1) = NEWXBASE + XCOMP
      POINTY(1) = NEWYBASE + YCOMP

C  DRAW THE BARB SHAFT, DOCUMENTATION FOR THE LINE SUBROUTINE CAN BE FOUND
C  IN NCAR GRAPHICS USER'S GUIDE VERSION 2.00 ON PAGE 50
      CALL LINE (NEWXBASE,NEWYBASE,POINTX(1),POINTY(1))

C  DETERMINE THE FEATHER LENGTH
      FETHLENX = 0.3 * YCOMP
      FETHLENY = -0.3 * XCOMP

C  SET THE PLACE KEEPER AND BOOST THE WIND MAGNITUDE
      PK = 0.9
      WINDVCT = WINDVCT + 2.5

C  BEGIN MAKING FEATHERS
10    CONTINUE

C    DRAW A FLAG FOR EVERY 50 KNOTS WIND MAGNITUDE

      IF (WINDVCT .GE. 50.0) THEN

C      DETERMINE THE POSITION OF THE FLAG TIP, POINT_(2)
C      AND DETERMINE POSITION WHERE FLAG BOTTOM MEETS THE SHAFT, POINT_(3)

        POINTX(2) = POINTX(1) + FETHLENX + 0.0005
        POINTY(2) = POINTY(1) + FETHLENY + 0.0005
        POINTX(3) = PK * XCOMP + NEWXBASE
        POINTY(3) = PK * YCOMP + NEWYBASE

C      DRAW FLAG

        CALL LINE (POINTX(1),POINTY(1),POINTX(2),POINTY(2))
        CALL LINE (POINTX(3),POINTY(3),POINTX(2),POINTY(2))

C      FILL IN FLAG, DOCUMENTATION FOR SFSGFA CAN BE FOUND IN NCAR
C      GRAPHICS GUIDE TO NEW UTILITIES VERSION 3.00 ON PAGE 4-8

        CALL SFSETR ('SP',0.000001)
        CALL SFSGFA (POINTX,POINTY,3,JUNK,5,IJUNK,7,2)

C      REMOVE 50 KNOTS FROM WIND MAGNITUDE (ALREADY DRAWN IN)

        WINDVCT = WINDVCT - 50.0

C      DETERMINE NEW BEGIN POINT FOR NEXT FLAG OR FEATHER

        PK = PK - 0.05
        POINTX(1) = PK * XCOMP + NEWXBASE
        POINTY(1) = PK * YCOMP + NEWYBASE
        PK = PK - 0.1

C    DRAW A FULL FEATHER FOR WIND MAGNITUDE OF EVERY 10 KNOTS

      ELSE IF (WINDVCT .GE. 10.0) THEN

C      CALCULATE POSITION OF FEATHER END

        POINTX(2) = POINTX(1) + FETHLENX + 0.0005
        POINTY(2) = POINTY(1) + FETHLENY + 0.0005

C      DRAW FEATHER

        CALL LINE (POINTX(1),POINTY(1),POINTX(2),POINTY(2))

C      REMOVE 10 KNOTS FROM WIND MAGNITUDE (ALREADY DRAWN IN)

        WINDVCT = WINDVCT - 10.0

C      DETERMINE NEW START POINT FOR NEXT FEATHER OR FLAG

        POINTX(1) = PK * XCOMP + NEWXBASE
        POINTY(1) = PK * YCOMP + NEWYBASE
        PK = PK - 0.1

C    DRAW A HALF FEATHER FOR EVERY 5 KNOTS OF WIND MAGNITUDE

      ELSE IF (WINDVCT .GE. 5.0) THEN

C      CALCULATE POSITION OF TIP OF HALF FEATHER

        POINTX(2) = POINTX(1) + 0.5 * FETHLENX + 0.0005
        POINTY(2) = POINTY(1) + 0.5 * FETHLENY + 0.0005

C      DRAW IN FEATHER

        CALL LINE (POINTX(1),POINTY(1),POINTX(2),POINTY(2))

C      TELL LOOP TO QUIT
        DONE = .TRUE.
      ELSE
        DONE = .TRUE.
      END IF

C  IF THERE IS STILL MORE WIND MAGNITUDE (>= 5 KNOTS) LOOP AGAIN

      IF (.NOT. DONE) GOTO 10

C  RESET USER COORDINATES TO THE INCOMING VALUES
      CALL SET (FLSV,FRSV,FBSV,FTSV,ULSV,URSV,UBSV,UTSV,LLSV)

      RETURN
      END
c
      SUBROUTINE PLOTDBXY
c-----------------------------------------------------------------------
c  This subroutine calls the necessary routines to plot a file of DB values
c  that were collected on a previous run ( or the average/maximum  of several
c  runs) of NAPS.  This archived file was named "DECIBEL.DAT"  by default 
c  or it was renamed by the user to a more meaningful name and stored for 
c  future  plotting or averaging.
c  --- Plotting is by interpolation to a cartesian coordinate system and
c  --- using NCAR graphics routines for the contouring.  Background map
c  --- is distance from blast center.
c-----------------------------------------------------------------------
      INTEGER isnd, ICHOICE, ISITE, IOPT, ICONTR, nsnds, nlevels,
     1        DBPLOTOPT
      REAL    PSIMIN, PSIMAX, LATS, LONS, LATB, LONB, input_delpsi,
     1        xminp,xmaxp,yminp,ymaxp
      LOGICAL DOELV, DOATT, white_backgrnd
      CHARACTER*60 snd_label
      COMMON  /INPUTS/ snd_label, ICHOICE, ISITE, IOPT, ICONTR, PSIMIN,
     1        PSIMAX, LATS, LONS, LATB, LONB, input_delpsi, isnd, DOELV,
     2        DOATT, white_backgrnd, xminp,xmaxp,yminp,ymaxp, nsnds,
     3        nlevels, DBPLOTOPT
c-----------------------------------------------------------------------
      CHARACTER*60 DBFLNAM
      COMMON /DBFILE/ DBFLNAM
c-----------------------------------------------------------------------
      integer KI,KT,MT1,MT2,MT3,MT4,MT8
      COMMON /LUNT/ KI,KT,MT1,MT2,MT3,MT4,MT8
c-----------------------------------------------------------------------
      CHARACTER METLABEL*80, FILLABEL*80, SITE*10
      COMMON /IDTAG/ METLABEL, FILLABEL, SITE
c-----------------------------------------------------------------------
      real    A,CONTRS,RMAX,DELPSI,RRATIO
      integer M,N,IRAMAX,NRO,NCONTS
      COMMON /SOUND/ A(751,361),M,N,CONTRS(51),RMAX,DELPSI,IRAMAX,
     &               NRO,NCONTS,RRATIO
c-----------------------------------------------------------------------
      integer IGUN
      real   XB,YB,ZB,WB,SFPRES,DIRANG,ZBAG
      COMMON /SOURCE/ XB,YB,ZB,WB,SFPRES,DIRANG,IGUN,ZBAG
c-----------------------------------------------------------------------
      real    XFAC,YFAC,DELRES,RXL,RYL,RXH,RYH,X1,Y1,X2,Y2,
     1  UB,UT,UR,UL,TRXB,TRYB
      COMMON /MAPCOR/ XFAC,YFAC,DELRES,RXL,RYL,RXH,RYH,X1,Y1,X2,Y2,
     1  UB,UT,UR,UL,TRXB,TRYB
c-----------------------------------------------------------------------
      integer ndelsi, nranges, nray
      real    Aza, rangea, dBa, xray, yray
      common /savea/  ndelsi, nranges(361), Aza(361), rangea(751,361),
     1                dBa(751,361), xray(751,101), yray(751,101), nray
c-----------------------------------------------------------------------
      LOGICAL DBFLG
      COMMON /FLAGS/ DBFLG
c-----------------------------------------------------------------------
      integer ICLORS
      COMMON /CLRMAP/ ICLORS(20)
c-----------------------------------------------------------------------
      integer ncdir,nodir,nmdir 
      CHARACTER*60 DATADIR,OUTDIR,METDIR,FILELV,FILEAT,FILMET,FILSUM,
     1  FILOUT,met_dsn
      COMMON /filenm/ DATADIR,OUTDIR,METDIR,FILELV,FILEAT,FILMET,FILSUM,
     1  FILOUT,met_dsn,ncdir,nodir,nmdir
c-----------------------------------------------------------------------
c     --- NCAR graphics fill routines declarations
      external fillc
      integer nxg,nyg
      integer lwrk,liwk
      integer lmap
      integer nwrk, ngrps
c     parameter (nxg=201, nyg=161)
      parameter (nxg=201, nyg=201)
      parameter (lwrk=2500, liwk=1000)
      parameter (lmap=500000)
      parameter (nwrk=250000,ngrps=100)
      real    rwrk(lwrk)
      integer iwrk(liwk)
      integer map(lmap)
      integer iarea(ngrps), igrp(ngrps)
      real    xwrk(nwrk), ywrk(nwrk)
c-----------------------------------------------------------------------
      real xg(nxg), yg(nyg), dBxy(nxg,nyg)
      real*8     DEGREES_PER_RADIAN
      parameter (DEGREES_PER_RADIAN = 57.29577951308232D0 )
c-----------------------------------------------------------------------
 1000 FORMAT (A)
 1100 FORMAT (A,$)
 200  CONTINUE
C     Get the data not already available when an archived file is requested.
      OPEN(UNIT=15,FILE=DBFLNAM,FORM='FORMATTED',STATUS='OLD',
     1 iostat=ios)
      if(ios.ne.0) then
        write(kt,*) 'error opening input decibel file:',DBFLNAM
        stop
      endif
      READ(15,1000) FILLABEL
      READ(15,1000) METLABEL
      SITE= METLABEL(1:10)
      READ(15,*)XB,YB,ZBAG,WB,RMAX,DELPSI,DELRES
      REWIND(15)
c     print *,'in PLOTDBXY'
c     print *,'XB,YB,ZBAG,WB,RMAX,DELPSI,DELRES=',
c    1         XB,YB,ZBAG,WB,RMAX,DELPSI,DELRES
c
c     --- read in contour levels
      CALL LEVELQ(1)
c
c     --- find max,min dB in file
      dBamax = -1.0E30
      dBamin = +1.0E30
      do j=1,ndelsi
        do i=1,nranges(j)
          dBamax = MAX(dBamax,dBa(i,j))
          dBamin = MIN(dBamin,dBa(i,j))
        enddo
      enddo
c     print *,'in PLOTDBXY: dBamax,dBamin=',dBamax,dBamin
c
c     ---- build a rectangular grid
      xmax=RMAX
      dxg=2.*RMAX/(nxg-1)
      dyg=dxg
      xlg=-xmax
      ylg=-xmax
      do ix=1,nxg
        xg(ix)=(ix-1)*dxg + xlg
      enddo
      do jy=1,nyg
        yg(jy)=(jy-1)*dyg + ylg
      enddo
      ixc=nxg/2+1
      jyc=nyg/2+1
      print *,'dxg,dyg,xlg,ylg=',dxg,dyg,xlg,ylg
c
c     --- wrap 360 deg data to 0 deg data
      Aza(ndelsi+1) =360.
      nranges(ndelsi+1)=nranges(1)
      do i=1,nranges(ndelsi+1)
        rangea(i,ndelsi+1)=rangea(i,1)
        dBa   (i,ndelsi+1)=dBa(i,1)
      enddo
c
c     --- transform the polar grid to equally spaced Cartesian grid
      dBxymax = -1.0E30
      dBxymin = +1.0E30
      do ix=1,nxg
        xg(ix)=(ix-1)*dxg + xlg
        do jy=1,nyg
          dBxy(ix,jy) = -1.
          yg(jy)=(jy-1)*dyg + ylg
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
          if(Azj.eq.360.) Azj=359.99
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
              elseif(r.eq.rangea(nranges(j),j)) then
                dBxy(ix,jy) = dBa(nranges(j),j)
                go to 603
              elseif(r.eq.rangea(nranges(j+1),j+1)) then
                dBxy(ix,jy) = dBa(nranges(j+1),j+1)
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
c          print *,'ix,jy,r,theta,dB=',ix,jy,r,Azj,dBxy(ix,jy)
          dBxymax = MAX(dBxy(ix,jy),dBxymax)
          dBxymin = MIN(dBxy(ix,jy),dBxymin)
        enddo
      enddo
c     print *,'in PLOTDBXY: dBxymax,dBxymin=',dBxymax,dBxymin
c     do j=1,nyg
c       write(kt,*) (dBxy(i,j),i=1,nxg)
c     enddo
c
c     --- use ncar graphics contouring facility
      call getset(xx1,xX2,yY1,yY2,xUL,xUR,yUB,yUT,l)
c     print *,'in PLOTDBXY: after getset: X1,X2,Y1,Y2,UL,UR,UB,UT=',
c    1            xx1,xX2,yY1,yY2,xUL,xUR,yUB,yUT
      X1=0.18
      Y1=0.18
      X2=(1.-X1)
      DX=X2-X1
      DY=DX
      Y2=Y1+DY
      UL=1.0
      UR=FLOAT(nxg)
      UB=1.0
      UT=FLOAT(nyg)
c     print *,'calling set: UL,UR,UB,UT=',UL,UR,UB,UT
c     print *,'             X1,X2,Y1,Y2=',X1,X2,Y1,Y2
      call set(X1,X2,Y1,Y2,UL,UR,UB,UT,1)
c     print*,' white_backgrnd = ',white_backgrnd
      if(white_backgrnd) then
c     if(white_backgrnd.and.cntfill) then
        lc=0
      else
        lc=1
      endif
      CALL COLOR(lc)    ! color text
      CALL GSPLCI(lc)   ! color lines
      CALL GSPMCI(lc)   ! color polymarker
c
      iset=1  ! viewport already set
      ihi=-1  ! don't label data or show H or L
      idash = -155  ! solid for + values, dashed for - values
      idash = +1  ! solid
      cmx = CONTRS(NCONTS)
      cmn = CONTRS(1)
      cnt = CONTRS(2)-CONTRS(1)
      ncl = NINT((cmx-cmn)/cnt)+1
c     print *,'cmx,cmn,cnt,ncl=',cmx,cmn,cnt,ncl
      call cpsetc('ILT',' ')      ! suppress info label
      call cpseti('LLP',0)        ! suppress contour labels
      call cpsetr('CIS',cnt)      ! contour interval specifier
      call cpsetr('CMN',cmn)      ! contour minimum
      call cpsetr('CMX',cmx)      ! contour maximum
      call cpseti('CLS',ncl)      ! draw ncl contours
c     call cpseti('CLS',-10)      ! draw ncl contours
      call cpseti('LIS',1)        ! label interval specifier
c      call cpseti('SET',iset)    ! call set flag - 1=no
c
c     --- use input contour interval, max,min
      call arinam(map,lmap)
      call cprect(dBxy,nxg,nxg,nyg,rwrk,lwrk,iwrk,liwk)
      call setusv('LW',1000)   ! light lines
      call set(X1,X2,Y1,Y2,UL,UR,UB,UT,1)
      call cpcnrc(dBxy,nxg,nxg,nyg,cmn,cmx,cnt,iset,ihi,idash)
c     call cpgeti('SET',iset)
      call cpclam(dBxy,rwrk,iwrk,map)
      call gsfais(1)   ! solid fill
      call arscam(map, xwrk, ywrk, nwrk, iarea, igrp, ngrps, fillc)
c     --- overlay black contour lines
      call setusv('LW',1000)   ! light lines
      call gsplci(0)   ! black lines
      iset=1      ! don't call set
      ihi=-1      ! don't label data or show H or L
      idash = +1  ! solid
      call cpcnrc(dBxy,nxg,nxg,nyg,cmn,cmx,cnt,iset,ihi,idash)
c     --- attach title
      CALL GETLABEL3
      DBFLG = .FALSE.
c
c     --- draw the frame
      mjrx=8
      mjry=8
      mnrx=10
      mnry=10
      ixlab=1
      iylab=1
      igph=5
      xint=0.
      yint=0.
      axw=4.                   ! line width of axes
      lnw=4.                   ! line width of labels
      mjw=4.                   ! line width of major ticks
      mnw=4.                   ! line width of minor ticks
      majl=15.                 ! major tic length
      call dashdc('$$',20,20)  ! solid
c     print *,'mjrx,mjry=',mjrx,mjry
      XL=xg(1)/1000.    ! convert to km for frame
      XR=xg(nxg)/1000.
      YB=yg(1)/1000.
      YT=yg(nyg)/1000.
c     print *,'calling set: X1,X2,Y1,Y2=',X1,X2,Y1,Y2
c     print *,'             XL,XR,YB,YT=',XL,XR,YB,YT
      call set(X1,X2,Y1,Y2,XL,XR,YB,YT,1)
      call gaseti('LTY',1)
      call pcseti('FN',21)
      call gslwsc(2.)
c     call gasetr('WAX',axw)  ! line width of axes
c     call gasetr('WLB',lnw)  ! line width of labels
c     call gasetr('WMJ',mjw)  ! line width of major ticks
c     call gasetr('WMN',mnw)  ! line width of minor ticks
c     call gasetr('XMJ',majl)
c     call gasetr('YMJ',majl)
      if(white_backgrnd) then
        lc=0
      else
        lc=1
      endif
      call gsfaci(lc)   ! black text
      CALL COLOR(lc)    ! color text
      CALL GSPLCI(lc)   ! color lines
      call labmod('(I3)','(I3)',0,0,3,3,18,18,0)
      call periml(mjrx,mnrx,mjry,mnry)
c     --- label ordinate
      call plchhq(cpux(60),cpuy(512),'y(km)',0.030,90.,0.)
c     --- label abscissa
      call plchhq(cpux(512),cpuy(80),'x(km)',0.030,0.,0.)
c     --- Place a mark at the center.
      CALL COLOR(lc)
      call setusv('LW',2000)
      xc=0.
      yc=0.
      call wtstr(xc,yc,'+',3,0,0)
c
c     --- restore viewport parameters
      X1=xx1
      X2=xX2
      Y1=yY1
      Y2=yY2
      UL=xUL
      UR=xUR
      UB=yUB
      UT=yUT
      call set(X1,X2,Y1,Y2,UL,UR,UB,UT,l)
c
      return
      end
c
      SUBROUTINE PLOTDB2
C-----------------------------------------------------------------------
C  This subroutine calls the necessary routines to plot a file of DB values
C  that were collected on a previous run ( or the average/maximum  of several
C  runs)  of NAPS.  This archived file was named "DECIBEL.DAT"  by default 
C  or it was renamed by the user to a more meaningful name and stored for 
C  future  plotting or averaging.
c  --- Plotting is by interpolation to a cartesian coordinate system and
c  --- using NCAR graphics routines for the contouring.  Background map
c  --- is the utm coordinate maps originally supplied with NAPS 5.1.
c-----------------------------------------------------------------------
      INTEGER isnd, ICHOICE, ISITE, IOPT, ICONTR, nsnds, nlevels,
     1        DBPLOTOPT
      REAL    PSIMIN, PSIMAX, LATS, LONS, LATB, LONB, input_delpsi,
     1        xminp,xmaxp,yminp,ymaxp
      LOGICAL DOELV, DOATT, white_backgrnd
      CHARACTER*60 snd_label
      COMMON  /INPUTS/ snd_label, ICHOICE, ISITE, IOPT, ICONTR, PSIMIN,
     1        PSIMAX, LATS, LONS, LATB, LONB, input_delpsi, isnd, DOELV,
     2        DOATT, white_backgrnd, xminp,xmaxp,yminp,ymaxp, nsnds,
     3        nlevels, DBPLOTOPT
c-----------------------------------------------------------------------
      CHARACTER*60 DBFLNAM
      COMMON /DBFILE/ DBFLNAM
c-----------------------------------------------------------------------
      integer KI,KT,MT1,MT2,MT3,MT4,MT8
      COMMON /LUNT/ KI,KT,MT1,MT2,MT3,MT4,MT8
c-----------------------------------------------------------------------
      CHARACTER METLABEL*80, FILLABEL*80, SITE*10
      COMMON /IDTAG/ METLABEL, FILLABEL, SITE
c-----------------------------------------------------------------------
      real    A,CONTRS,RMAX,DELPSI,RRATIO
      integer M,N,IRAMAX,NRO,NCONTS
      COMMON /SOUND/ A(751,361),M,N,CONTRS(51),RMAX,DELPSI,IRAMAX,
     &               NRO,NCONTS,RRATIO
c-----------------------------------------------------------------------
      integer IGUN
      real   XB,YB,ZB,WB,SFPRES,DIRANG,ZBAG
      COMMON /SOURCE/ XB,YB,ZB,WB,SFPRES,DIRANG,IGUN,ZBAG
c-----------------------------------------------------------------------
      real    XFAC,YFAC,DELRES,RXL,RYL,RXH,RYH,X1,Y1,X2,Y2,
     1  UB,UT,UR,UL,TRXB,TRYB
      COMMON /MAPCOR/ XFAC,YFAC,DELRES,RXL,RYL,RXH,RYH,X1,Y1,X2,Y2,
     1  UB,UT,UR,UL,TRXB,TRYB
c-----------------------------------------------------------------------
      integer ndelsi, nranges, nray
      real    Aza, rangea, dBa, xray, yray
      common /savea/  ndelsi, nranges(361), Aza(361), rangea(751,361),
     1                dBa(751,361), xray(751,101), yray(751,101), nray
c-----------------------------------------------------------------------
      LOGICAL DBFLG
      COMMON /FLAGS/ DBFLG
c-----------------------------------------------------------------------
      integer ICLORS
      COMMON /CLRMAP/ ICLORS(20)
c-----------------------------------------------------------------------
      integer ncdir,nodir,nmdir 
      CHARACTER*60 DATADIR,OUTDIR,METDIR,FILELV,FILEAT,FILMET,FILSUM,
     1  FILOUT,met_dsn
      COMMON /filenm/ DATADIR,OUTDIR,METDIR,FILELV,FILEAT,FILMET,FILSUM,
     1  FILOUT,met_dsn,ncdir,nodir,nmdir
c-----------------------------------------------------------------------
c     --- NCAR graphics fill routines declarations
      external fillc
      integer nxg,nyg
      integer lwrk,liwk
      integer lmap
      integer nwrk, ngrps
      parameter (nxg=201, nyg=161)
c     parameter (nxg=201, nyg=201)
      parameter (lwrk=2500, liwk=1000)
      parameter (lmap=500000)
      parameter (nwrk=250000,ngrps=100)
      real    rwrk(lwrk)
      integer iwrk(liwk)
      integer map(lmap)
      integer iarea(ngrps), igrp(ngrps)
      real    xwrk(nwrk), ywrk(nwrk)
c-----------------------------------------------------------------------
      real xg(nxg), yg(nyg), dBxy(nxg,nyg)
      real*8     DEGREES_PER_RADIAN
      parameter (DEGREES_PER_RADIAN = 57.29577951308232D0 )
c-----------------------------------------------------------------------
c
      XFAC =  200000.     ! UTM adjustments for the plotting routines.
      YFAC = 4300000.
c     print*,'XFAC,YFAC,DELRES,RXL,RYL,RXH,RYH,UB,UT,UR,UL,TRXB,TRYB=',
c    1        XFAC,YFAC,DELRES,RXL,RYL,RXH,RYH,UB,UT,UR,UL,TRXB,TRYB
 1000 FORMAT (A)
 1100 FORMAT (A,$)
 200  CONTINUE
C     Get the data not already available when an archived file is requested.
      OPEN(UNIT=15,FILE=DBFLNAM,FORM='FORMATTED',STATUS='OLD',
     1 iostat=ios)
      if(ios.ne.0) then
        write(kt,*) 'error opening input decibel file:',DBFLNAM
        stop
      endif
      READ(15,1000) FILLABEL
      READ(15,1000) METLABEL
      SITE= METLABEL(1:10)
      READ(15,*)XB,YB,ZBAG,WB,RMAX,DELPSI,DELRES
      REWIND(15)
c     print *,'XB,YB,ZBAG,WB,RMAX,DELPSI,DELRES=',
c    1         XB,YB,ZBAG,WB,RMAX,DELPSI,DELRES
c
      CALL LEVELQ(1)
      CALL REDFIN(RMAX)
      CALL MPCNVT
      call setusv('LW',1000)
      CALL BLASTMAP
      dBamax = -1.0E30
      dBamin = +1.0E30
      do j=1,ndelsi
        do i=1,nranges(j)
          dBamax = MAX(dBamax,dBa(i,j))
          dBamin = MIN(dBamin,dBa(i,j))
        enddo
      enddo
c     print *,'in PLOTDB2: dBamax,dBamin=',dBamax,dBamin
c     ---- build a rectangular grid
      dxg=500.
      dyg=500.
      xlg=UL*10. + xfac
      ylg=UB*10. + yfac
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
        xg(ix)=(ix-1)*dxg + xlg
        do jy=1,nyg
          dBxy(ix,jy) = -1.
          yg(jy)=(jy-1)*dyg + ylg
          delx = xg(ix)-XB
          dely = yg(jy)-YB
          rsq = delx**2 + dely**2
          r = SQRT(rsq)
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
c          print *,'ix,jy,r,theta,dB=',ix,jy,r,Azj,dBxy(ix,jy)
          dBxymax = MAX(dBxy(ix,jy),dBxymax)
          dBxymin = MIN(dBxy(ix,jy),dBxymin)
        enddo
      enddo
c     print *,'in PLOTDB2: dBxymax,dBxymin=',dBxymax,dBxymin
c     do j=1,nyg
c       write(kt,*) (dBxy(i,j),i=1,nxg)
c     enddo
c     --- use ncar graphics contouring facility
      call getset(xx1,xX2,yY1,yY2,xUL,xUR,yUB,yUT,l)
c     print *,'in PLOTDB2: after getset: X1,X2,Y1,Y2,UL,UR,UB,UT=',
c    1            xx1,xX2,yY1,yY2,xUL,xUR,yUB,yUT
      XL=1.0
      XR=FLOAT(nxg)
      YB=1.0
      YT=FLOAT(nyg)
c     print *,'calling set: UL,UR,UB,UT=',XL,XR,YB,YT
c     print *,'             X1,X2,Y1,Y2=',X1,X2,Y1,Y2
      call set(X1,X2,Y1,Y2,XL,XR,YB,YT,1)
      if(white_backgrnd) then
        CALL COLOR(1)    ! black text
        CALL GSPLCI(1)   ! black lines
      else
        CALL COLOR(0)    ! white text
        CALL GSPLCI(0)   ! white lines
      endif
c
      iset=1  ! viewport already set
      ihi=-1  ! don't label data or show H or L
      idash = -155  ! solid for + values, dashed for - values
      idash = +1  ! solid
      cmx = CONTRS(NCONTS)
      cmn = CONTRS(1)
      cnt = CONTRS(2)-CONTRS(1)
      ncl = NINT((cmx-cmn)/cnt)+1
c     print *,'cmx,cmn,cnt,ncl=',cmx,cmn,cnt,ncl
c     call cpsetc('ILT',' ') ! suppress info label
      call cpseti('LLP',0)   ! suppress contour labels
      call cpsetr('CIS',cnt)   ! contour interval specifier
      call cpsetr('CMN',cmn)   ! contour minimum
      call cpsetr('CMX',cmx)   ! contour maximum
      call cpseti('CLS',ncl)   ! draw ncl contours
c     call cpseti('CLS',-10)   ! draw ncl contours
      call cpseti('LIS',1)     ! label interval specifier
      call cpseti('SET',iset)  ! call set flag - 1=no
c     --- use input contour interval, max,min
      call arinam(map,lmap)
cc    print *,'in PLOTDB2: calling cprect: nxg,nyg=',nxg,nyg
      call cprect(dBxy,nxg,nxg,nyg,rwrk,lwrk,iwrk,liwk)
      call setusv('LW',1000)   ! light lines
      call getset(xx1,xX2,yY1,yY2,xUL,xUR,yUB,yUT,l)
cc    print *,'in PLOTDB2: calling cpcldr: X1,X2,Y1,Y2,UL,UR,UB,UT=',
cc   1            xx1,xX2,yY1,yY2,xUL,xUR,yUB,yUT
      call set(X1,X2,Y1,Y2,XL,XR,YB,YT,1)
cc    call getset(xx1,xX2,yY1,yY2,xUL,xUR,yUB,yUT,l)
cc    print *,'in PLOTDB2: calling cpcldr: X1,X2,Y1,Y2,UL,UR,UB,UT=',
cc   1            xx1,xX2,yY1,yY2,xUL,xUR,yUB,yUT
cc    print *,'in PLOTDB2: calling cpcnrc: cmn,cmx,cnt=',cmn,cmx,cnt
      call cpcnrc(dBxy,nxg,nxg,nyg,cmn,cmx,cnt,iset,ihi,idash)
      call cpgeti('SET',iset)
      call getset(xx1,xX2,yY1,yY2,xUL,xUR,yUB,yUT,l)
cc    print *,'in PLOTDB2: calling cpcldr: X1,X2,Y1,Y2,UL,UR,UB,UT=',
cc   1            xx1,xX2,yY1,yY2,xUL,xUR,yUB,yUT
c     call cpcldr(dBxy,rwrk,iwrk)
      call cpclam(dBxy,rwrk,iwrk,map)
      call gsfais(1)   ! solid fill
      call arscam(map, xwrk, ywrk, nwrk, iarea, igrp, ngrps, fillc)
      call getset(xx1,xX2,yY1,yY2,xUL,xUR,yUB,yUT,l)
cc    print *,'in PLOTDB2: after getset: X1,X2,Y1,Y2,UL,UR,UB,UT=',
cc   1            xx1,xX2,yY1,yY2,xUL,xUR,yUB,yUT
cc    print *,'calling set: UL,UR,UB,UT=',UL,UR,UB,UT
cc    print *,'             X1,X2,Y1,Y2=',X1,X2,Y1,Y2
      call set(X1,X2,Y1,Y2,UL,UR,UB,UT,1)
      CALL GETLABEL
      REWIND(15)
      DBFLG = .FALSE.
c###############################################
c     --- Place a mark at the blast site.
      if(white_backgrnd) then
        lc=0
      else
        lc=1
      endif
      CALL COLOR(lc)
      call wtstr(TRXB,TRYB,'+',2,0,0)
      call setusv('LW',1000)
c     --- Place a mark at the sonding location
      call latlon_to_utm(LATS,LONS,XS,YS,izone,0,0)  ! 0,0=sphere,forward
      XSS = (XS-XFAC)/10.
      YSS = (YS-YFAC)/10.
c     --- plot as an open circle
c     icolor=4  ! blue-green
c     icolor=2  ! red
c     icolor=lc ! black
c     CALL NGWSYM('N',9,XSS,YSS,0.015,icolor,0)
      CALL COLOR(lc)
      call wtstr(XSS,YSS,'*',3,0,0)
c     --- Draw boundaries over filled countors
      CALL BLASTMAP
c
      RETURN
      END
c
      SUBROUTINE PLOTDB3(cntfill,chfill)
C----------------------------------------------------------------------
C  This subroutine calls the necessary routines to plot a file of DB values
C  that were collected on a previous run ( or the average/maximum  of several
C  runs)  of NAPS.  This archived file was named "DECIBEL.DAT"  by default 
C  or it was renamed by the user to a more meaningful name and stored for 
C  future  plotting or averaging.
c  --- Plotting is by interpolation to a cartesian coordinate system and
c  --- using NCAR graphics routines for the contouring.  Background map
c  --- is the world vector shoreline (wvs) data.  If input logical 
c  --- parameter cntfill=true, then contours are colorfilled.  If chfill=
c  --- true background map is colorfilled green/blue.
C----------------------------------------------------------------------
      implicit none
      logical cntfill,chfill
c-----------------------------------------------------------------------
      INTEGER isnd, ICHOICE, ISITE, IOPT, ICONTR, nsnds, nlevels,
     1        DBPLOTOPT
      REAL    PSIMIN, PSIMAX, LATS, LONS, LATB, LONB, input_delpsi,
     1        xminp,xmaxp,yminp,ymaxp
      LOGICAL DOELV, DOATT, white_backgrnd
      CHARACTER*60 snd_label
      COMMON  /INPUTS/ snd_label, ICHOICE, ISITE, IOPT, ICONTR, PSIMIN,
     1        PSIMAX, LATS, LONS, LATB, LONB, input_delpsi, isnd, DOELV,
     2        DOATT, white_backgrnd, xminp,xmaxp,yminp,ymaxp, nsnds,
     3        nlevels, DBPLOTOPT
c-----------------------------------------------------------------------
      CHARACTER*60 DBFLNAM
      COMMON  /DBFILE/ DBFLNAM
      integer KI,KT,MT1,MT2,MT3,MT4,MT8
      COMMON  /LUNT/ KI,KT,MT1,MT2,MT3,MT4,MT8
      CHARACTER METLABEL*80, FILLABEL*80, SITE*10
      COMMON /IDTAG/  METLABEL, FILLABEL, SITE
c-----------------------------------------------------------------------
      real    A,CONTRS,RMAX,DELPSI,RRATIO
      integer M,N,IRAMAX,NRO,NCONTS
      COMMON /SOUND/ A(751,361),M,N,CONTRS(51),RMAX,DELPSI,IRAMAX,
     &               NRO,NCONTS,RRATIO
c-----------------------------------------------------------------------
      INTEGER IGUN
      REAL    XB,YB,ZB,WB,SFPRES,DIRANG,ZBAG
      COMMON  /SOURCE/ XB,YB,ZB,WB,SFPRES,DIRANG,IGUN,ZBAG
c-----------------------------------------------------------------------
      real    XFAC,YFAC,DELRES,RXL,RYL,RXH,RYH,X1,Y1,X2,Y2,
     1  UB,UT,UR,UL,TRXB,TRYB
      COMMON /MAPCOR/ XFAC,YFAC,DELRES,RXL,RYL,RXH,RYH,X1,Y1,X2,Y2,
     1  UB,UT,UR,UL,TRXB,TRYB
c-----------------------------------------------------------------------
      integer ndelsi, nranges, nray
      real    Aza, rangea, dBa, xray, yray
      common /savea/  ndelsi, nranges(361), Aza(361), rangea(751,361),
     1                dBa(751,361), xray(751,101), yray(751,101), nray
      COMMON  /FLAGS/ DBFLG
c-----------------------------------------------------------------------
      integer ICLORS
      COMMON /CLRMAP/ ICLORS(20)
c-----------------------------------------------------------------------
      integer ncdir,nodir,nmdir 
      CHARACTER*60 DATADIR,OUTDIR,METDIR,FILELV,FILEAT,FILMET,FILSUM,
     1  FILOUT,met_dsn
      COMMON /filenm/ DATADIR,OUTDIR,METDIR,FILELV,FILEAT,FILMET,FILSUM,
     1  FILOUT,met_dsn,ncdir,nodir,nmdir
      LOGICAL DBFLG
      integer i, j, ix, jy, i1, i2, ls, ier, ic, ipt1, ipt2
      integer iset, ihi, idash, ncl,NP
      real    latmax, latmin, lonmax, lonmin
      real    dBamax, dBamin, xlon1,xlon2,ylat1,ylat2,t1x,t2x,t3x,
     1        t1y,t2y, t3y, phi, cosphi, dxg, dyg, dBxymax, dBxymin,
     2        XN, delx, dely, rsq, r, rl, theta,psi1, psi2, Azj, r1, r2,
     3        drange, dB1, dB2, dBdpsi, cmx, cmn, cnt,UCL,UCR,UCB,UCT,
     4        LXG, LYG, UV1(3), UVB(3), XBT, YBT, dist
      real    xx1,xX2,yY1,yY2,xUL,xUR,yUB,yUT,px(1),py(1)
c     --- NCAR graphics fill routines declarations
      external fillc
      integer nxg,nyg
      integer lwrk,liwk
      integer lmap
      integer nwrk, ngrps
c     parameter (nxg=201, nyg=161)
      parameter (nxg=201, nyg=201)
      parameter (lwrk=2500, liwk=1000)
      parameter (lmap=500000)
      parameter (nwrk=250000,ngrps=100)
      real    rwrk(lwrk)
      integer iwrk(liwk)
      integer map(lmap)
      integer iarea(ngrps), igrp(ngrps)
      real    xwrk(nwrk), ywrk(nwrk)
c-----------------------------------------------------------------------
      real    xg(nxg), yg(nyg), dBxy(nxg,nyg)
      REAL*8  REARTH, DEGREES_PER_RADIAN
      PARAMETER (REARTH = 6370.999D3)
      PARAMETER (DEGREES_PER_RADIAN = 57.29577951308232D0 )
c-----------------------------------------------------------------------
c     print *,'in PLOTDB3:cntfill,chfill=',cntfill,chfill
c
c     --- read in the countor levels from the dbcolor file
      CALL LEVELQ(1)
c
c     --- Initializations
      latmax=39.6
      latmin=39.1
      lonmax=-75.8
      lonmin=-76.6
c
c     --- set viewport parameters
      ipt1=1012
      ipt2=ipt1-50
      t1x=.10
      t1y=.10
      t3y=1.
      phi=0.5*(latmax+latmin)
      cosphi=cos(3.14159*phi/180.)
      rl=ABS(lonmax-lonmin)/(latmax-latmin)
      t3x=1.*cosphi*rl
      if(t3x.gt.1.) then
        t3y=1./t3x
        t3x=1.
      endif
c     t2x=t1x+.75*t3x
c     t2y=t1y+.75*t3y
      t2x=t1x+.80*t3x
      t2y=t1y+.80*t3y
      xmaxp=lonmax
      xminp=lonmin
      ymaxp=latmax
      yminp=latmin
      ls=1
      call set(t1x,t2x,t1y,t2y,xminp,xmaxp,yminp,ymaxp,ls)
c     print *,'initial call to set:'
c     print *,'t1x,t2x,t1y,t2y,xminp,xmaxp,yminp,ymaxp,ls=',
c    1         t1x,t2x,t1y,t2y,xminp,xmaxp,yminp,ymaxp,ls
c
      call setusv('LW',1000)
      dBamax = -1.0E30
      dBamin = +1.0E30
      do j=1,ndelsi
        do i=1,nranges(j)
          dBamax = MAX(dBamax,dBa(i,j))
          dBamin = MIN(dBamin,dBa(i,j))
        enddo
      enddo
c     print *,'in PLOTDB3: dBamax,dBamin=',dBamax,dBamin
c     --- build a rectangular grid within the plot frame 
c     --- from the input polar one
c
c     --- save the viewport parameters
      X1=t1x
      X2=t2x
      Y1=t1y
      Y2=t2y
      xlon1=xminp
      xlon2=xmaxp
      ylat1=yminp
      ylat2=ymaxp
      UL=xlon1
      UR=xlon2
      UB=ylat1
      UT=ylat2
c
c     --- get the grid coordinates of the blast center
      call defuvp(ylat1, xlon1, UV1)
      call defuvp(LATB, LONB, UVB)
      call abetuv(UV1,UVB,theta)
      dist=REARTH*theta
      call comphd(UV1, UVB, psi1, ier)
      XBT = dist*SIN(psi1/DEGREES_PER_RADIAN)
      YBT = dist*COS(psi1/DEGREES_PER_RADIAN)
c     print *,'XBT,YBT=',XBT,YBT
c     CALL endpt(ylat1, xlon1, zbag, psi1, DIST, LATBT, LONBT)
c     print *,'       LATB,LONB=',LATB,LONB
c     print *,'check: LATB,LONB=',LATBT,LONBT
c 
c     --- fit a nxgXnyg grid into the frame (use flat earth approx)
      LYG=REARTH*ABS(ylat2-ylat1)/DEGREES_PER_RADIAN
      COSPHI = COS(LATB/DEGREES_PER_RADIAN)
      LXG=REARTH*COSPHI*ABS(xlon2-xlon1)/DEGREES_PER_RADIAN
      dxg = LXG/FLOAT(nxg-1)  ! m
      dyg = LYG/FLOAT(nyg-1)  ! m
      do ix=1,nxg
        xg(ix)=(ix-1)*dxg
      enddo
      do jy=1,nyg
        yg(jy)=(jy-1)*dyg
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
c     --- interpolate polar grid to equally spaced Cartesian grid
      do ix=1,nxg
        do jy=1,nyg
          dBxy(ix,jy) = -1.
          delx = xg(ix)-XBT
          dely = yg(jy)-YBT
          rsq = delx**2 + dely**2
          r = SQRT(rsq)
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
c               --- grid is outside range of data so set value to 0
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
c          print *,'ix,jy,r,theta,dB=',ix,jy,r,Azj,dBxy(ix,jy)
          dBxymax = MAX(dBxy(ix,jy),dBxymax)
          dBxymin = MIN(dBxy(ix,jy),dBxymin)
        enddo
      enddo
c     print *,'in PLOTDB3: dBxymax,dBxymin=',dBxymax,dBxymin
c     do j=1,nyg
c       write(kt,*) (dBxy(i,j),i=1,nxg)
c     enddo
c
c     --- use ncar graphics contouring facility
      UCL=1.0
      UCR=FLOAT(nxg)
      UCB=1.0
      UCT=FLOAT(nyg)
c     print *,'calling set: XL,XR,YB,YT=',UCL,UCR,UCB,UCT
c     print *,'             X1,X2,Y1,Y2=',X1,X2,Y1,Y2
      call set(X1,X2,Y1,Y2,UCL,UCR,UCB,UCT,ls)
      if(white_backgrnd) then
        CALL COLOR(1)    ! black text
        CALL GSPLCI(1)   ! black lines
      else
        CALL COLOR(0)    ! white text
        CALL GSPLCI(0)   ! white lines
      endif
c
      iset=1  ! viewport already set
      ihi=-1  ! don't label data or show H or L
      idash = -155  ! solid for + values, dashed for - values
      idash = +1  ! solid
      call cpsetc('ILT',' ') ! suppress info label
      call cpseti('LLP',0)   ! suppress contour labels
      call cpsetr('CIS',cnt)   ! contour interval specifier
      call cpsetr('CMN',cmn)   ! contour minimum
      call cpsetr('CMX',cmx)   ! contour maximum
      call cpseti('CLS',ncl)   ! draw ncl contours
c     call cpseti('CLS',-10)   ! draw ncl contours
      call cpseti('LIS',1)     ! label interval specifier
      call cpseti('SET',iset)  ! call set flag - 1=no
c########  contours lines
      if(.not.cntfill) then
c       if(white_backgrnd) then
c         --- flush the background white
c         call wflush
c       endif
c       --- Draw in background map
        call CHMAP(latmin,latmax,lonmin,lonmax,chfill)
c       --- draw contours lines each of a color specified in the 
c       --- dbcolor file
        CALL GSLN(1)      ! 1=solid
        CALL GSPLCI(ICLORS(1))
        call setusv('LW',2000)
        cmn = CONTRS(1)
        cnt = CONTRS(2)-CONTRS(1)
        cmx = cmn + cnt/2.
c       print *,'plotting contour=',cmn
        call cpcnrc(dBxy,nxg,nxg,nyg,cmn,cmx,cnt,iset,ihi,idash)
        do i=2,NCONTS
c         --- merge contours with same color
          if(iclors(i).ne.iclors(i-1)) then
            CALL GSPLCI(ICLORS(i))
            call setusv('LW',2000)
            cmn = CONTRS(i)
            cnt = CONTRS(2)-CONTRS(1)
            cmx = cmn + cnt/2.
            call cpcnrc(dBxy,nxg,nxg,nyg,cmn,cmx,cnt,iset,ihi,idash)
          endif
        enddo
        call setusv('LW',1000)
c######### contour fill
      else
c       --- Draw in background map
        if(chfill) call CHMAP(latmin,latmax,lonmin,lonmax,chfill)
c       --- use input contour interval, max,min
        cmx = CONTRS(NCONTS)
        cmn = CONTRS(1)
        cnt = CONTRS(2)-CONTRS(1)
        ncl = NINT((cmx-cmn)/cnt)+1
c       print *,'cmx,cmn,cnt,ncl=',cmx,cmn,cnt,ncl
        call arinam(map,lmap)
c       print *,'in PLOTDB3: calling cprect: nxg,nyg=',nxg,nyg
        call cprect(dBxy,nxg,nxg,nyg,rwrk,lwrk,iwrk,liwk)
        call gsln(1)      ! 1=solid
        call setusv('LW',1000)   ! light lines
        call getset(xx1,xX2,yY1,yY2,xUL,xUR,yUB,yUT,ls)
c      print *,'in PLOTDB3: after getset: X1,X2,Y1,Y2,UL,UR,UB,UT=',
c    1           xx1,xX2,yY1,yY2,xUL,xUR,yUB,yUT
c     print *,'calling set: XL,XR,YB,YT=',UCL,UCR,UCB,UCT
c     print *,'             X1,X2,Y1,Y2=',X1,X2,Y1,Y2
        call set(X1,X2,Y1,Y2,UCL,UCR,UCB,UCT,ls)
c       call getset(xx1,xX2,yY1,yY2,xUL,xUR,yUB,yUT,ls)
c       print *,'in PLOTDB3: after getset: X1,X2,Y1,Y2,UL,UR,UB,UT=',
c    1            xx1,xX2,yY1,yY2,xUL,xUR,yUB,yUT
c       print *,'in PLOTDB3: calling cpcnrc: cmn,cmx,cnt=',cmn,cmx,cnt
        call cpcnrc(dBxy,nxg,nxg,nyg,cmn,cmx,cnt,iset,ihi,idash)
        call cpclam(dBxy,rwrk,iwrk,map)
        call arscam(map, xwrk, ywrk, nwrk, iarea, igrp, ngrps, fillc)
        call setusv('LW',1000)   ! light lines
c
c set contours to black
        CALL GSPLCI(0)   ! black lines
        call cpcnrc(dBxy,nxg,nxg,nyg,cmn,cmx,cnt,iset,ihi,idash)
      endif
      DBFLG = .FALSE.
c
c     --- reset the viewport to lat,lon coordinates
      call getset(xx1,xX2,yY1,yY2,xUL,xUR,yUB,yUT,ls)
c      print *,'in PLOTDB3: after getset: X1,X2,Y1,Y2,UL,UR,UB,UT=',
c    1           xx1,xX2,yY1,yY2,xUL,xUR,yUB,yUT
      call set(X1,X2,Y1,Y2,UL,UR,UB,UT,ls)
c     print *,'calling set: UL,UR,UB,UT=',UL,UR,UB,UT
c     print *,'             X1,X2,Y1,Y2=',X1,X2,Y1,Y2
c     --- Draw in background map
      if(cntfill.and.(.NOT.chfill)) 
     1  call CHMAP(latmin,latmax,lonmin,lonmax,chfill)
c
c     --- Place a mark at the blast site.
      call gaseti('LTY',1)  ! use PLCHHQ to draw labels
      call pcseti('FN',21)  ! font 21
      call gsln(1)     ! 1=solid
      call gsfaci(0)   ! black text
      call gslwsc(5.)  ! 5X line width
      call gsmksc(2.)  ! 2X size
      call gspmci(0)   ! black text
      call gstxfp(-12,2) ! font 12, precision 2
      px(1)=LONB
      py(1)=LATB
      NP=1
      call points(px,py,NP,-2,0)  ! -2=+
c     --- Place a mark at the sonding location
      px(1)=LONS
      py(1)=LATS
      NP=1
      call points(px,py,NP,-3,0)  ! -3=*
c
c     --- Draw in the microphone locations
      call MIKES3(.false.)   ! false=no colorfill
c
c     --- label the plot
      CALL GETLABEL3
c
      return
      end
c
      subroutine CHMAP(latmin,latmax,lonmin,lonmax,chfill)
c     --- Draws the background map of the Aberdeen and Chesapeake Bay
c     --- areas.  chfill is input logical that specifies if
c     --- the map is to be color filled.
      implicit none
      real    latmax, latmin, lonmax, lonmin
      logical chfill
c
      integer namax, nmax
      parameter (namax=40 , nmax=4000)
      real    alat(namax,nmax), alon(namax,nmax)
      real    ylat(nmax), xlon(nmax)
      integer npa(namax)
      integer i, iread, imax, ios, j, k, lc, N, nblk, nlines,
     1        np, na, nt, k1,k2, kk, idi, idb, ls
      integer nfill, idfill(namax), ifl, icbay
      character*10 text
      character*80 record
      real    cpux, cpuy
      real    axw,lnw,mjw,mnw,majl,minl, r, rl, latk, lonk, xl,yl
      real    blklatmax, blklatmin, blklonmax, blklonmin, hchar
      real    xx1,xX2,yY1,yY2,xUL,xUR,yUB,yUT
      integer mjrx, mjry, mnrx, mnry
      real    line(8)
      character*60 cname(namax)
      logical balt_fill
      parameter (balt_fill=.false.)
c-----------------------------------------------------------------------
      logical land_fill, water_fill
      integer id,ibalt
      common  /fill/ id,ibalt,land_fill, water_fill
c-----------------------------------------------------------------------
      INTEGER isnd, ICHOICE, ISITE, IOPT, ICONTR, nsnds, nlevels,
     1        DBPLOTOPT
      REAL    PSIMIN, PSIMAX, LATS, LONS, LATB, LONB, input_delpsi,
     1        xminp,xmaxp,yminp,ymaxp
      LOGICAL DOELV, DOATT, white_backgrnd
      CHARACTER*60 snd_label
      COMMON  /INPUTS/ snd_label, ICHOICE, ISITE, IOPT, ICONTR, PSIMIN,
     1        PSIMAX, LATS, LONS, LATB, LONB, input_delpsi, isnd, DOELV,
     2        DOATT, white_backgrnd, xminp,xmaxp,yminp,ymaxp, nsnds,
     3        nlevels, DBPLOTOPT
c-----------------------------------------------------------------------
      integer ncdir,nodir,nmdir 
      CHARACTER*60 DATADIR,OUTDIR,METDIR,FILELV,FILEAT,FILMET,FILSUM,
     1  FILOUT,met_dsn
      COMMON /filenm/ DATADIR,OUTDIR,METDIR,FILELV,FILEAT,FILMET,FILSUM,
     1  FILOUT,met_dsn,ncdir,nodir,nmdir
c-----------------------------------------------------------------------
      real    XFAC,YFAC,DELRES,RXL,RYL,RXH,RYH,X1,Y1,X2,Y2,
     1  UB,UT,UR,UL,TRXB,TRYB
      COMMON /MAPCOR/ XFAC,YFAC,DELRES,RXL,RYL,RXH,RYH,X1,Y1,X2,Y2,
     1  UB,UT,UR,UL,TRXB,TRYB
c-----------------------------------------------------------------------
c     --- NCAR graphics fill routines declarations
      external fillm
      integer igroup, idleft, idright, ngroups
      integer lmap
      integer nwrk, ngrps
      parameter (lmap=500000)
      parameter (nwrk=250000,ngrps=10)
      integer map(lmap)
      integer iarea(ngrps), igrp(ngrps)
      real    xwrk(nwrk), ywrk(nwrk)
c-----------------------------------------------------------------------
c     
c     print *,'enter CHMAP'
c     --- save viewport parameters and set viewport to map coordinates
      call getset(xx1,xX2,yY1,yY2,xUL,xUR,yUB,yUT,ls)
      call set(X1,X2,Y1,Y2,UL,UR,UB,UT,ls)
c
c     --- initializations
      if(white_backgrnd) then
        lc=0
      else
        lc=1
      endif
      CALL COLOR(lc)
      do id=1,namax
        idfill(id)=0
      enddo
c
c     --- read in the bay, state and county boundaries
      na=0
      open(unit=5, file=datadir(1:ncdir)//'mdwvs_county.dat',
     1   access='sequential',form='formatted',iostat=ios)
      if(ios.ne.0) then
        print *,'error opening aberdeen file'
        stop
      endif
      nblk=0
      do iread=1,200
        read(5,199,end=21) record
        nblk=nblk+1
        CALL shftlft(record)
c       print *,'reading nblk=',nblk
c       print *,record(1:77)
        N = INDEX(record,' ') - 1
        np=0
        read(record(1:N), '(I30)', IOSTAT = IOS) np
        nlines=np/8
        if((np/8)*8 .ne. np) nlines=nlines+1
        record = record(N+1: )
        CALL shftlft(record)
        N = INDEX(record,' ') - 1
        blklatmin=0.
        read(record(1:N), '(E30.0)', IOSTAT = IOS) blklatmin
        record = record(N+1: )
        CALL shftlft(record)
        N = INDEX(record,' ') - 1
        blklatmax=0.
        read(record(1:N), '(E30.0)', IOSTAT = IOS) blklatmax
        record = record(N+1: )
        CALL shftlft(record)
        N = INDEX(record,' ') - 1
        blklonmin=0.
        read(record(1:N), '(E30.0)', IOSTAT = IOS) blklonmin
        record = record(N+1: )
        CALL shftlft(record)
        N = INDEX(record,' ') - 1
        blklonmax=0.
        read(record(1:N), '(E30.0)', IOSTAT = IOS) blklonmax
        record = record(N+1: )
        CALL shftlft(record)
c       --- filter block.  Use it if any corner point is within frame
c       if(((blklatmin.gt.latmin).and.(blklatmin.lt.latmax).and.
c    1      (blklonmin.gt.lonmin).and.(blklonmin.lt.lonmax)).or.
c    2     ((blklatmin.gt.latmin).and.(blklatmin.lt.latmax).and.
c    3      (blklonmax.gt.lonmin).and.(blklonmax.lt.lonmax)).or.
c    4     ((blklatmax.gt.latmin).and.(blklatmax.lt.latmax).and.
c    5      (blklonmax.gt.lonmin).and.(blklonmax.lt.lonmax)).or.
c    6     ((blklatmax.gt.latmin).and.(blklatmax.lt.latmax).and.
c    7      (blklonmin.gt.lonmin).and.(blklonmin.lt.lonmax)) )
c    8  then
c         --- a corner point is within boundaries so read in the data
          na=na+1
          if(na.gt.namax) then
            print *,'array bounds exceeded for temp arrays'
            stop
          endif
          cname(na)=record
          k=0
          do j=1,nlines
            read(5,111,end=21) (line(i),i=1,8)
            imax=4
            if(j.eq.nlines) then
              r=FLOAT(np)/8
              r=nlines-r
              imax=NINT(4*(1-r))
            endif
            do i=1,imax
              k=k+1
              npa(na)=k
              alat(na,k)=line(2*i-1)
              alon(na,k)=line(2*i)
            enddo
          enddo
      enddo
   21 continue
      close(unit=5)
c     print *,'after reading in aberdeen bdys: nblk,na=',nblk,na
  111 format(4(4X,F6.3,2X,F8.3))
  199 format(A80)
      do id=1,na
c       print *,id,cname(id)
      enddo
      if(white_backgrnd) then
        CALL COLOR(1)    ! black text
        CALL GSPLCI(1)   ! black lines
c       call wflush
      else
        CALL COLOR(0)    ! white text
        CALL GSPLCI(0)   ! white lines
      endif
c
      if(.not.chfill) go to 200
c
c     --- assign areas and groups.  group=1 is appropriate for geographic
c     --- boundaries
      call arinam(map,lmap)
      igroup=1
      ngroups=1
c     --- assign Chesapeake Bay
      do id=1,na
        record=cname(id)
        if(record(1:14).eq.'Chesapeake Bay') then
          icbay=id
c         print *,'Chesapeake Bay name found: id=',id
          go to 22
        endif
      enddo
      print *,'error: Chesapeake Bay name not found'
      stop
   22 continue
      land_fill=.true.
      water_fill=.true.
      id=icbay
      np=npa(id)
c     print *,'id=',icbay
      nfill=1
      idfill(nfill)=id
      k1=0
      k2=0
      do kk=1,np
        latk=alat(id,kk)
        lonk=alon(id,kk)
        if(latk.gt.yminp) then
          if(k1.eq.0) then
            k1=kk
          endif
          ylat(kk-k1+1)=latk
          xlon(kk-k1+1)=lonk
        endif
        if((latk.lt.yminp).and.(k1.gt.0)) then
          k2=kk
        endif
c         print *,'i,k,lon,lat=',i,k,xlon(k),ylat(k)
      enddo
      np=k2+1
      ylat(np)=ylat(1)
      xlon(np)=xlon(1)
c     --- assign area 2 to the bay (2 to left (water)),
c     --- 3 to right (land))
      idleft=2
      idright=3
      call aredam(map,xlon,ylat,np,igroup,idleft,idright)
c     --- preprocess to shorten segments and remove overlapping segments
c     call arprmap(map,0,0,0)
      call gsfais(1)  ! solid fill
      call arscam(map,xwrk,ywrk,nwrk,iarea,igrp,ngroups,fillm)
c
c     --- overlay Baltimore in orange
      do id=1,na
        record=cname(id)
        if(record(1:21).eq.'Baltimore county line') then
          ibalt=id
          go to 32
        endif
      enddo
      print *,'error: Baltimore county line name not found'
      stop
   32 continue
      id=ibalt
      if(balt_fill) then
        land_fill=.true.
        water_fill=.false.
        np=npa(id)
        nfill=nfill+1
        idfill(nfill)=id
        do k=1,np
          ylat(k)=alat(id,k)
          xlon(k)=alon(id,k)
        enddo
c       --- assign area 3 to land (to left) and area 2 to water (right)
        call arinam(map,lmap)
        idleft=3
        idright=2
        call aredam(map,xlon,ylat,np,igroup,idleft,idright)
c       --- preprocess to shorten segments and remove overlapping segments
c       call arprmap(map,0,0,0)
        call gsfais(1)  ! solid fill
        call arscam(map,xwrk,ywrk,nwrk,iarea,igrp,ngroups,fillm)
      else
        npa(ibalt)=13
      endif
c
c     --- overlay Delaware Bay
      do id=1,na
        record=cname(id)
        if(record(1:12).eq.'Delaware Bay') then
          idb=id
c         print *,'Delaware Bay name found: id=',id
          go to 42
        endif
      enddo
      print *,'error: Delaware Bay name not found'
      stop
   42 continue
      land_fill=.false.
      water_fill=.true.
      id=idb
      np=npa(id)
      nfill=nfill+1
      idfill(nfill)=id
      do k=1,np
        ylat(k)=alat(id,k)
        xlon(k)=alon(id,k)
      enddo
      np=np+1
      ylat(np)=ylat(1)
      xlon(np)=xlon(1)
c     --- assign area 3 to land (to left) and area 2 to water (right)
      call arinam(map,lmap)
      idleft=3
      idright=2
      call aredam(map,xlon,ylat,np,igroup,idleft,idright)
c     --- preprocess to shorten segments and remove overlapping segments
c     call arprmap(map,0,0,0)
      call gsfais(1)  ! solid fill
      call arscam(map,xwrk,ywrk,nwrk,iarea,igrp,ngroups,fillm)
c
c     --- overlay the islands
c     id=20  ! Spesuite Island
c     id=21  ! Miller Island
c     id=22  ! Pooles Island
c     id=23  ! Kent Island
      do id=1,na
        record=cname(id)
        CALL shftlft(record)
        N = INDEX(record,' ') - 1
        record = record(N+1: )
        CALL shftlft(record)
        if(record(1:6).eq.'Island') then
          idi=id
          land_fill=.true.
          water_fill=.false.
          np=npa(idi)
c         print *,'Island id,np=',idi,np
c         print *,'name=',cname(idi)
          nfill=nfill+1
          idfill(nfill)=idi
          do k=1,np
            ylat(k)=alat(idi,k)
            xlon(k)=alon(idi,k)
          enddo
c         --- assign area 3 to the bay (3 to left (land)),
c         --- 2 to right (water)
          call arinam(map,lmap)
          idleft=3
          idright=2
          call aredam(map,xlon,ylat,np,igroup,idleft,idright)
c         --- preprocess to shorten segments and remove overlapping segments
c         call arprmap(map,0,0,0)
          call gsfais(1)  ! solid fill
          call arscam(map,xwrk,ywrk,nwrk,iarea,igrp,ngroups,fillm)
        endif
      enddo
  200 continue
c
c     --- plot lines
      do id=1,na
c       print *,'in CHMAP, plotting lines, id=',id
c       --- don't outline previously filled water boundaries
        do ifl=1,nfill
          if(idfill(ifl).eq.id) go to 33
        enddo
        np=npa(id)
        do k=1,np
          ylat(k)=alat(id,k)
          xlon(k)=alon(id,k)
          ylat(k)=AMIN1(ylat(k),ymaxp)
          ylat(k)=AMAX1(ylat(k),yminp)
          xlon(k)=AMIN1(xlon(k),xmaxp)
          xlon(k)=AMAX1(xlon(k),xminp)
c         print *,'i,k,lon,lat=',id,k,xlon(k),ylat(k)
        enddo
c       --- plot and label curves
        if(chfill) then
          if((id.eq.4).or.(id.eq.8)) then  ! rivers
c           call gsln(1)     ! solid lines
            call dashdc('$$',20,20)  ! solid
            call gslwsc(3.)
            call gsplci(16)   ! blue lines
          else
            call gsplci(1)   ! white
            if((id.eq.1).or.(id.eq.2).or.(id.eq.3)) then  ! state lines
              call gslwsc(3.)
              call dashdc('$$$$$$$$$$$$''''''$''''''',2,0)  ! dashed-dotted
            else
              call gslwsc(2.)
              call dashdc('$$''''''''',2,0)  ! dotted
            endif
          endif
        else
          call gsplci(0)   ! black
          call gslwsc(1.)
        endif
        call curved(xlon,ylat,np)
   33   continue
      enddo
c
c     --- draw the frame
c     call gsln(1)  ! solid lines
c     call dashdb(3)  ! solid 
      call dashdc('$$',20,20)  ! solid
      rl=ABS(lonmax-lonmin)*10.  ! 10=ticks marcs at 1/10 deg
      mjrx=NINT(rl)
      mjry=NINT((latmax-latmin)*10)
c     print *,'mjrx,mjry=',mjrx,mjry
      mnrx=0
      mnry=0
      call gaseti('LTY',1)
      call pcseti('FN',21)
c     axw=4.
c     lnw=4.
c     mjw=4.
c     mnw=4.
c     call gasetr('WAX',axw)
c     call gasetr('WLB',lnw)
c     call gasetr('WMJ',mjw)
c     call gasetr('WMN',mnw)
c     print *,'axw,lnw,mjw,mnw=',axw,lnw,mjw,mnw
c     majl=15.
c     call gasetr('XMJ',majl)
c     call gasetr('YMJ',majl)
c     print *,'majl,minl=',majl,minl
      call gsfaci(0)   ! black text
      CALL GSPLCI(0)   ! black lines
c     call labmod('(f5.1)','(f5.1)',5,5,3,3,20,20,0)
c     call periml(mjrx,mnrx,mjry,mnry)
      call gridal(mjrx,mnrx,mjry,mnry,0,0,5,0.,0.)
c     --- attach title
c     call plchhq(cpux(512),cpuy(ipt2),cname,0.030,0.,0.)
c     --- label ordinate
c     call plchhq(cpux(20),cpuy(512),'lat',0.030,90.,0.)
c     xl=cpux(120)
      xl=UL
      yl=latmin
      write(text,230) yl
  230 FORMAT(F5.1)
      call shftlft(text)
      nt=index(text,' ')
      text=text(1:nt-1)//'N'
      hchar=0.018  ! character height
      xl=xl-(hchar/2.)*(UR-UL) ! 1/2 Char ht to left of frame
      yl=yl+(.7*hchar)*(UT-UB) ! .7 char ht up
      call plchhq(xl,yl,text(1:nt),hchar,0.,1.)
      yl=latmax
      write(text,230) yl
      call shftlft(text)
      nt=index(text,' ')
      text=text(1:nt-1)//'N'
      call plchhq(xl,yl,text(1:nt),hchar,0.,1.)
c     --- label abscissa
c     call plchhq(cpux(512),cpuy(60),'lon',0.030,0.,0.)
      yl=UB-1.6*hchar*(UT-UB) ! 1.6Xchar height below frame
      xl=ABS(lonmin)
      write(text,230) xl
      call shftlft(text)
      nt=index(text,' ')
      text=text(1:nt-1)//'W'
      call plchhq(lonmin,yl,text(1:nt),hchar,0.,0.)
      xl=ABS(lonmax)
      write(text,230) xl
      call shftlft(text)
      nt=index(text,' ')
      text=text(1:nt-1)//'W'
      call plchhq(lonmax,yl,text(1:nt),hchar,0.,0.)
c
c     --- draw the APG boundary
      call bounds3
c
c     --- restore viewport parameters
      call set(xx1,xX2,yY1,yY2,xUL,xUR,yUB,yUT,ls)
c
      return
      end
c
      subroutine fillm(xwrk, ywrk, nwrk, iarea, igrp, ngrps)
c     --- Fills background map.
c     --- This routine is required by ncar graphics to assign areas
c     --- and colors to fill.
      implicit none
c-----------------------------------------------------------------------
      integer ngrps, nwrk
      integer iarea(*), igrp(*)
      real    xwrk(*), ywrk(*)
      real    x1,x2,y1,y2,UL,UR,UB,UT
      integer ifill, j, icall,ls
c-----------------------------------------------------------------------
      INTEGER isnd, ICHOICE, ISITE, IOPT, ICONTR, nsnds, nlevels,
     1        DBPLOTOPT
      REAL    PSIMIN, PSIMAX, LATS, LONS, LATB, LONB, input_delpsi,
     1        xminp,xmaxp,yminp,ymaxp
      LOGICAL DOELV, DOATT, white_backgrnd
      CHARACTER*60 snd_label
      COMMON  /INPUTS/ snd_label, ICHOICE, ISITE, IOPT, ICONTR, PSIMIN,
     1        PSIMAX, LATS, LONS, LATB, LONB, input_delpsi, isnd, DOELV,
     2        DOATT, white_backgrnd, xminp,xmaxp,yminp,ymaxp, nsnds,
     3        nlevels, DBPLOTOPT
c-----------------------------------------------------------------------
      integer ICLORS
      COMMON /CLRMAP/ ICLORS(20)
c-----------------------------------------------------------------------
      logical land_fill, water_fill
      integer id,ibalt
      common  /fill/ id,ibalt,land_fill, water_fill
c-----------------------------------------------------------------------
      SAVE icall
      DATA icall/0/
c-----------------------------------------------------------------------
c
      icall=icall+1
c     print *,'in fillm: ngrps=',ngrps
c     print *,'in fillm: white_backgrnd=',white_backgrnd
c     print *,'in fillm: icall=',icall
c     print *,'in fillm: land_fill, water_fill=',land_fill,water_fill
c     print *,'in fillm: id,ibalt=',id,ibalt
      call getset(x1,x2,y1,y2,UL,UR,UB,UT,ls)
c     print *,'in fillm getset parameters: X1,X2,Y1,Y2,UL,UR,UB,UT=',
c    1            x1,x2,y1,y2,UL,UR,UB,UT
c
      ifill=1
c     ---- If any area identifier is le 1 don't fill
      do j=1,ngrps
        if(iarea(j).le.1) ifill=0
c       print *,'j,iarea(j),igrp(j),ifill=',j,iarea(j),igrp(j),ifill
      enddo
c     ---- Otherwise fill in the color according to its area identifier
c     ---- 2=blue, 3=green
      if(ifill.ne.0) then
c       --- first ensure all fill areas are inside frame
        do j=1,nwrk
c         print *,'j,xwrk,ywrk=',j,xwrk(j),ywrk(j)
          xwrk(j)=AMIN1(xwrk(j),UR)
          xwrk(j)=AMAX1(xwrk(j),UL)
          ywrk(j)=AMIN1(ywrk(j),UT)
          ywrk(j)=AMAX1(ywrk(j),UB)
        enddo
        ifill=0
        do j=1,ngrps
          if(igrp(j).eq.1) ifill=iarea(j)
        enddo
        if((ifill.eq.2).and.water_fill) then
c         print *,'calling blue fill'
          call gsfaci(16)   ! blue
c         call gsfaci(7)   ! blue
          call gfa(nwrk,xwrk,ywrk)
        elseif((ifill.eq.3).and.land_fill) then
          if(id.eq.ibalt) then
c           print *,'calling yellow fill'
            call gsfaci(6)   ! yellow
          else
c           print *,'calling green fill'
            call gsfaci(17)   ! green
          endif
          call gfa(nwrk,xwrk,ywrk)
        endif
      endif
      return
      end
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
      integer ifill, jfill
c-----------------------------------------------------------------------
      INTEGER isnd, ICHOICE, ISITE, IOPT, ICONTR, nsnds, nlevels,
     1        DBPLOTOPT
      REAL    PSIMIN, PSIMAX, LATS, LONS, LATB, LONB, input_delpsi,
     1        xminp,xmaxp,yminp,ymaxp
      LOGICAL DOELV, DOATT, white_backgrnd
      CHARACTER*60 snd_label
      COMMON  /INPUTS/ snd_label, ICHOICE, ISITE, IOPT, ICONTR, PSIMIN,
     1        PSIMAX, LATS, LONS, LATB, LONB, input_delpsi, isnd, DOELV,
     2        DOATT, white_backgrnd, xminp,xmaxp,yminp,ymaxp, nsnds,
     3        nlevels, DBPLOTOPT
c-----------------------------------------------------------------------
      real    A,CONTRS,RMAX,DELPSI,RRATIO
      integer M,N,IRAMAX,NRO,NCONTS
      COMMON /SOUND/ A(751,361),M,N,CONTRS(51),RMAX,DELPSI,IRAMAX,
     &               NRO,NCONTS,RRATIO
c-----------------------------------------------------------------------
      integer ICLORS
      COMMON /CLRMAP/ ICLORS(20)
c-----------------------------------------------------------------------
c
c     print *,'in fillc: ngrps=',ngrps
c     print *,'in fillc: ICLORS=',ICLORS
      jfill=1
c     ---- If any area identifier is negative don't fill
      do j=1,ngrps
        if(iarea(j).lt.0) jfill=0
c       print *,'j,iarea(j),igrp(j),jfill=',j,iarea(j),igrp(j),jfill
      enddo
c     ---- Otherwise fill in the color according to its area identifier
c     ---- relative to edge group 3 (contour lines)
c     call gsfais(3)  ! hatch fill
      call gsfais(1)  ! solid fill
c     call sfsetr('spacing',.005)
c     call sfsetr('angle',45.)
c     call setusv('LW',3000)
      if(jfill.ne.0) then
        ifill=0
        do j=1,ngrps
          if(igrp(j).eq.3) ifill=iarea(j)
        enddo
c       if(ifill.gt.1 .and. ifill.lt.NCONTS) then
        if(ifill.gt.1 .and. ifill.le.NCONTS) then
c         --- use for solid fill
          call gsfaci(ICLORS(ifill-1))   ! set color to fill
c         print *,'ifill,color to fill=',ifill-1,ICLORS(ifill-1)
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
c       if(white_backgrnd) then
c         call gsfaci(1)   ! white background
c         call gfa(nwrk-1,xwrk,ywrk)
c       endif
      endif
      return
      end
C
      SUBROUTINE GETLABEL3
C-----------------------------------------------------------------------
C  FUNCTION: This routine will put the color legend and the ID title on
C            the finished decibel contour graph.
C
C-----------------------------------------------------------------------
      implicit none
c-----------------------------------------------------------------------
      integer IGUN
      real    XB,YB,ZB,WB,SFPRES,DIRANG,ZBAG
      COMMON /SOURCE/ XB,YB,ZB,WB,SFPRES,DIRANG,IGUN,ZBAG
c-----------------------------------------------------------------------
      CHARACTER METLABEL*80, FILLABEL*80, SITE*10
      COMMON /IDTAG/  METLABEL, FILLABEL, SITE
c-----------------------------------------------------------------------
      integer ncdir,nodir,nmdir 
      CHARACTER*60 DATADIR,OUTDIR,METDIR,FILELV,FILEAT,FILMET,FILSUM,
     1  FILOUT,met_dsn
      COMMON /filenm/ DATADIR,OUTDIR,METDIR,FILELV,FILEAT,FILMET,FILSUM,
     1  FILOUT,met_dsn,ncdir,nodir,nmdir
c-----------------------------------------------------------------------
      real    A,CONTRS,RMAX,DELPSI,RRATIO
      integer M,N,IRAMAX,NRO,NCONTS
      COMMON /SOUND/ A(751,361),M,N,CONTRS(51),RMAX,DELPSI,IRAMAX,
     &               NRO,NCONTS,RRATIO
c-----------------------------------------------------------------------
      real    XFAC,YFAC,DELRES,RXL,RYL,RXH,RYH,X1,Y1,X2,Y2,
     1  UB,UT,UR,UL,TRXB,TRYB
      COMMON /MAPCOR/ XFAC,YFAC,DELRES,RXL,RYL,RXH,RYH,X1,Y1,X2,Y2,
     1  UB,UT,UR,UL,TRXB,TRYB
c-----------------------------------------------------------------------
      integer ICLORS
      COMMON /CLRMAP/ ICLORS(20)
c-----------------------------------------------------------------------
      INTEGER isnd, ICHOICE, ISITE, IOPT, ICONTR, nsnds, nlevels,
     1        DBPLOTOPT
      REAL    PSIMIN, PSIMAX, LATS, LONS, LATB, LONB, input_delpsi,
     1        xminp,xmaxp,yminp,ymaxp
      LOGICAL DOELV, DOATT, white_backgrnd
      CHARACTER*60 snd_label
      COMMON  /INPUTS/ snd_label, ICHOICE, ISITE, IOPT, ICONTR, PSIMIN,
     1        PSIMAX, LATS, LONS, LATB, LONB, input_delpsi, isnd, DOELV,
     2        DOATT, white_backgrnd, xminp,xmaxp,yminp,ymaxp, nsnds,
     3        nlevels, DBPLOTOPT
c-----------------------------------------------------------------------
      real    x,y,xleb,xreb,ybeb,yteb,ybl,dyb,xl,yl
      real    cpux,cpuy
      integer i,idy,iy,iy1,ihov,iftp,nbox,nlbs,j,lbab,LFIN(20)
      CHARACTER*25 TEXT
      CHARACTER*3 LLBS(20)
c-----------------------------------------------------------------------
c
      RXL = xminp
      RYL = yminp
      RXH = xmaxp
      RYH = ymaxp
      if(white_backgrnd) then
        CALL COLOR(0)    ! black text
        call gsfaci(0)   ! black text
      else
        CALL COLOR(1)    ! white text
        call gsfaci(1)   ! black text
      endif
c
      I   = NCONTS
      idy = 35
C Place a legend on the graph.
      x = cpux(50)
      iy1=950
      iy= iy1
      y = cpuy(iy)
      call plchhq(x,y,FILLABEL,0.017,0.,-1.)
      write(text,230) ZBAG
  230 FORMAT('BLAST HT =',F5.0)
      iy=iy-idy
      y = cpuy(iy)
      call plchhq(x,y,TEXT,0.017,0.,-1.)
      write(text,240) WB
  240 FORMAT('BLAST WT =',F5.0)
      iy=iy-idy
      y = cpuy(iy)
      call plchhq(x,y,TEXT,0.017,0.,-1.)
c
      x = cpux(600)
      iy= iy1-idy
      y = cpuy(iy)
      write(text,250) SITE
 250  FORMAT('SITE = ',A10)
      call plchhq(x,y,TEXT,0.017,0.,-1.)
      if((IGUN.ge.1).and.(IGUN.le.6)) then
        write(text,261) IGUN
  261   format('IGUN =',I2)
      else
        write(text,260) IGUN
  260   format('IGUN =',I2,' (uniform blast)')
      endif
      iy=iy-idy
      y = cpuy(iy)
      call plchhq(x,y,TEXT,0.017,0.,-1.)
c     --- Place sounding grid point location in lower rt corner
      x = cpux(500)
      y = cpuy(30)
      call plchhq(x,y,snd_label,0.015,0.,-1.)
c
c     --- draw a label bar
      call setusv('LW',1000)
      CALL COLOR(0)    ! black text
      CALL GSPLCI(1)   ! white lines
      call gsfaci(2)   ! black text
      j=1
      write(text,204) NINT(contrs(1))
      LFIN(1)=iclors(1)  ! color indices
      LLBS(1)=text
      do i=2,NCONTS
c       --- merge contours with same color
        if(iclors(i).ne.iclors(i-1)) then
          j=j+1
          write(text,204) NINT(contrs(i))
          LFIN(j)=iclors(i)  ! color indices
          LLBS(j)=text
c         print *,'i,contrs(i),LLBS(i)=',i,contrs(i),LLBS(i)
        endif
      enddo
 204  FORMAT(I3)
      ihov=1  ! 0=horizontal,1=vertical
      iftp=1  ! 0=softfill, otherwise solid fill according to LFIN
      nbox=j
      NLBS=nbox
      LBAB=0  ! 0=no label,1=label to right,2=to left,3=labels on both sides
      xleb=0.96
      xreb=0.995
      ybeb=0.5
      yteb=0.75
      call gsfais(1)  ! solid fill
      call lblbar(ihov,xleb,xreb,ybeb,yteb,nbox,1.0,1.0,LFIN,iftp,
     1  LLBS,NLBS,LBAB)
c     --- Label boxes
      dyb=(yteb-ybeb)/FLOAT(nbox)
      yl=ybeb+dyb/2.
      xl=UR+(xleb-X2)*(UR-UL)/(X2-X1)
      ybl=UB+(yl-Y1)*(UT-UB)/(Y2-Y1)
      dyb=dyb*(UT-UB)/(Y2-Y1)
c     print *,'UL,UR,UB,UT=',UL,UR,UB,UT
c     print *,'X1,X2,Y1,Y2=',X1,X2,Y1,Y2
c     print *,'xl,ybl,dyb=',xl,ybl,dyb
      call gsfaci(0)   ! black text
      call gspmci(0)   ! black text
      do j=1,nbox
        yl=(j-1)*dyb+ybl
c       print *,'j,xl,yl,LLBS=',j,xl,yl,LLBS(j)
        call plchhq(xl,yl,LLBS(j),0.015,0.,+1.)
      enddo
      call plchhq(xl,yl+dyb,'dB',0.015,0.,+1.)
c     x = cpux(950)
c     y = cpuy(890)
c     call wtstr(x,y,text,1,0,-1)
c     write(text,205) NINT(contrs(1))
c     y = cpuy(500)
c     call wtstr(x,y,text,1,0,-1)
 205  FORMAT(I3,' dB' )
c
      return
      end
c
      subroutine INITPLT
c-----------------------------------------------------------------------
      INTEGER isnd, ICHOICE, ISITE, IOPT, ICONTR, nsnds, nlevels,
     1        DBPLOTOPT
      REAL    PSIMIN, PSIMAX, LATS, LONS, LATB, LONB, input_delpsi,
     1        xminp,xmaxp,yminp,ymaxp
      LOGICAL DOELV, DOATT, white_backgrnd
      CHARACTER*60 snd_label
      COMMON  /INPUTS/ snd_label, ICHOICE, ISITE, IOPT, ICONTR, PSIMIN,
     1        PSIMAX, LATS, LONS, LATB, LONB, input_delpsi, isnd, DOELV,
     2        DOATT, white_backgrnd, xminp,xmaxp,yminp,ymaxp, nsnds,
     3        nlevels, DBPLOTOPT
c-----------------------------------------------------------------------
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
      CALL GSCR (1,2, 1.0,0.0, 0.0)  ! 2= red
      CALL GSCR (1,3, 1.0,0.12,0.0)  ! 3= red-red-orange
      CALL GSCR (1,4, 1.0,0.25,0.0)  ! 4= red-orange
      CALL GSCR (1,5, 1.0,0.50,.75)  ! 5= light pink
      CALL GSCR (1,6, 1.0,0.00,0.5)  ! 6= bright pink
c     CALL GSCR (1,7, 1.0,0.00,0.0)  ! 7= red
      CALL GSCR (1,7, 0.5,0.00,1.0)  ! 7= purple OK
      CALL GSCR (1,8, 1.0,0.35,0.0)  ! 8= red-orange
      CALL GSCR (1,9, 1.0,0.50,0.0)  ! 9= orange-read
      CALL GSCR (1,10, 1.0,0.63,0.0) !10= red-orange 
      CALL GSCR (1,11, 1.0,0.88,0.0) !11= red-orange 
      CALL GSCR (1,12,1.0,1.0, 0.0)  !12= yellow 
c     CALL GSCR (1,13,0.6,1.0, 0.0)  !
      CALL GSCR (1,13,0.10,0.1,1.0)  !13=dark blue
      CALL GSCR (1,14,0.0,1.0, 0.0)  !14= green
c     CALL GSCR (1,15,0.10,0.1,1.0)  !
c     CALL GSCR (1,15,0.5,0.0,0.5)   !15=purple
c     CALL GSCR (1,15,0.7,0.4,0.9)   !15=purple
      CALL GSCR (1,15,0.7,0.5,1.0)   !15=purple

c     --- use for background map
c                      rd   gr  bl
c   TK's light background
ctk   CALL GSCR (1,16,0.4,1.0,1.0) ! 16=light blue
      CALL GSCR (1,16,0.2,0.8,1.0) ! 16=light blue
corig CALL GSCR (1,16,0.0,0.7,1.0) ! 16=light blue
ctk   CALL GSCR (1,17,0.7,1.0,0.7) ! 17=light green
      CALL GSCR (1,17,0.5,1.0,0.6) ! 17=light green
corig CALL GSCR (1,17,0.3,1.0,0.5) ! 17=light green
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

      CALL GACWK (1)
      CALL GSELNT(0)
      CALL GSCLIP(0)
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
      subroutine wflush
      REAL xwrk(4), ywrk(4)
c     --- flushes the background white when using ncar graphics
      LL=1
      XUL = 0.
      XUR = 1.
      YUB = 0.
      YUT = 1.
c     --- save off current window values
      CALL GETSET (FLSV,FRSV,FBSV,FTSV,ULSV,URSV,UBSV,UTSV,LLSV)
      call set(0.,1.,0.,1.,XUL,XUR,YUB,YUT,LL)
      xwrk(1)=XUL
      ywrk(1)=YUB
      xwrk(2)=XUR
      ywrk(2)=YUB
      xwrk(3)=XUR
      ywrk(3)=YUT
      xwrk(4)=XUL
      ywrk(4)=YUT
      call gsfaci(1)   ! white background
      call gfa(4,xwrk,ywrk)
c     --- RESET USER COORDINATES TO THE INCOMING VALUES
      CALL SET (FLSV,FRSV,FBSV,FTSV,ULSV,URSV,UBSV,UTSV,LLSV)
      return
      end
c
      subroutine scale1(xmin, xmax, n, xminp, xmaxp, dist,ierr)
c given xmin, xmax and n, scale1 finds a new range xminp and
c xmaxp divisible into approximately n linear intervals of
c size dist.  on return ierr=-1 if improper inputs, otherwise
c ierr is set to 0.
c vint is an array of acceptable values for dist (times an
c integer power of 10).
c sqr is an array of geometric means of adjacent values of
c vint.  it is used as break points to determine which vint
c value to assign to dist.  del accounts for computer
c roundoff.  it should be greater than the roundoff expected
c from a division and float operation, and less than the
c minimum increment of the plotting device dived by the
c plot size times the number of intervals n.
c ref: algorithm 462 from collected algorithms of the CACM
      implicit none
      real    xmin, xmax, xminp, xmaxp, dist
      integer n, ierr
      real    a, al, b, del, fm1, fm2, fn
      integer nal, i, m1, m2
      real    vint(4), sqr(3)
      data vint/1., 2., 5., 10./
      data sqr/1.414214, 3.162278, 7.071068/
      data del/0.00002/
c
c check whether proper input values were supplied
      if((xmin.ge.xmax).or.(n.le.0)) then
        ierr=-1
        return
      endif
c
c find approximate interval size a
      ierr = 0
      fn = n
      a = (xmax-xmin)/fn
      al = alog10(a)
      nal = al
      if(a.lt.1.) nal = nal-1
c scale a into variable b between 1 and 10
      b = a/(10.**nal)
c find the closest permissible value for b
      do 20 i=1,3
        if(b.lt.sqr(i)) go to 30
   20 continue
      i=4
c compute the interval size
   30 dist = vint(i)*10.**nal
      fm1 = xmin/dist
      m1 = fm1
      if(fm1.lt.0.) m1=m1-1
      if(abs(float(m1)+1.-fm1).lt.del) m1 = m1 + 1
c find the new minimum and maximum limits
      xminp = dist*float(m1)
      fm2 = xmax/dist
      m2 = fm2+1.
      if(fm2.lt.(-1.)) m2=m2-1
      if(abs(fm2+1.-float(m2)).lt.del) m2 = m2 - 1
      xmaxp = dist*float(m2)
c adjust limits to account for roundoff if necessary
      if(xminp.gt.xmin) xminp = xmin
      if(xmaxp.lt.xmax) xmaxp = xmax
c
      return
      end
c
      subroutine abetuv(a,b,theta)
c
c  purpose:  computes the angle in radians between two earth-centered
c            unit vectors a and b. (the angle is between 0 and pi).
c                                                                               
      IMPLICIT NONE                                                             
c     --- calling parameter declarations:
      REAL*4 a(3), b(3), theta
c     --- local declarations:
      REAL*8 cross_prod(3), dot_prod, cmag, pi
      REAL       cos_pi_over_4
      PARAMETER (pi = 3.141592653589793D0)
      PARAMETER (cos_pi_over_4 = 0.707106781)
c
c     --- use the dot product for large angles
      dot_prod = a(1)*b(1) + a(2)*b(2) + a(3)*b(3)
      if (dabs(dot_prod) .lt. cos_pi_over_4 ) then
        theta = dacos(dot_prod)
      else
c       --- use the cross product for small angles
        cross_prod(1) = a(2)*b(3) - a(3)*b(2)
        cross_prod(2) = a(3)*b(1) - a(1)*b(3)
        cross_prod(3) = a(1)*b(2) - a(2)*b(1)
        cmag = DSQRT(cross_prod(1)**2 + cross_prod(2)**2 +
     1               cross_prod(3)**2)
        theta = DASIN(cmag)
        if (dot_prod .lt. 0.0) theta = pi - theta
      endif
c
      return
      end
c
      subroutine comphd(vec1, vec2, heading, ier)
c
c  purpose: computes heading from point 1 to point 2 (positive
c           clockwise from north at point 1), given input earth-fixed
c           unit vectors representing locations of 1 and 2.
c  calling parameter interface:
c  inputs:  vec1, vec2 - earth-fixed unit vectors through point 1
c                        and point 2 respectively
c  outputs: heading - resultant heading from point 1 to point 2 (deg),
c                     measured clockwise from north at point 1
c                     (0 le heading le 360)
c           ier - error flag set to -1 if points 1 and 2 coincide,
c                 0 otherwise
c
      implicit none
c     --- calling parameter declarations:
      real*4  vec1(3), vec2(3), heading
      integer ier
c     --- local declarations:
      real*8  lat1, lon1, nmag, sinlat, sinlon, coslat, coslon
      real*8  w(3), t12(3), normal(3)
      real*8     DEGREES_PER_RADIAN
      parameter (DEGREES_PER_RADIAN = 57.29577951308232D0 )
c
c     --- compute a vector, n, normal to the input vectors
c     --- by developing their cross product
      normal(1) = vec1(2) * vec2(3) - vec1(3) * vec2(2)
      normal(2) = vec1(3) * vec2(1) - vec1(1) * vec2(3)
      normal(3) = vec1(1) * vec2(2) - vec1(2) * vec2(1)
      nmag = normal(1)**2 + normal(2)**2 + normal(3)**2
c
c     --- if magnitude of normal is zero return input heading
      if(abs(nmag).lt.1.0e-6) then
        ier=-1
        return
      endif
      ier=0
c
c     --- compute tangent vector perpendicular to normal and vec1 and
c     --- pointing in the direction of point 2 (develop cross product)
      t12(1) = normal(2) * vec1(3) - normal(3) * vec1(2)
      t12(2) = normal(3) * vec1(1) - normal(1) * vec1(3)
      t12(3) = normal(1) * vec1(2) - normal(2) * vec1(1)
c
c     --- transform tangent vector from earth-fixed to local
      lat1 = ASIN (vec1(3))
      lon1 = ATAN2(vec1(2),vec1(1))
c
      sinlat = DSIN(lat1)
      coslat = DCOS(lat1)
      sinlon = DSIN(lon1)
      coslon = DCOS(lon1)
      w(1) = -sinlon*t12(1) + coslon*t12(2)
      w(2) = -sinlat*coslon*t12(1) - sinlat*sinlon*t12(2)
     1       + coslat*t12(3)
c
c     --- compute the heading relative to local north at point 1
c     --- of the great circle arc between point 1 and point 2.
      heading = DATAN2(w(1),w(2)) * DEGREES_PER_RADIAN
c
c     --- make sure 0. < heading < 360.
      if(heading.ge.360.0) heading=heading-360.                              
      if(heading.lt.0.0)   heading=heading+360.                              
c
      return
      end
c
      subroutine defuvp( lat, lon, unit_vector )
c
c  purpose:  computes a unit earth-fixed position vector, from the
c            earth center to the point defined by lat, lon (deg).
c
      IMPLICIT NONE
      REAL*4  lat, lon
      REAL*4  unit_vector(3)
      REAL*8  cos_lat, latr, lonr
      REAL*8  degrees_per_radian
      PARAMETER (degrees_per_radian = 57.29577951308232D0 )
c
c     --- compute the unit earth-fixed position vector from the earth's
c     --- center to the point defined by lat and lon.  convert lat and
c     --- lon to radians.
      latr= lat / degrees_per_radian
      lonr= lon / degrees_per_radian
      cos_lat = DCOS(latr)
      unit_vector(1) = cos_lat * DCOS(lonr)
      unit_vector(2) = cos_lat * DSIN(lonr)
      unit_vector(3) = DSIN(latr)
c
      return
      end
c
      subroutine endpt( INITIAL_LAT, INITIAL_LON, ALTITUDE, HEADING,
     1 DISTANCE, FINAL_LAT, FINAL_LON )
C***********************************************************************        
C*                                                                              
C* PURPOSE:        COMPUTES AN ENDPOINT LATITUDE AND LONGITUDE (PT 2)           
C*                 USING A FLAT EARTH APPROXIMATION.
C*                                                                              
C* DESCRIPTION:    GIVEN AN INITIAL LATITUDE AND LONGITUDE (PT 1), THE          
C*                 GROUND TRACK HEADING FROM POINT 1 TO POINT 2, AND            
C*                 THE LINEAR DISTANCE BETWEEN POINT 1 AND POINT 2,            
C*                 COMPUTES AN APPROXIMATE ENDPOINT LATITUDE AND 
C*                 LONGITUDE (PT 2).  THE VALIDITY OF THE APPROXIMATION
C*                 DEPENDS ON THE DISTANCE COVERED.
C*                                                                              
C* CALLING INTERFACE:                                                           
C*                                                                              
C*   INPUT:                                                                     
C*       INITIAL_LAT - LATITUDE OF INITIAL POINT (DEG)
C*       INITIAL_LON - LONGITUDE OF INITIAL POINT (DEG)
C*       HEADING     - HEADING AT THE INITIAL POINT (DEG)
C*       DISTANCE    - LINEAR DISTANCE BETWEEN INITIAL AND FINAL POINTS (m)
C*       ALTITUDE    - MSL ALTITUDE (m) AT THE INITIAL POINT                 
C*                                                                              
C*   OUTPUT:                                                                    
C*       FINAL_LAT   - LATITUDE OF FINAL POINT (DEG)
C*       FINAL_LON   - LONGITUDE OF FINAL POINT (DEG)
C***********************************************************************        
c
      IMPLICIT NONE                                                             
      REAL*4 INITIAL_LAT, INITIAL_LON, FINAL_LAT, FINAL_LON, HEADING,
     1       DISTANCE, ALTITUDE                                 
      REAL*4 ARC_DIST, SINPSI, COSPSI, COSPHI
      REAL*8  REARTH, DEGREES_PER_RADIAN
      PARAMETER (REARTH = 6370.999D3)
      PARAMETER (DEGREES_PER_RADIAN = 57.29577951308232D0 )                     
c
C     --- INITIALIZE LOCAL VARIABLES.
      FINAL_LAT = -999.99
      FINAL_LON = -999.99                                           
      COSPHI = COS(INITIAL_LAT/DEGREES_PER_RADIAN)
      COSPSI = COS(HEADING/DEGREES_PER_RADIAN)
      SINPSI = SIN(HEADING/DEGREES_PER_RADIAN)
C                                                                               
C     --- DETERMINE NEW END POINT
      ARC_DIST = REARTH + ALTITUDE
C                                                                               
      FINAL_LAT = INITIAL_LAT + ((DISTANCE*COSPSI/ARC_DIST)*
     1                           DEGREES_PER_RADIAN)      
      FINAL_LON = INITIAL_LON + ((DISTANCE*SINPSI/(ARC_DIST*COSPHI))*
     1                           DEGREES_PER_RADIAN)
C
C     --- CHECK FOR 180 LONGITUDE CROSSING.  INPUTS AND OUTPUTS ARE 
C     --- ASSUMMED TO BE -90 <= LAT <= +90, -180 <= LON <= +180.
      IF(FINAL_LAT.GT.90.) THEN
        FINAL_LAT = FINAL_LAT -90.
      ENDIF
C
      IF(FINAL_LAT.LT.-90.) THEN
        FINAL_LAT = FINAL_LAT + 90.
      ENDIF
C
      IF(FINAL_LON.GT.180.) THEN
        FINAL_LON = FINAL_LON - 360.
      ENDIF
C
      IF(FINAL_LON.LT.-180.) THEN
        FINAL_LON = FINAL_LON + 360.
      ENDIF
C                          
      RETURN
      END
c
      subroutine latlon_to_utm(lat,lon,xe,yn,izone,iellipse,idir)
c     --- converts lat,lon to utm if idir ge 0, utm to lat lon otherwise
c     --- inputs:
c         idir ge 0 is forward, lt 0 inverse (utm to lat,lon)
c         iellipse = 0 to use sphere, ellipsoid otherwise
c         for forward transformation (idir ge 0)
c         - lat, lon (deg)
c         for inverse transformation (idir < 0)
c         - izone (UTM zone)  
c         - xe, yn (UTM coordinates)
c     --- outputs:
c         for forward transformation (idir ge 0)
c         - xe, yn (UTM coordinates)
c         - izone (UTM zone)  
c         for inverse transformation (idir < 0)
c         - lat, lon (deg)
      implicit none
      real*4 lat,lon,xe,yn
      integer iellipse,idir,izone
      real*8 phi,phi0,lon0,deltalambda,D,lambda
      real*8 sinphi, cosphi, tanphi, Rp, etasq, f1, f2, f3,
     1  fx, fy, dm, dm0, X, Y, z, E, N, A, Asq, B, k0
      real*8 FN, FE
      real*8 DPI, DRADDEG
      real*8 ae, esq
      PARAMETER (DPI=3.14159265358979323846264338327950D0)
      PARAMETER (DRADDEG = 1.74 53292 51994 32957 69237 E-02) ! [rotfac]
c     PARAMETER (ae = 6.378135D6 , esq=0.006694317778D0)    ! wgs-72
c     PARAMETER (ae = 6.378137D6 , esq=0.00669437999013D0)  ! wgs-84
c     PARAMETER (ae = 6.3782064D6 , esq=0.00676866D0)       ! Clarke 1866 ellipsoid
c     PARAMETER (ae = 6.378137D6 , esq=0.0D0)               ! sphere
c     PARAMETER (ae = 6.370999D6 , esq=0.0D0)               ! NAPS
      PARAMETER (ae = 6.340000D6 , esq=0.0D0)               ! best agrrement
c
      phi0=0.
c
      if(iellipse.eq.0) then
c       --- sphere
        k0 = 1.0
        FE = 5.0D5
        FN = 0.0D0
        if(idir.lt.0) then
c         --- inverse function returns lat,lon given input n,e,zone
          D=yn/(ae*k0) + phi0
          x=(xe-FE)/(ae*k0)
          phi=ASIN(sin(D)/cosh(x))
          deltalambda=ATAN(sinh(x)/cos(D))
          lat = phi/DRADDEG
          lon0 = -180.+6.D0*izone-3.D0
          lon = deltalambda/DRADDEG + lon0
        else
c         --- forward function returns n,e given input lat,lon
          izone=iabs(INT((-180.-lon)/6.))+1
          lon0 = -180.+6.D0*izone-3.D0
          deltalambda=(lon-lon0)
          deltalambda=deltalambda*DRADDEG
          phi=ABS(lat*DRADDEG)
          sinphi=dsin(phi)
          cosphi=dcos(phi)
          tanphi=sinphi/cosphi
          B = cosphi*dsin(deltalambda)
          X = 0.5*k0*DLOG((1.+B)/(1.-B))
          z = tanphi/DCOS(deltalambda)
          Y = k0*(DATAN(z)-phi0)
          N = k0*ae*Y    ! m
          E = k0*ae*X    ! m
          N = N + FN  ! m
          E = E + FE  ! m
          xe=E
          yn=N
        endif
c     print *,'Sphere: E,N=',E,N
c
      else
c
c     --- ellipsoid
      k0 = 0.9996D0
      FE = 5.0D5
      FN = 0.0D0
      if(lat.lt.0.) FN=1.0D7
c
      sinphi=dsin(phi)
      cosphi=dcos(phi)
      tanphi=sinphi/cosphi
      Rp = ae/sqrt(1.-esq*sinphi*sinphi)
      etasq = esq*cosphi*cosphi/(1.d0-esq)
      f1 = (1.d0 - esq*(0.25d0 + .046875D0*esq))*phi    ! 1/4, 3/64
      f2 = esq*(0.375D0 + .09375D0*esq)*dsin(2.d0*phi)  ! 3/8, 3/32
      f3 = .05859375D0*esq*esq*dsin(4.d0*phi)           ! 15/256
      dm = ae*(f1 - f2 + f3)
      f1 = (1.d0 - esq*(0.25d0 + .046875D0*esq))*phi0   ! 1/4, 3/64
      f2 = esq*(0.375D0 + .09375D0*esq)*dsin(2.d0*phi0) ! 3/8, 3/32
      f3 = .05859375D0*esq*esq*dsin(4.d0*phi0)          ! 15/256
      dm0= ae*(f1 - f2 + f3)
      fx = 1.D0 - tanphi*tanphi + etasq
      fy = 5.D0 - tanphi*tanphi + 9.D0*etasq + 4.D0*etasq*etasq
      A = deltalambda*cosphi 
      Asq = A**2 
      X = Rp*A*(1.D0 + fx*Asq/6.D0)  !m
      Y = dm-dm0 + (Rp*tanphi*Asq/2.D0)*(1.D0 + fy*Asq/12.D0)
      if(lat.lt.0.) Y=-Y
      N = k0*Y    ! m
      E = k0*X    ! m
      N = N + FN  ! m
      E = E + FE  ! m
      xe=E
      yn=N
c     print *,'Ellipsoid: E,N=',E,N
      endif
c
      return
      end
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
c     include 'ncarg_stub.f'
