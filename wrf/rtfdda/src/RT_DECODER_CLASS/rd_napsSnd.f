       program rd_napssndg
       parameter (nobs=500)
c
       real lon, lat, alt
       real time,pres,temp,dewpt,
     &      rh,uwind,vwind,
     &      wspd,wdir
       real dz,lon2,lat2
       real range,ang,alt2
       real q_pres,q_tt,q_rh,
     &      q_uu,q_vv,q_uv
       integer stindx
       integer mdate, no, nc
       integer iyr, imo, idy, ihr, imn, isc
       character*12 data_type, id, site, type_id
       character*16  a1,a2,a3,a4,a5,a6,a7,a8,a9
       character*16  b1,b2,b3,b4,b5,b6

      real ht(nobs),spd(nobs),dir(nobs),
     &           wqc(nobs),slp(nobs),ztd(nobs)
      real tt(nobs),ttc(nobs),ww(nobs),
     &           prs(nobs),uu(nobs),vv(nobs)

      integer   ht_qc(nobs),spd_qc(nobs),dir_qc(nobs),
     &          tt_qc(nobs),ttc_qc(nobs),pp_qc(nobs),
     &          uu_qc(nobs),vv_qc(nobs),rh_qc(nobs),
     &          td_qc(nobs)

       character*14 ndate
       character*20 date,tit
       character*40 strname, strnam2, string2, string4
       logical bogus
       character*1 aline , cline(132)

       NAMELIST / record1 / xlatc , xlonc , xn
       data deg2rad / .0174532925 /
      QSW(G,E)=3.799*EXP(17.269*(E-273.16)/(E-35.86))/G
      QSI(G,E)=3.799*EXP(21.874*(E-273.16)/(E-7.660))/G

       strname  = '99001                                   '
       strnam2  = 'FM-14 SYNOP                             '
       string2  = '                                        '
       string4  = '                                        '

       type_id = 'ATC     '
       lat = 39.47000           
       lon = -76.07

c namelist_file
       namelist_file = 11
c input unit
       iunin = 10
c output unit
       iuno = 50

c       xlatc  = 40.57000
c       xlonc  = -115.4900
c       xn     = 0.7155668

       OPEN ( UNIT=namelist_file ,
     &        FILE='namelist.input' ,
     &        STATUS = 'OLD' ,
     &        FORM = 'FORMATTED' ,
     &        IOSTAT = iopen_status )

       IF ( iopen_status .NE. 0 ) THEN
         PRINT '(A)' , 'Error opening NAMELIST file.'
         STOP 'namelist_file_open'
       ENDIF

       READ ( UNIT = namelist_file , NML = record1 )

       PRINT '(A)','Successfully READ the Prof NAMELIST file.'

       bogus  = .false.
       spval  = 999.
       qc_spv = -888888
       qc_val = 2.3
       qc_value = 1.0

       do no=1,nobs
        slp(no) = qc_spv
        ztd(no) = qc_spv
        prs(no) = qc_spv
        uu(no)  = qc_spv
        vv(no)  = qc_spv
        ht(no)  = qc_spv
       enddo

       do no=1,nobs
        pp_qc(no) = qc_spv
        ht_qc(no) = qc_spv
        uu_qc(no) = qc_spv
        vv_qc(no) = qc_spv
        rh_qc(no) = qc_spv
        tt_qc(no) = qc_spv
       enddo

      do ki=1,10000
      n=1
      read (iunin,'(A1)',END=999) tit(1:1)
      read (iunin,'(I3)',END=999) iupto
      read (iunin,'(A1)',END=999) tit(1:1)
      read (iunin,'(i4,5i2)') iyr,imo,idy,ihr,imn,isc
      read (iunin,'(A20)') tit
      read (iunin,'(I5,F9.1)') ielev, psfc
      read (iunin,'(A20)') tit
      xlev = ielev
       print *,' yr-mn-dy-hr-mn-sec= ',iyr,imo,idy,ihr,imn,isc
      do i=1,iupto
c     read (iunin,'(I5,F9.1,5F9.2)')
c    & no, h, temp, rh, wspd, wdir, pres
      read (iunin,'(I6,F9.1,F7.2,F8.2,F7.2,F7.2,F8.2)')
     & no, h, temp, rh, wspd, wdir, pres
      qv = 0.01*rh*QSW(pres,temp+273.16)
      dewpt= DEWPT2(pres,qv)
       print *,' no,Td= ',no,dewpt

    
   
       if( pres .ne. spval )then
        prs(n)  = pres*100.
        pp_qc(n)= 1.
       else
        prs(n)  = qc_spv
        pp_qc(n)= qc_spv
       endif

       if( wdir .ne. spval .and. wspd .ne. spval )then
        spd(n)    = wspd
        spd_qc(n) = 1.
        dir(n)    = wdir
        dir_qc(n) = 1.
       else
        spd(n) = qc_spv 
        dir(n) = qc_spv
        spd_qc(n) = qc_spv
        dir_qc(n) = qc_spv
       endif

       if( vwind .ne. spval .and. uwind .ne. spval )then
cyliu "diff" sign is wrong
c       diff = lon - xlonc
        diff = xlonc - lon 
        if ( diff .GT.  180. ) diff = diff - 360.
        if ( diff .LT. -180. ) diff = diff + 360.
        alpha = diff * xn * deg2rad * SIGN(1. , xlatc)
        uu(n) = vwind * SIN(alpha) + uwind * COS(alpha)
        vv(n) = vwind * COS(alpha) - uwind * SIN(alpha)
        uu_qc(n) = 1. 
        vv_qc(n) = 1. 
       else
        uu(n) = qc_spv
        vv(n) = qc_spv
        uu_qc(n) = qc_spv 
        vv_qc(n) = qc_spv 
       endif

       if( temp .ne. spval )then
        tt(n)    = temp + 273.16
        tt_qc(n) = 1.
       else
        tt(n)    = qc_spv 
        tt_qc(n) = qc_spv
       endif

       if( dewpt .ne. spval )then
        ztd(n)   = dewpt + 273.16 
        td_qc(n) = 1.
       else
        ztd(n)   = qc_spv 
        td_qc(n) = qc_spv
       endif
       
       n = n + 1
      enddo

c
       stindx   = 999
       iseq_num = 99
       kkl      = n-1

       write (ndate(1:4),fmt='(i4.4)') iyr
       write (ndate(5:6),fmt='(i2.2)') imo
       write (ndate(7:8),fmt='(i2.2)') idy
       write (ndate(9:10),fmt='(i2.2)') ihr
       write (ndate(11:12),fmt='(i2.2)') imn
       write (ndate(13:14),fmt='(i2.2)') isec

       call write_obs (stindx,prs,ht,tt,ztd,spd,dir,
     &                 slp,xlev,lat,lon,
     &                 uu, vv, ht_qc, tt_qc, td_qc,
     &            dir_qc, spd_qc, ndate, imn, isec, kkl,
     &          strname,string2,strnam2,string4,bogus,
     &                                  iseq_num,iuno )
      enddo

 999   continue
       print *,' read complete successfully '
       stop 99999
       end
c
c x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x
c
       SUBROUTINE write_obs (stindx, p , z , t , td , spd , dir ,
     *                    slp , ter , xlat , xlon ,
     *                    uu  , vv  , zpp_qc, tt_qc, td_qc ,
     *                    zuu_qc , zvv_qc ,
     *                    mdate , imin , isec, kx ,
     * string1 , string2 , string3 , string4 , bogus , iseq_num ,
     * iunit )

       integer stindx                                  ! station index
       dimension z(kx),t(kx),td(kx),spd(kx),dir(kx),
     *           uu(kx), vv(kx), p(kx)
       integer   zpp_qc(kx), tt_qc(kx), td_qc(kx),
     *           zuu_qc(kx), zvv_qc(kx)

      character *20 date_char
      character *14 mdate
      character *40 string1, string2 , string3 , string4
      CHARACTER *120 rpt_format
      CHARACTER *120 meas_format
      CHARACTER *120 end_format
      logical bogus

      rpt_format =  ' ( 2f20.5 , 2a40 , '
     *             // ' 2a40 , 1f20.5 , 5i10 , 3L10 , '
     *             // ' 2i10 , a20 ,  13( f13.5 , i7 ) ) '
      meas_format =  ' ( 10( f13.5 , i7 ) ) '
      end_format = ' ( 3 ( i7 ) ) '

      date_char(7:20) = mdate(1:14)
      date_char(1:6)='      '

      print *,' date_char= ',date_char(1:20)

      WRITE ( UNIT = iunit , ERR = 19 , FMT = rpt_format )
     *        xlat,xlon, string1 , string2 ,
     *        string3 , string4 , ter, kx*6, 0,0,iseq_num,0,
     *        .true.,bogus,.false.,
     *         -888888, -888888, date_char ,
     *         slp,0,-888888.,0, -888888.,0, -888888.,0, -888888.,0,
     *               -888888.,0,
     *               -888888.,0, -888888.,0, -888888.,0, -888888.,0,
     *               -888888.,0,
     *               -888888.,0, -888888.,0

      do 100 k = 1 , kx
         WRITE ( UNIT = iunit , ERR = 20 , FMT = meas_format )
     *          p(k),0, z(k),zpp_qc(k), t(k),tt_qc(k),
     *          td(k),td_qc(k),
     *          spd(k),zvv_qc(k), dir(k),zuu_qc(k),
     *          uu(k),zuu_qc(k), vv(k),zvv_qc(k), -888888.,0, -888888.,0
c
         print *,' k= ',k,' p-z-ppqc= ',p(k),z(k),zpp_qc(k),
     1        ' t-ttqc= ',t(k),tt_qc(k),' td-tdqc= ',td(k),td_qc(k),
     2        ' spd-dir= ',spd(k),dir(k),' uu-uuqc= ',uu(k),zuu_qc(k),
     3        ' vv-vvqc= ',vv(k),zvv_qc(k)
c
100   continue

      WRITE ( UNIT = iunit , ERR = 21 , FMT = meas_format )
     * -777777.,0, -777777.,0,float(kx),0,
     * -888888.,0, -888888.,0, -888888.,0,
     * -888888.,0, -888888.,0, -888888.,0,
     * -888888.,0

      WRITE ( UNIT = iunit , ERR = 19 , FMT = end_format )  kx, 0, 0

      return

19    continue
      print *,' troubles writing a sounding - err=19 '
      stop 19
20    continue
      print *,' troubles writing a sounding - err=20 '
      stop 20
21    continue
      print *,' troubles writing a sounding - err=21 '
      stop 21
      END

      FUNCTION DEWPT2(P,Q)
C THIS FUNCTION RETURNS THE DEWPOINT (C) GIVEN THE WATER VAPOR
C PRESSURE (MB). THE EMPIRICAL FORMULA IS FROM BOLTON (1980) MWR.
      EW = P * Q / (0.622 + Q)
      ENL = ALOG(AMAX1(EW,1.E-15))
      DEWPT2 = (243.5 * ENL - 440.8) / (19.48 - ENL)
      RETURN
      END
