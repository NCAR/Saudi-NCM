      program merge
c
      parameter (nobs=300)

      dimension ht(nobs),spd(nobs),dir(nobs),
     1           wqc(nobs),slp(nobs),ztd(nobs)
      dimension tt(nobs),ttc(nobs),ww(nobs),
     1          prs(nobs),uu(nobs),vv(nobs)
      integer   ht_qc(nobs),spd_qc(nobs),dir_qc(nobs),
     1          tt_qc(nobs),ttc_qc(nobs),pp_qc(nobs),
     2          uu_qc(nobs),vv_qc(nobs),rh_qc(nobs),
     3          td_qc(nobs)

      character*1 aa, HH
      character*20 HH2
      character*20 name1, name2, cline
      integer iyr,imo,idy,ihr,imn,isec,ndat, iday(12)
      real lat, lon, xlev
      integer ilev
      character*14 ndato, ndate
      character*40 strname, strnam2, string2, string4
      character*60 filename,abc
      logical      ishere
      logical bogus


      data iday /31,28,31,30,31,30,31,31,30,31,30,31 /
      data deg2rad / .0174532925 /

       read (*,10) filename
 10    format (a)
        print *, filename
       inquire (file=filename,EXIST=ishere)
       if (.not. ishere) then
         write(*,6001) filename
 6001    format(1x,'CANNOT SEE FILE: ', a60)
       else
         write(*,6002) filename
 6002    format(1x,'I SEE FILE: ', a60)
       endif
 
      read (filename(1:4),fmt='(i4)') iyr
      read (filename(5:6),fmt='(i2)') imo
      read (filename(7:8),fmt='(i2)') idy
      read (filename(10:11),fmt='(i2)') ihr
      read (filename(12:13),fmt='(i2)') imn
c the average time is between (ihr-1) and ihr, so the valid time 
c for this averaged data should be ihr-0.5 
      print *, iyr, imo,idy,ihr,imn
      iun1 = 10
c read the T-file with unit 20
      iun2 = 20
c output unit
      iuno = 50

      bogus  = .false.
      spval  = 9999.
      qc_spv = -888888
      qc_val = 2.3
      qc_value = 1.0

c
      OPEN ( iun1 ,
     &       FILE=filename ,
     &       STATUS = 'OLD' ,
     &       FORM = 'FORMATTED' ,
     &       IOSTAT = iopen_status)

      do no=1,nobs
       slp(no) = qc_spv
       ztd(no) = spval
       prs(no) = spval
       uu(no)  = spval
       vv(no)  = spval
       tt(no)  = spval
       ttc(no)  = spval
      enddo

      do no=1,nobs
       pp_qc(no) = qc_spv
       ht_qc(no) = qc_spv 
       uu_qc(no) = qc_spv 
       vv_qc(no) = qc_spv 
       rh_qc(no) = qc_spv 
       tt_qc(no) = qc_spv
       ttc_qc(no)= qc_spv
      enddo
 
       strname  = '99001                                   '
       strnam2  = 'FM-12 LID-P                             '
       string2  = ' PROFILER LID                           '
       string4  = '                                        '

 
      do n1 = 1,16
       read(iun1,"a2")  abc(1:2)
      enddo
      read(iun1,"(a32,2f14.5,f9.1)") abc(1:32),lat,lon,xlev
      print *,lat, lon, xlev
      do n1 = 1, nobs
      read(iun1,"(f8.2,2f10.3)",end=1234) ht(n1),spd(n1),dir(n1)
       if(spd(n1).gt.0.and.dir(n1).gt.0) then
       ht(n1) = ht(n1) + xlev
       ht_qc(n1)=0
       dir_qc(n1)=0
       spd_qc(n1)=0
       endif
c      print *, "ht_qc=", n1,ht_qc(n1), spd_qc(n1)
      enddo

1234   nobst = n1-1
 
      do no=1,nobst
       if(prs(no) .eq. spval)then
        prs(no)   = qc_spv
       endif   
       if(ht(no) .eq. spval)then
        ht(no)    = qc_spv
        ht_qc(no) = qc_spv
       endif 
       if(spd(no) .eq. spval)then
        spd(no)    = qc_spv
        spd_qc(no) = qc_spv
       endif 
       if(dir(no) .eq. spval)then
        dir(no)    = qc_spv
        dir_qc(no) = qc_spv
       endif
       if(tt(no) .eq. spval)then
        tt(no)    = qc_spv
        ztd(no)   = qc_spv
        tt_qc(no) = qc_spv
       else
        tt(no)    = tt(no) + 273.16
        ztd(no)   = qc_spv
       endif
       if(ttc(no) .eq. spval)then
        ttc(no)    = qc_spv
        ttc_qc(no) = qc_spv
       endif
 
       if( dir(no).ne.qc_spv.and.spd(no).ne.qc_spv)then
      dir_map = dir(no)  ! no rotation case
      if ( dir_map .GT. 360. ) dir_map = dir_map - 360.
      if ( dir_map .LT.   0. ) dir_map = 360     + dir_map
      uu(no) = -1. * spd(no) * SIN ( dir_map * deg2rad )
      vv(no) = -1. * spd(no) * COS ( dir_map * deg2rad )
       else
         uu(no) = qc_spv 
         vv(no) = qc_spv 
       endif

c       print *,' no= ',no,' ht-htqc= ',ht(no),ht_qc(no),
c     1          ' spd-spdqc= ',spd(no),spd_qc(no),
c     2          ' dir-dirqc= ',dir(no),dir_qc(no),
c     3          ' tt-ttqc= ',tt(no),tt_qc(no),
c     4          ' ttc-ttcqc= ',ttc(no),ttc_qc(no),
c     5          ' ww-wqc= ',ww(no),wqc(no)
c       print *,'  '

      enddo

       igmts = 0
       stindx= 999
       kkl   = nobst

       write (ndato(1:4),fmt='(i4.4)') iyr
       write (ndato(5:6),fmt='(i2.2)') imo
       write (ndato(7:8),fmt='(i2.2)') idy
       write (ndato(9:10),fmt='(i2.2)') ihr
       write (ndato(11:12),fmt='(i2.2)') imn
       write (ndato(13:14),fmt='(i2.2)') isec

       call geth_newdate (ndate, ndato, igmts)
       iseq_num = 99
 
       print *,' kkl= ',kkl,' ndat= ',ndat,
     1     ' strname= ',strname,' strnam2= ',strnam2,
     2     ' string2= ',string2,' bogus= ',bogus

       call write_obs (stindx,prs,ht,tt,ztd,spd,dir,
     *                 slp,xlev,lat,lon,
     *                 uu, vv, ht_qc, tt_qc, rh_qc,
     *            dir_qc, spd_qc, ndate, imn, isec, kkl,
     *          strname,string2,strnam2,string4,bogus,
     *                                  iseq_num,iuno )
 

 305  continue
 101  continue
      print *,' program completed successfully '
      stop 99999
      end
c
c x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x
c
      subroutine sort(ht,spd,dir,tt,ttc,ww,nobst)
c
c sort in ascending order of height
c
      dimension ht(nobst), spd(nobst), dir(nobst),
     1          tt(nobst), ttc(nobst), ww(nobst)
 
      do n=2,nobst
       ht1  = ht(n)
       spd1 = spd(n)
       dir1 = dir(n)
       tt1  = tt(n)
       ttc1 = ttc(n)
       ww1  = ww(n)
 
        do nn=n-1,1,-1
         if(ht(nn).le.ht1) goto 101
          ht(nn+1) = ht(nn)
          spd(nn+1)= spd(nn)
          dir(nn+1)= dir(nn)
          tt(nn+1) = tt(nn)
          ttc(nn+1)= ttc(nn)
          ww(nn+1) = ww(nn)
        enddo

 101    continue
        ht(nn+1) = ht1
        spd(nn+1)= spd1
        dir(nn+1)= dir1
        tt(nn+1) = tt1
        ttc(nn+1)= ttc1
        ww(nn+1) = ww1
       enddo 
       
       return
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
c    1        ' t-ttqc= ',t(k),tt_qc(k),' td-tdqc= ',td(k),td_qc(k),
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

c
c x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x
c
      SUBROUTINE geth_newdate (ndate, odate, idt)

c  From old date ('YYYYMMDDHHMMSSffff') and
c  delta-time, compute the new date.

c  on entry     -  odate  -  the old hdate.
c                  idt    -  the change in time
c  on exit      -  ndate  -  the new hdate.

      CHARACTER*14 ndate
      CHARACTER*14 odate


c  yrold    -  indicates the year associated with "odate"
c  moold    -  indicates the month associated with "odate"
c  dyold    -  indicates the day associated with "odate"
c  hrold    -  indicates the hour associated with "odate"
c  miold    -  indicates the minute associated with "odate"
c  scold    -  indicates the second associated with "odate"

c  yrnew    -  indicates the year associated with "ndate"
c  monew    -  indicates the month associated with "ndate"
c  dynew    -  indicates the day associated with "ndate"
c  hrnew    -  indicates the hour associated with "ndate"
c  minew    -  indicates the minute associated with "ndate"
c  scnew    -  indicates the second associated with "ndate"

c  mday     -  a list assigning the number of days in each month

c  i        -  loop counter
c  nday     -  the integer number of days represented by "idt"
c  nhour    -  the integer number of hours in "idt" after taking out
c              all the whole days
c  nmin     -  the integer number of minutes in "idt" after taking out
c              all the whole days and whole hours.
c  nsec     -  the integer number of minutes in "idt" after taking out
c              all the whole days, whole hours, and whole minutes.

      INTEGER len, nlen, olen
      INTEGER yrnew, monew, dynew, hrnew, minew, scnew, frnew
      INTEGER yrold, moold, dyold, hrold, miold, scold, frold
      INTEGER mday(12), nday, nhour, nmin, nsec, nfrac, i, ifrc
      LOGICAL opass
      CHARACTER*10 hfrc
      CHARACTER*1  sp

c  Assign the number of days in a months

      mday( 1) = 31
      mday( 2) = 28
      mday( 3) = 31
      mday( 4) = 30
      mday( 5) = 31
      mday( 6) = 30
      mday( 7) = 31
      mday( 8) = 31
      mday( 9) = 30
      mday(10) = 31
      mday(11) = 30
      mday(12) = 31

c  Break down old hdate into parts

      hrold = 0
      miold = 0
      scold = 0
      frold = 0
      olen = LEN(odate)
      IF (olen.GE.11) THEN
         sp = odate(11:11)
      else
         sp = ' '
      END IF

c  Use internal READ statements to convert the CHARACTER string
c  date into INTEGER components.

      READ(odate(1:4),  '(I4)') yrold
      READ(odate(5:6),  '(I2)') moold
      READ(odate(7:8), '(I2)') dyold
      IF (olen.GE.9) THEN
         READ(odate(9:10),'(I2)') hrold
         IF (olen.GE.11) THEN
            READ(odate(11:12),'(I2)') miold
            IF (olen.GE.13) THEN
               READ(odate(13:14),'(I2)') scold
               IF (olen.GT.14) THEN
                  READ(odate(21:olen),'(I2)') frold
               END IF
            END IF
         END IF
      END IF

c  Set the number of days in February for that year.

      mday(2) = nfeb(yrold)

c  Check that ODATE makes sense.

      opass = .TRUE.

c  Check that the month of ODATE makes sense.

      IF ((moold.GT.12).or.(moold.LT.1)) THEN
         WRITE(*,*) 'GETH_NEWDATE:  Month of ODATE = ', moold
         opass = .FALSE.
      END IF

c  Check that the day of ODATE makes sense.

      IF ((dyold.GT.mday(moold)).or.(dyold.LT.1)) THEN
         WRITE(*,*) 'GETH_NEWDATE:  Day of ODATE = ', dyold
         opass = .FALSE.
      END IF

c  Check that the hour of ODATE makes sense.

      IF ((hrold.GT.23).or.(hrold.LT.0)) THEN
         WRITE(*,*) 'GETH_NEWDATE:  Hour of ODATE = ', hrold
         opass = .FALSE.
      END IF

c  Check that the minute of ODATE makes sense.

      IF ((miold.GT.59).or.(miold.LT.0)) THEN
         WRITE(*,*) 'GETH_NEWDATE:  Minute of ODATE = ', miold
         opass = .FALSE.
      END IF

c  Check that the second of ODATE makes sense.

      IF ((scold.GT.59).or.(scold.LT.0)) THEN
         WRITE(*,*) 'GETH_NEWDATE:  Second of ODATE = ', scold
         opass = .FALSE.
      END IF

c  Check that the fractional part  of ODATE makes sense.

      IF (.not.opass) THEN
         WRITE(*,*) 'GETH_NEWDATE: Crazy ODATE: ', odate(1:olen), olen
         STOP 'odate_3'
      END IF

c  Date Checks are completed.  Continue.

c  Compute the number of days, hours, minutes, and seconds in idt

      IF (olen.GT.14) THEN !idt should be in fractions of seconds
         ifrc = olen-20
         ifrc = 10**ifrc
         nday   = ABS(idt)/(86400*ifrc)
         nhour  = MOD(ABS(idt),86400*ifrc)/(3600*ifrc)
         nmin   = MOD(ABS(idt),3600*ifrc)/(60*ifrc)
         nsec   = MOD(ABS(idt),60*ifrc)/(ifrc)
         nfrac = MOD(ABS(idt), ifrc)
      ELSE IF (olen.eq.14) THEN  !idt should be in seconds
         ifrc = 1
         nday   = ABS(idt)/86400 ! Integer number of days in delta-time
         nhour  = MOD(ABS(idt),86400)/3600
         nmin   = MOD(ABS(idt),3600)/60
         nsec   = MOD(ABS(idt),60)
         nfrac  = 0
      ELSE IF (olen.eq.12) THEN !idt should be in minutes
         ifrc = 1
         nday   = ABS(idt)/1440 ! Integer number of days in delta-time
         nhour  = MOD(ABS(idt),1440)/60
         nmin   = MOD(ABS(idt),60)
         nsec   = 0
         nfrac  = 0
      ELSE IF (olen.eq.10) THEN !idt should be in hours
         ifrc = 1
         nday   = ABS(idt)/24 ! Integer number of days in delta-time
         nhour  = MOD(ABS(idt),24)
         nmin   = 0
         nsec   = 0
         nfrac  = 0
      ELSE IF (olen.eq.8) THEN !idt should be in days
         ifrc = 1
         nday   = ABS(idt)/24 ! Integer number of days in delta-time
         nhour  = 0
         nmin   = 0
         nsec   = 0
         nfrac  = 0
      ELSE
         WRITE(*,'(''GETH_NEWDATE: Strange length for ODATE: '', i3)')
     1        olen
         WRITE(*,*) odate(1:olen)
         STOP 'odate_4'
      END IF

      IF (idt.GE.0) THEN

         frnew = frold + nfrac
         IF (frnew.GE.ifrc) THEN
            frnew = frnew - ifrc
            nsec = nsec + 1
         END IF

         scnew = scold + nsec
         IF (scnew .GE. 60) THEN
            scnew = scnew - 60
            nmin  = nmin + 1
         END IF

         minew = miold + nmin
         IF (minew .GE. 60) THEN
            minew = minew - 60
            nhour  = nhour + 1
         END IF

         hrnew = hrold + nhour
         IF (hrnew .GE. 24) THEN
            hrnew = hrnew - 24
            nday  = nday + 1
         END IF

         dynew = dyold
         monew = moold
         yrnew = yrold
         DO i = 1, nday
            dynew = dynew + 1
            IF (dynew.GT.mday(monew)) THEN
               dynew = dynew - mday(monew)
               monew = monew + 1
               IF (monew .GT. 12) THEN
                  monew = 1
                  yrnew = yrnew + 1
                  ! If the year changes, recompute the number of days in February
                  mday(2) = nfeb(yrnew)
               END IF
            END IF
         END DO

      ELSE IF (idt.LT.0) THEN

         frnew = frold - nfrac
         IF (frnew .LT. 0) THEN
            frnew = frnew + ifrc
            nsec = nsec - 1
         END IF

         scnew = scold - nsec
         IF (scnew .LT. 00) THEN
            scnew = scnew + 60
            nmin  = nmin + 1
         END IF

         minew = miold - nmin
         IF (minew .LT. 00) THEN
            minew = minew + 60
            nhour  = nhour + 1
         END IF

         hrnew = hrold - nhour
         IF (hrnew .LT. 00) THEN
            hrnew = hrnew + 24
            nday  = nday + 1
         END IF

         dynew = dyold
         monew = moold
         yrnew = yrold
         DO i = 1, nday
            dynew = dynew - 1
            IF (dynew.eq.0) THEN
               monew = monew - 1
               IF (monew.eq.0) THEN
                  monew = 12
                  yrnew = yrnew - 1
                  ! If the year changes, recompute the number of days in February
                  mday(2) = nfeb(yrnew)
               END IF
               dynew = mday(monew)
            END IF
         END DO
      END IF

c  Now construct the new mdate

      nlen = LEN(ndate)

      IF (nlen.GT.14) THEN
         WRITE(ndate(1:14),19) yrnew, monew, dynew, hrnew, minew, scnew
         WRITE(hfrc,'(I10)') frnew+1000000000
         ndate = ndate(1:14)

      ELSE IF (nlen.eq.14) THEN
         WRITE(ndate(1:14),19) yrnew, monew, dynew, hrnew, minew, scnew
 19   format(I4,I2.2,I2.2,I2.2,I2.2,I2.2)
         IF (nlen.eq.14) ndate = ndate(1:14)

      ELSE IF (nlen.eq.12) THEN
         WRITE(ndate,16) yrnew, monew, dynew, hrnew, minew
 16   format(I4,I2.2,I2.2,I2.2,I2.2)

      ELSE IF (nlen.eq.10) THEN
         WRITE(ndate,13) yrnew, monew, dynew, hrnew
 13   format(I4,I2.2,I2.2,I2.2)

      ELSE IF (nlen.eq.8) THEN
         WRITE(ndate,10) yrnew, monew, dynew
 10   format(I4,I2.2,I2.2)

      END IF

c      IF (olen.GE.11) ndate(11:11) = sp

      return
      end
c
c x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x
c
      FUNCTION nfeb ( year ) RESULT (num_days)
  
c Compute the number of days in February for the given year
  
      IMPLICIT NONE
  
      INTEGER :: year
      INTEGER :: num_days
  
      num_days = 28 ! By default, February has 28 days ...
      IF (MOD(year,4).eq.0) THEN
         num_days = 29  ! But every four years, it has 29 days ...
         IF (MOD(year,100).eq.0) THEN
            num_days = 28  ! Except every 100 years, when it has 28 days ...
            IF (MOD(year,400).eq.0) THEN
               num_days = 29  ! Except every 400 years, when it has 29 days.
            END IF
         END IF
      END IF
  
      END FUNCTION nfeb
