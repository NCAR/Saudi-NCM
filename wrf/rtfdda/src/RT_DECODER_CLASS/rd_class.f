       program rd_sndg

       parameter (nobs=5000)
c
c 080801
c
c reading YPG soundings
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
       character*12 data_type, id, site
       character*16 type_id
       character*16  a1,a2,a3,a4,a5,a6,a7,a8,a9
       character*16  b1,b2,b3,b4,b5,b6
       character*80  line

      real ht(nobs),spd(nobs),dir(nobs),
     &           wqc(nobs),slp(nobs),ztd(nobs)
      real tt(nobs),ttc(nobs),ww(nobs),
     &           prs(nobs),uu(nobs),vv(nobs)

      integer   ht_qc(nobs),spd_qc(nobs),dir_qc(nobs),
     &          tt_qc(nobs),ttc_qc(nobs),pp_qc(nobs),
     &          uu_qc(nobs),vv_qc(nobs),rh_qc(nobs),
     &          td_qc(nobs)

       character*14 ndate
       character*20 date
       character*40 strname, strnam2, string2, string4
       logical bogus
       character*1 aline , cline(132)
       character*4 ahr, amn, asc

       data deg2rad / .0174532925 /

       strname  = '99001                                   '
       strnam2  = 'FM-32 TEMP                              '
       string2  = '                                        '
       string4  = '                                        '

       iunin = 10
c output unit
       iuno = 50

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

       read (iunin,'(35x,a10)') data_type
        print *,' data_type= ',data_type
       read (iunin,'(35x,a5 )') id
        print *,' id= ',id
       read (iunin,'(35x,a6 )') site
        print *,' site= ',site
       read (iunin,*) a1,a2,a3,a4,a5,b1,b2,b3,b4,lon,lat,alt
        print *,' lon-lat-alt= ',lon,lat,alt
       read (iunin,*) a1,a2,a3,a4,a5,a6,a7,a8,a9,iyr,imo,idy,b1

       print *,' 1-a1-a2-a3-a4-a5-a6-a7-a8-a9-b1= ',
     &                 a1,a2,a3,a4,a5,a6,a7,a8,a9,b1

       call parse_line(b1,':',1,ahr)
       read(ahr,*) ihr
       call parse_line(b1,':',2,amn)
       read(amn,*) imn
       call parse_line(b1,':',3,asc)
       read(asc,*) isc

       if(isc >= 60) call advance_s(iyr,imo,idy,ihr,imn,isc)

       print *,' 2-a1-a2-a3-a4-a5-a6-a7-a8-a9-b1= ',
     &                 a1,a2,a3,a4,a5,a6,a7,a8,a9,b1
       print *,' yr-mn-dy-hr-mn-sec= ',iyr,imo,idy,ihr,imn,isc
    
       xlev = alt

       read (iunin,'(a80)') line
       call parse_line(line,',',2,type_id)
       print*,'type_id after parse_line = ',type_id

       do n=1,9
        read (iunin,'(a1 )') aline
       enddo

       write(string2( 1:10), '(a10)') data_type
       write(string2(12:16), '(a5 )') id
       write(string2(18:23), '(a6 )') site
       write(string2(25:40), '(a16 )') type_id

       n = 1
 10    continue     
       read (iunin,*,end=999) 
     &   no,pres,temp,
     &   dewpt,rh,uwind,vwind,wspd,wdir,
     &   dz,lon2,lat2,
     &   range,ang,alt2,
     &   q_pres,q_tt,q_rh,q_uu,q_vv,q_uv

        print *,' no,pres,temp= ',no,pres,temp
        print *,' dewpt,rh,uwind,vwind,wspd,wdir= ',
     &             dewpt,rh,uwind,vwind,wspd,wdir 
        print *,' dz,lon2,lat2= ',dz,lon2,lat2
        print *,' range,ang,alt2= ',range,ang,alt2
        print *,' q_pres,q_tt,q_rh,q_uu,q_vv,q_uv= ',
     &             q_pres,q_tt,q_rh,q_uu,q_vv,q_uv
   
       if( pres .ne. spval )then
        prs(n)  = pres*100.
        pp_qc(n)= 0.
       else
        prs(n)  = qc_spv
        pp_qc(n)= qc_spv
       endif
        ! thin the obs
        if(prs(n) .ge. 85000 .and. n .gt. 1) then
           if (abs(prs(n)-prs(n-1)) .lt. 200) goto 10
        else 
           if (abs(prs(n)-prs(n-1)) .lt. 500) goto 10
        endif

       if( wdir .ne. spval .and. wspd .ne. spval )then
        spd(n)    = wspd
        spd_qc(n) = 0.
        dir(n)    = wdir
        dir_qc(n) = 0.
       else
        spd(n) = qc_spv 
        dir(n) = qc_spv
        spd_qc(n) = qc_spv
        dir_qc(n) = qc_spv
       endif

       if( vwind .ne. spval .and. uwind .ne. spval )then
        uu(n) = uwind 
        vv(n) = vwind
        uu_qc(n) = 0. 
        vv_qc(n) = 0. 
       else
        uu(n) = qc_spv
        vv(n) = qc_spv
        uu_qc(n) = qc_spv 
        vv_qc(n) = qc_spv 
       endif

       if( temp .ne. spval )then
        tt(n)    = temp + 273.16
        tt_qc(n) = 0.
       else
        tt(n)    = qc_spv 
        tt_qc(n) = qc_spv
       endif

       if( dewpt .ne. spval )then
        ztd(n)   = dewpt + 273.16 
        td_qc(n) = 0.
       else
        ztd(n)   = qc_spv 
        td_qc(n) = qc_spv
       endif
       
       n = n + 1
       goto 10

 999   continue
c
       stindx   = 999
       iseq_num = 99
       kkl      = n-1

       write (ndate(1:4),fmt='(i4.4)') iyr
       write (ndate(5:6),fmt='(i2.2)') imo
       write (ndate(7:8),fmt='(i2.2)') idy
       write (ndate(9:10),fmt='(i2.2)') ihr
       write (ndate(11:12),fmt='(i2.2)') imn
       write (ndate(13:14),fmt='(i2.2)') isc

       call write_obs (stindx,prs,ht,tt,ztd,spd,dir,
     &                 slp,xlev,lat,lon,
     &                 uu, vv, ht_qc, tt_qc, td_qc,
     &            dir_qc, spd_qc, ndate, imn, isc, kkl,
     &          strname,string2,strnam2,string4,bogus,
     &                                  iseq_num,iuno )

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
     *                    mdate , imin , isc, kx ,
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

!

      SUBROUTINE advance_s(year,month,day,hour,minute,sec)

      integer, dimension(12) ::
     &days=(/31,28,31,30,31,30,31,31,30,31,30,31/)
      integer :: year,month,day,hour,minute,sec

      if(mod(year,400) == 0) then
        days(2)=29
      else
        if((mod(year,4) == 0) .and. (mod(year,100) /= 0)) days(2)=29
      endif

      do while (sec >= 60)
        sec=sec-60
        minute=minute+1
        if(minute >= 60) then
          minute=minute-60
          hour=hour+1
          if(hour >= 24) then
            hour=hour-24
            day=day+1;
            if(day > days(month)) then
              day=day-days(month)
              month=month+1
              if(month > 12) then
                month=month-12
                year=year+1
              endif
            endif
          endif
        endif
      end do

      return

      END
