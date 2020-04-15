       program rdraws


c            10        20        30        40        50        60        70
c 123456789 123456789 123456789 123456789 123456789 123456789 123456789 123456789 
c 1PARM = MNET;SLAT;SLON;SELV;TMPF;SKNT;DRCT;GUST;PMSL;ALTI;DWPF;RELH;WTHR;P24I    
c 2
c 3   STN  YYMMDD/HHMM      MNET     SLAT     SLON     SELV     TMPF     SKNT
c 4                         DRCT     GUST     PMSL     ALTI     DWPF     RELH
c 5                         WTHR     P24I
c 6   CEZ    970405/1230      1.00    37.29  -108.63  1803.00    32.00     9.00
c 7                         190.00 -9999.00 -9999.00    29.59    24.80     0.00
c 8                             RW -9999.00

c     ... This program reads in a data set called RAWS which is available
c         from the University of Utah on the web at 
c         ftp://ftp.met.utah.edu/pub/wx/sflist_all.utah

c     ... The standard input to this program is a little strange.  It
c         wants to know what station you would like to look at, the
c         yymmdd, hhmm, and the site type.  So as to allow some generality,
c         the 3-character name may be specified, or ";;;" may be used as
c         the wildcard for "all stations".  Similarly, the YYMMDD, HHmm and
c         site type may be specified directly as integers, or as -1 for the
c         "all" category.  Following are a couple of example input files.
c         Since this is FORTRAN, the first character in the line will be a
c         "C", whihc is not part of the input file.

c     ... Listing of every observation in the file, at all times.
C;;;
C-1,-1,-1

c     ... Listing of every observation at a specific date and time.
C;;;
C970418,1200,-1

c     ... Listing of every observation from DPG.
C;;;
C-1,-1,3

c         
c         

      character *15 staid
      character *3 staname,statest
      parameter (input=9,ioutput=10)

c     ... Read the info from the user

      call getinfo(statest,idate,itime,ifacility)

c     ... Read the header

      call header(input,iend)
      if(iend.ne.0) goto 1000

      ifirst=0
      istation=0
      istationkeep=0
100   continue

c        ... Read the next station's data

         call rdit(staname,iyymmdd,ihhmm,metnet,xlat,xlon,
     *             elev,t,spd,dir,gust,slp,altim,td,rh,iend,
     *             input)
 2       if(iend.ne.0) goto 1000

         istation=istation+1

c        ... Is this a keeper?

         call goodone (staname,iyymmdd,ihhmm,metnet,
     *             statest,idate,itime,ifacility,keep)

c        ... Output the particularly good obs.

         if(keep.eq.1) then
            istationkeep=istationkeep+1
            call output(staname,iyymmdd,ihhmm,metnet,xlat,xlon,
     *             elev,t,spd,dir,gust,slp,altim,td,rh,ifirst)

            p=-888888.
            z=elev
            t=t+273.15
            if (t.lt.-500) t=-888888.
            td=td+273.15
            if (td.lt.-500) td=-888888.
            if (t.lt.-500) td=-888888.
            if (t.lt.td) then
               t=-888888.
               td=-888888.
            endif
            if((spd.lt.-500).or.(dir.lt.0)) then
               spd=-888888.
               dir=-888888.
            endif
            if((slp.lt.-500).or.(slp.gt.2000)) then
               slp=-888888.
            else
                slp=slp*100
            endif
            ter=elev
            mdate=iyymmdd*100+ihhmm/100
            kx=1
            iseq_num=istationkeep 
 
             if(metnet .ne. 3) then

              write (staid,fmt='("99",i3.3,2x,"METNET=",i1)' )
     *            istationkeep,metnet
              call write_obs (p,z,t,td,spd,dir, 
     *                 slp, ter, xlat, xlon, mdate, kx, 
     *        staid(1:15) //'                         ',  
     *        staname(1:3) // '  SURFACE OBS FROM UNIVERSITY OF UTAH',
     *        'FM-12 SYNOP PRETEND                     ',
     *        '                                        ',
     *        .FALSE.,iseq_num , ioutput )

c             else if( 
c     *           (metnet .eq. 3 .and. staname(1:3) .eq. 'S18') .or. 
c     *           (metnet .eq. 3 .and. staname(1:3) .eq. 'S19') .or. 
c     *           (metnet .eq. 3 .and. staname(1:3) .eq. 'S20') )then

c              write (staid,fmt='("97",i3.3,2x,"METNET=",i1)' )
c     *            istationkeep,metnet
c              call write_obs (p,z,t,td,spd,dir, 
c     *                 slp, ter, xlat, xlon, mdate, kx, 
c     *        staid(1:15) //'                         ',  
c     *        staname(1:3) // '  ADDITIONAL SURFACE DATA FROM DPG   ',
c     *        'FM-12 SYNOP PRETEND                     ',
c     *        '                                        ',
c     *        .FALSE.,iseq_num , ioutput )
             endif

         endif

      goto 100

c     ... End of read data loop

1000  continue


      stop 99999
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine getinfo(statest,idate,itime,ifacility)
      character *3 statest
      read(*,10) statest
10    format(a3)
      read *,idate,itime,ifacility
cc      print *,statest,' ',idate,itime,ifacility
      write(15,200) statest,idate,itime,ifacility
 200  format(1x,' statest= ',a3,' date= ',i3,' time ',2i3)

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine header (input,iend)
      character *1 foo
      read (input,100,end=1000) foo
      read (input,100) foo
      read (input,100) foo
      read (input,100) foo
      read (input,100) foo
      read (input,100) foo

      read (input,100) foo
      read (input,100) foo
      read (input,100) foo

100   format(a1)
      iend=0
      return
1000  continue
      print *,'found end of input data from header, maybe a day change'
      iend=1
      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine rdit(staname,iyymmdd,ihhmm,metnet,xlat,xlon,
     *                elev,t,spd,dir,gust,slp,altim,td,rh,iend,
     *                input)
      character *3 staname

      read(input,101,end=1000) staname,iyymmdd,ihhmm,metnet,xlat,xlon,
     *                         elev,t,spd,dir,gust,slp,altim,td,rh

      iend=0
      return

101   format(4x,a3,4x,i6,1x,i4,6x,i1,3x,5(1x,f8.2),
     *       27x,6(1x,f8.2))

1000  continue
      iend=1
      print *,'found end of input data'
      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine goodone (staname,iyymmdd,ihhmm,metnet,
     *             statest,idate,itime,ifacility,keep)
      character *3 staname,statest

      if(((staname.eq.statest).or.(statest.eq.';;;')).and.
     *   ((iyymmdd.eq.idate).or.(idate.lt.0)) .and.
     *   ((ihhmm  .eq.itime).or.(itime.lt.0)) .and.
     *   ((metnet .eq.ifacility).or.(ifacility.lt.0))) then
         keep=1
      else
         keep=0
      endif
      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine output(staname,iyymmdd,ihhmm,metnet,xlat,xlon,
     *             elev,t,spd,dir,gust,slp,altim,td,rh,ifirst)
      character *3 staname

      if(ifirst.eq.0) then
         write(15,1000)
         write(15,1001)
         write(15,1002)
1000     format ('  DATE    ','  RAWS  ','STATION',
     * '  LAT   ', '  LON   ', ' ELEV   ', '   T    ',     
     * '   TD   ', '  SPD   ', '  DIR   ', '  SLP')
1001     format ('YYMMDDHHmm','  SITE  ',' NAME  ',
     * ' degree ', ' degree ', '   m    ', '   C    ',     
     * '    C   ', '  m/s   ', ' degree ', '  hPa')
1002     format ('----------------------------------------------',
     * '--------------------------------------------')
         ifirst=1
      endif

   
      if((td.lt.-1000).and.(rh.gt.0).and.(t.gt.-1000)) then
         td=1./(1./(273.15+(t-32.)/1.8)-(1./5418.12)*alog(rh/100.))
         td=td-273.15
      else if(td.gt.-1000) then
         td=(td-32.)/1.8
      endif

      if(spd.gt.0) then
         spd=spd*0.5144444
      endif

c     if((t.gt.-1000).and.(altim.gt.-1000).and.(slp.lt.-1000)) then
c        slp=(altim*33.86)*
c    *       exp((9.8*elev)/(287.*((t-32.)/1.8+273.15)))
c     endif
      if(t.gt.-1000) t=(t-32.)/1.8

      if( (metnet .ne. 3) .or.
     *         (metnet .eq. 3 .and. staname(1:3) .eq. 'S18') .or.
     *         (metnet .eq. 3 .and. staname(1:3) .eq. 'S19') .or.
     *         (metnet .eq. 3 .and. staname(1:3) .eq. 'S20') )then

       write (15,1010) iyymmdd,ihhmm,metnet,staname,xlat,xlon,elev,
     *               t,td,spd,dir,slp
1010   format(i6.6,i4.4,3x,i2,3x,2x,a3,2x,2(f8.2),6(f8.1))
      endif

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      SUBROUTINE write_obs ( p , z , t , td , spd , dir , 
     *                      slp , ter , xlat , xlon , mdate , kx , 
     * string1 , string2 , string3 , string4 , bogus , iseq_num ,
     * iunit )

      dimension p(kx), z(kx),t(kx),td(kx),spd(kx),dir(kx)

      character *20 date_char
      character *40 string1, string2 , string3 , string4
      CHARACTER *84  rpt_format 
      CHARACTER *22  meas_format 
      CHARACTER *14  end_format
      logical bogus


      rpt_format =  ' ( 2f20.5 , 2a40 , ' 
     *             // ' 2a40 , 1f20.5 , 5i10 , 3L10 , ' 
     *             // ' 2i10 , a20 ,  13( f13.5 , i7 ) ) '
      meas_format =  ' ( 10( f13.5 , i7 ) ) '
      end_format = ' ( 3 ( i7 ) ) ' 

      write (date_char(9:16),fmt='(i8)') mdate
      if (mdate/1000000 .GT. 70 ) then
         date_char(7:8)='19'
      else
         date_char(7:8)='20'
      endif
      date_char(17:20)='0000'
      date_char(1:6)='      '

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
         WRITE ( UNIT = iunit , ERR = 19 , FMT = meas_format ) 
     *          p(k), 0, z(k),0, t(k),0, td(k),0, 
     *          spd(k),0, dir(k),0, 
     *          -888888.,0, -888888.,0,-888888.,0, -888888.,0
100   continue
      WRITE ( UNIT = iunit , ERR = 19 , FMT = meas_format ) 
     * -777777.,0, -777777.,0,float(kx),0,
     * -888888.,0, -888888.,0, -888888.,0, 
     * -888888.,0, -888888.,0, -888888.,0, 
     * -888888.,0
      WRITE ( UNIT = iunit , ERR = 19 , FMT = end_format )  kx, 0, 0

      return
19    continue
      print *,'troubles writing a sounding'
      stop 19
      END
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
