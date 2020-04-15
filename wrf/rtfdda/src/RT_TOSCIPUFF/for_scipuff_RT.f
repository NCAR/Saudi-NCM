      program for_scipuff_RT
!
      integer, parameter :: iout0=11,iout1=12
      real, parameter :: kappa=0.286
      character*132 :: fn,use
      integer, dimension(50,20) :: bhi
      real, dimension(20,20) :: bhr
      character(len=80), dimension(50,20) :: bhic
      character(len=80), dimension(20,20) :: bhrc
      integer :: flag
      integer :: ndim
      real :: time
      integer, dimension(4) :: start_index, end_index
      character (len=80) :: exe_name
      character (len= 4) :: staggering
      character (len= 4) :: ordering
      character (len=24) :: start_date
      character (len=24) :: current_date
      character (len= 9) :: name
      character (len=25) :: units
      character (len=46) :: description
      character (len= 4) :: yyyy
      character (len= 2) :: mm,dd,hh,mn
      character (len= 3) :: mm_tmp,dd_tmp,hh_tmp,mn_tmp
      character :: domain
      integer :: AllocateStatus
      integer :: domain_id
      integer :: year,month,day,hour,minute
      integer :: year0,month0,day0,hour0,minute0
      integer :: e_hour,e_min,e_sec,total_min
      integer*8 :: nsecs,nsecs0
      real, dimension(:,:,:), allocatable :: data
      real, dimension(:,:,:), allocatable :: uc,vc,ud,vd,wfull,whalf
      real, dimension(:,:,:), allocatable :: t,pp,theta,zagl
      real, dimension(:,:,:), allocatable :: lat,lon,ter,mapfct,pstar
      real, dimension(:,:,:), allocatable :: pbl,hfx
      real, dimension(:,:,:), allocatable :: sigma
      real, dimension(:), allocatable :: sigma_f
      real :: r=287.04,g=9.8
      character (len=2) :: top
      integer :: end_index1,end_index2,end_index3
!
      integer, dimension(12) :: days=(/31,28,31,30,31,30,31,31,30,31,30,31/)
!
      i=iargc()
      if(i == 0) then
        call getarg(0,use)
        j=index(use,' ')
        print*,'Usage: ',use(1:j),'filename [number of vertical levels]'
        stop
      endif
!
      call getarg(1,fn)

      ktop=20
      if(i > 1) then
        call getarg(2,top)
        print*,'Overwrite default number of levels to ',top,'!'
        read(top,'(i2)') ktop
      endif

      open(10,file=fn,status='old',form='unformatted')
!
      read(10,iostat=ierr) flag

      icnt=0                             ! A counter to determine whether to
                                         ! write .cfg file. Does when icnt=1.
      do while(ierr == 0)

         if(flag == 0) then
            read(10,iostat=ier) bhi, bhr, bhic, bhrc
            if(ier/=0) then
               write(*,'("Error reading big header")')
               call abort()
            endif
!
         elseif(flag == 1) then
            read(10,iostat=ier) ndim, start_index, end_index, time, &
                staggering, ordering, current_date, name, units, description
            if(ier/=0) then
              write(*,'("Error reading subheader")')
              call abort()
            endif
!
            

            ix=end_index(1)
            jx=end_index(2)
            kx=end_index(3)
            dx=bhr(9,1)*0.001      !! in km
            xlatc=bhr(2,1)
            xlonc=bhr(3,1)
            domain_id=bhi(13,1)

            if(domain_id == 1) then
              domain='A'
            elseif(domain_id == 2) then
              domain='B'
            elseif(domain_id == 3) then
              domain='C'
            elseif(domain_id == 4) then
              domain='D'
            endif
!
            allocate(data(ix,jx,kx))
!
            read(10) data
!
            if(name(1:4) == 'U   ') then
              allocate(ud(ix,jx,kx))
              end_index1=ix
              end_index2=jx
              end_index3=kx
              do kk=1,kx
              do ii=1,ix
              do jj=1,jx
                 ud(ii,jj,kk)=data(ii,jj,kk)
              enddo
              enddo
              enddo
            elseif(name(1:4) == 'V   ') then
              allocate(vd(ix,jx,kx))
              do kk=1,kx
              do ii=1,ix
              do jj=1,jx
                 vd(ii,jj,kk)=data(ii,jj,kk)
              enddo
              enddo
              enddo
            elseif(name(1:4) == 'W   ') then
              allocate(wfull(ix,jx,kx))
              do kk=1,kx
              do ii=1,ix
              do jj=1,jx
                 wfull(ii,jj,kk)=data(ii,jj,kk)
              enddo
              enddo
              enddo
            elseif(name(1:4) == 'T   ') then
              allocate(t(ix,jx,kx))
              do kk=1,kx
              do ii=1,ix
              do jj=1,jx
                 t(ii,jj,kk)=data(ii,jj,kk)
              enddo
              enddo
              enddo
            elseif(name(1:4) == 'PP  ') then
              allocate(pp(ix,jx,kx))
              do kk=1,kx
              do ii=1,ix
              do jj=1,jx
                 pp(ii,jj,kk)=data(ii,jj,kk)
              enddo
              enddo
              enddo
            elseif(name(1:4) == 'LATI') then
              allocate(lat(ix,jx,kx))
              do kk=1,kx
              do ii=1,ix
              do jj=1,jx
                 lat(ii,jj,kk)=data(ii,jj,kk)
              enddo
              enddo
              enddo
            elseif(name(1:4) == 'LONG') then
              allocate(lon(ix,jx,kx))
              do kk=1,kx
              do ii=1,ix
              do jj=1,jx
                 lon(ii,jj,kk)=data(ii,jj,kk)
              enddo
              enddo
              enddo
            elseif(name(1:4) == 'TERR') then
              allocate(ter(ix,jx,kx))
              do kk=1,kx
              do ii=1,ix
              do jj=1,jx
                 ter(ii,jj,kk)=data(ii,jj,kk)
              enddo
              enddo
              enddo
            elseif(name(1:8) == 'MAPFACCR') then
              allocate(mapfct(ix,jx,kx))
              do kk=1,kx
              do ii=1,ix
              do jj=1,jx
                 mapfct(ii,jj,kk)=data(ii,jj,kk)
              enddo
              enddo
              enddo
            elseif(name(1:4) == 'PSTA') then
              allocate(pstar(ix,jx,kx))
              do kk=1,kx
              do ii=1,ix
              do jj=1,jx
                 pstar(ii,jj,kk)=data(ii,jj,kk)
              enddo
              enddo
              enddo
            elseif(name(1:4) == 'PBL ') then
              allocate(pbl(ix,jx,kx))
              do kk=1,kx
              do ii=1,ix
              do jj=1,jx
                 pbl(ii,jj,kk)=data(ii,jj,kk)
		 if(pbl(ii,jj,kk) < 5.) pbl(ii,jj,kk)=5.
              enddo
              enddo
              enddo
            elseif(name(1:4) == 'SHFL') then
              allocate(hfx(ix,jx,kx))
              do kk=1,kx
              do ii=1,ix
              do jj=1,jx
                 hfx(ii,jj,kk)=data(ii,jj,kk)
              enddo
              enddo
              enddo
            elseif(name(1:4) == 'SIGM') then
              allocate(sigma(ix,jx,kx))
              do kk=1,kx
              do ii=1,ix
              do jj=1,jx
                 sigma(ii,jj,kk)=data(ii,jj,kk)
              enddo
              enddo
              enddo
            endif
!
            deallocate(data)
!
         elseif(flag == 2) then

            icnt=icnt+1

            allocate(uc(end_index1,end_index2,end_index3))
            allocate(vc(end_index1,end_index2,end_index3))
            allocate(whalf(end_index1,end_index2,end_index3))
            allocate(theta(end_index1,end_index2,end_index3))
            allocate(zagl(end_index1,end_index2,end_index3))
            allocate(sigma_f(end_index3+1))
!
            sigma_f(1)=0.
            do kk=2,end_index3
               sigma_f(kk)=2.*sigma(kk-1,1,1)-sigma_f(kk-1)
            enddo
            sigma_f(end_index3+1)=1.
!
            ptop=bhr(2,2)     !! in pa
            tlp=bhr(4,5)      !! base state lapse rate
            t0=bhr(3,5)       !! base state sea level temperature in K
            p0=bhr(2,5)       !! base state sea level pressure in Pa
!
!           year0=bhi(5,11)         !! start time, year
!           month0=bhi(6,11)        !! start time, month
!           day0=bhi(7,11)          !! start time, day
!           hour0=bhi(8,11)         !! start time, hour
!           min0=bhi(9,11)          !! start time, minute
            freq=bhr(4,12)          !! time interval (min) btw output
!
            read(current_date,'(i4,4(x,i2))') year,month,day,hour,minute
!
            imin_R=mod(minute,5)
            if(imin_R <= 2) then
              minute=minute-imin_R
            else
              minute=minute+(5-imin_R)
            endif
!
            if(minute >= 60) then
              minute=minute-60
              hour=hour+1
            endif

            if(hour >= 24) then
               hour=hour-24
               day=day+1
            endif

            if(mod(year,100) == 0) then
              if(mod(year,400) == 0) then
                days(2)=29
              endif
            else
              if(mod(year,4) == 0) then
                days(2)=29
              endif
            endif

            if(day > days(month)) then
              day=day-days(month)
              month=month+1
            endif 

            if(month > 12) then
              month=month-12
              year=year+1
            endif
               
!
            write(yyyy,'(i4)') year
            write(mm_tmp,'(i3)') 100+month
            mm=mm_tmp(2:3)
            write(dd_tmp,'(i3)') 100+day
            dd=dd_tmp(2:3)
            write(hh_tmp,'(i3)') 100+hour
            hh=hh_tmp(2:3)
            write(mn_tmp,'(i3)') 100+minute
            mn=mn_tmp(2:3)
!
            call sec_from_date_time(year,1,1,0,0,nsecs0)
            call sec_from_date_time(year,month,day,hour,minute,nsecs)

            isec_diff=nsecs-nsecs0
            call elaps(isec_diff,e_hour,e_min,e_sec)
            close(iout0)
            open(iout0,file=domain//yyyy//mm//dd//hh//mn,form='formatted')
!
            do kk=1,end_index3
            do ii=1,end_index1-1
            do jj=1,end_index2-1
               uc(ii,jj,kk)=0.25*(ud(ii,jj,kk)+ud(ii+1,jj,kk)+ &
                                  ud(ii,jj+1,kk)+ud(ii+1,jj+1,kk))
               vc(ii,jj,kk)=0.25*(vd(ii,jj,kk)+vd(ii+1,jj,kk)+ &
                                  vd(ii,jj+1,kk)+vd(ii+1,jj+1,kk))
               theta(ii,jj,kk)=t(ii,jj,kk)*(1000./((sigma(kk,1,1)* &
                               pstar(ii,jj,1)+ptop+pp(ii,jj,kk))*0.01))**kappa
               pr=sigma(kk,1,1)*pstar(ii,jj,1)+ptop
               term1=r*tlp*(alog(pr/p0))**2/2./g
               term2=r*t0*alog(pr/p0)/g
               zagl(ii,jj,kk)=-(term1+term2)-ter(ii,jj,1)
            enddo
            enddo
            enddo
!
            do kk=1,end_index3
            do ii=1,end_index1-1
            do jj=1,end_index2-1
               whalf(ii,jj,kk)=0.5*(wfull(ii,jj,kk)+wfull(ii,jj,kk+1))
            enddo
            enddo
            enddo
!
            if(icnt == 1) then
              open(iout1,file=domain//yyyy//mm//dd//hh//mn//'.cfg', &
                   form='formatted')
              write(iout1,'(i2,2i4," : kmax3d, jmax, imax")') ktop, &
                   end_index2-1,end_index1-1
              write(iout1,'(2f11.6," : x,y (km) - grid spacing")') dx,dx
              write(iout1,'(2f11.6," : xlonc, xlatc - central lon/lat (deg)", &
&                   " of coarse domain")') xlonc,xlatc
              write(iout1,'(i4,4i3," : year,month,day,hour,min of forecast", &
&                  " start time (UTC)")') year,1,1,0,0
              write(iout1,'("5 : numfields - number of fields in this file")')
              write(iout1,'("TERRAIN")')
              write(iout1,'("1 1 :k,ktop")')
              do jj=1,end_index2-1
                 write(iout1,'(100f7.1)') (ter(ii,jj,1),ii=1,end_index1-1)
              enddo
!
              write(iout1,'("LATITCRS")')
              write(iout1,'("1 1 :k,ktop")')
              do jj=1,end_index2-1
                 write(iout1,'(100f10.4)') (lat(ii,jj,1),ii=1,end_index1-1)
              enddo
!
              write(iout1,'("LONGICRS")')
              write(iout1,'("1 1 :k,ktop")')
              do jj=1,end_index2-1
                 write(iout1,'(100f10.4)') (lon(ii,jj,1),ii=1,end_index1-1)
              enddo
!
              write(iout1,'("MAPFACCR")')
              write(iout1,'("1 1 :k,ktop")')
              do jj=1,end_index2-1
                 write(iout1,'(100f8.5)') (mapfct(ii,jj,1),ii=1,end_index1-1)
              enddo
!
              write(iout1,'("ZAGL")')
              do kk=end_index3,end_index3-ktop+1,-1
                 write(iout1,'(2i3," :k,ktop")') end_index3+1-kk,ktop
                 do jj=1,end_index2-1
                    write(iout1,'(100f8.1)') (zagl(ii,jj,kk),ii=1,end_index1-1)
                 enddo
              enddo
              close(iout1)
            endif
!
            write(iout0,'(i2,2i4," : kmax3d, jmax, imax")') end_index3, &
                  end_index2-1,end_index1-1
            write(iout0,'(i4,4i3," : year,month,day,hour,min of forecast", &
&                " start time (UTC)")') year,1,1,0,0
            write(iout0,'(i4,2i3," : elapsed time hours,min,seconds")') &
                  e_hour,e_min,e_sec
            write(iout0,'("6 : numfields - number of fields in this file")')
            write(iout0,'("THETA")')
            do kk=end_index3,end_index3-ktop+1,-1
               write(iout0,'(2i3," :k,ktop")') end_index3+1-kk,ktop
               do jj=1,end_index2-1
                  write(iout0,'(100f6.1)') (theta(ii,jj,kk),ii=1,end_index1-1)
               enddo
            enddo
!
            write(iout0,'("UC")')
            do kk=end_index3,end_index3-ktop+1,-1
               write(iout0,'(2i3," :k,ktop")') end_index3+1-kk,ktop
               do jj=1,end_index2-1
                  write(iout0,'(100f7.2)') (uc(ii,jj,kk),ii=1,end_index1-1)
               enddo
            enddo
!
            write(iout0,'("VC")')
            do kk=end_index3,end_index3-ktop+1,-1
               write(iout0,'(2i3," :k,ktop")') end_index3+1-kk,ktop
               do jj=1,end_index2-1
                  write(iout0,'(100f7.2)') (vc(ii,jj,kk),ii=1,end_index1-1)
               enddo
            enddo
!
            write(iout0,'("WHALF")')
            do kk=end_index3,end_index3-ktop+1,-1
               write(iout0,'(2i3," :k,ktop")') end_index3+1-kk,ktop
               do jj=1,end_index2-1
                  write(iout0,'(100f8.4)') (whalf(ii,jj,kk),ii=1,end_index1-1)
               enddo
            enddo
!
            kk=1
            write(iout0,'("PBL HT")')
            write(iout0,'(2i3," :k,ktop")') kk,kk
            do jj=1,end_index2-1
               write(iout0,'(100f8.2)') (pbl(ii,jj,1),ii=1,end_index1-1)
            enddo
!
            write(iout0,'("HFX")')
            write(iout0,'(2i3," :k,ktop")') kk,kk
            do jj=1,end_index2-1
               write(iout0,'(100f7.1)') (hfx(ii,jj,1),ii=1,end_index1-1)
            enddo
!
            deallocate(t,pp,uc,vc,ud,vd,wfull,whalf,theta,zagl)
            deallocate(pstar,lat,lon,ter,mapfct,pbl,hfx)
            deallocate(sigma,sigma_f)
         endif
!
         read(10,iostat=ierr) flag
!
      enddo
!
!     deallocate(t,pp,uc,vc,ud,vd,wfull,whalf,theta,zagl)
!     deallocate(pstar,lat,lon,ter,mapfct,pbl,hfx)
!     deallocate(sigma,sigma_f)
!
      end program for_scipuff_RT
!
!
!
      subroutine elaps(isecs,e_hour,e_min,e_sec)

      integer :: e_hour,e_min,e_sec
!
      e_hour=int(isecs/3600.)
      e_min=int((mod(isecs,3600))/60.)
      e_sec=isecs-e_hour*3600-e_min*60

      return
      end subroutine elaps
!
!
!

      subroutine sec_from_date_time(iyear,month,iday,ihour,iminute,n_secs)
!
      integer, dimension(12) :: mdays
      integer*8 :: n_secs
!
      mdays=(/31,28,31,30,31,30,31,31,30,31,30,31/)
!
      n_secs=0
!
      do iy=1,iyear-1970
         idays=365
!
         if(mod(iy+1969,100) == 0) then
           if(mod(iy+1969,400) == 0) then
             idays=366
           endif
         else
           if(mod(iy+1,4) == 0) idays=366
         endif
!
         n_secs=n_secs+idays*86400
      enddo
!
      if(mod(iyear,100) == 0) then
        if(mod(iyear,400) == 0) then
          mdays(2)=29
        endif
      else
        if(mod(iyear,4) == 0) mdays(2)=29
      endif
!
      do im=1,month-1
         n_secs=n_secs+mdays(im)*86400
      enddo
!
      do id=1,iday-1
         n_secs=n_secs+86400
      enddo
!
      do ih=1,ihour
         n_secs=n_secs+3600
      enddo
!
      do im=1,iminute
         n_secs=n_secs+60
      enddo
!
      return
      end
