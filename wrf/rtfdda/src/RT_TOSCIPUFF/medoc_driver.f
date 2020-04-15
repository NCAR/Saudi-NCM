      program medoc_driver
!
!     parameter(ni=51,nj=51,nk=20)
!
      character*80 :: filename,filename2, exe_name
      character*2 :: proj
      character*80 :: cfg_file
      character*3 :: ahour,amin
      character*8 :: timex
      character*80 :: time0
      integer :: flag0,openmode
      integer, dimension(12) :: days=(/31,28,31,30,31,30,31,31,30,31,30,31/)
      logical :: there
!
      real, allocatable, dimension(:,:) :: xlat,xlon,terr,pblhgt,htflux
      real, allocatable, dimension(:,:,:) :: u,v,w,th,zagl
      real, allocatable, dimension(:) :: ztrans,ylat,ylon
      real, allocatable, dimension(:,:,:) :: zadjust
!
      real, allocatable, dimension(:,:,:) :: slopef
      real, allocatable, dimension(:,:,:) :: hfact
!     real slopef(ni,nj,nk),hfact(4,ni,nj)

      integer, allocatable, dimension(:,:,:) :: kindex
      integer, allocatable, dimension(:,:) :: iindex,jindex
!
      real, allocatable, dimension(:,:,:) :: CalcBuffer1
      real, allocatable, dimension(:,:) :: buffer1,buffer2
      real, allocatable, dimension(:) :: kbuff

      character(len=120), allocatable, dimension(:) :: arg

      character(len=80) :: outf='medoc.fmt'

!     integer kbuff(nk)

      j=iargc()
      if(j == 0) then
        call getarg(0,exe_name)
        print*,'Usage: ',trim(exe_name),'-inp <input MET file> -cfg <input config file> -L (Lambert conformal) | -P (polar Stereographic) [-out <output file name>]'
        stop
      endif

      allocate(arg(j))

      do i=1,j
         call getarg(i,arg(i))
      enddo

      do i=1,j
         if(index(arg(i),'-inp') > 0) filename=arg(i+1)
         if(index(arg(i),'-cfg') > 0) cfg_file=arg(i+1)
         if(index(arg(i),'-out') > 0) outf=arg(i+1)
         if(index(arg(i),'-L') > 0) xn=0.716
         if(index(arg(i),'-P') > 0) xn=1.0
      enddo

!
      nfile1=11
!
      call readcfg(nfile1,cfg_file,nx,ny,nz,dxx,dyy,xlatc,xlonc, &
                   jyear,jmon,jday,jhour,jmin,flag0)
!
      if(flag0 < 0) then
        call ConvertError(cfg_file,flag0)
        stop
      endif
!
      allocate(xlat(nx,ny))
      allocate(xlon(nx,ny))
      allocate(u(nx,ny,nz))
      allocate(v(nx,ny,nz))
      allocate(w(nx,ny,nz))
      allocate(th(nx,ny,nz))
      allocate(terr(nx,ny))
      allocate(zagl(nx,ny,nz))
      allocate(pblhgt(nx,ny))
      allocate(htflux(nx,ny))

      allocate(ztrans(nz))
      allocate(zadjust(nx,ny,nz))
      allocate(ylat(ny)) 
      allocate(ylon(nx))

      allocate(slopef(nx,ny,nz))
      allocate(hfact(4,nx,ny))
      allocate(kindex(nx,ny,nz))
      allocate(iindex(nx,ny))
      allocate(jindex(nx,ny))

      allocate(CalcBuffer1(nx,ny,nz))
      allocate(buffer1(nx,ny))
      allocate(buffer2(nx,ny))

      allocate(kbuff(nz))

      write(ahour,'(i3)') 100+jhour
      write(amin,'(i3)')  100+jmin
      write(timex,101) jmon,jday,jyear
  101 format(i1,'/',i1,'/',i4)
      time0=timex//' '//ahour(2:3)//':'//amin(2:3)//':00'
!
      dx=dxx
      dy=dyy
!
      openmode=-1
!
      call readmm5(nfile1,cfg_file,1,1,0,1,'LATITCRS  ',nx,ny,1,xlat, &
                   flag0)
      if(flag0 < 0) then
        call ConvertError(cfg_file,flag0)
        stop
      endif
!
      call readmm5(nfile1,cfg_file,1,1,0,1,'LONGICRS  ',nx,ny,1,xlon, &
                   flag0)
      if(flag0 < 0) then
        call ConvertError(cfg_file,flag0)
        stop
      endif
!
      call newgrid(nx,ny,xlat,xlon,ylat,ylon,dlat,dlon)
!
      call hparams(nx,ny,xlatc,xlonc,xlat,xlon,ylat,ylon,dx,dy,iindex, &
                   jindex,hfact,xn)
!
      call readmm5(nfile1,cfg_file,1,1,0,1,'TERRAIN   ',nx,ny,1,terr, &
                   flag0)
      if(flag0 < 0) then
        call ConvertError(cfg_file,flag0)
        stop
      endif
!
      call hinterp2(nx,ny,hfact,iindex,jindex,terr,buffer1)
!
      call readmm5(nfile1,cfg_file,1,1,0,1,'ZAGL      ',nx,ny,nz,zagl, &
      flag0)
      if(flag0 < 0) then
        call ConvertError(cfg_file,flag0)
        stop
      endif
!
      call hinterp3(nx,ny,nz,hfact,iindex,jindex,zagl,buffer1)
!
      call setztr(nx,ny,nz,zagl,terr,zadjust,ztrans)
!
      call zparams(nx,ny,nz,ztrans,zadjust,terr,slopef,kindex)
!
!------------------MAIN TIME LOOP-----------------------
!
!     do ifile=1,325
         ifxflag=1
!        read(10,'(a)',end=999) filename
         call getfxtim(nfile1,filename,ihour,imin,isec,flag0)
         if(flag0 < 0) then
           call ConvertError(filename,flag0)
           stop
         endif
!
!        print*,'ihour imin isec: ',ihour,imin,isec
         iyear=jyear
         imon=jmon
         iday=jday
         imin=jmin+imin
         ihour=ihour+jhour
         if(imin >= 60) then
           imin=imin-60
           ihour=ihour+1
         endif
         do it=1,400         !! ADDED BY RONG 1/4/2001
         if(ihour >= 24) then
           ihour=ihour-24
           iday=iday+1
         else                !! ADDED BY RONG 1/4/2001
           goto 77           !! ADDED BY RONG 1/4/2001
         endif
!
         if(mod(iyear,100) == 0) then
           if(mod(iyear,400) == 0) then
             days(2)=29
           endif
         else
           if(mod(iyear,4) == 0) then
             days(2)=29
           endif
         endif
!
         if(iday > days(imon)) then
           iday=iday-days(imon)
           imon=imon+1
         endif
!
         if(imon > 12) then
           imon=imon-12
           iyear=iyear+1
         endif
         enddo                !! ADDED BY RONG 1/4/2001
   77    continue             !! ADDED BY RONG 1/4/2001
!
         call readmm5(nfile1,filename,1,1,0,1,'UC        ',nx,ny,nz,u, &
         flag0)
         if(flag0 < 0) then
           call ConvertError(filename,flag0)
           stop
         endif
!
         call readmm5(nfile1,filename,1,1,0,1,'VC        ',nx,ny,nz,v, &
         flag0)
         if(flag0 < 0) then
           call ConvertError(filename,flag0)
           stop
         endif
!
         call readmm5(nfile1,filename,1,1,0,1,'WHALF     ',nx,ny,nz,w, &
         flag0)
         if(flag0 < 0) then
           call ConvertError(filename,flag0)
           stop
         endif
!
         call readmm5(nfile1,filename,1,1,0,1,'THETA     ',nx,ny,nz,th, &
         flag0)        
         if(flag0 < 0) then
           call ConvertError(filename,flag0)
           stop
         endif
!
         if(ifxflag == 1) then
           call readmm5(nfile1,filename,1,1,0,1,'PBL HT    ',nx,ny,1, &
           pblhgt,flag0)
           if(flag0 == -4) then
             ifxflag=0
             goto 199
           endif
           if(flag0 < 0) then
             call ConvertError(filename,flag0)
             stop
           endif
         endif
!
         if(ifxflag == 1) then
           ifxflag=0
           do ii=1,nx
           do jj=1,ny
              if(pblhgt(ii,jj) > 1.) then
                ifxflag=1
                goto 199
              endif
           enddo
           enddo
         endif
!
  199    continue
!
         if(ifxflag == 1) then
           call readmm5(nfile1,filename,1,1,0,1,'HFX       ',nx,ny,1, &
           htflux,flag0)
           if(flag0 < 0) then
             call ConvertError(filename,flag0)
             stop
           endif
         endif
!
         if(ifxflag == 1) then
           call hinterp2(nx,ny,hfact,iindex,jindex,htflux,buffer1)
           call hinterp2(nx,ny,hfact,iindex,jindex,pblhgt,buffer1)
         endif
!
!  Convert U and V to spherical
!
!  The last argument is passed into the subroutine as xn. Value 1.0 is for
!  polar stereographic projection. Use 0.716 for Lambert conformal projection

         call model2sph(nx,ny,nz,xlonc,xlon,u,v,buffer1,buffer2,xn)
!
!  Interpolate U,V,W, and TH
!
!        write(6,'(6(f12.4,1x))') ((htflux(i,j),i=1,nx),j=1,ny)
         call hinterp3(nx,ny,nz,hfact,iindex,jindex,u,buffer1)
         call hinterp3(nx,ny,nz,hfact,iindex,jindex,v,buffer1)
         call hinterp3(nx,ny,nz,hfact,iindex,jindex,w,buffer1)
         call hinterp3(nx,ny,nz,hfact,iindex,jindex,th,buffer1)
         call zinterp(nx,ny,nz,slopef,kindex,u,kbuff)
         call zinterp(nx,ny,nz,slopef,kindex,v,kbuff)
         call zinterp(nx,ny,nz,slopef,kindex,w,kbuff)
         call zinterp(nx,ny,nz,slopef,kindex,th,kbuff)
!
!  Write Medoc file
!
         openmode=openmode+1
         filename2=trim(outf)
         nfile2=12
         iyear=iyear-iyear/100*100
         call wrtmedoc(nx,ny,nz,iyear,imon,iday,ihour,imin,isec,ztrans, &
                       dlat,dlon,ylat,ylon,terr,u,v,w,th,pblhgt,htflux, &
                       nfile2,filename2,openmode,ifxflag)
!     enddo
 999  continue

      deallocate(xlat)
      deallocate(xlon)
      deallocate(u)
      deallocate(v)
      deallocate(w)
      deallocate(th)
      deallocate(terr)
      deallocate(zagl)
      deallocate(pblhgt)
      deallocate(htflux)

      deallocate(ztrans)
      deallocate(zadjust)
      deallocate(ylat)
      deallocate(ylon)

      deallocate(slopef)
      deallocate(hfact)
      deallocate(kindex)
      deallocate(iindex)
      deallocate(jindex)

      deallocate(CalcBuffer1)
      deallocate(buffer1)
      deallocate(buffer2)
      deallocate(kbuff)
!
      end program medoc_driver
!
!
!
      subroutine ConvertError(filename,flag)
!
      character*80 :: filename
      integer :: flag
!
!     i=index(filename,' ')
      if(flag == -1) then
        print*,'Error opening ',filename,' - MM5 Conversion Terminated'
      else
        print*,filename,' Contains an Error, Err # = ',flag, &
        ' - MM5 Conversion Terminated'
      endif
!
      return
      end subroutine ConvertError
