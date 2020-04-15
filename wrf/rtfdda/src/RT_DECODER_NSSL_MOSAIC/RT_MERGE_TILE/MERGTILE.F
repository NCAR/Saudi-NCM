!234567
!Usage of the program:
! ./MERGTILE  output_dir DIA_domain2.cfg 200602132015 WSMR 2
      program main
      use MODULE_READ_MM5
      implicit none
      character(len=256) output_dir,FILENAME,cfg_file,tiles,date_str,input_dir
      character(len=256) str_tmp,RADAR_DIR1
      integer IYYD,IMMD,IDDD,IHHD,IMND
      integer nfile1 ,nix,njy, nkz
      integer flag0,ywflag
      integer,allocatable,dimension(:,:):: iindex, jindex
      real BADPT
      integer num_titles
      real maxlon,maxlat, minlon, minlat
      real max_lon,max_lat, min_lon, min_lat
      real dxlat, dxlon, xlatc, xlonc
      real ymaxr,yminr,xmaxr,xminr
      integer imdv_nx,imdv_ny,imdv_nz
      real xlat0,xlon0
      integer nxrad,nyrad,nzrad, start_x, start_y, index_x,index_y
      real,allocatable,dimension(:,:,:):: v_mmcs2, v_tile ,hfact,dz_tmp
      real xlon01 ,xlat01 
      integer i,j,k, ii, jj, iii, kyw
      real,allocatable,dimension(:):: xlon_tile, xlat_tile, dztmp, v_level
      real dxkmr,dykmr
      character(len=19) dateStr, range, domain
      integer domainID, factor , idx, idy
      
      BADPT=-999.

!     read the argument 
      call getarg(1,output_dir)
      call getarg(2,cfg_file) !! mm5 configuration file.
      call getarg(3,date_str)
      call getarg(4,range)
      call getarg(5,domain)
      read(domain,*) domainID
      call getarg(6,input_dir)

      read(date_str,44)IYYD,IMMD,IDDD,IHHD,IMND
 44   FORMAT(I4.4,I2.2,I2.2,I2.2,I2.2,I2.2)    
      print*,'dataset at:',IYYD,IMMD,IDDD,IHHD,IMND


      write(dateStr,"(I4.4,'-',I2.2,'-',I2.2,'_',I2.2,':',I2.2,':00')" ) &
            IYYD,IMMD,IDDD,IHHD,IMND

!     call readcfg(nfile1,cfg_file,nix,njy,nkz,dxx,dyy,ylatc,ylonc,flag0)
!     call readcfg_mm5(cfg_file,nix,njy,xlatc,xlonc)
      call readcfg_wrf(cfg_file,nix,njy,xlatc,xlonc)
        print*,'nx,ny=',nix,njy,xlatc,xlonc

!     allocate(ylat(nix,njy))
!     allocate(ylon(nix,njy))

      allocate(hfact(4,nix,njy))
      allocate(iindex(nix,njy))
      allocate(jindex(nix,njy))

!     read the namelist.mosaic and get the maximum and minimum point of the MMM.
      open(17,file="namelist.mosaic",form="formatted")
      read(17,*) num_titles,factor
      print*, "num_titles = ",num_titles
      
      if(num_titles.le.0) then      ! find all of the tile information
         close(17)
         print*, "check all TITLES and give a necessary list"
! find the maximu and minum point of the MMM.
         maxlon = -9999
         maxlat = -9999
         minlon = 9999
         minlat = 9999
         print*, "nix=",nix," njy=",njy
         do j = 1, njy
           do i = 1, nix
              if(maxlon.lt.ylon(i,j)) maxlon = ylon(i,j)
              if(maxlat.lt.ylat(i,j)) maxlat = ylat(i,j)
              if(minlon.gt.ylon(i,j)) minlon = ylon(i,j)
              if(minlat.gt.ylat(i,j)) minlat = ylat(i,j)
           enddo
         enddo
         print*, "MM5 domain information ..."
         print*, "maxlon=",maxlon
         print*, "minlon=",minlon
         print*, "maxlat=",maxlat
         print*, "minlat=",minlat
         print*, "list the selected tiles"

         open(17,file="table",form="formatted")
         read(17,"(a11)") tiles
         read(17,*) dxlat, dxlon
         print*, "dxlat = ",dxlat," dxlon = ",dxlon
         min_lon = 9999
         min_lat = 9999
         max_lat = -9999
         max_lon = -9999
         do iii = 1, 4
            read(17,"(a11,4F7.2,3I6)") tiles,  &
        ymaxr,yminr,xmaxr,xminr,imdv_nx,imdv_ny,imdv_nz
         do j = 1, njy
           do i = 1, nix
           if( ((ylon(i,j).le.xmaxr).and.(ylon(i,j).ge.xminr)).and.  &
              ((ylat(i,j).le.ymaxr).and.(ylat(i,j).ge.yminr)) )then
                   print*, "selected title : ",trim(tiles)
                   if(xminr .le. min_lon) min_lon = xminr
                   if(yminr .le. min_lat) min_lat = yminr
                   if(xmaxr .ge. max_lon) max_lon = xmaxr
                   if(ymaxr .ge. max_lat) max_lat = ymaxr
                   goto 999
               endif
           enddo
         enddo
999      continue
enddo
         print*, "the suggesed nxrad = ",(max_lon-min_lon)/dxlon/factor+1
         print*, "the suggesed nyrad = ",(max_lat-min_lat)/dxlat/factor+1
         print*, "the suggesed nzrad = ",imdv_nz
         print*, "the degree factor =",factor
         print*, "the start lat = ",min_lat
         print*, "the start lon = ",min_lon
         close(17)
         stop 1000
      endif  ! end of find the tiles.

! now read the tiles and matched to a big file.
      read(17,*) xlat0,xlon0
      read(17,*) nxrad,nyrad,nzrad  !! size of the matched tiles.

      print*, "start point of the tile: lat= ",xlat0, " lon= ",xlon0
      print*, "suggested size of the total tiles: ",nxrad,nyrad,nzrad
      allocate(v_mmcs2(nix,njy,nzrad))   ! for mm domain
      allocate(v_tile(nxrad,nyrad,nzrad))  ! for the matched tiles
         v_tile = BADPT
      allocate(xlon_tile(nxrad))  ! for the matched tiles
      allocate(xlat_tile(nyrad))  ! for the matched tiles
      allocate(v_level(nzrad))     ! for the vertical level
      do k = 1, nzrad
        v_level(k) = 500.0*float(k)
      enddo 

!yw     allocate(dztmp(2001*2001*31))

      do iii = 1, num_titles
         read(17,"(a256)") RADAR_DIR1
       write(str_tmp,"('/',I4.4,I2.2,I2.2,'/',I4.4,I2.2,I2.2,'_',I2.2,I2.2)") &
          IYYD, IMMD,IDDD,IYYD, IMMD,IDDD,IHHD,IMND
       filename = ""
       filename = trim(input_dir)//"/"//trim(RADAR_DIR1)//trim(str_tmp)
       call checkFile(trim(filename),ywflag)


!      call getdim(filename,imdv_nx,imdv_ny,imdv_nz,3,"mrefl_mosaic")
      call getdim(filename,imdv_nx,imdv_ny,imdv_nz,3,"MREFL")
      print*, "find tile dims:",imdv_nx,imdv_ny,imdv_nz
      allocate(dztmp(imdv_nx*imdv_ny*imdv_nz+1))

       if(ywflag .eq. -99) then
           print*, trim(date_str),"  incomplete data"
           goto 990
       endif

         print*, "read tile: ",trim(filename)


!cyw     read data from the netcdf format of the original tile

      dztmp =  BADPT
         call read_tile(trim(filename),dztmp,  &
              xminr,yminr,dxkmr,dykmr,v_level,ywflag,nzrad)

       if(ywflag .eq. -99) then
           print*, trim(date_str),"  failed time in read_tile"
           goto 990
       endif

! define original (CIWS) grid
         print*, "xminr=",xminr, "yminr=",yminr
         xlon01 = xminr
         xlat01 = yminr
         dxlon = dxkmr
         dxlat = dykmr
         print*, "dxlat= ",dxlat," dxlon= ",dxlon
         print*, "xlon01= ",xlon01," xlat01= ",xlat01
         print*, "imdv_nx = ",imdv_nx," imdv_ny= ",imdv_ny
         print*, "imdv_nz = ",imdv_nz

             start_x=(xlon01-xlon0)/dxlon
             start_y=(xlat01-xlat0)/dxlat

         print*, "start_x=",start_x," start_y=",start_y, "factor=",factor
!            if(start_x.lt.0 .or. start_y.lt.0) then
!              print*, "error to stop"
!              print*, "start_x/start_y ",start_x,start_y
!                    stop 888
!            endif


             kyw = 0
             do k = 1, nzrad
               do j = 1, imdv_ny
                 do i = 1, imdv_nx
                   kyw = kyw+1
                   if((mod(start_x+i-1,factor).eq.0).and.  &
                       (mod(start_y+j-1,factor).eq.0) ) then
                      idx = (start_x+i-1)/factor + 1
                      idy = (start_y+imdv_ny-j)/factor + 1
!                     idy = (start_y+j-1)/factor + 1
                      if( (idx.ge.1) .and. (idx.le.nxrad)) then
                        if( (idy.ge.1).and. (idy.le.nyrad)) then
                          if(dztmp(kyw).eq. -999 ) dztmp(kyw) = BADPT
!                         if(dztmp(kyw) .gt. 80) print*, "bad data is :",dztmp(kyw)
                          v_tile(idx,idy,k) = dztmp(kyw)
                       endif
                     endif
                   endif
                 enddo
               enddo
             enddo

 990  continue
      deallocate(dztmp)
      end do
! end of iii loop.

      dxlat = dxlat * factor
      dxlon = dxlon * factor
      do i = 1, nxrad
         xlon_tile(i)= xlon0 + (i-1)*dxlon
      enddo
      do j = 1, nyrad
         xlat_tile(j)= xlat0 + (j-1)*dxlat
      enddo

! now interpolate to MM5 grid.
! locate MM5 grid points in obs coordinates (iindex,jindex)
! hfact - horizontal interpolation factors for the 4 surrounding points
!

      call hparamsM(xlat0,xlon0,dxlat,dxlon,nix,njy,ylat,ylon,iindex,jindex,hfact)

! horizontal interpolation
!     - may choose to exclude MM5 points not surrounded by good data
        CALL hinterp3M(nxrad,nyrad,nzrad,v_tile,nix,njy,hfact,iindex,jindex  &
                      ,v_mmcs2,BADPT)

!     call output_nc_var(v_mmcs2,nix,njy,nzrad,BADPT,"dz",&
!        trim(output_dir)//"/"//dateStr//"_"//trim(range)//"_D"//trim(domain)//".nc", &
!        0,0,1,1,ylat,ylon,dateStr,trim(range),domainID)
      
!     filename =  trim(output_dir)//"/"//dateStr//"_"//trim(range)//"_D"//trim(domain)//".nc" 
      filename =  trim(output_dir)//"/"//dateStr//"_"//"DBZ_D"//trim(domain)//".nc" 
      call output_netcdf( filename ,v_mmcs2,"dz",dateStr,range,nzrad,v_level,"Level")

!yw          v_level = 0
!yw          v_mmcs2 = 0
!yw      nix = 51, njy = 66
!yw      call readnetcdf(filename,v_mmcs2,"dz",v_level,"Level",nix,njy,nzrad)
!yw      call output_netcdf( "test.nc",v_mmcs2,"dz",dateStr,range,nzrad,v_level,"Level")

      stop
      end

