!234567
!Usage of the program:
! ./MERGTILE  output_dir DIA_domain2.cfg 200602132015 WSMR 2
      program main
      use MODULE_READ_MM5
      implicit none
      character(len=256) output_dir,FILENAME,cfg_file,tiles,date_str,input_dir
      character(len=256) str_tmp
      integer IYYD,IMMD,IDDD,IHHD,IMND
      integer nfile1 ,nix,njy, nkz
      integer flag0,ywflag
      integer,allocatable,dimension(:,:):: iindex, jindex
      real BADPT
      integer num_titles
      real maxlon,maxlat, minlon, minlat
      real max_lon,max_lat, min_lon, min_lat
      real dxlat, dxlon, xlatc, xlonc
      integer imdv_nx,imdv_ny,imdv_nz
      real xlat0,xlon0
      integer nxrad,nyrad,nzrad, start_x, start_y, index_x,index_y
      real,allocatable,dimension(:,:,:):: v_mmcs2, v_tile ,hfact,dz_tmp
      real xlon01 ,xlat01 
      integer i,j,k, ii, jj, iii, kyw
      real,allocatable,dimension(:):: xlon_tile, xlat_tile, dztmp, v_level
      real dxkmr,dykmr
      character(len=19) dateStr, range, domain, xy_order
      integer domainID, factor , idx, idy
      character(len=5) zkmStr
      character(len=64) DBZvar
      integer iargc, is_xy_order
      
      BADPT=-999.
      is_xy_order = 0

!     read the argument 
!      print*,'  == Number of argument: ',iargc()
      if (iargc() .lt. 6) then
         print*,'  Usage: :INTERP_ncepMRMS output_dir cfg_file date_str range domain input_dir'
         print*,'     cfg_file:  geo_em.d02.nc'
         print*,'     date_str:  yyyymmddhh, for example, 2015021219'
         print*,'        range:  ATC'
         print*,'       domain:  Domain ID , 2 or 3'
         stop 1
      endif
      call getarg(1,output_dir)
      call getarg(2,cfg_file) !! mm5 configuration file.
      call getarg(3,date_str)
      call getarg(4,range)
      call getarg(5,domain)
      read(domain,*) domainID
      call getarg(6,input_dir)
      if (iargc() .gt. 6) then
         call getarg(7,xy_order)
         if (trim(xy_order) .eq. 'xy' .or. trim(xy_order) .eq. 'XY') then
            is_xy_order = 1
         endif
      endif
      print*,'  == is_xy_order: ',is_xy_order

      read(date_str,44)IYYD,IMMD,IDDD,IHHD,IMND
 44   FORMAT(I4.4,I2.2,I2.2,I2.2,I2.2,I2.2)    
      print*,'dataset at:',IYYD,IMMD,IDDD,IHHD,IMND

      write(dateStr,"(I4.4,'-',I2.2,'-',I2.2,'_',I2.2,':',I2.2,':00')" ) &
            IYYD,IMMD,IDDD,IHHD,IMND

! readcfg_mm5: MM5 grid index convention: nix for S-N; njy for W-E
!     call readcfg_mm5(cfg_file,nix,njy,xlatc,xlonc)
      if ( is_xy_order .eq. 0 ) then
         ! cfg_wrf index follows MM5 convention: nix for S-N; njy for W-E 
         call readcfg_wrf(cfg_file,nix,njy,xlatc,xlonc)
      else
      ! cfg_wrf_XY follows WRF index convention: nix for W-E; njy for S-N
         call readcfg_wrf_XY(cfg_file,nix,njy,xlatc,xlonc)
      endif

      print*,'nx,ny=',nix,njy,xlatc,xlonc

!     allocate(ylat(nix,njy))
!     allocate(ylon(nix,njy))

      allocate(hfact(4,nix,njy))
      allocate(iindex(nix,njy))
      allocate(jindex(nix,njy))

! read the namelist.mosaic and get the maximum and minimum point of the match
! area 
! each NCEP MRMS dataset consists of data at one vertical level  
! the variable name in the netcdf file is var209_9_1_${v_level}mabovemeansealevel
!
      open(17,file="namelist.mosaic.ncep",form="formatted")
      read(17,*) num_titles,factor
      print*, "num_titles = ",num_titles, "factor =",factor

! now read the tiles and matched to a big file.
      read(17,*) xlat0,xlon0
      read(17,*) nxrad,nyrad,nzrad  !! size of the matched tiles.
      read(17,*) xlat01,xlon01,dxlat,dxlon !! origin and resolution of the Mosaic grid

      print*, "start point of the tile: lat= ",xlat0, " lon= ",xlon0
      print*, "suggested size of the search area: ",nxrad,nyrad,nzrad
      allocate(v_mmcs2(nix,njy,nzrad))   ! for model domain
      allocate(v_tile(nxrad,nyrad,nzrad))  ! for the matched tiles
         v_tile = BADPT
      allocate(xlon_tile(nxrad))  ! for the matched tiles
      allocate(xlat_tile(nyrad))  ! for the matched tiles
      allocate(v_level(nzrad))     ! for the vertical level

      do iii = 1, num_titles
         read(17,"(a5,1x,f6.0,a64)") zkmStr,v_level(iii),DBZvar

         write(str_tmp,"('/',I4.4,I2.2,I2.2,'/',I4.4,I2.2,I2.2,'_',I2.2,I2.2)") &
          IYYD, IMMD,IDDD,IYYD, IMMD,IDDD,IHHD,IMND
!        filename = trim(input_dir)//trim(str_tmp)//"_"//zkmStr//".nc"
         filename = trim(input_dir)//"/mergedRefl-"//zkmStr//".nc"
!        print*,'v_level=',v_level(iii),trim(DBZvar),filename

         call checkFile(trim(filename),ywflag)
         if(ywflag .eq. -99) then
           print*, trim(date_str),"  incomplete data"
           goto 990
         endif
         call getdim(filename,imdv_nx,imdv_ny,imdv_nz,2,trim(DBZvar))
!
! mx 11-10-2014
! read in NCEP MRMS: netcf4 files converted from grib2
! each file covers the CONUS domain and one vertical level; read one file to dztmp
!
         print*, "read ztile: ",trim(filename),imdv_nx,imdv_ny,imdv_nz
         allocate(dztmp(imdv_nx*imdv_ny*imdv_nz+1))
         dztmp =  BADPT
         call read_tile_zlevel(trim(filename),dztmp,DBZvar,ywflag)
         if(ywflag .eq. -99) then
           print*, trim(date_str),"  failed in read_tile_zlevel"
           goto 990
         endif
         print*, "dxlat= ",dxlat," dxlon= ",dxlon
         print*, "xlon01= ",xlon01," xlat01= ",xlat01
         print*, "imdv_nx = ",imdv_nx," imdv_ny= ",imdv_ny, "imdv_nz = ",imdv_nz
!
! store data on a (equal or smaller) search area w resolution factor to v_tile
!
         start_x=(xlon01-xlon0)/dxlon
         start_y=(xlat01-xlat0)/dxlat
          
             kyw = 0
             do k = 1, imdv_nz
               do j = 1, imdv_ny
                 do i = 1, imdv_nx
                   kyw = kyw+1
                   if((mod(start_x+i-1,factor).eq.0).and.  &
                       (mod(start_y+j-1,factor).eq.0) ) then
                      idx = (start_x+i-1)/factor + 1
!                     idy = (start_y+imdv_ny-j)/factor + 1
                      idy = (start_y+j-1)/factor + 1
                      if( (idx.ge.1) .and. (idx.le.nxrad)) then
                        if( (idy.ge.1).and. (idy.le.nyrad)) then
                          if(dztmp(kyw).eq. -999 ) dztmp(kyw) = BADPT
                          if(dztmp(kyw) .gt. 80) then
                             print*, "bad data is :",dztmp(kyw),i,j,k,iii
                             dztmp(kyw) = BADPT
                          endif
                          v_tile(idx,idy,iii) = dztmp(kyw)
                       endif
                     endif
                   endif
                 enddo
               enddo
             enddo
 990  continue
      deallocate(dztmp)
  enddo
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
      
!     filename = trim(output_dir)//"/"//dateStr//"_"//trim(range)//"_D"//trim(domain)//".nc" 
      filename = trim(output_dir)//"/"//dateStr//"_"//"DBZ_D"//trim(domain)//".nc" 
      call output_netcdf( filename ,v_mmcs2,"dz",dateStr,range,nzrad,v_level,"Level")

      stop
      end
