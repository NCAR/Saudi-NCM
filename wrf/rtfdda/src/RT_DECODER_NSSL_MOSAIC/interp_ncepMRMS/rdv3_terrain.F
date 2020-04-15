! This utility program is written in free-format Fortran 90.
! It requires a Fortran 90 compiler to compile. 
! Type the following to compile: On LINUX Portland group:
!   pgf90 -Mfreeform -r4 -i4 -pc 32 -byteswapio yw_rdv3_rain_1hr.f -o rdv3_rain_1hr /opt/netcdf-3.5.1/lib/libnetcdf.a
!   usage:  ./rdv3_rain_3hr mm5_outpu Cycle AIRDAT 2
!
MODULE MODULE_READ_MM5

        implicit none
        include "netcdf.inc"
        
        integer, dimension(50,20),public :: bhi
        real, dimension(20,20) ,public:: bhr
        character(len=80), dimension(50,20) ,public:: bhic
        character(len=80), dimension(20,20) ,public:: bhrc


        real ,public:: time
        integer, dimension(4) ,public:: start_index, end_index
        character (len= 4) ,public:: staggering
        character (len= 4) ,public:: ordering
        character (len=10) ,public:: yyyymmddhh 
        character (len=24) ,public:: current_date
        character (len= 9) ,public:: name
        character (len=25) ,public:: units
        character (len=46) ,public:: description
        character (len=10) ,public:: data_type
        character (len=30) ,public:: namein
        character (len=10) ,public:: out_units
        character (len=80) ,public:: text,ywcycle


        real, allocatable, dimension(:,:),public :: ylat, ylon

contains


    subroutine output_netcdf(file_name,var,var_name,dateStr,range,nzrad,v_level,var_name2)
        character(len=*) file_name,var_name,dateStr,range,var_name2
        integer :: l,domain_id,nzrad
        integer :: ncid,ncout,varid, status, nchar,vlevel_id    ! size of text
        integer :: lon_dim,lat_dim, time_dim,time_dims,nzrad_dim
        integer :: lon_id,lat_id,time_id,time_ids    
        integer :: dims_v(2),indx(4),icount(4),timedims(2), dims4(4)
        
        character(len=24) :: start_date
        real :: lat_SW_corner, lon_SW_corner,lat_NE_corner,lon_NE_corner
        real :: var(end_index(1)-1,end_index(2)-1,nzrad)
        real :: v_level(nzrad)
!yw       real :: var(end_index(1)-1,end_index(2)-1,nzrad)
       
        namein = "Hourly accumulated Rainfall"
        out_units  = "unknow"

        print*, "filename = ",trim(file_name)
     
              status = nf_create(trim(file_name), NF_NETCDF4, ncout)
        
! assign global attributes:
              lat_SW_corner = ylat(1,1)
              lon_SW_corner = ylon(1,1)
              lat_NE_corner = ylat(end_index(1)-1,end_index(2)-1)
              lon_NE_corner = ylon(end_index(1)-1,end_index(2)-1)
              
              status = nf_put_att_text (ncout, nf_GLOBAL, 'TITLE', &
                15, 'READ REFLECTIVITY')
              status = nf_put_att_text(ncout, NF_GLOBAL,'current_date', &
                  len(trim(dateStr)), trim(dateStr))
              status = nf_put_att_int (ncout, nf_GLOBAL,'WEST-EAST_GRID_DIMENSION', &  
                NF_INT, 1, bhi(17,1))
              status = nf_put_att_int (ncout,nf_GLOBAL,'SOUTH-NORTH_GRID_DIMENSION',&
                NF_INT, 1, bhi(16,1))
              status = nf_put_att_int (ncout, NF_GLOBAL, &
                     'map_proj', NF_INT, 1, bhi(7,1))
              status = nf_put_att_text (ncout, NF_GLOBAL, &
                       'range', len(trim(range)), trim(range)) 

              select case (bhi(7,1))
              case (1)
                text = "Lambert Conformal"
                nchar = len_trim(text)
                status = nf_put_att_text (ncout, NF_GLOBAL, &
                            'map_projection', nchar, trim(text))
              case (2)
                text = "Polar Stereographic"
                nchar = len_trim(text)
                status = nf_put_att_text (ncout, nf_GLOBAL, &
                        'map_projection', nchar, trim(text))
              case (3)
                text = "Mercator"
                nchar = len_trim(text)
                status = nf_put_att_text (ncout, nf_GLOBAL, &
                                'map_projection', nchar, trim(text))
              case default
                stop("Projection not known")
              end select
        
              status = nf_put_att_int (ncout, nf_GLOBAL,'coarse_grid_xdim', &
                NF_INT, 1, bhi(6,1))
              status = nf_put_att_int (ncout, nf_GLOBAL,'coarse_grid_ydim', &  
                NF_INT, 1, bhi(5,1))
              status = nf_put_att_real (ncout, nf_GLOBAL,'coarse_grid_size', &
                NF_REAL, 1, bhr(1,1))
              status = nf_put_att_real (ncout, nf_GLOBAL, 'central_latitude', &
                NF_REAL, 1, bhr(2,1))
              status = nf_put_att_real (ncout, nf_GLOBAL, 'central_longitude', &
                NF_REAL, 1, bhr(3,1))
              status = nf_put_att_real (ncout, nf_GLOBAL, 'truelat1', &
                NF_REAL, 1, bhr(5,1))
              status = nf_put_att_real (ncout, nf_GLOBAL, 'truelat2', &
                NF_REAL, 1, bhr(6,1))
              status = nf_put_att_real (ncout, nf_GLOBAL, 'cone_factor', &
                NF_REAL, 1, bhr(4,1))
              status = nf_put_att_real (ncout, nf_GLOBAL, 'pole_position', &
                NF_REAL, 1, bhr(7,1))
              status = nf_put_att_real (ncout, nf_GLOBAL, 'grid_spacing', &
                NF_REAL, 1, bhr(9,1))
              status = nf_put_att_int (ncout, nf_GLOBAL, 'domain_id', &
                NF_INT, 1, bhi(13,1))
              status = nf_put_att_int (ncout, nf_GLOBAL,'domain_ratio', &  
                NF_INT, 1, bhi(20,1))
              status = nf_put_att_int (ncout, nf_GLOBAL,'domain_ydim', &
                NF_INT, 1, bhi(16,1))
              status = nf_put_att_int (ncout, nf_GLOBAL,'domain_xdim', &
                NF_INT, 1, bhi(17,1))
              status = nf_put_att_real (ncout, nf_GLOBAL, 'domain_x_start', &  
                NF_REAL, 1, bhr(11,1))
              status = nf_put_att_real (ncout, nf_GLOBAL,'domain_y_start', &
                NF_REAL, 1, bhr(10,1))
              status = nf_put_att_real (ncout, nf_GLOBAL, 'reference_ptop', &
                NF_REAL, 1, bhr(2,2))
              status = nf_put_att_real (ncout, nf_GLOBAL,'lat_SW_corner', &  
                NF_REAL, 1,lat_SW_corner)
              status = nf_put_att_real (ncout, nf_GLOBAL,'lon_SW_corner', &  
                NF_REAL, 1,lon_SW_corner)
              status = nf_put_att_real (ncout, nf_GLOBAL,'lat_NE_corner', &
                NF_REAL, 1,lat_NE_corner)
              status = nf_put_att_real (ncout, nf_GLOBAL,'lon_NE_corner', &
                NF_REAL, 1,lon_NE_corner)

! define dimensions
              if (ordering .eq. 'YX  ' ) then
                status = nf_def_dim(ncout, 'south_north', end_index(1)-1, lat_dim)
                status = nf_def_dim(ncout, 'west_east ', end_index(2)-1, lon_dim)
              else
                status = nf_def_dim(ncout, 'west_east', end_index(1)-1, lat_dim)
                status = nf_def_dim(ncout, 'south_north ', end_index(2)-1, lon_dim)
              endif
              status = nf_def_dim(ncout, 'vertical', nzrad, nzrad_dim)
              status = nf_def_dim(ncout, 'Time', 1, time_dim)
              status = nf_def_dim(ncout, 'DateStrLen', 19, time_dims)
      
!    Define variables:
              timedims=(/time_dims,time_dim/)  
              status= nf_def_var(ncout, 'Times', nf_char, 2,timedims,time_ids)

              dims_v=(/nzrad_dim,time_dim/)  
              status= nf_def_var(ncout, trim(var_name2), nf_real, 2,dims_v,vlevel_id)

              dims4 = (/lat_dim,lon_dim,nzrad_dim,time_dim/)
              status = nf_def_var(ncout, trim(var_name), nf_real, 4, dims4, varid)
              status = nf_def_var_deflate(ncout, varid, 0, 1, 4)
              status = nf_def_var_FLETCHER32(ncout, varid, NF_FLETCHER32)

!    Assign variable attributes:  
              status = nf_put_att_text(ncout, time_id,'name', 12,'radar reflectivity')
!             status = nf_put_att_text(ncout, time_id,'units',3,'min')
              status = nf_put_att_text(ncout, time_ids,'name',4,'date')              
!             status = nf_put_att_text(ncout, varid,'description', 30, namein)
              status = nf_put_att_text(ncout, varid,'name', 12, &
                'INITIAL TIME')
              status = nf_put_att_text(ncout, varid,'units', 10, out_units )
              status = nf_put_att_text(ncout, varid,'ordering',4, ordering)
!             status = nf_put_att_text(ncout, varid,'staggering',4, staggering)
!             status = nf_put_att_text(ncout, varid,'precip_accu_time_mn', &
!                       2,'60')
! Change the mode of netcdf operation
              status = nf_enddef(ncout)
!
! Store variables:
              indx = (/1,1,1,1/)

!             status = NF_PUT_VARA_INT(ncout,time_id,indx,1,int(time))
              icount = (/19,1,1,1/)
              status = NF_PUT_VARA_TEXT(ncout,time_ids,indx,icount,trim(dateStr)) 

              icount = (/nzrad,1,1,1/)
              status = NF_PUT_VARA_REAL (ncout, vlevel_id,indx,icount,v_level)

!yw           icount = (/end_index(1)-1,end_index(2)-1,nzrad,1/)
              icount = (/end_index(1)-1,end_index(2)-1,nzrad,1/)
              status = NF_PUT_VARA_REAL (ncout, varid,indx,icount,var)
!yw              status = nf_put_var_real(ncout,varid,var)
! close file
              status = nf_close(ncout)

         end subroutine output_netcdf

    subroutine readcfg_mm5(infile,nx,ny,xlatc,xlonc)        
!      read configuration and lat and lon from terrain file.
        character(len=*) infile
        real xlatc,xlonc
        integer nx,ny

        integer :: iunit_in  = 10, iunit_out = 11
        integer :: ndim, i,j, num
        integer :: flag
        integer :: ierr, ier, time_info
        
        real, allocatable, dimension(:,:,:,:) :: data


        num = 0
        time_info = 1

        open(iunit_in, file=infile, form='unformatted', status='old' &
             , action='read')
        !open(iunit_out, file='rain.dat', form='unformatted')
       
        read(iunit_in, iostat=ierr) flag
        do while (ierr == 0)
          if (flag == 0) then
            read(iunit_in,iostat=ier) bhi, bhr, bhic, bhrc
            if(ier/=0) then
              write(*,'("Error reading big header")')
              call abort()
            endif
          elseif (flag == 1) then
            READ (iunit_in,iostat=ier) ndim, start_index, end_index &
               , time, staggering, ordering &
               , current_date, name, units, description
            if(ier/=0) then
              write(*,'("Error reading subheader")')
              call abort()
            endif

            if (ndim == 1) then
              allocate(data(end_index(1), 1, 1, 1))
            elseif (ndim == 2) then
              allocate(data(end_index(1), end_index(2), 1, 1))
            elseif (ndim == 3) then
              allocate(data(end_index(1), end_index(2), end_index(3), 1))
            endif

            read(iunit_in) data
            if (name == "LATITCRS") then
              allocate(ylat(end_index(1)-1, end_index(2)-1))
              ylat    = 0.
              ylat = data(1:end_index(1)-1,1:end_index(2)-1,1,1)
              num = num +1
!             print*, "read ylat "
            end if
            if( name == "LONGICRS") then
              allocate(ylon(end_index(1)-1, end_index(2)-1))
              ylon    = 0.
              ylon = data(1:end_index(1)-1,1:end_index(2)-1,1,1)
!             print*, "read ylon "
              num = num +1
            endif
             
            deallocate(data)
            if(num .gt. 1) goto 999
          else 
            stop "read flag error."
          endif 
            read(iunit_in, iostat=ierr) flag
        end do
999     continue

        nx = end_index(1) - 1 ! for MM5 this is north-south
        ny = end_index(2) - 1 ! for MM5 this is west-east
        print*, "first dimentison:",  end_index(1)-1  
        print*, "second dimentison:", end_index(2)-1  

        xlatc = bhr(2,1)
        xlonc = bhr(3,1)

!       print*, ylat(1,1),ylat(nx-1,ny-1)
!       print*, ylon(1,1),ylon(nx-1,ny-1)

        close(iunit_in)
        return 
        end subroutine readcfg_mm5

 
        subroutine readcfg_wrf(infile,nx,ny,xlatc,xlonc)        
        character(len=*) infile
        integer :: ncid, status
        integer :: lonDimId, latDimId
        integer :: lonLen, latLen
        integer :: xlatVarId, xlonVarId
        real, allocatable, dimension(:,:,:) :: data
        real xlatc,xlonc,dx,truelat1,truelat2
        integer nx,ny,map_proj,i,j

        status = nf_open(infile, nf_nowrite, ncid)
        status = NF_INQ_dimId (NCID, 'west_east',   lonDimId)
        status = NF_INQ_dimId (NCID, 'south_north', latDimId)
        status = NF_INQ_VARID (NCID, 'XLAT_M',      xlatVarId)
        status = NF_INQ_VARID (NCID, 'XLONG_M',     xlonVarId)

        status = NF_INQ_DIMLEN(NCID, lonDimId,      lonLen)
        status = NF_INQ_DIMLEN(NCID, latDimId,      latLen)
        status = NF_GET_ATT_REAL(NCID, NF_GLOBAL, 'MOAD_CEN_LAT',  xlatc)
        status = NF_GET_ATT_REAL(NCID, NF_GLOBAL, 'STAND_LON',  xlonc)
!       status = NF_GET_ATT_REAL(NCID, NF_GLOBAL, 'CEN_LAT',  xlatc)
!       status = NF_GET_ATT_REAL(NCID, NF_GLOBAL, 'CEN_LON',  xlonc)
        status = NF_GET_ATT_INT (NCID, NF_GLOBAL, 'MAP_PROJ', map_proj)
        status = NF_GET_ATT_REAL(NCID, NF_GLOBAL, 'DX',       dx)
        status = NF_GET_ATT_REAL(NCID, NF_GLOBAL, 'TRUELAT1', truelat1)
        status = NF_GET_ATT_REAL(NCID, NF_GLOBAL, 'TRUELAT2', truelat2)

        nx = latLen
        ny = lonLen
! nx: S-N; ny: W-E; using mm5 convention
!       print*,'nx,ny=',nx,ny,xlatc,xlonc
!
        allocate(data(lonLen, latLen, 1))
        status = nf_get_vara_real(NCID, xlatVarId,(/1,1,1/), (/lonLen, latLen,1/), data)
!
! switch 1st and 2nd dimension in lar/lon to match that of mm5 and the output_netcdf
        allocate(ylat(nx,ny))
        allocate(ylon(nx,ny))
        do i = 1,ny
        do j = 1,nx
          ylat(j,i)=data(i,j,1) 
        enddo
        enddo
        status = nf_get_vara_real(NCID, xlonVarId,(/1,1,1/), (/lonLen, latLen,1/), data)
        do i = 1,ny
        do j = 1,nx
          ylon(j,i)=data(i,j,1) 
        enddo
        enddo
        print*, ylat(1,1),ylat(nx-1,ny-1)
        print*, ylon(1,1),ylon(nx-1,ny-1)
!       status = NF_GET_ATT_TEXT(NCID, data, 'MemoryOrder', ordering)
!       status = NF_GET_ATT_TEXT(NCID, data, 'stagger',     staggering)
!       print*,'ordering=',ordering,'stagger=',staggering
        ordering='YX  '
        end_index(1)=nx+1
        end_index(2)=ny+1
        bhi(7,1) = map_proj
        bhr(5,1)=truelat1
        bhr(6,1)=truelat2
        bhr(9,1)=dx 
        bhi(16,1)=nx 
        bhi(17,1)=ny 
        status = nf_close(ncid)
        end subroutine readcfg_wrf 

        subroutine readcfg_wrf_XY(infile,nx,ny,xlatc,xlonc)        
        character(len=*) infile
        integer :: ncid, status
        integer :: lonDimId, latDimId
        integer :: lonLen, latLen
        integer :: xlatVarId, xlonVarId
        real, allocatable, dimension(:,:,:) :: data
        real xlatc,xlonc,dx,truelat1,truelat2
        integer nx,ny,map_proj,i,j

        status = nf_open(infile, nf_nowrite, ncid)
        status = NF_INQ_dimId (NCID, 'west_east',   lonDimId)
        status = NF_INQ_dimId (NCID, 'south_north', latDimId)
        status = NF_INQ_VARID (NCID, 'XLAT_M',      xlatVarId)
        status = NF_INQ_VARID (NCID, 'XLONG_M',     xlonVarId)

        status = NF_INQ_DIMLEN(NCID, lonDimId,      lonLen)
        status = NF_INQ_DIMLEN(NCID, latDimId,      latLen)
        status = NF_GET_ATT_REAL(NCID, NF_GLOBAL, 'MOAD_CEN_LAT',  xlatc)
        status = NF_GET_ATT_REAL(NCID, NF_GLOBAL, 'STAND_LON',  xlonc)
!       status = NF_GET_ATT_REAL(NCID, NF_GLOBAL, 'CEN_LAT',  xlatc)
!       status = NF_GET_ATT_REAL(NCID, NF_GLOBAL, 'CEN_LON',  xlonc)
        status = NF_GET_ATT_INT (NCID, NF_GLOBAL, 'MAP_PROJ', map_proj)
        status = NF_GET_ATT_REAL(NCID, NF_GLOBAL, 'DX',       dx)
        status = NF_GET_ATT_REAL(NCID, NF_GLOBAL, 'TRUELAT1', truelat1)
        status = NF_GET_ATT_REAL(NCID, NF_GLOBAL, 'TRUELAT2', truelat2)

! nx: W-E; ny: S-N; using wrf convention
        print*, 'ordering =',ordering
        nx = lonLen
        ny = latLen
!       print*,'nx,ny=',nx,ny,xlatc,xlonc
!
        allocate(data(lonLen, latLen, 1))
        status = nf_get_vara_real(NCID, xlatVarId,(/1,1,1/), (/lonLen, latLen,1/), data)

        allocate(ylat(nx,ny))
        allocate(ylon(nx,ny))
        do i = 1,nx
        do j = 1,ny
          ylat(i,j)=data(i,j,1) 
        enddo
        enddo
        status = nf_get_vara_real(NCID, xlonVarId,(/1,1,1/), (/lonLen, latLen,1/), data)
        do i = 1,nx
        do j = 1,ny
          ylon(i,j)=data(i,j,1) 
        enddo
        enddo
        print*, ylat(1,1),ylat(nx-1,ny-1)
        print*, ylon(1,1),ylon(nx-1,ny-1)
!       status = NF_GET_ATT_TEXT(NCID, data, 'MemoryOrder', ordering)
!       status = NF_GET_ATT_TEXT(NCID, data, 'stagger',     staggering)
!       print*,'ordering=',ordering,'stagger=',staggering
        ordering='XY  '
        end_index(1)=nx+1
        end_index(2)=ny+1
        bhi(7,1) = map_proj
        bhr(5,1)=truelat1
        bhr(6,1)=truelat2
        bhr(9,1)=dx 
        bhi(16,1)=ny   ! SOUTH-NORTH
        bhi(17,1)=nx   ! WEST-EAST
        status = nf_close(ncid)
        end subroutine readcfg_wrf_XY 
END MODULE  MODULE_READ_MM5
