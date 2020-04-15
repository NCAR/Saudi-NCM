!234567
       program main
       character(len=256) :: filename
       integer ix,jy,nz
       filename = "/raid/radar_ranges/WSMR/2006-04-04_16:55:00_WSMR_D3.nc"
          call  getdim(filename, ix,jy,nz,3,"dz")
         print*, ix,jy,nz
       stop 
       end

       subroutine getdim(filename,ix,jy,nz,num_d,var_name)
          implicit none
#include <netcdf.inc>
          character(len=*) filename,var_name
          integer ix,jy,nz,num_d
          integer dims(num_d),idims(num_d)
          integer i,j,iret, ncid,varid

          iret = nf_open(trim(filename),NF_NOWRITE, ncid)

          iret = nf_inq_varid(ncid,trim(var_name),  varid)
          iret = nf_inq_vardimid (ncid, varid, dims)
          if (iret .ne. 0) then
             print*, "Error: netcdf file dimension inqury",filename
             stop 999
          endif

          do i = 1,num_d
             iret = nf_inq_dimlen(ncid, dims(i), idims(i) )
          end do
          ix = idims(1)
          jy = idims(2)
          nz = idims(3)
       end
