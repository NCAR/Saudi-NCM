      subroutine readcfg(nfile,mfile,nx,ny,nk,dx,dy,xlatc,xlonc,iflag)
!
!-----read ().cfg for some mm5/scipuff parameters. Arrays, which are
!-----also in ().cfg are read by readmm5
!
!-----note: in mm5, j corresponds to x (e-w) and i to y (n-s)
!-----In subroutine readmm5, however, these are switched to (i,j) <-> (x,y).
!
      character(len=*):: mfile

      iflag = 0
      open(nfile,file=mfile,status='old',form='formatted',err=100)
      read(nfile,*,err=200) nk,nx,ny
      read(nfile,*,err=200) dx,dy
      read(nfile,*,err=200) xlonc,xlatc
!     read(nfile,*,err=200) iyear,imonth,iday,ihour,iminute
      close(nfile)
      return
  100 iflag=-1
      return
  200 iflag=-2
      close(nfile)
      return
      end


      subroutine readmm5(nfile,mfile,iopen,iclose,irewind,isearch, &
                       msearch,ni,nj,nk,buffer,iflag)
!
!-----Read processed data to obtain fields
!
!-----msearch%string: string (up to 10 chars) containing name of field to read
!
!-----for the following 0 = no and 1 = yes
!
!     iopen - open file
!     iclose - close file
!     irewind - rewind file
!     isearch - search (usually 1)
!
!
!     include 'parametr.inc'
      character*80 mbuf
      character*1 mblnk1(80)
      character*80 mblnk80
      character*80 findfld
      character*10 msearch
      character(len =*) ::  mfile

      integer*2 issame80

      
      dimension buffer(ni,nj,nk)

      equivalence (mblnk80,mblnk1(1))

      data mblnk1 /80*' '/


      findfld = mblnk80
      findfld = msearch

      if (iopen .eq. 1) then
        ix=index(mfile,' ')
        if(ix .eq. 0) then
          open(nfile,file=mfile,status='old',form='formatted', &
               err=100)
        else
          open(nfile,file=mfile(1:ix-1),status='old',form='formatted', &
               err=100)
        endif
        go to 10
      end if

      if (irewind .eq. 1) rewind(nfile,err=200)

   10 if (isearch .eq. 0) go to 50

   20 mbuf = mblnk80
      read(nfile,'(a80)',err=300,end=400) mbuf
      if (issame80(findfld,mbuf,10) .eq. 1) go to 30
      go to 20

   30 continue

      read(nfile,*,err=300,end=400) k1,kmax
      if (nk .gt. kmax) go to 600
      if (k1 .ne. 1) go to 300

      do k=1,nk
        if (k .ne. 1) then
          read(nfile,*,err=300,end=400) k1,kmax
          if (k1 .ne. k) go to 300
        end if
!
!-----read in values and flip i and j indeces
!
        do i=1,ni
          read(nfile,*,err=300,end=400) (buffer(i,j,k),j=1,nj)
        end do
      end do

   50 if (iclose .eq. 1) then
        close(nfile,err=700)
        return
      end if


!
!-----file open error
!
  100 iflag = -1
      go to 700
!
!-----file rewind error
!
  200 iflag = -2
      close(nfile,err=700)
      go to 700
!
!-----file read error
!
  300 iflag = -3
      close(nfile,err=700)
      go to 700
!
!-----msearch (variable name) not found
!
  400 iflag = -4
      close(nfile,err=700)
      go to 700
!
!-----close error
!
  500 iflag = -5
      go to 700
!
!-----k dimension exceeds max
!
  600 iflag = -6
      close(nfile,err=700)

  700 continue
      return
      end

      integer*2 function issame80(string1,string2,maxch)
!
!-----compare string1 to string2. Null characters terminate strings
!-----and trailing blanks are ignored
!
!
!     issame = 0 - different
!            = 1 - same
!
      character(len=*) :: string1,string2

      issame80=0
      iend = min(maxch,80)

      do i=1,iend
        ich = i
        if (string1(i:i) .ne. string2(i:i)) go to 10
        if (ichar(string1(i:i)) .eq. 0.and. &
            ichar(string2(i:i)) .eq. 0) then
          issame80 = 1
          return
        end if
      end do
      issame80=1
      return

   10 continue
      if (ichar(string1(ich:ich)) .eq. 0) then
        do i=ich,iend
          ic = ichar(string2(i:i))
          if (ic .ne. 0.and.ic .ne. 32) return
        end do
        issame80 = 1
        return
      end if

      if (ichar(string2(ich:ich)) .eq. 0) then
        do i=ich,iend
          ic = ichar(string1(i:i))
          if (ic .ne. 0.and.ic .ne. 32) return
        end do
        issame80 = 1
        return
      end if

      return

      end


      subroutine checkFile(RADAR_DIR1,ywflag)
      character(len=*):: RADAR_DIR1
      integer ywflag
      ywflag = 0
        open(95,file=trim(RADAR_DIR1),status='old',ERR=99)
        close(95)
      return
99    continue
      print*, "open following file failed and set to bad data: "
      print*, trim(RADAR_DIR1)
      ywflag = -99
      close(99)
      return
      end

      subroutine hparamsM(xlat0,xlon0,dxlat,dxlon,ni,nj,ylat,ylon, &
                        iindex,jindex,hfact)
!
!-----compute parameters for interpolating xlat/xlon grid (mosaic)
!     to MM5 lat/lon grid
!
      dimension ylat(ni,nj),ylon(ni,nj)
      dimension iindex(ni,nj),jindex(ni,nj),hfact(4,ni,nj)
!
!     dll_export hparamsM
!
      do 10 i=1,ni
      do 10 j=1,nj

!-----find MM5 cell in original lat/lon grid
!
         xtarget = (ylon(i,j) - xlon0)/dxlon + 1.0
         ytarget = (ylat(i,j) - xlat0)/dxlat + 1.0
         itarget = xtarget
         jtarget = ytarget
         x = xtarget - itarget
         y = ytarget - jtarget
!
!-----adjust for precision
!
      x = min(x,1.0)
      x = max(x,0.0)
      y = min(y,1.0)
      y = max(y,0.0)
!
      iindex(i,j) = itarget
      jindex(i,j) = jtarget
!
!-----interpolation factors
!
!     1 - lower left
!     2 - upper left
!     3 - lower right
!     4 - upper right

      hfact(1,i,j) = (1.-x) * (1.-y)
      hfact(2,i,j) = (1.-x) * y
      hfact(3,i,j) = x * (1.-y)
      hfact(4,i,j) = x * y

   10 continue
!
      return
      end

      subroutine hinterp3M(nx,ny,nz,data &
                          ,ni,nj,hfact,iindex,jindex,dataout,BADPT)
!
!-----interpolate (nx,ny,nz) grid to (ni,nj,nz) grid for 3-d field
!  need to take care of missing data
!
      dimension data(nx,ny,nz),dataout(ni,nj,nz)
      dimension hfact(4,ni,nj),iindex(ni,nj),jindex(ni,nj)
!
!     dll_export hinterp3

      do 30 k=1,nz
      do 10 i=1,ni
      do 10 j=1,nj
      i1 = iindex(i,j)
      i2 = i1 + 1
      j1 = jindex(i,j)
      j2 = j1 + 1
      IF(I1.LT.1.OR.I2.GT.NX.OR.J1.LT.1.OR.J2.GT.NY) then
         dataout(i,j,k) = BADPT
      ELSE IF(data(i1,j1,k).EQ.BADPT.OR.data(i1,j2,k).EQ.BADPT.OR.  &
              data(i2,j1,k).EQ.BADPT.OR.data(i2,j2,k).EQ.BADPT) THEN
         dataout(i,j,k) = data(i2,j2,k)
         if(data(i1,j1,k).NE.BADPT) dataout(i,j,k) = data(i1,j1,k)
         if(data(i1,j2,k).NE.BADPT) dataout(i,j,k) = data(i1,j2,k)
         if(data(i2,j1,k).NE.BADPT) dataout(i,j,k) = data(i2,j1,k)

      ELSE
      dataout(i,j,k) = data(i1,j1,k) * hfact(1,i,j) +  &
                       data(i1,j2,k) * hfact(2,i,j) +  &
                       data(i2,j1,k) * hfact(3,i,j) +  &
                       data(i2,j2,k) * hfact(4,i,j)
      ENDIF
   10 continue

   30 continue
!
      return
      end

      subroutine read_tile(filename,dztmp,&
             xminr,yminr,dxkmr,dykmr,v_level,flag,nzrad)
          include "netcdf.inc"  
!         integer imdv_nx,imdv_ny,imdv_nz
          real dxkmr,dykmr,xminr,yminr
          character(len=*) :: filename
          character(len=256) :: name
          integer flag
          real dztmp(*)
          real, allocatable, dimension(:)::buf 
          integer i, j, ncid, iret,varid, k
          integer ivtype,ndims, natts, dims(10),idims(10)
          integer nvars, ngatts, unlimdimid, length
          character(len=10):: buf_ch(nzrad)
          real v_level(nzrad)
          character(len = 4):: tmp_ch
          integer scale
          character(len = 256)::text

          flag = 99

          id_time = 0
          iret = nf_open(filename, NF_NOWRITE, ncid)
          if (iret .ne. 0) then
              print*,"failed to open the netcdf file. ",filename
              flag = -99
              return
          endif
          iret = nf_inq_varid(ncid,"MREFL",varid)
          if (iret .ne. 0) then
              flag = -99
              return
          endif
          iret = nf_inq_vardimid (ncid, varid, dims)
          if (iret .ne. 0) then
              flag = -99
              return
          endif
          do i = 1,10 
             iret = nf_inq_dimlen(ncid, dims(i), idims(i) )
          end do
!         imdv_nx = idims(1)
!         imdv_ny = idims(2)
!         imdv_nz = idims(3)

          length = idims(1)*idims(2)*idims(3)

!         print*, "read tile first 3 dimensions:",idims(1),idims(2),idims(3)

          iret = nf_inq_varid(ncid,"MREFL",varid)
          iret = nf_get_att_int(ncid, varid, "Scale",scale)
          if (iret .ne. 0) then
              scale =1.0
          else if(scale .eq. 0) then 
              print*, "Error: scale is 0"
              return
          endif

          allocate(buf(length))

!         write(6,*) "before read the value." 
          call flush(6)
          iret = nf_get_var_real(ncid, varid, buf)

!         write(6,*) "after read the value." 
          call flush(6)
        
          if (iret .ne. 0) then
              flag = -99
              return
          endif
!         print*, "scale =",scale
          dztmp(1:length) = buf(:)/scale

          deallocate(buf)

!get attribute values for: xminr,yminr,dxkmr,dykmr
        iret = nf_get_att_real(ncid, NF_GLOBAL, "Longitude",xminr)
        iret = nf_get_att_real(ncid, NF_GLOBAL, "Latitude",yminr)
         iret = nf_get_att_real(ncid, NF_GLOBAL, "LatGridSpacing",dykmr)
         iret = nf_get_att_real(ncid, NF_GLOBAL, "LonGridSpacing",dxkmr)
        yminr = yminr - (idims(2)-1)*dykmr


          print*, xminr,yminr,dxkmr,dykmr 


          iret = nf_inq_varid(ncid,"Ht",varid)
          iret = nf_get_var_real(ncid, varid, v_level)
          

          iret=nf_close(ncid)
          print*, "Finish read_tile."
      return
      end

      subroutine read_tile_zlevel(filename,dztmp,DBZvar,flag)
          include "netcdf.inc"
          character(len=*) :: filename,DBZvar
          character(len=256) :: name
          integer flag
          real dztmp(*)
          real, allocatable, dimension(:)::buf
          integer i, j, ncid, iret,varid, k
          integer ivtype,ndims, natts, dims(10),idims(10)
          integer nvars, ngatts, unlimdimid, length
          character(len=10):: buf_ch(nzrad)
          character(len = 4):: tmp_ch
          integer scale
          character(len = 256)::text

          flag = 99

          id_time = 0
          iret = nf_open(filename, NF_NOWRITE, ncid)
          if (iret .ne. 0) then
              print*,"failed to open the netcdf file. ",filename
              flag = -99
              return
          endif
          iret = nf_inq_varid(ncid,trim(DBZvar),varid)
          if (iret .ne. 0) then
              flag = -99
              return
          endif
          iret = nf_inq_vardimid (ncid, varid, dims)
          if (iret .ne. 0) then
              flag = -99
              return
          endif

          do i = 1,10
             iret = nf_inq_dimlen(ncid, dims(i), idims(i) )
          end do

          length = idims(1)*idims(2)*idims(3)
!         print*, "read tile first 3 dimensions:",idims(1),idims(2),idims(3)

          iret = nf_inq_varid(ncid,trim(DBZvar),varid)
          iret = nf_get_att_int(ncid, varid, "Scale",scale)
          if (iret .ne. 0) then
              scale =1.0
          else if(scale .eq. 0) then
              print*, "Error: scale is 0"
              return
          endif

          allocate(buf(length))

!         write(6,*) "before read the value."
          call flush(6)
          iret = nf_get_var_real(ncid, varid, buf)

!         write(6,*) "after read the value."
          call flush(6)

          if (iret .ne. 0) then
              flag = -99
              return
          endif
!         print*, "scale =",scale
          dztmp(1:length) = buf(:)/scale
          deallocate(buf)
          iret=nf_close(ncid)
          print*, "Finish read_tile_zlevel"
      return
      end

       subroutine output_nc_var(array,idim,jdim,nzrad,bad_data, var_name,file_name,&
           dxx,dyy,ylatc,ylonc,ylat,ylon,dateStr,range,domainID)
      implicit none
#include <netcdf.inc>
          integer idim,jdim, nzrad
          real array(idim,jdim,nzrad), bad_data
          integer dim(4), dim2(2)
          character(len=*) file_name,var_name,dateStr,range
          integer   iret,ncid,varid,idim_id,jdim_id,nzrad_id
          integer time_id,dateStr_id
          integer i,j, domainID
          real dxx,dyy,ylatc,ylonc,ylat(idim,jdim),ylon(idim,jdim)

          iret = nf_create(trim(file_name), nf_clobber, ncid)

          iret = nf_def_dim(ncid, "west_east", idim,idim_id)
          iret = nf_def_dim(ncid, "south_north", jdim,jdim_id)
          iret = nf_def_dim(ncid, "nzrad", nzrad,nzrad_id)
          iret = nf_def_dim(ncid, "Time",1,time_id)

          iret = nf_def_dim(ncid, "DateStrLen",19,dateStr_id)
          dim2(1) = dateStr_id
          dim2(2) = time_id
          iret = nf_def_var(ncid,"Times",NF_CHAR,2,dim2,varid)

          dim(1)=idim_id
          dim(2)=jdim_id
          dim(3)=nzrad_id
          dim(4)=time_id
          print*, "idim=",idim, "jdim=",jdim
!         stop
          iret = nf_def_var(ncid,var_name,NF_FLOAT,4,dim,varid)

          iret = nf_put_att_text(ncid, NF_GLOBAL, &
              "range", len(trim(range)), trim(range))

          iret = nf_put_att_int(ncid, NF_GLOBAL, &
              "domain_id", NF_INT, 1, domainID)

          iret = nf_put_att_real(ncid, NF_GLOBAL, &
              "missing_value", NF_FLOAT, 1, bad_data)
          iret = nf_put_att_real(ncid, NF_GLOBAL, &
              "central_latitude", NF_FLOAT, 1, ylatc)
          iret = nf_put_att_real(ncid, NF_GLOBAL, &
              "central_longitude", NF_FLOAT, 1, ylonc)
          iret = nf_put_att_real(ncid, NF_GLOBAL, &
              "grid_spacing_dxx", NF_FLOAT, 1, dxx)
          iret = nf_put_att_real(ncid, NF_GLOBAL, &
              "grid_spacing_dyy", NF_FLOAT, 1, dyy)
       
          iret = nf_put_att_real(ncid, NF_GLOBAL, &
              "lat_SW_corner", NF_FLOAT, 1, ylat(1,1))
          iret = nf_put_att_real(ncid, NF_GLOBAL, &
              "lon_SW_corner", NF_FLOAT, 1, ylon(1,1))
          iret = nf_put_att_real(ncid, NF_GLOBAL, &
              "lat_NE_corner", NF_FLOAT, 1, ylat(idim,jdim))
          iret = nf_put_att_real(ncid, NF_GLOBAL, &
              "lon_NE_corner", NF_FLOAT, 1, ylon(idim,jdim))

          iret = nf_enddef(ncid)

!   output
          iret = nf_inq_varid(ncid,var_name,varid)
          iret = nf_put_var_real(ncid,varid,array)

          iret = nf_inq_varid(ncid,"Times",varid)
          iret = nf_put_var_text(ncid,varid,dateStr)

          iret=nf_close(ncid)
      return
      end

      subroutine readnetcdf(filename,var,var_name1,v_level,var_name2,ix,jx,nz)
          implicit none
#include <netcdf.inc>
          character(len=*)::filename,var_name1,var_name2
          integer ix, jx, nz
          real var(ix,jx,nz),v_level(nz)
          integer i,j, length,len_var,dimid,dims(3) 
          integer iret, ncid,varid
          iret = nf_open(trim(filename),NF_NOWRITE, ncid)

          iret = nf_inq_varid(ncid,trim(var_name1),  varid)
          iret = nf_get_var_real(ncid, varid, var)

          iret = nf_inq_varid(ncid,trim(var_name2),  varid)
          iret = nf_get_var_real(ncid, varid, v_level)

          iret = nf_close(ncid)
          return
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
             return
             stop 999
          endif

          do i = 1,num_d
             iret = nf_inq_dimlen(ncid, dims(i), idims(i) )
          end do
          ix = idims(1)
          jy = idims(2)
          if (num_d .ge. 3) then
             nz = idims(3)
          else
             nz = 1
          endif
          iret = nf_close(ncid)
       end 
