!=======================================================================
! CVS: $Id: WRF_ncread.F90,v 1.1 2012/04/19 16:41:38 hsoh Exp $
! CVS: $Source: /cvs/apps/4dwx/RTFDDA/src/RADAR_TO_WRFFDDA/WRF_ncread.F90,v $
! CVS: $Name: R1-W381-20180202 $
!=======================================================================
!BOP ===================================================================
!
! !MODULE: WRF_ncread - Read information from WRF netCDF output files
!
! !DESCRIPTION:
!    Contains subroutines to open a netCDF file, get the values of the
!    dimensions, read in 3d or 4d fields, read in time character strings,
!    and error check.
!
! !REVISION HISTORY:
!
! 2005-Aug-26 - J. Schramm - first version
!
! !INTERFACE: ----------------------------------------------------------

module WRF_ncread

! !USES:

    use WRF_kinds
    use netcdf

!EOP

   implicit none

   private    ! except

! !PUBLIC TYPES:

   ! none

! !PUBLIC MEMBER FUNCTIONS:

   public :: ncread_open        ! Open a netCDF file
   public :: getCharTimes       ! Get character string times from a WRF netCDF file
   public :: get_dimension      ! Get a dimension from a netCDF file
   public :: get_gl_real_att    ! Get a global real attribute from a netCDF file
   public :: get_gl_int_att     ! Get a global ingeger attribute from a netCDF file
   public :: ncread_VarDimSizes ! Get variable dimensions from a netCDF file
   public :: ncread_VarDimNum   ! Get number of dimensions of a netCDF field
   public :: ncread_field4dG    ! Read a field from a netCDF file
   public :: handleErr          ! Error handler for netCDF routines

! !PUBLIC DATA MEMBERS:

   ! none

!EOP

   logical, parameter :: debug = .false. ! Set to .true. to turn on write
                                         ! statements in this module
!===============================================================================
contains
!===============================================================================
!BOP ===========================================================================
!
! !IROUTINE: ncread_open -- Open netcdf file
!
! !DESCRIPTION:
!   Open netcdf file
!
! !REVISION HISTORY:
!     2005-Aug-26 - J. Schramm - first version
!
! !INTERFACE: ------------------------------------------------------------------

subroutine ncread_open(fileName,fid)

   implicit none

! !INPUT/OUTPUT PARAMETERS:

   character(*),intent(in) :: fileName
   integer(INT),intent(out) :: fid

!EOP

   !----- local -----
   integer(INT)   :: rCode

   !----- format -----
   character(*),parameter :: subName = "(ncread_open)"
   character(*),parameter :: F00     = "('(ncread_open) ',2a)"

   rCode = nf90_open(fileName, nf90_nowrite, fid)
   call handleErr(rCode, subName//" ERROR opening input data file")

end subroutine ncread_open

!===============================================================================
!BOP ===========================================================================
!
! !IROUTINE: getCharTimes -- Read character times from WRF netCDF file
!
! !DESCRIPTION:
!    Get WRF character string times from netCDF file.
!
! !REVISION HISTORY:
!
!     2005-Aug-26 - J. Schramm - first version
!
! !INTERFACE: ------------------------------------------------------------------

subroutine getCharTimes(fname, varName, file_times)

   implicit none

! !INPUT/OUTPUT PARAMETERS:
   character(*),intent(in) :: fname                  ! name
   character(*),intent(in) :: varName                ! name of time variable
   character(char_date),intent(out) :: file_times(:) ! times read from netCDF file

!EOP

   !----- local -----
   integer(INT) :: i               ! loop index
   integer(INT) :: fid             ! netCDF file ID
   integer(INT) :: rCode           ! error code
   integer(INT) :: vid             ! netCDF variable ID
   integer(INT) :: did             ! netCDF dimension ID
   integer(INT) :: ndims           ! # of dimensions of Times variable
   integer(INT) :: istart_t(2)     ! record # to start reading Times
   integer(INT) :: iend_t(2)       !
   integer(INT)        ,allocatable :: nTimesDims(:) ! dimensions of Times array
   integer(INT)        ,allocatable :: dimIDs(:)     ! dimension ids for time array

   !----- format -----
   character(*),parameter :: subName = "(getCharTimes)"
   character(*),parameter :: F00 = "('(getCharTimes) ',4a)"
   character(*),parameter :: F01 = "('(getCharTimes) ',a,i6)"
   character(*),parameter :: F02 = "('(getCharTimes) ',a,2i6)"

   call ncread_open(fname, fid)

   !---------------------------------------------------------------------------
   !--- Check time character string variable ---
   !---------------------------------------------------------------------------
   rCode = nf90_inq_varid(fid, varName, vid)  ! Get variable ID
   call handleErr(rCode, subName//" ERROR finding Times var in data file")

   rCode = nf90_inquire_variable(fid,vid,ndims=ndims)  ! Get # of dims
   call handleErr(rCode,subName//' ERROR inquire variable Times ndims')
   if (debug) write(6,F01) 'Times variable has ndims = ', ndims

   allocate(dimIDs(ndims))   
   allocate(nTimesDims(ndims))

   rCode = nf90_inquire_variable(fid,vid,dimids=dimIDs)  ! Get dim IDs
   call handleErr(rCode,subName//' ERROR inquire variable Times dimIDs')

   !---------------------------------------------------------------------------
   !--- get dimension sizes for all Times dims
   !--- Note:  dimensions in ncdump are reversed from what fortran reads
   !---------------------------------------------------------------------------
   do i=1,ndims
      rCode = nf90_inquire_dimension(fid,dimIDs(i),len=nTimesDims(i))
      call handleErr(rCode, subName//" ERROR inquire Times dim sizes")
   enddo
   if (debug) write(6,F02) 'Sizes of Times variable dimensions are= ', &
   &                       (nTimesDims(i),i=1,ndims)

   !---------------------------------------------------------------------------
   !--- Read time character strings from file, really an array of single chars
   !---------------------------------------------------------------------------
   do i = 1, nTimesDims(2)            ! Loop number of char strings
      istart_t(1) = 1                 ! First index of variable
      istart_t(2) = i                 ! Record number to read
      iend_t(1)   = nTimesDims(1)     ! Length of char string (# if indices to read)
      iend_t(2)   = 1                 ! Number of records to read
      rCode = nf90_get_var(fid,vid, file_times(i), start=istart_t, count=iend_t)
   enddo
   if (debug) write(6,F00) 'Times from netCDF file are = ', &
   &                       (trim(adjustl(file_times(i))),i=1,nTImesDims(2))

   rCode = nf90_close(fid)
   call handleErr(rCode, subName//" ERROR closing input data file")

   deallocate(dimIDs, nTimesDims)

end subroutine getCharTimes

!===============================================================================
!BOP ===========================================================================
!
! !IROUTINE: get_gl_real_att -- Read a real global attribute
!
! !DESCRIPTION:
!     Read a real, global attribute from a netCDF file.
!
! !REVISION HISTORY:
!     2005-Sep-12 - J. Schramm - first version
!
! !INTERFACE: ------------------------------------------------------------------

subroutine get_gl_real_att(fname, attName, value)

! !USES:

   implicit none

! !INPUT/OUTPUT PARAMETERS:

   character(*),intent(in) :: fname      ! netCDF file name
   character(*),intent(in) :: attName    ! name of global attribute to read
   real(R8)    ,intent(out):: value      ! Value of real attribute

!EOP

   !----- local -----
   integer(INT) :: fid             ! netCDF file ID
   integer(INT) :: rCode           ! error code

   !----- formats -----
   character(*),parameter :: subName = "(get_gl_read_att)"
   character(*),parameter :: F00 = "('(get_gl_read_att) ',3a,f10.4)"

   call ncread_open(fname, fid)
   rCode = nf90_get_att(fid, nf90_global, attName, value )
   call handleErr(rCode, subName//" ERROR finding global attribute "//attName)

   if (debug) write(6,F00) ' global attribute ',trim(attName),' is ',value

   rCode = nf90_close(fid)
   call handleErr(rCode, subName//" ERROR closing input data file")

end subroutine get_gl_real_att
!===============================================================================
!BOP ===========================================================================
!
! !IROUTINE: get_gl_int_att -- Read an integer global attribute
!
! !DESCRIPTION:
!     Read a integer, global attribute from a netCDF file.
!
! !REVISION HISTORY:
!     2005-Sep-12 - J. Schramm - first version
!
! !INTERFACE: ------------------------------------------------------------------

subroutine get_gl_int_att(fname, attName, value)

! !USES:

   implicit none

! !INPUT/OUTPUT PARAMETERS:

   character(*),intent(in) :: fname      ! netCDF file name
   character(*),intent(in) :: attName    ! name of global attribute to read
   integer(INT),intent(out):: value      ! Value of integer attribute

!EOP

   !----- local -----
   integer(INT) :: fid             ! netCDF file ID
   integer(INT) :: rCode           ! error code

   !----- formats -----
   character(*),parameter :: subName = "(get_gl_int_att)"
   character(*),parameter :: F00 = "('(get_gl_int_att) ',3a,f10.4)"

   call ncread_open(fname, fid)
   rCode = nf90_get_att(fid, nf90_global, attName, value )
   call handleErr(rCode, subName//" ERROR finding global attribute "//attName)

   if (debug) write(6,F00) ' global attribute ',trim(attName),' is ',value

   rCode = nf90_close(fid)
   call handleErr(rCode, subName//" ERROR closing input data file")

end subroutine get_gl_int_att

!BOP ===========================================================================
!
! !IROUTINE: get_dimension -- Read a dimension from a netCDF file
!
! !DESCRIPTION:
!     Read a dimension from a netCDF file.
!
! !REVISION HISTORY:
!     2005-Aug-25 - J. Schramm - first version
!
! !INTERFACE: ------------------------------------------------------------------

subroutine get_dimension(fname, varName, nDimSize)

! !USES:

   implicit none

! !INPUT/OUTPUT PARAMETERS:

   character(*), intent(in) :: fname          ! netCDF file name
   character(*), intent(in) :: varName        ! name of varialbe to read
   integer(INT), intent(out):: nDimSize       ! Size of dimension

!EOP

   !----- local -----
   integer(INT) :: fid             ! netCDF file ID
   integer(INT) :: did             ! netCDF dimension ID
   integer(INT) :: rCode           ! error code

   !----- formats -----
   character(*),parameter :: subName = "(get_dimension)"
   character(*),parameter :: F00 = "('(get_dimension) ',a,i6)"

   call ncread_open(fname, fid)

   rCode = nf90_inq_dimid(fid, varName, did)   ! Return dimension ID
   call handleErr(rCode, subName//" ERROR finding dim "//varName//" in data file")

   rCode = nf90_inquire_dimension(fid,did,len=ndimSize)   ! Return dimension size
   call handleErr(rCode, subName//" ERROR getting "//varName//" dim in data file")
   if (debug) write(6,F00) varName//' dimension has size = ', ndimSize

   rCode = nf90_close(fid)
   call handleErr(rCode, subName//" ERROR closing input data file")

end subroutine get_dimension

!===============================================================================
!BOP ===========================================================================
!
! !IROUTINE: ncread_varDimNum -- return num of dimensions of a variable
!
! !DESCRIPTION:
! Return number of dimensions in a named variable
! \newline
! General Usage:
!    call shr_ncread_varDimNum('myfile','sst',ndims)
! \newline
! !REVISION HISTORY:
!     2005-Sep-19 - J. Schramm - modified to use without shr code
!     2005-Apr-21 - T. Craig - first version
!
! !INTERFACE: ------------------------------------------------------------------

subroutine ncread_varDimNum(fileName, varName, ns, rc)

   implicit none

! !INPUT/OUTPUT PARAMETERS:

   character(*)        ,intent(in)   :: fileName ! nc file name
   character(*)        ,intent(in)   :: varName  ! name of variable
   integer(INT),intent(out)          :: ns       ! number of dims of var
   integer(INT),intent(out),optional :: rc       ! return code

!EOP

   !----- local -----
   integer(INT) :: fid
   integer(INT) :: vid
   integer(INT) :: rCode

   !----- formats -----
   character(*),parameter :: subName = "(shr_ncread_varDimNum)"
   character(*),parameter :: F00     = "('(shr_ncread_varDimNum) ',4a)"
   character(*),parameter :: F01     = "('(shr_ncread_varDimNum) ',a,i6)"

!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------

   call ncread_open(fileName,fid)

   !--- read variable info ---
   rcode = nf90_inq_varid(fid,trim(varName),vid)
   call handleErr(rCode, subName//" ERROR inq varid")
   rcode = nf90_inquire_variable(fid,vid,ndims=ns)
   call handleErr(rCode, subName//" ERROR inq var")
   if (debug > 1) write(6,F01) trim(varName)//' has dims = ',ns

   rCode = nf90_close(fid)
   call handleErr(rCode, subName//" ERROR closing input data file")

   if (present(rc)) rc = rCode

end subroutine ncread_varDimNum

!===============================================================================
!BOP ===========================================================================
!
! !IROUTINE: ncread_varDimSizes -- return var dim sizes
!
! !DESCRIPTION:
! Return variable dim sizes as requested by optional arguments
! /newline
! General Usage:
!    call ncread_varDimSizes('myfile','sst',ns1,ns2,ns3)
! /newline
! !REVISION HISTORY:
!     2005-Sep-19 - J. Schramm - modified to use without shr code
!     2005-Apr-21 - T. Craig - first version
!
!
! !INTERFACE: ------------------------------------------------------------------

subroutine ncread_varDimSizes(fileName, varName, n1, n2, n3, n4, n5, n6, rc)

   implicit none

! !INPUT/OUTPUT PARAMETERS:

   character(*)        ,intent(in)           :: fileName ! nc file name
   character(*)        ,intent(in)           :: varName  ! name of variable
   integer(INT),intent(out),optional :: n1       ! size of dim1 in var
   integer(INT),intent(out),optional :: n2       ! size of dim2 in var
   integer(INT),intent(out),optional :: n3       ! size of dim3 in var
   integer(INT),intent(out),optional :: n4       ! size of dim4 in var
   integer(INT),intent(out),optional :: n5       ! size of dim5 in var
   integer(INT),intent(out),optional :: n6       ! size of dim6 in var
   Integer(INT),intent(out),optional :: rc       ! return code

!EOP

   !----- local -----
   integer(INT),parameter :: maxn = 6     ! max number of dims available
   integer(INT) :: n                      ! counter
   integer(INT) :: fid                    ! file id
   integer(INT) :: vid                    ! variable id
   integer(INT) :: ndims                  ! number of dims
   integer(INT),allocatable :: dids(:)    ! dimids
   integer(INT),allocatable :: ns(:)      ! size of dims
   integer(INT) :: rCode                  ! error code

   !----- formats -----
   character(*),parameter :: subName = "(ncread_varDimSizes)"
   character(*),parameter :: F00     = "('(ncread_varDimSizes) ',4a)"
   character(*),parameter :: F01     = "('(ncread_varDimSizes) ',a,i6)"

  !-------------------------------------------------------------------------------
  !
  !-------------------------------------------------------------------------------

   call ncread_open(fileName,fid)

   rCode = nf90_inq_varid(fid,trim(varName),vid)
   call handleErr(rCode,subName//' ERROR inq varid vid')
   rCode = nf90_inquire_variable(fid,vid,ndims=ndims)
   call handleErr(rCode,subName//' ERROR inquire variable ndims')
   allocate(dids(ndims))
   allocate(ns(maxn))
   rCode = nf90_inquire_variable(fid,vid,dimids=dids)
   call handleErr(rCode,subName//' ERROR inquire variable dimids')

   !--- get dim sizes for all dims or to maxn, default result is 1 ---
   ns = 1
   do n=1,min(ndims,maxn)
   rcode = nf90_inquire_dimension(fid,dids(n),len=ns(n))
   call handleErr(rCode, subName//" ERROR inquire dimension")
   enddo

   rCode = nf90_close(fid)
   call handleErr(rCode, subName//" ERROR closing input data file")

   !--- copy to output optional arguments ---
   if (present(n1)) then
      n1 = ns(1)
   endif
   if (present(n2)) then
      n2 = ns(2)
   endif
   if (present(n3)) then
      n3 = ns(3)
   endif
   if (present(n4)) then
      n4 = ns(4)
   endif
   if (present(n5)) then
      n5 = ns(5)
   endif
   if (present(n6)) then
      n6 = ns(6)
   endif
   deallocate(dids)
   deallocate(ns)

   if (present(rc)) rc = rCode

end subroutine ncread_varDimSizes

!===============================================================================
!BOP ===========================================================================
!
! !IROUTINE: ncread_field4dG -- read in field data from a file
!
! !DESCRIPTION:
!     Read in field data from a cdf file, fld is 4d in this case
!     Must specify rfld or ifld as optional arguments
!     dimN are the dimension names associated with the 4d input array,
!        if N>4, this represents dimensions outside 4d array which can
!        be optionally set to a specific index using dimNi
!     dimNi are the index to be used for the dimn dimension name
!
! \newline
! General Usage:
!    call ncread_field4dG('myfile','sst',rfld=a4d)
!    call ncread_field4dG('myfile','sst',rfld=a4d,dim1='lon',dim2='lat',dim3='time',dim3i=21)
!    call ncread_field4dG('myfile','tracer',rfld=a4d,dim5='tracer_n',dim5i=3)
! \newline
! !REVISION HISTORY:
!     2005-Apr-28 - T. Craig   - first version
!     2005-Sep-1  - J. Schramm - modified to use without shr code
!
! !INTERFACE: ------------------------------------------------------------------

subroutine ncread_field4dG(fn, fldName, rfld, ifld, &
     dim1, dim1i, dim2, dim2i, dim3, dim3i, dim4, dim4i, &
     dim5, dim5i, dim6, dim6i, rc)

   implicit none

! !INPUT/OUTPUT PARAMETERS:

   character(*),intent(in)           :: fn       ! nc file name
   character(*),intent(in)           :: fldName  ! name of field
   real(R8)    ,intent(out),optional :: rfld(:,:,:,:) ! field array
   integer(INT),intent(out),optional :: ifld(:,:,:,:) ! field array
   character(*),intent(in) ,optional :: dim1     ! name of dim1 in fld
   integer(INT),intent(in) ,optional :: dim1i    ! dim1 index
   character(*),intent(in) ,optional :: dim2     ! name of dim2 in fld
   integer(INT),intent(in) ,optional :: dim2i    ! dim2 index
   character(*),intent(in) ,optional :: dim3     ! name of dim3 in fld
   integer(INT),intent(in) ,optional :: dim3i    ! dim3 index
   character(*),intent(in) ,optional :: dim4     ! name of dim4 in fld
   integer(INT),intent(in) ,optional :: dim4i    ! dim4 index
   character(*),intent(in) ,optional :: dim5     ! name of dim5 in fld
   integer(INT),intent(in) ,optional :: dim5i    ! dim5 index
   character(*),intent(in) ,optional :: dim6     ! name of dim6 in fld
   integer(INT),intent(in) ,optional :: dim6i    ! dim6 index
   integer(INT),intent(out),optional :: rc       ! return code

!EOP
   !----- local -----
   integer(INT),parameter :: maxd = 4                ! max num of dims of array
   integer(INT)           :: fid                     ! file id
   integer(INT)           :: vid                     ! var id
   integer(INT)           :: xtype                   ! var type
   integer(INT)           :: ndims                   ! number of dims
   integer(INT)           :: n,n1,n2,n3,n4,k         ! counters
   integer(INT)         ,allocatable :: dimid(:)     ! dimension ids for array
   integer(INT)         ,allocatable :: dids(:)      ! dimension ids for cdf
   integer(INT)         ,allocatable :: start(:)     ! cdf start array
   integer(INT)         ,allocatable :: count(:)     ! cdf count array
   integer(INT)         ,allocatable :: len(:)       ! size of dim
   character(char_short),allocatable :: name(:)      ! name of dim
   real(R8)             ,allocatable :: rin(:,:)     ! local 2d array
   integer(INT)         ,allocatable :: iin(:,:)     ! local 2d array
   integer(INT)         ,allocatable :: start2d(:)   ! start for 2d local array
   integer(INT)         ,allocatable :: count2d(:)   ! count for 2d local array
   logical :: found                                  ! search logical
   integer(INT) :: rCode                             ! error code
   integer(INT),parameter :: debug_local=0              ! set to 2 to write info

   !----- formats -----
   character(*),parameter :: subName = "(ncread_field4dG)"
   character(*),parameter :: F00     = "('(ncread_field4dG) ',4a)"
   character(*),parameter :: F01     = "('(ncread_field4dG) ',2a,3i6,2x,a)"
   character(*),parameter :: F02     = "('(ncread_field4dG) ',a,i6)"
   character(*),parameter :: F03     = "('(ncread_field4dG) ',5a,i6)"

!-------------------------------------------------------------------------------
! 
!-------------------------------------------------------------------------------

   !--- check that rfld or ifld is present ---
   if (present(rfld).and.present(ifld)) then
     write (6,F00) subName//' ERROR both rfld and ifld should not be sent'
   endif
   if (.not.present(rfld).and..not.present(ifld)) then
     write (6,F00) subName//' ERROR both rfld and ifld should not be sent'
   endif

   call ncread_open(fn,fid)

   !--- get variable id and ndims for vid
   rCode = nf90_inq_varid(fid,trim(fldName),vid)
   call handleErr(rCode,subName//" ERROR var "//fldName//" does not exist")

   rCode = nf90_inquire_variable(fid,vid,ndims=ndims)
   call handleErr(rCode,subName//" ERROR inquire variable "//fldName//" ndims")
   if (debug) write(6,F02) "Variable "//fldName//" has ndims = ", ndims

   !--- allocate locals
   n4 = max(ndims,maxd)
   allocate(dimid(n4))  ; dimid = 0
   allocate(dids(n4))   ; dids=0
   allocate(name(n4))   ; name=' '
   allocate(len(n4))    ; len=1
   allocate(start(n4))  ; start=1
   allocate(count(n4))  ; count=1
   allocate(start2d(n4)); start2d=1
   allocate(count2d(n4)); count2d=1

   !--- get dimension info for vid
   rCode = nf90_inquire_variable(fid,vid,dimids=dids)    ! Get dim ID's
   call handleErr(rCode,subName//" ERROR inquire variable "//fldName//" dimIDs")

   do n=1,ndims
     rCode = nf90_inquire_dimension(fid,dids(n),name=name(n),len=len(n))
     call handleErr(rCode,subName//' ERROR inquire dimension len')
     if (debug) then
        write(6,*) "Variable "//fldName//" has dim "//trim(name(n))//" = ", len(n)
     endif
   enddo

   !--- set dimid from dim
   if (present(dim1)) then
     do n=1,ndims
       if (trim(dim1) == trim(name(n))) dimid(1) = n
     enddo
   endif
   if (present(dim2)) then
     do n=1,ndims
       if (trim(dim2) == trim(name(n))) dimid(2) = n
     enddo
   endif
   if (present(dim3)) then
     do n=1,ndims
       if (trim(dim3) == trim(name(n))) dimid(3) = n
     enddo
   endif
   if (present(dim4)) then
     do n=1,ndims
       if (trim(dim4) == trim(name(n))) dimid(4) = n
     enddo
   endif
   if (present(dim5)) then
     do n=1,ndims
       if (trim(dim5) == trim(name(n))) dimid(5) = n
     enddo
   endif
   if (present(dim6)) then
     do n=1,ndims
       if (trim(dim6) == trim(name(n))) dimid(6) = n
     enddo
   endif

   !--- set dimid for non user set dimension based on what's left
   do n1=1,max(maxd,ndims)
     k = 1
     do while (dimid(n1) == 0)
       found = .false.
       do n2 = 1,maxd
         if (dimid(n2) == k) found = .true.
       enddo
       if (found) then
         k = k + 1
       else
         dimid(n1) = k
       endif
     enddo
   enddo

   !--- set count to len if n exists in variable, otherwise set to 1
   do n1=1,maxd
     if (dimid(n1) <= ndims) then
       count(dimid(n1)) = len(dimid(n1))
     else
       count(dimid(n1)) = 1
     endif
   enddo

   !--- modify start and count from user inputs
   if (present(dim1i)) then
     if (dim1i < 1 .or. dim1i > len(dimid(1))) then
        write (6,F00) subName//' ERROR dim1i setting'
        STOP
     endif
     start(dimid(1)) = dim1i
     count(dimid(1)) = 1
   endif
   if (present(dim2i)) then
     if (dim2i < 1 .or. dim2i > len(dimid(2))) then
        write (6,F00) subName//' ERROR dim2i setting'
        STOP
     endif
     start(dimid(2)) = dim2i
     count(dimid(2)) = 1
   endif
   if (present(dim3i)) then
     if (dim3i < 1 .or. dim3i > len(dimid(3))) then
        write (6,F00) subName//' ERROR dim3i setting'
        STOP
     endif
     start(dimid(3)) = dim3i
     count(dimid(3)) = 1
   endif
   if (present(dim4i)) then
     if (dim4i < 1 .or. dim4i > len(dimid(4))) then
       write (6,F00) subName//' ERROR dim4i setting'
       STOP
     endif
     start(dimid(4)) = dim4i
     count(dimid(4)) = 1
   endif
   if (present(dim5i)) then
     if (dim5i < 1 .or. dim5i > len(dimid(5))) then
       write (6,F00) subName//' ERROR dim5i setting'
       STOP
     endif
     start(dimid(5)) = dim5i
     count(dimid(5)) = 1
   endif
   if (present(dim6i)) then
     if (dim6i < 1 .or. dim6i > len(dimid(6))) then
       write (6,F00) subName//' ERROR dim6i setting'
       STOP
     endif
     start(dimid(6)) = dim6i
     count(dimid(6)) = 1
   endif

   !--- error check, fld size must match variable size
   do n=1,maxd
     if (present(rfld)) then
       if (size(rfld,n) /= count(dimid(n))) then
         write (6,F00) subName//' ERROR fld size does not agree with count'
         STOP
       endif
     endif
     if (present(ifld)) then
       if (size(ifld,n) /= count(dimid(n))) then
         write(6,F00) subName//' ERROR fld size does not agree with count'
         STOP
       endif
     endif
   enddo

   !--- fill fld, prepare both int and real arrays, just in case
   !--- use rin/iin and transpose if needed
   if (dimid(1) > dimid(2)) then
     allocate(rin(count(dimid(2)),count(dimid(1))))
     allocate(iin(count(dimid(2)),count(dimid(1))))
   else
     allocate(rin(count(dimid(1)),count(dimid(2))))
     allocate(iin(count(dimid(1)),count(dimid(2))))
   endif
   start2d = start
   count2d = count
   count2d(dimid(3)) = 1
   count2d(dimid(4)) = 1
   do n4 = 1,count(dimid(4))
   do n3 = 1,count(dimid(3))
     start2d(dimid(3)) = n3 + start(dimid(3)) - 1
     start2d(dimid(4)) = n4 + start(dimid(4)) - 1
     if (present(rfld)) then
       rCode = nf90_get_var(fid,vid,rin,start=start2d,count=count2d)
     elseif (present(ifld)) then
       rCode = nf90_get_var(fid,vid,iin,start=start2d,count=count2d)
     endif
     call handleErr(rCode,subName//' ERROR get var')

     if (debug_local > 1) then
       write(6,*) subName,' size rfld',size(rfld,1),size(rfld,2), &
                                       size(rfld,3),size(rfld,4)
       write(6,*) subName,' size ifld',size(ifld,1),size(ifld,2), &
                                       size(ifld,3),size(ifld,4)
       write(6,*) subName,' size rin',size(rin,1),size(rin,2)
       write(6,*) subName,' size iin',size(iin,1),size(iin,2)
       write(6,*) subName,' dimid ',dimid
       write(6,*) subName,' start ',start
       write(6,*) subName,' count ',count
       write(6,*) subName,' start2d ',start2d
       write(6,*) subName,' count2d ',count2d
       write(6,*) subName,' min/max rin ',minval(rin),maxval(rin)
       write(6,*) subName,' min/max iin ',minval(iin),maxval(iin)
     endif
     do n2 = 1,count(dimid(2))
       do n1 = 1,count(dimid(1))
         if (dimid(1) > dimid(2)) then
             if (present(rfld)) then
               rfld(n1,n2,n3,n4) = rin(n2,n1)
             elseif (present(ifld)) then
               ifld(n1,n2,n3,n4) = iin(n2,n1)
             endif
         else
             if (present(rfld)) then
               rfld(n1,n2,n3,n4) = rin(n1,n2)
             elseif (present(ifld)) then
               ifld(n1,n2,n3,n4) = iin(n1,n2)
             endif
         endif
       enddo
     enddo
   enddo
   enddo
   deallocate(rin)
   deallocate(iin)

   deallocate(dimid)
   deallocate(dids)
   deallocate(start)
   deallocate(count)
   deallocate(name)
   deallocate(len)
   deallocate(start2d)
   deallocate(count2d)

   rCode = nf90_close(fid)
   call handleErr(rCode, subName//" ERROR closing input data file")

   if (present(rc)) rc = rCode

end subroutine ncread_field4dG

!===============================================================================
!BOP ===========================================================================
!
! !IROUTINE: handleErr -- Print netCDF error message
!
! !DESCRIPTION:
!   Print the error message corresponding to the netCDF error status
!
! \newline
! General Usage:
!   call handleErr(rCode,'Error message')
! \newline

! !REVISION HISTORY:
!     2005-Aug-22 - J. Schramm - first version
!
! !INTERFACE: ------------------------------------------------------------------

subroutine handleErr(rCode, str)

   implicit none

! !INPUT/OUTPUT PARAMETERS:

   integer(INT) ,intent (in) :: rCode
   character(*) ,intent (in) :: str

!EOP

   !----- formats -----
   character(*),parameter :: F00     = "('(handleErr) ',4a)"

   if (rCode /= nf90_noerr) then
      write(6,F00) "netCDF error: ",trim(nf90_strerror(rCode))
      call flush(6)
      STOP
   end if

end subroutine handleErr

!=======================================================================

end module WRF_ncread

!=======================================================================

