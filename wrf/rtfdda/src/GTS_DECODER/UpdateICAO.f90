!------------------------------------------------------------------------------!
! Merge (remove duplicates, add new, update old) ICAO station location 
!   files, for information purposes only
! Generates 3 files with: (1) new station locations, (2) moved stations, and
!   (3) stations with changed elevation
! Information files above can then be used to plot usig NCL script 
!   PlotStations.ncl (ncl World=True/False PlotStations.ncl)
!
! NOTE: Code only seems to work when compiled under Sarge!!
!       So, if "Segmentation Fault" occurs, move to sarge system and 
!       recompile.
!
! Andrea HAHMANN, June 2006
! hahmann@ucar.edu
! Copyright UCAR [RAP] 1996 - 2006. All Rights Reserved.
!------------------------------------------------------------------------------!
program UpdateGTS

  implicit none
  integer, external :: iargc
  integer :: j

  integer, parameter :: imax = 26*36*36*36
  character :: icao_id*4
  character :: cntry_id*2
  character :: city_id*3
  character :: descrptn*60
  character :: cntry_nam*12
  character :: stn_lat*10, stn_1*3, stn_2*2, stn_3*2
  character :: stn_lng*10, clat*1, clng*1
  character :: stn_height*7
  integer, parameter                :: llen = 100
  character ( len =  6 ), parameter :: fmt = '(a100)'
  integer                           :: i, height, lat, lon, ikey

  character (len=255) :: line
  character (len=132) :: exename, File1, File2
  character (len=llen) :: newline,blkline
  character ( len = 20 ), parameter :: cfmt = '(i4,1x,a60)'
  
  type StationRecord
     character(len=4) :: ICAOid 
     integer :: lat, lon, elev
     character(len=60) :: description
     character(len=12) :: country
  end type StationRecord
  type List_Node
     type(StationRecord) :: Data
     type(List_Node), pointer :: Next
  end type List_Node
  type(List_Node), pointer :: StatList, TempPtr
  type(StationRecord) :: NewStat

  integer :: k, nobs=0, nmoved=0, nupdown=0
  logical :: iprint = .false.

  if (iargc() < 2) then
    call getarg(0,exename)
    print*,'Usage: ',TRIM(exename),' File1 File2'
    stop
  endif

  call getarg(1,File1)
  call getarg(2,File2)

  open(unit=10,file=File1)            ! read all stations from file1

  read_file1: do  !!k=1,20
     read ( 10, '(A)', end=99 ) line
     nobs = nobs + 1

     CALL strcut ( icao_id,    line, ';',  1 )
     CALL strcut ( cntry_id,   line, ';',  2 )
     CALL strcut ( city_id,    line, ';',  3 )
     CALL strcut ( descrptn,   line, ';',  4 )
     CALL strcut ( cntry_nam,  line, ';',  6 )
     CALL strcut ( stn_lat,    line, ';',  8 )
     CALL strcut ( stn_lng,    line, ';',  9 )
     CALL strcut ( stn_height, line, ';', 12 )

     NewStat%ICAOid = icao_id

     i = len_trim(stn_lat)
     clat    = stn_lat(i:i)
     stn_lat = stn_lat(:i-1)
     stn_1 = ""
     stn_2 = ""
     stn_3 = ""
     call strcut ( stn_1, stn_lat, '-', 1 )
     call strcut ( stn_2, stn_lat, '-', 2 )
     call strcut ( stn_3, stn_lat, '-', 3 )
     if ( stn_3 .eq. "" ) then
        lat = str2int( stn_1 )*100. + str2int(stn_2)*100./60.
     else
        lat = str2int( stn_1 )*100. + str2int(stn_2)*100./60.+ str2int(stn_3)*100./3600.
     endif
     if ( clat .eq. 's' ) then
        lat = - lat 
     endif
     NewStat%lat = lat

     i = len_trim(stn_lng)
     clng    = stn_lng(i:i)
     stn_lng = stn_lng(:i-1)
     stn_1 = ""
     stn_2 = ""
     stn_3 = ""
     call strcut ( stn_1, stn_lng, '-', 1 )
     call strcut ( stn_2, stn_lng, '-', 2 )
     call strcut ( stn_3, stn_lng, '-', 3 )
     if ( stn_3 .eq. "" ) then
        lon = str2int( stn_1 )*100. + str2int(stn_2)*100./60.
     else
        lon = str2int( stn_1 )*100. + str2int(stn_2)*100./60. &
            + str2int(stn_3)*100./3600.
     endif
     if ( clng .eq. 'w' ) then
        lon = - lon
     endif
     NewStat%lon = lon

     NewStat%elev = str2int(stn_height)
     NewStat%country = cntry_nam

     call Add_to_list(StatList,NewStat,nobs,nmoved,nupdown,iprint)
  end do read_file1

99 print *,'Total number of obs in file 1:',nobs
  close(10)

  open(unit=20,file=File2)

  iprint = .true.
  open(unit=44,file='NewStations.dat')
  open(unit=55,file='MovedStations.dat')
  open(unit=66,file='UpDownStations.dat')

  nobs = 0
  read_file2: do !!k=1,100
     read (20, '(A)', end=98 ) line
     nobs = nobs + 1

     CALL strcut ( icao_id,    line, ';',  1 )
     CALL strcut ( cntry_id,   line, ';',  2 )
     CALL strcut ( city_id,    line, ';',  3 )
     CALL strcut ( descrptn,   line, ';',  4 )
     CALL strcut ( cntry_nam,  line, ';',  6 )
     CALL strcut ( stn_lat,    line, ';',  8 )
     CALL strcut ( stn_lng,    line, ';',  9 )
     CALL strcut ( stn_height, line, ';', 12 )

     NewStat%ICAOid = icao_id
     i = len_trim(stn_lat)
     clat    = stn_lat(i:i)
     stn_lat = stn_lat(:i-1)
     stn_1 = ""
     stn_2 = ""
     stn_3 = ""
     call strcut ( stn_1, stn_lat, '-', 1 )
     call strcut ( stn_2, stn_lat, '-', 2 )
     call strcut ( stn_3, stn_lat, '-', 3 )
     if ( stn_3 .eq. "" ) then
        lat = str2int( stn_1 )*100. + str2int(stn_2)*100./60.
     else
        lat = str2int( stn_1 )*100. + str2int(stn_2)*100./60.+ str2int(stn_3)*100./3600.
     endif
     if ( clat .eq. 's' ) then
        lat = - lat 
     endif
     NewStat%lat = lat

     i = len_trim(stn_lng)
     clng    = stn_lng(i:i)
     stn_lng = stn_lng(:i-1)
     stn_1 = ""
     stn_2 = ""
     stn_3 = ""
     call strcut ( stn_1, stn_lng, '-', 1 )
     call strcut ( stn_2, stn_lng, '-', 2 )
     call strcut ( stn_3, stn_lng, '-', 3 )
     if ( stn_3 .eq. "" ) then
        lon = str2int( stn_1 )*100. + str2int(stn_2)*100./60.
     else
        lon = str2int( stn_1 )*100. + str2int(stn_2)*100./60. &
            + str2int(stn_3)*100./3600.
     endif
     if ( clng .eq. 'w' ) then
        lon = - lon
     endif
     NewStat%lon = lon

     NewStat%elev = str2int(stn_height)
     NewStat%country = cntry_nam

     call Add_to_list(StatList,NewStat,nobs,nmoved,nupdown,iprint)
     
  end do read_file2
98 print *,'Total number of obs in file 2:',nobs
  close(20)
  close(44)
  close(55)
  close(66)

  print *,'Total number of stations moved:', nmoved
  print *,'Total number of stations with diff elev:', nupdown


!!  call Output_StatList(StatList)

contains

! ------------------------------------------------------------
! This subroutine determines if NewStat is already in the linked
!   list StatList (using search).  If it is not, it is added 
!   in ascending order of station ID
! ------------------------------------------------------------

subroutine Add_to_List(StatList, NewStat, nobs, nmoved, nupdown, iprint)

  type(List_Node), pointer :: StatList
  type(StationRecord) :: NewStat
  integer :: AllocateStatus
  integer, intent(inout) :: nobs, nmoved, nupdown
  type(List_Node), pointer :: PredPtr, CurrPtr, TempPtr
  logical :: in_the_list, iprint
  real, save :: R = 6372.795, pi= 3.14159625, deg2rad
  real :: dist, olat, olon, nlat, nlon

  deg2rad = pi/(100.*180.)

  if (.not.associated(StatList)) then     ! List is empty

     allocate(StatList, STAT=AllocateStatus)
     if (AllocateStatus /= 0) stop '*** Not enough memory ***'

     StatList%Data = NewStat
     nullify(StatList%Next)
     
  else                                    ! List is not empty

     call search(StatList, NewStat, PredPtr, CurrPtr, in_the_list)
  
     if (in_the_list) then     ! already in the list, 
                               ! check if the loc/elev is the same
     
!        print '(a,i5.5,a)', 'Station ', NewStat%ICAOid, &
!             ' already in the list, skip'

        if (CurrPtr%Data%lat /= NewStat%lat .or. &
             CurrPtr%Data%lon /= NewStat%lon) then

           olat = deg2rad*CurrPtr%Data%lat
           nlat = deg2rad*NewStat%lat
           olon = deg2rad*CurrPtr%Data%lon
           nlon = deg2rad*NewStat%lon

           dist = 2.*asin(sqrt(sin((olat-nlat)/2.)**2 + &
                cos(olat)*cos(nlat) * sin((olon-nlon)/2.)**2))
           dist = R*dist

           print '(a,2(a4,2x),4i8,f10.2)','Station has moved...', &
                CurrPtr%Data%ICAOid, NewStat%ICAOid, &
                CurrPtr%Data%lat,NewStat%lat, &
                CurrPtr%Data%lon,NewStat%lon, dist

           if (iprint) write(55,'(5i9)') &
                CurrPtr%Data%lat,NewStat%lat, &
                CurrPtr%Data%lon,NewStat%lon, int(1000*dist)

           CurrPtr%Data%lat = NewStat%lat
           CurrPtr%Data%lon = NewStat%lon
           nmoved = nmoved + 1

        end if

        if (CurrPtr%Data%elev /= NewStat%elev) then
           print '(a,2x,a4,2x,a4,2x,2i8)','Station has diff elevation...', &
                CurrPtr%Data%ICAOid, NewStat%ICAOid, &
                CurrPtr%Data%elev,NewStat%elev

           if (iprint) write(66,'(4i8)') &
                NewStat%lat, NewStat%lon, &
                CurrPtr%Data%elev,NewStat%elev
           
           CurrPtr%Data%elev = NewStat%elev
           nupdown = nupdown + 1
        end if

        nobs = nobs - 1
     
     else     ! not in list

        allocate(TempPtr, STAT=AllocateStatus)
        if (AllocateStatus /= 0) stop '*** Not enough memory ***'
        TempPtr%Data = NewStat

        if (iprint) write(44,'(3i8)') &
             NewStat%lat, NewStat%lon, &
             NewStat%elev

        if (associated(CurrPtr,StatList)) then ! Add to start of list

           print *, 'Add NewStat to beg of list'
           TempPtr%Next => StatList
           StatList => TempPtr

        else                          ! Add between PredPtr and CurrPtr
        
!           print *,'Add ',NewStat%ICAOid,' after ', &
!                PredPtr%Data%ICAOid

           TempPtr%Next => CurrPtr
           PredPtr%Next => TempPtr

        end if

     end if
     
  end if

end subroutine Add_to_List

! ------------------------------------------------------------
! This subroutine searches StatList for a node containing the 
!   station ID
! If it is found, CurrPtr points to the node and in_the_list is
!   set to True; 
! otherwise PredPtr and CurrPtr are set to the predecessor and
!   current node and in_the_list is set to False
! ------------------------------------------------------------
        
Subroutine search(StatList, NewStat, PredPtr, CurrPtr, in_the_list)

  type(List_Node), pointer :: StatList, PredPtr, CurrPtr
  type(StationRecord) :: NewStat
  logical, intent(out) :: in_the_list

  ! Initialize pointers to first item of list

  print *,NewStat
  CurrPtr => StatList
  PredPtr => StatList
  in_the_list = .False.

  ! Traverse the list until the Station ID is found
  ! or the end of the list is encountered

  do
     if ( in_the_list .or. .not. associated(CurrPtr) ) exit

     if (CurrPtr%Data%ICAOid == NewStat%ICAOid) then  ! station found
        in_the_list = .True.
     else
        if (CurrPtr%Data%ICAOid > NewStat%ICAOid) exit  ! went past station
        PredPtr => CurrPtr
        CurrPtr => CurrPtr%next              ! move to next node
     end if

  end do

end Subroutine search

subroutine Output_StatList(StatList)

  type(List_Node), pointer :: StatList, Ptr
  INTEGER, PARAMETER                :: llen = 100
  INTEGER                           :: iout1=31
  CHARACTER ( LEN = llen )          :: newline
  CHARACTER                         :: blk = ' '
  CHARACTER :: id*5, region*1
  CHARACTER :: call_letter*5 = '_WMO_'
  CHARACTER :: latitude*6
  CHARACTER :: longitude*7
  CHARACTER :: altitude*6

  Ptr => StatList

  ! print out list information until end of list reached

  OPEN ( iout1, FILE = 'gts_sttnid_input.wmo.txt' )
  do 
     if (.not. associated(Ptr)) exit   ! end of list reached
     
     write(id,'(i5.5)') Ptr%Data%ICAOid
     write(latitude,'(i5.4,"N")') Ptr%Data%lat
     write(longitude,'(i6.5,"E")') Ptr%Data%lon
     write(altitude,'(1x,i4.4,"m")') Ptr%Data%elev

     newline = id // blk // latitude// blk // longitude// blk // &
          altitude// blk // region// blk // call_letter// blk // &
          Ptr%Data%description

     WRITE ( iout1, fmt ) newline

!     print '(i5.5, 3f10.2)', Ptr%Data%StatID, Ptr%Data%lat/100., &
!          Ptr%Data%lon/100., 1.0*Ptr%Data%elev
     Ptr => Ptr%next
  end do

end subroutine Output_StatList

FUNCTION str2int ( string )
  IMPLICIT NONE
  INCLUDE 'inc.special_symbols'
  CHARACTER ( LEN = * ), INTENT ( IN ) :: string
  INTEGER                               :: str2int
  CHARACTER ( LEN = 4 )                 :: fmt
  CHARACTER ( LEN = len ( string ) )    :: tmp_string
  
  fmt = '(I' // ACHAR ( IACHAR ( '0' ) + LEN ( string ) ) // ')'
  tmp_string = ADJUSTR ( string )
  READ ( tmp_string, fmt, ERR = 10 ) str2int
  RETURN
  
10 CONTINUE
  IF ( string .EQ. repeat ( '/', len ( string ) ) ) THEN
     str2int = MISSING
  ELSE
     str2int = UNDEFINED
  ENDIF
  
ENDFUNCTION str2int
! ----------------------------------------------------------------------

FUNCTION icao_key ( id )

   IMPLICIT NONE
   CHARACTER ( LEN = * ), INTENT ( IN ) :: id
   INTEGER                              :: icao_key, ia
   INTEGER, PARAMETER     :: imax = 26*36*36*36
   CHARACTER ( LEN = 36 ) :: cset = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789'

   ia = VERIFY ( id, cset )
   if ( ia .NE. 0 ) THEN
      icao_key = 0
   else
      icao_key = (SCAN(cset,id(1:1))-1)*26*36*36 + &
                 (SCAN(cset,id(2:2))-1)   *36*36 + &
                 (SCAN(cset,id(3:3))-1)      *36 + &
                 (SCAN(cset,id(4:4))-1)          + 1
   ENDIF
   
   if ( icao_key .GT. imax ) icao_key = 0

ENDFUNCTION icao_key
! ----------------------------------------------------------------------

SUBROUTINE strcut ( strout, strin, delimitor, key )

   ! equivalent to
   !    set strout = `echo $strin | cut -ddelimitor -fkey
   !
   ! Created : May 22, 1995    Alexis Lau (HKUST/NCAR)

   IMPLICIT NONE
   CHARACTER ( LEN = * ), INTENT ( OUT ) :: strout
   CHARACTER ( LEN = * ), INTENT ( IN  ) :: strin
   CHARACTER ( LEN = 1 ), INTENT ( IN  ) :: delimitor
   INTEGER                               :: key, i, j
   CHARACTER ( LEN = len ( strin ) )     :: strtmp

   strtmp = strin
   i = 0
   strout = ""
   DO WHILE ( i < key ) 
      i = i + 1
      j = scan ( strtmp, delimitor )
      IF ( j > 0 ) THEN
         strout = strtmp(:j-1)
         strtmp = strtmp(j+1:)
      ELSE 
         if ( i .eq. 1 ) then
           strout = strin
         else if ( i .eq. key ) then
           strout = strtmp
         else
           strout = ""
         endif
         EXIT
      ENDIF
   ENDDO

ENDSUBROUTINE strcut

end program UpdateGTS
