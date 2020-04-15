MODULE write_headers

use WRF_constants
use time_routines
use WRF_tools

CONTAINS

      subroutine writeYPG2_header(ounit,sname,lat,lon,elev,date_time)
!
      implicit none

      integer, intent(in) :: ounit
      character(*), intent(in) :: sname
      real*8, intent(in) :: lat, lon, elev
      character(*), intent(in) :: date_time

      integer :: ielev,ielev_ft
      character(2) :: hh,mn,mm,dd
      character(4) :: yyyy
      character(1) :: tab=char(9)

      ielev = nint(elev)    ! in m
      ielev_ft = nint(elev*m2ft) ! in ft

      hh = date_time(12:13)
      mn = '00'
      mm = date_time(6:7)
      dd = date_time(9:10)
      yyyy = date_time(1:4)

      write(ounit,'(33x,a)') "0000L FLIGHT"
      write(ounit,'(78a)') repeat('-',78)
      write(ounit,'(6a)') "Station Name:",sname(1:6),tab,tab, &
            "Radiosonde Type:",tab
      write(ounit,'("Latitude:",f10.2,2a,"Radiosonde Number:",a)') lat, &
            tab,tab,tab
      write(ounit,'("Longitude:",f9.2,2a,"Altitude:",i4," feet", &
            x,"(",i4,x,"Meters)",/)') lon, &
            tab,tab,ielev_ft,ielev
      write(ounit,'("Ground Check Corrections")')
      write(ounit,'("Pressure:",3a,"RH1:",a)') tab,tab,tab,tab
      write(ounit,'("Temperature:",3a,"RH2:",a,/)') tab,tab,tab,tab
      write(ounit,'("Launch Time:",x,a2,":",a2,x,"UTC",2a, &
                    "Sounding Started on:",x,a2,"/",a2,"/",a4,/)') &
           hh,mn,tab,tab,mm,dd,yyyy
      write(ounit,'(78a)') repeat('-',78)
      write(ounit,'(39x,a)') "Dew Pt     Air       Wind       Wind"
      write(ounit,'("   Altitude     Press     Temp    RH    Temp    Density    Direct     Speed")')
      write(ounit,'(" (FT above msl)  (MB)    ( °F)    (%)   (°F)    (g/m^3)     (degs)     (Kts)")')

      write(ounit,'(78a)') repeat('-',78)

      return

      end subroutine writeYPG2_header
!
!
!
      subroutine writeREDI_header(ounit,sname,elev,rnge,date_time)

      implicit none

      integer, intent(in) :: ounit
      character(*), intent(in) :: sname
      real*8, intent(in) :: elev
      character(*) :: rnge
      character(*), intent(in) :: date_time

      integer :: ielev
      character(2) :: hh,mn,mm,dd
      character(4) :: yyyy
      character(2) :: yy
      character(10) :: date_time_compact, date_time_new ! YYYYMMDDHH

      integer :: imm
      integer :: gmt2lst_offset = -7 ! hours

      character(3), dimension(12) :: amonths=(/'JAN','FEB','MAR','APR', &
      'MAY','JUN','JUL','AUG','SEP','OCT','NOV','DEC'/)

      ielev = nint(elev)    ! in m

      hh = date_time(12:13)
      dd = date_time(9:10)
      mm = date_time(6:7)
      yyyy = date_time(1:4)

      date_time_compact = yyyy//mm//dd//hh

      rnge = upper(rnge)
      if (trim(rnge) == 'YPG') then
         call adjust_hour(date_time_compact,gmt2lst_offset,date_time_new)
      else
         date_time_new = date_time_compact
      endif

      mn = '00'
      hh = date_time_new(9:10)
      dd = date_time_new(7:8)
      mm = date_time_new(5:6)
      yy = date_time_new(3:4)

      read(mm,'(i2)') imm

      write(ounit,'("DATE:",a,"/",a,"/",a,a,a,3x,2a,i7)') dd, &
           amonths(imm), yy, trim(rnge),sname(1:4), hh, mn, ielev

      return

      end subroutine writeREDI_header

END MODULE write_headers
