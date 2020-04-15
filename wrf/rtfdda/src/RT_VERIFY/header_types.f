MODULE header_types
    TYPE Location
      REAL :: lat,lon
      CHARACTER (LEN=40) :: desc1,desc2
    END TYPE Location

    TYPE Info
      CHARACTER (LEN=40) :: platform,src
      REAL :: elev
      INTEGER, dimension(5) :: iflags
      LOGICAL, dimension(3) :: lflags
    END TYPE Info

    TYPE Time
      INTEGER :: time1,time2
      CHARACTER (LEN=20) :: time_str
    END TYPE Time

    TYPE DAT
      REAL :: val
      INTEGER :: qc_flag
    END TYPE DAT

    TYPE Terrestrial
      TYPE(DAT), dimension(13) :: obs
    END TYPE Terrestrial

    TYPE Header
      TYPE(Location) :: loc
      TYPE(Info) :: name_src
      TYPE(Time) :: time_tag
      TYPE(Terrestrial) :: ground
    END TYPE Header

END MODULE header_types
