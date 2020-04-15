MODULE Meas_types

    use header_types, only : DAT

    TYPE meas
      TYPE(DAT), dimension(10) :: obs
    END TYPE meas 

END MODULE Meas_types
