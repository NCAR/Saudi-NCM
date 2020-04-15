      PROGRAM dates
      INTEGER ccyymmddhh,ccyy,mmddhh,mm,dd,hh,dh

      READ *, ccyymmddhh, dh
      ccyy   = ccyymmddhh / 1000000
      mmddhh = MOD ( ccyymmddhh , 1000000 )
      mm     = mmddhh / 10000
      dd     = MOD ( mmddhh , 10000 ) / 100
      hh     = MOD ( mmddhh , 100 )

      hh = hh + dh
   10 IF ( hh .LT. 0 ) THEN
         hh = hh + 24
         CALL change_date ( ccyy, mm, dd, -1 )
      ELSEIF ( hh .GT. 23 ) THEN
         hh = hh - 24
         CALL change_date ( ccyy, mm, dd, 1 )
      ELSE
         WRITE (*,'(I4.4,3I2.2)') ccyy,mm,dd,hh
         STOP
      ENDIF
      GOTO 10
      END
!
      SUBROUTINE change_date ( ccyy, mm, dd, delta )
      INTEGER ccyy, mm, dd, delta
      INTEGER mmday(12)
      DATA    mmday/31,28,31,30,31,30,31,31,30,31,30,31/

      mmday(2) = 28
      IF ( MOD(ccyy,4) .EQ. 0 ) THEN
         IF     ( MOD(ccyy,400) .EQ. 0 ) THEN
            mmday(2) = 29
         ELSEIF ( MOD(ccyy,100) .NE. 0 ) THEN
            mmday(2) = 29
         ENDIF
      ENDIF

      dd = dd + delta
      IF ( dd .EQ. 0 ) THEN
         mm = mm - 1
         IF ( mm .EQ. 0 ) THEN
            mm = 12
            ccyy = ccyy - 1
         ENDIF
         dd = mmday(mm)
      ELSEIF ( dd .GT. mmday(mm) ) THEN
         dd = 1
         mm = mm + 1
         IF ( mm .GT. 12 ) THEN
            mm = 1
            ccyy = ccyy + 1
         ENDIF
      ENDIF
      RETURN
      END
